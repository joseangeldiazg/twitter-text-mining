
install.packages(c("NLP", "openNLP", "RWeka", "qdap","devtools","dplyr"))
install.packages(c("SnowballC", "tm", "RColorBrewer", "wordcloud"  ))

#**********************************************************
#Carga de datos en Dataframes con Spark
#**********************************************************

#Iniciamos sesión en Spark

if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = "/Users/joseadiazg/spark-2.2.0-bin-hadoop2.7")
}

library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
sparkR.session(master = "local[*]", sparkConfig = list(spark.driver.memory = "6g"))


#Cargamos los datos en dataframes

tweets <- read.json(c("/Users/joseadiazg/Desktop/data/enero.json", "/Users/joseadiazg/Desktop/data/febrero.json",
                      "/Users/joseadiazg/Desktop/data/mayo.json","/Users/joseadiazg/Desktop/data/junio.json",
                      "/Users/joseadiazg/Desktop/data/julio.json"))

#Filtramos los que no sean RT ya que los RT estan repetidos y pueden hacernos falsear el modelo

head(filter(tweets, tweets$is_retweet==FALSE))

#Cargamos los datos en un dataframe filtrado

filterdf<-filter(tweets, tweets$is_retweet==FALSE)

#TODO: Un RT puede ser considerado a favor en la mayoria de los casos, por ello,
#queda pendiente una versión en la que tendrán mas peso estos "documentos".

#Traemos los datos de la sesion Spark a una sesión en local

localdf<-collect(tweets)

#Creamos un nuevo dataframe con todos aquellos Tuits que no son RT

noretuits<-collect(filterdf)

#***********************************************************
#Minería de textos
#***********************************************************

library(tm)

#Construimos un conjunto de datos con el texto de los Tuits

myCorpus <- Corpus(VectorSource(localdf$text))

#Pasamos todos a minuscula 

myCorpus <- tm_map(myCorpus, content_transformer(tolower))

#Borramos URLS que no tienen ningun sentido en nuestro proceso de minado

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

# Borramos caracteres raros tales como emojis o caracteres no alfabéticos

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

# Eliminamos stop words en ingles

# Añadimos la palabra "via" ya que se usa para referenciar usuarios en tweeter

myStopwords <- c(setdiff(stopwords('english'), c("via")))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# Borramos los espacios extra

myCorpus <- tm_map(myCorpus, stripWhitespace)

#Mantenemos una copia

myCorpusCopy <- myCorpus

#******************************************
# STEAMING
#******************************************

myCorpus <- tm_map(myCorpus, stemDocument)

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary) 
  x <- paste(x, sep="", collapse=" ") 
  PlainTextDocument(stripWhitespace(x))
}

myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
myCorpus <- Corpus(VectorSource(myCorpus))

#****************************************
# Obtenemos la matriz de frecuencias
#****************************************

#Matriz de frecuencias con el proceso de steaming

tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm)

#Matriz de frecuencias sobre todo el contenido

tdmAll <- TermDocumentMatrix(myCorpusCopy,control = list(wordLengths = c(1, Inf)))
mAll <- as.matrix(tdmAll)

#***************************************
# Pintamos la matriz de nube de terminos
#***************************************

library(RColorBrewer)
library(wordcloud)

#Con la nube de terminos podemos hacernos una idea de que se hablaba
#en Twitter durante estos meses.

word.freq <- sort(rowSums(m), decreasing = T)
pal <- brewer.pal(9, "BuGn")[-(1:4)]

#wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,random.order = F, colors = pal)

#Tenemos 1496370 tuits, para poder pintar la nube de palabras y hacernos una idea de lo que se 
#cuece en twitter durante esos meses, nos quedaremos con las palabras que al menos aparecen en
#1000 tuits

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 1000,random.order = F, colors = pal)

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 2000,random.order = F, colors = pal)

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3000,random.order = F, colors = pal)



#***************************************
# Name Entity Recognition
#***************************************

#Vamos a obtener los tuits de los que se habla de personas físicas

library(NLP)
library(openNLP)
library(magrittr)


#Tenemos que crear un corpus con el texto de los tuits

tuitstext <- myCorpus %>%
  lapply(paste0, collapse = " ") %>%
  lapply(as.String)

head(tuitstext)


annotate_entities <- function(doc, annotation_pipeline) {
  annotations <- annotate(doc, annotation_pipeline)
  AnnotatedPlainTextDocument(doc, annotations)
}


itinerants_pipeline <- list(
  Maxent_Sent_Token_Annotator(),
  Maxent_Word_Token_Annotator(),
  Maxent_Entity_Annotator(kind = "person"))


