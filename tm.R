install.packages("devtools")
install.packages("dplyr")
install.packages("SnowballC")
install.packages("tm")
install.packages("RColorBrewer")
install.packages("wordcloud")
#Carga de datos en Dataframes con Spark

#Iniciamos sesión en Spark

if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = "/Users/joseadiazg/spark-2.2.0-bin-hadoop2.7")
}
library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
sparkR.session(master = "local[*]", sparkConfig = list(spark.driver.memory = "6g"))


#Cargamos los datos en dataframes

tweets <- read.json(c("/Users/joseadiazg/Desktop/data/filterdata/enerofilter.json", "/Users/joseadiazg/Desktop/data/filterdata/febrerofilter.json",
                      "/Users/joseadiazg/Desktop/data/filterdata/mayofilter.json","/Users/joseadiazg/Desktop/data/filterdata/juniofilter.json"))

#Si solo queremos cargar uno

#tweets <- read.df("/Users/joseadiazg/Desktop/output.json", "json")

#Filtramos los que no sean RT ya que los RT estan repetidos y pueden hacernos falsear el modelo

head(filter(tweets, tweets$is_retweet==FALSE))

#Cargamos los datos en un dataframe filtrado

#TODO: consultas sql para reducir la dimensionalidad. 

filterdf<-filter(tweets, tweets$is_retweet==FALSE)

#Traemos los datos de la sesion Spark a una sesión en local

localdf<-collect(tweets)

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

tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
m <- as.matrix(tdm)

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
