#**********************************************************
#Instalación de los paquetes necesarios
#**********************************************************
install.packages(c("NLP", "openNLP", "RWeka", "qdap","devtools","dplyr"))
install.packages(c("SnowballC", "tm", "RColorBrewer", "wordcloud"  ))
install.packages("openNLPmodels.en",
                 repos = "http://datacube.wu.ac.at/",
                 type = "source")
install.packages("doParallel")

#*********************************
# Cargamos librerias
#*********************************
library(tm)
library(RColorBrewer)
library(wordcloud)
library(NLP)
library(openNLP)
library(magrittr)
library(foreach)
library(doParallel)
library(rJava)
library(arules)
library(plyr)

#*********************************
# Opciones
#*********************************

options(java.parameters = "-Xmx8000m")

jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}  

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

#Traemos los datos de la sesion Spark a una sesión en local

localdf<-collect(tweets)

#Creamos un nuevo dataframe con todos aquellos Tuits que no son RT

noretuits<-collect(filterdf)

#***********************************************************
#Limpieza de datos
#***********************************************************

#Construimos un conjunto de datos con el texto de los Tuits

myCorpus <- Corpus(VectorSource(localdf$text))

#Borramos URLS que no tienen ningún sentido en nuestro proceso de minado
#Añadimos expresiones regulares para las principales redes sociales 

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
removePics <- function(x) gsub("pictwit[^[:space:]]*", "", x)
removeSmartURL <- function(x) gsub("smarturl[^[:space:]]*", "", x)
removeTwitter <- function(x) gsub("twittercom[^[:space:]]*", "", x)
removeYoutube <- function(x) gsub("youtubecom[^[:space:]]*", "", x)
removeYoutube2 <- function(x) gsub("youtube[^[:space:]]*", "", x)
removeFb <- function(x) gsub("fbme[^[:space:]]*", "", x)
removeBitly <- function(x) gsub("bitly[^[:space:]]*", "", x)
removeInstagram <- function(x) gsub("instagramcom[^[:space:]]*", "", x)
removeVine <- function(x) gsub("vineco[^[:space:]]*", "", x)

myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
myCorpus <- tm_map(myCorpus, content_transformer(removePics))
myCorpus <- tm_map(myCorpus, content_transformer(removeSmartURL))
myCorpus <- tm_map(myCorpus, content_transformer(removeTwitter))
myCorpus <- tm_map(myCorpus, content_transformer(removeYoutube))
myCorpus <- tm_map(myCorpus, content_transformer(removeYoutube2))
myCorpus <- tm_map(myCorpus, content_transformer(removeFb))
myCorpus <- tm_map(myCorpus, content_transformer(removeInstagram))
myCorpus <- tm_map(myCorpus, content_transformer(removeVine))
myCorpus <- tm_map(myCorpus, content_transformer(removeBitly))

# Borramos caracteres raros tales como emojis o caracteres no alfabéticos

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

# Eliminamos stop words en inglés

# Añadimos la palabra "via" ya que se usa para referenciar usuarios en twitter

myStopwords <- c(setdiff(stopwords('english'), c("via")))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# Borramos los espacios extra

myCorpus <- tm_map(myCorpus, stripWhitespace)

#*******************************************************
# Name Entety Recognition
#*******************************************************

#Declaramos una función para obtener los nombres

entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

#Función que realiza el proceso NER

obtieneNombres<-function(tuit, i)
{
  gc()
  jgc()
  person_ann <- Maxent_Entity_Annotator(kind = "person")
  word_ann <- Maxent_Word_Token_Annotator()
  sent_ann <- Maxent_Sent_Token_Annotator()
  pipeline <- list(sent_ann,word_ann,person_ann)
  text<-as.String(tuit)
  annotations <- annotate(text, list(sent_ann, word_ann))
  names_annotations<- annotate(text, pipeline)
  names_doc <- AnnotatedPlainTextDocument(text, names_annotations)
  names<-entities(names_doc, kind = "person")
  return(ifelse(identical(names, character(0)),as.integer(i),names))
}

#*************************************
#PROGRAMACIÓN PARALELA
#*************************************

namesList=list()

cores=detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

t <- proc.time() # Inicia el cronómetro

namesList <-foreach(i=1:1000,
                    .combine=c, 
                    .packages = c("openNLP", "NLP", "tm", "base","rJava")) %dopar% 
                    {
                      oneTweet<-as.String(myCorpus$content[i])
                      obtieneNombres(oneTweet, i)
                    }

stopCluster(cl)

proc.time()-t    # Detiene el cronómetro

#*************************************************
#Limpiamos los tuits que no referencian a personas
#*************************************************

finalExample<-list()

for(i in 1:length(mySmallCorpus))
{
  if(!(namesList[i]==i))
  {
    finalExample<-c(finalExample,mySmallCorpus$content[i])
  }
}

finalExample

#**************************************************
# Creamos un nuevo corpus para aplicar Text Mining
# sobre los tuits que hablan sobre personas.
#**************************************************

finalCorpus <- Corpus(VectorSource(finalExample))

finalCorpus$content[2]

#**************************************************
# Limpiamos de nuevo los datos
#**************************************************

# Hacemos una copia

finalCorpusCopy<-finalCorpus

# Pasamos a minuscula ya que antes no lo habiamos hecho para mejorar el proceso de NER

finalCorpus <- tm_map(finalCorpus, content_transformer(tolower))

# Eliminamos stop words en inglés de nuevo ya el cambio a minuscula puede hacer que encontremos nuevas

myStopwords <- c(setdiff(stopwords('english'), c("via")))
finalCorpus <- tm_map(finalCorpus, removeWords, myStopwords)

# Borramos caracteres raros tales como emojis o caracteres no alfabéticos

finalCorpus <- tm_map(finalCorpus, content_transformer(removeNumPunct))

# Eliminamos stop words en ingles
# Añadimos la palabra "via" ya que se usa para referenciar usuarios en tweeter

myStopwords <- c(setdiff(stopwords('english'), c("via")))
finalCorpus <- tm_map(finalCorpus, removeWords, myStopwords)

# Borramos los espacios extra
finalCorpus <- tm_map(finalCorpus, stripWhitespace)

#Llegados a este punto solo tendremos tuits que hablan de personas