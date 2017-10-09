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
#*********************************
# Limpieza de datos
#*********************************

#Obtenemos los primeros 100000 tuits

mySmallCorpus <- Corpus(VectorSource(localdf$text[1:100000]))

head(mySmallCorpus$content)

#Borramos los espacios extra

mySmallCorpus <- tm_map(mySmallCorpus, stripWhitespace)

#Borramos las URL, tenemos una funcion definida para ello
#Vamos a evitar pasar todo a minuscula, ya que esto puede hacer que el proceso NER empeore

mySmallCorpus <- tm_map(mySmallCorpus, content_transformer(removeURL))
mySmallCorpus <- tm_map(mySmallCorpus, content_transformer(removePics))
mySmallCorpus <- tm_map(mySmallCorpus, content_transformer(removeSmartURL))
mySmallCorpus <- tm_map(mySmallCorpus, content_transformer(removeTwitter))
mySmallCorpus <- tm_map(mySmallCorpus, content_transformer(removeYoutube))
mySmallCorpus <- tm_map(mySmallCorpus, content_transformer(removeYoutube2))
mySmallCorpus <- tm_map(mySmallCorpus, content_transformer(removeFb))
mySmallCorpus <- tm_map(mySmallCorpus, content_transformer(removeInstagram))
mySmallCorpus <- tm_map(mySmallCorpus, content_transformer(removeVine))
mySmallCorpus <- tm_map(mySmallCorpus, content_transformer(removeBitly))

# Borramos caracteres raros tales como emojis o caracteres no alfabéticos
mySmallCorpus <- tm_map(mySmallCorpus, content_transformer(removeNumPunct))

copyMySmallCorpus<-mySmallCorpus

#*******************************************************
# Name Entety Recognition con pocos datos (100000 tuits)
#*******************************************************

#Declaramos las variables para el proceso NER

person_ann <- Maxent_Entity_Annotator(kind = "person")
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann)


#Declaramos una funcion para obtener los nombres

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

obtieneNombres<-function(tuit)
{
  person_ann <- Maxent_Entity_Annotator(kind = "person")
  word_ann <- Maxent_Word_Token_Annotator()
  sent_ann <- Maxent_Sent_Token_Annotator()
  pipeline <- list(sent_ann,word_ann,person_ann)
  text<-as.String(tuit)
  annotations <- annotate(text, list(sent_ann, word_ann))
  names_annotations<- annotate(text, pipeline)
  names_doc <- AnnotatedPlainTextDocument(text, names_annotations)
  names<-entities(names_doc, kind = "person")
}

#Declaramos estructruas de datos para guardar los nombres de cada tuit

listPosition=list()
listName=list()
namesList=list()

#Hacemos name entity recognition sobre cada tuit

t <- proc.time() # Inicia el cronómetro

for(i in 1:10)
{
  oneTweet<-as.String(mySmallCorpus$content[i])
  names<-obtieneNombres(oneTweet)
  if(!(identical(names, character(0))))
  {
    namesList<-c(list(names), namesList)
  }
}

proc.time()-t    # Detiene el cronómetro
#Nos quedamos sin repetidos

namesListUnique<-unique(namesList)

namesListUnique

proc.time()-t    # Detiene el cronómetro





#PROGRAMACIÓN PARALELA

cores=detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

t <- proc.time() # Inicia el cronómetro

namesList <-foreach(i=1:10, 
                    .combine=c, 
                    .packages = c("openNLP", "NLP", "tm", "base")) %dopar% 
                    {
                      oneTweet<-as.String(mySmallCorpus$content[i])
                      ListTemp<-obtieneNombres(oneTweet)
                      ListTemp
                    }

stopCluster(cl)
proc.time()-t    # Detiene el cronómetro
namesListUnique<-unique(namesList)

namesListUnique

proc.time()-t    # Detiene el cronómetro



#PROGRAMACIÓN PARALELA

cores=detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

t <- proc.time() # Inicia el cronómetro

namesList <-foreach(i=1:100, .combine=rbind, .packages = c("openNLP", "NLP", "tm", "base")) %:% 
  foreach(k=1:3, .combine=c) %dopar% 
  {
    oneTweet<-as.String(mySmallCorpus$content[i])
    ListTemp<-obtieneNombres(oneTweet)
    ListTemp
  }
stopCluster(cl)

proc.time()-t    # Detiene el cronómetro
namesListUnique<-unique(namesList)

namesListUnique

proc.time()-t    # Detiene el cronómetro
