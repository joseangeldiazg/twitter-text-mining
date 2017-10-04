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

#Declaramos estructruas de datos para guardar los nombres de cada tuit

listPosition=list()
listName=list()
namesList=list()

#Hacemos name entity recognition sobre cada tuit

t <- proc.time() # Inicia el cronómetro

for(i in 1:length(mySmallCorpus))
{
  oneTweet<-as.String(mySmallCorpus$content[i])
  annotations <- annotate(oneTweet, list(sent_ann, word_ann))
  names_annotations<- annotate(oneTweet, pipeline)
  names_doc <- AnnotatedPlainTextDocument(oneTweet, names_annotations)
  names<-entities(names_doc, kind = "person")
  if(!(identical(names, character(0))))
  {
    namesList<-c(list(names), namesList)
  }
}

#Nos quedamos sin repetidos

namesListUnique<-unique(namesList)

namesListUnique

proc.time()-t    # Detiene el cronómetro



