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

#Obtenemos un conjunto de datos pequeño

pruebaconpocos<-mySmallCorpus$content

pruebaconpocos<-as.String(pruebaconpocos)

pruebaconpocos

#Declaramos las variables para el proceso NER

person_ann <- Maxent_Entity_Annotator(kind = "person")
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann)


annotations <- annotate(pruebaconpocos, list(sent_ann, word_ann))

head(annotations)

names_annotations<- annotate(pruebaconpocos, pipeline)

names_doc <- AnnotatedPlainTextDocument(pruebaconpocos, names_annotations)

#Utilizamos la funcion creada que nos ayuda con los nombres. 

entities<-entities(names_doc, kind = "person")

