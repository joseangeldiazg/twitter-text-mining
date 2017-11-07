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
#*********************************
# Opciones
#*********************************

options(java.parameters = "-Xmx8000m")

jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}  
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
  index<-i
  annotations <- annotate(text, list(sent_ann, word_ann))
  names_annotations<- annotate(text, pipeline)
  names_doc <- AnnotatedPlainTextDocument(text, names_annotations)
  names<-entities(names_doc, kind = "person")
  if(!(identical(names, character(0))))
  {
    return(names)
  }
  else
  {
    return(as.Integer(index))  
  }
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
                      oneTweet<-as.String(mySmallCorpus$content[i])
                      obtieneNombres(oneTweet, i)
                    }

stopCluster(cl)

proc.time()-t    # Detiene el cronómetro

namesListUnique<-unique(namesList)
namesListUnique

#*************************************************
#Limpiamos los tuits que no referencian a personas
#*************************************************

finalExample<-list()

for(i in 1:1000)
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

# Borramos caracteres raros tales como emojis o caracteres no alfabéticos

finalCorpus <- tm_map(finalCorpus, content_transformer(removeNumPunct))

# Eliminamos stop words en ingles

# Añadimos la palabra "via" ya que se usa para referenciar usuarios en tweeter

myStopwords <- c(setdiff(stopwords('english'), c("via")))
finalCorpus <- tm_map(finalCorpus, removeWords, myStopwords)

# Borramos los espacios extra
finalCorpus <- tm_map(finalCorpus, stripWhitespace)

(finalCorpus$content[1])

#*************************************************
#Creamos las transacciones para las reglas de asociación
#*************************************************

items <- strsplit(as.character(finalCorpus$content), " ")
transactions <- as(items, "transactions")

help(apriori)

itemsets <- apriori(transactions, parameter = list(sup = 0.1, conf = 0.1, target="frequent itemsets",minlen=1))
inspect(itemsets)


rules <- apriori(transactions, parameter = list(sup = 0.1, conf = 0.1, target="rules",minlen=1))
inspect(rules)



# TODO: Usar FPgrowth para reglas de asociación. 
# TODO: ¿Posible enfoque con clustering jerarquico?
