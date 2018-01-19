#**********************************************************
# Name Entity Recognition on R - CLUSTER
#**********************************************************

# Este script ha sido elaborado para su ejecución en el cluster.
# Subimos los datos desde local y ejecutaremos las funciones del proceso NER

#**********************************************************
# Cargamos librerias
#**********************************************************

library(NLP)
library(openNLP)
library(magrittr)
library(foreach)
library(doParallel)
library(rJava)
library(devtools)
install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")

#*******************************************************
# Configuración de Java
#*******************************************************

jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}  


#*******************************************************
# Lectura de datos
#*******************************************************

#Hemos cargado en local los datos a un txt, ahora tenemos que leer las líneas en un vector para la función obtiene nombres

datos<-scan("tuits.txt", what="character", sep="\n")



#*******************************************************
# Name Entity Recognition
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

#Tiene 32 nucleos quitamos uno del proceso para no saturar. Nos quedamos con 31. 

cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

t <- proc.time() # Inicia el cronómetro

namesList <-foreach(i=1:length(datos),
                    .combine=c, 
                    .packages = c("openNLP", "NLP", "base","rJava")) %dopar% 
                    {
                      oneTweet<-as.String(datos[i])
                      obtieneNombres(oneTweet, i)
                    }

stopCluster(cl)

proc.time()-t    # Detiene el cronómetro

copyNamesList <- namesList


#*************************************
# Escribimos los datos en disco
#*************************************

fileConn<-file("./entidades.txt")

write(namesList, file=fileConn, sep="\n", append=TRUE, ncolumns=1)

close(fileConn)
