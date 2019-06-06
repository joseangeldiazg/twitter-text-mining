#****************************************************************************
#*******************************LIBRERIAS************************************
#****************************************************************************

# En este script se cargan todas las librerías necesarias para la ejecución de los demás scripts. 
# Ejecutarlo 1 vez antes de comenzar a trabajar con el código 

install.packages("openNLPmodels.en",repos = "http://datacube.wu.ac.at/",type = "source")
install.packages("RWeka", "qdap", "devtools", "dplyr", "wordcloud","rJava", "openNLP", "foreach", "doParallel", "plyr", "reshape2", "syuzhet", "ggplot2", "arulesViz")

#Libreria para el análisis de sentimientos

install.packages("gh")
install.packages("curl")
install.packages("httr")
install.packages("usethis")
install.packages("devtools")

library(devtools)
if(!require(Rstem)) install_url("http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
if(!require(sentiment)) install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
library(Rstem)
library(sentiment)
library(plotly)


#Resto de librerias 

require(RWeka)
require(qdap)
require(devtools)
require(dplyr)
require(tm)
require(RColorBrewer)
require(wordcloud)
require(NLP)
require(openNLP)
require(magrittr)
require(foreach)
require(doParallel)
require(rJava)
require(arules)
library(dplyr)  
require(plyr)
require(reshape2)
require(syuzhet)
require(slam)
require(ggplot2)
require(arules)
require (arulesViz)
library(dplyr)


#****************************************************************************
#*******************************LIBRERIAS DIFUSAS****************************
#****************************************************************************

require(lfl)
require(FuzzyR)
require(RKEEL)
