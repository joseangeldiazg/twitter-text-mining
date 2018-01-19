#****************************************************************************
#*******************************LIBRERIAS************************************
#****************************************************************************

# En este script se cargan todas las librerias necesarias para la ejecución de los demás scripts. 
# Ejecutarlo 1 vez antes de comenzar a trabajar con el código 

install.packages("openNLPmodels.en",repos = "http://datacube.wu.ac.at/",type = "source")

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
require(plyr)
require(reshape2)
require(syuzhet)
require(slam)
require(ggplot2)
require(arules)
require (arulesViz)
