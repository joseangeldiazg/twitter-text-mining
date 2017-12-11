
#****************************************************************************
# Análisis exploratorio de los datos
#****************************************************************************



#****************************************
# Obtenemos la matriz de frecuencias
#****************************************

#Matriz de frecuencias con todo el contenido

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
#10000 tuits

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10000,random.order = F, colors = pal)

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 20000,random.order = F, colors = pal)

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 30000,random.order = F, colors = pal)


#
