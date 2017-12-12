
#****************************************************************************
# Análisis exploratorio de los datos
#****************************************************************************

#****************************************
# Obtenemos la matriz de frecuencias
#****************************************

#Matriz de frecuencias con todo el contenido

tdm <- TermDocumentMatrix(finalCorpus,control = list(wordLengths = c(1, Inf)))
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

# Tenemos 1000 tuits que hablan de personas, para poder pintar la nube de palabras y hacernos una idea de lo que se
# habla en twitter durante esos meses, nos quedaremos con las palabras que al menos aparecen en de 4 a 12 tuits

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 4,random.order = F, colors = pal)

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 8,random.order = F, colors = pal)

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 12,random.order = F, colors = pal)

# Parece que debido a nuestro proceso NER  hemos conseguido acotar la información a personas, y quitarnos información sobre marcas o lugares de enmedio. 
# Vamos a crear por otro lado, un histograma para ver la frecuencia de las palabras que más se usan en nuestro dataset.

library(ggplot2)

palabrasMasUsadas<-data.frame(cbind(names(word.freq[word.freq>15]),word.freq[word.freq>15]))
colnames(palabrasMasUsadas)<-c("Palabra", "Frecuencia")

palabrasMasUsadas$Palabra<-factor(palabrasMasUsadas$Palabra, levels = palabrasMasUsadas$Palabra)

ggplot(data=palabrasMasUsadas, aes(x=Palabra,y=Frecuencia)) +
      geom_bar(stat="identity", position="stack") +
      coord_flip() 

# No es muy revelador, y tendremos el problema de que cuando usemos el dataset completo el aumento de palabras y frecuencias harán del gráfico una mancha
# de la que dificilmente podrámos obtener información relevante. Lo que si podemos concluir es que para la aplicacion de algoritmos de reglas de asociacion,
# deberemos tener soportes muy bajos ya que las frecuencias son muy bajas. 








