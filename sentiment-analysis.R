#****************************************************************************
# Análisis de sentiminetos
#****************************************************************************

#Librerias

library(syuzhet)
library(tm)

#****************************************************************************

# Por último dado que vamos a intentar polarizar las reglas de asociación acorde a sentimientos vamos a realizar un gráfico de sentimientos básico
# para hacernos una idea de que estamos tratando y qué tenemos entre manos. 


#Obtenemos los sentimientos
d<-get_nrc_sentiment(finalCorpus$content)


#Creamos un data frame con los sentimientos obtenidos

td<-data.frame(t(d))

#Creamos un histograma para ver su distribución

td_new <- data.frame(rowSums(td[1:length(td)]))
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
qplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment)+ggtitle("Twitter sentiments")

# Parece que tenemos un número muy elevado de tuits que hablan veracidad o afirmaciones. Esto es normal ya que en twitter mucha gente de echos o noticias por lo
# que era de esperar que este sentimiento fuera el mayoritario. Vemos que la diversión o el agrado tiene también una gran representación, junto con la anticipacion, 
# este último debe ser analizado para ver que términos se asocian con anticipación, ya que es ambiguo. El resto de los sentimientos están mas o menos equilibrados.


# Con el fin de ahondar que palabras se asocian con que sentimientos vamos a usar los colores del anterior gráfico sobre una nube de palabras

all = c(
  paste(finalCorpus$content[d$anger > 0], collapse=" "),
  paste(finalCorpus$content[d$anticipation > 0], collapse=" "),
  paste(finalCorpus$content[d$disgust > 0], collapse=" "),
  paste(finalCorpus$content[d$fear > 0], collapse=" "),
  paste(finalCorpus$content[d$joy > 0], collapse=" "),
  paste(finalCorpus$content[d$sadness > 0], collapse=" "),
  paste(finalCorpus$content[d$surprise > 0], collapse=" "),
  paste(finalCorpus$content[d$trust > 0], collapse=" ")
)

all

# Creamos un corpus con las palabras, por nuestro proceso de EDA sabemos que hay palabras muy largas que no valen de nada, por lo que nos quedaremos de nuevo 
# con las que tienen una longitud de hasta 13 caracteres. 

corpus = Corpus(VectorSource(all))

stdm = TermDocumentMatrix(corpus, control = list(wordLengths = c(1, 13)))

stdm = as.matrix(stdm)

colnames(stdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')

#Por último creamos el gráfico

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, 'Emotion Comparison Word Cloud')
comparison.cloud(stdm, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1.5, max.words=4000)

# Con el gráfico queda bastante más claro los datos con los que estamos trabajando y como se han polarizado los sentimientos. Vemos que trump, suscita 
# sorpresa y que es la palabra mas usada dentro de esta categoria. El sentimiento anticipacion parece focalizarse en tiempos, y momentos temporales. 

# Vemos que al gente está enfadada con los politicos, por los asesinatos o la pirateria entre otros casos. 

# Un sentimiento muy interesante es el miedo, donde vemos un claro ejemplo de la sociedad americana. Vemos que la policia o el ejercito suscitan miedo, pero tambien 
# aparece la palabra transgenero, bien sabido es la homofobia del pais que tenemos entre manos, por lo que parece que nuestro proceso ha funcionado bastante bien. 
