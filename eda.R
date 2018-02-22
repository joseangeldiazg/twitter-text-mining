#****************************************************************************
# Análisis exploratorio de los datos
#****************************************************************************

#Vemos que la proporción de tuits que referencia a personas segun nuestro proceso de NER  es bastante reducida frente al conjunto del dataset

barplot(c(length(myCorpus$content),length(finalCorpus$content)))

#Concretamente el % de tuits que referencia a entidades de tipo persona son:

(length(finalCorpus$content)/length(myCorpus$content))*100

#8,3024

#****************************************************************************
# Obtenemos la matriz de frecuencias
#****************************************************************************

# Vamos a obtener la matriz de frecuencias de todo el contenido pero antes, para evitar problemas quizá podamos eliminar ciertas palababras raras
# o largas que en nuestro proceso tendrán poca imporancia, sin olvidarnos que venimos de Twitter y que aunque hemos limpiado los datos, tendremos algunas
# palabras que provengan de enlaces o hashtags que poco interes pueden suscitar en nuestro contenido. Vamos a probar a dibujar en nube de palabras estas. 

tdmlimpiar<- TermDocumentMatrix(finalCorpus,control = list(wordLengths = c(18, Inf)))
m <- as.matrix(tdmlimpiar)
word.freq <- sort(rowSums(m), decreasing = T)
pal <- brewer.pal(9, "BuGn")[-(1:4)]
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,random.order = F, colors = pal)

#Vemos que son palabras para nada últiles y sin duda deben ser limpiadas, haremos varias pruebas para ver que rango de tamaño mantenemos

tdm1_10 <- TermDocumentMatrix(finalCorpus,control = list(wordLengths = c(1, 10)))
tdm1_13 <- TermDocumentMatrix(finalCorpus,control = list(wordLengths = c(1, 13)))
tdm1_15 <- TermDocumentMatrix(finalCorpus,control = list(wordLengths = c(1, 15)))
tdm1_20 <- TermDocumentMatrix(finalCorpus,control = list(wordLengths = c(1, 20)))
tdm1_Inf <- TermDocumentMatrix(finalCorpus,control = list(wordLengths = c(1, Inf)))

terms<-c(135149, 123658, 117418,109790,86729)
names(terms)<-c("Todas Palabras", "Tam [1-20]", "Tam [1-15]", "Tam [1-13]" ,"Tam [1-10]")

barplot(terms)


# Podemos ver como si cortamos el rango de palabras en las que tienen un tamaño entre 1 y 10 quizá estemos perdiendo contenido relevante, ya que 
# bajamos mucho en la variedad de términos. Por encima de 15 no es necesario coger ya que la variedad es poca y probablemente seán errores, por lo
# que el número óptimo lo situaremos en el rango de 1-13

tdm <- TermDocumentMatrix(finalCorpus,control = list(wordLengths = c(1, 13)))


#Vamos a comprobar que no se pierde contenido relevante, para ello mostramos las palabras de más de 13 
#carácteres y sus frecuencias. 

tdm.mas.13 <- TermDocumentMatrix(finalCorpus,control = list(wordLengths = c(14, Inf)))

maxFrequent.mas.13<-findFreqTerms(tdm.mas.13, 2)

tdm.mas.13<-tdm.mas.13[maxFrequent.mas.13,]

m.mas.13 <- as.matrix(tdm.mas.13)

#Ahora creamos un gráfico para ver cuales son las palabras más usadas dentro de las eliminadas. 

word.freq.mas.13 <- sort(rowSums(m.mas.13), decreasing = T)
pal <- brewer.pal(9, "BuGn")[-(1:4)]

head(word.freq.mas.13,150)

palabrasMasUsadas<-data.frame(cbind(names(word.freq.mas.13[word.freq.mas.13>20]),word.freq.mas.13[word.freq.mas.13>20]))
colnames(palabrasMasUsadas)<-c("Palabra", "Frecuencia")

palabrasMasUsadas$Palabra<-factor(palabrasMasUsadas$Palabra, levels = palabrasMasUsadas$Palabra)

ggplot(data=palabrasMasUsadas, aes(x=Palabra,y=Frecuencia)) +
  geom_bar(stat="identity", position="stack") +
  coord_flip()

palabrasMasUsadas2<-as.numeric(word.freq.mas.13[word.freq.mas.13>30])
names(palabrasMasUsadas2)<-names(word.freq.mas.13[word.freq.mas.13>30]) 

barplot(palabrasMasUsadas2, horiz=T, xlim = c(0,1000), xpd=F, las=2, space=c(2))


#*********************************************************************


# Dado que el estudio se basa en reglas de asociación, dificilmente encontraremos alguna regla útil en palabras con frecuencias por debajo de 20,
# ya que usaremos valores de soporte aceptables, por ello, eliminaremos los términos poco frecuentes, es decir, nos quedaremos con valores de frecuencia 
# mayores o iguales a 20

maxFrequent<-findFreqTerms(tdm, 20)

tdm.new<-tdm[maxFrequent,]

#Tenemos 7322 terminos distintos, por lo que ahora podremos obtener nuestra matriz y mantener la misma en memoria

m <- as.matrix(tdm.new)

#****************************************************************************
# Graficos de la matriz en nube de terminos
#****************************************************************************

#Con la nube de terminos podemos hacernos una idea de que se hablaba en Twitter durante estos meses.

word.freq <- sort(rowSums(m), decreasing = T)
pal <- brewer.pal(9, "BuGn")[-(1:4)]

# Tenemos 140718 tuits que hablan de personas, para poder pintar la nube de palabras y hacernos una idea de lo que se
# habla en twitter durante esos meses, nos quedaremos con las palabras que al menos aparecen por encima de 300 veces en varios intervalos 

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 300, random.order = F, colors = pal)

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 400, random.order = F, colors = pal)

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 500, random.order = F, colors = pal)

# Parece que debido a nuestro proceso NER  hemos conseguido acotar la información a personas, y quitarnos información sobre marcas o lugares de en medio. 
# Igualmente vemos como empiezan a aparecer personas interesantes en el proceso por lo que parece que nuestro modelo empieza a obtener información relevante.



#****************************************************************************
# Histogramas
#****************************************************************************

# Vamos a crearun histograma para ver la frecuencia de las palabras que más se usan en nuestro dataset.


palabrasMasUsadas<-data.frame(cbind(names(word.freq[word.freq>1700]),word.freq[word.freq>1700]))
colnames(palabrasMasUsadas)<-c("Palabra", "Frecuencia")

palabrasMasUsadas$Palabra<-factor(palabrasMasUsadas$Palabra, levels = palabrasMasUsadas$Palabra)

ggplot(data=palabrasMasUsadas, aes(x=Palabra,y=Frecuencia)) +
      geom_bar(stat="identity", position="stack") +
      coord_flip()

palabrasMasUsadas2<-as.numeric(word.freq[word.freq>1700])
names(palabrasMasUsadas2)<-names(word.freq[word.freq>1700]) 
  
barplot(palabrasMasUsadas2, horiz=T, xlim = c(1000,7000), xpd=F, las=2)

# Si usaramos el dataset completo el aumento de palabras y frecuencias harán del gráfico una mancha de la que dificilmente podrámos obtener información
# relevante. Lo que si podemos concluir es que para las palabras con mucha frecuencia, usadas más de 1700 veces, ya aparecen ciertos términos interesantes 
# como trump, clinton, drake... estos referencias a personas que probablemente fueron tendencia en estos meses.

# Viendo las frecuencias de este gráfico podemos concluir también que tendremos que utilizar soportes muy bajos en nuestras reglas de asociación, ya que de
# otro modo obtendremos pocos resultados o estos no serán apropiados. 

# Dado que parece que encontramos datos y palabras relacionados entre si, usaremos un estudio de 2-gramas para comprobar que palabras aparecen juntas con 
# más frecuencia.


#****************************************************************************
# N-Gramas
#****************************************************************************


BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

# Pasamos a VCORPUS nuestro corpus inicial ya que de otro modo no es posible utilizar el tokenizer de weka

vs <- VectorSource(finalCorpus$content)

bigramCorpus<-VCorpus(vs, readerControl=list(readPlain, language="en", load=TRUE))

rm(vs)

bigram.twitterTdm <- DocumentTermMatrix(bigramCorpus, control = list(tokenize = BigramTokenizer))

#Obtenemos las frecuencias de los términos dobles

maxFrequent2grams<-findFreqTerms(bigram.twitterTdm, 40)

tdm2.new<-bigram.twitterTdm[,maxFrequent2grams]

m2grams <- as.matrix(tdm2.new)

#Realizamos un gráfico de los 30 bigrams más usados

wordcount <- colSums(m2grams)
topten <- head(sort(wordcount, decreasing=TRUE), 30)

dfplot <- as.data.frame(melt(topten))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word,
                      levels=dfplot$word[order(dfplot$value,
                                               decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity") + coord_flip() 
fig <- fig + xlab("Two-grams in Twitter Corpus")
fig <- fig + ylab("Frecuencia")
print(fig)


#Puede resultar interesante obtener un estudio de los trigramas para ver que palabras se relacionan más con los personajes que parecen ir apareciendo


TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

trigram.twitterTdm <- DocumentTermMatrix(bigramCorpus, control = list(tokenize = TrigramTokenizer))

#Obtenemos las frecuencias de los terminos dobles

maxFrequent3grams<-findFreqTerms(trigram.twitterTdm, 40)

tdm3.new<-trigram.twitterTdm[,maxFrequent3grams]

m3grams <- as.matrix(tdm3.new)

#Realizamos un gráfico de los 30 bigrams más usados

wordcount <- colSums(m3grams)
topten <- head(sort(wordcount, decreasing=TRUE), 40)

dfplot <- as.data.frame(melt(topten))
dfplot$word <- dimnames(dfplot)[[1]]
dfplot$word <- factor(dfplot$word,
                      levels=dfplot$word[order(dfplot$value,
                                               decreasing=TRUE)])

fig <- ggplot(dfplot, aes(x=word, y=value)) + geom_bar(stat="identity") + coord_flip() 
fig <- fig + xlab("3-grams in Twitter Corpus")
fig <- fig + ylab("Frecuencia")
print(fig)


# Los trigramas no parecen muy útiles ya que al tratarse de un problema en el que hemos obtenido datos de Twitter sin ningún filtro, estos están muy influenciados
# por aplicaciones, como youtube y acciones propias de las redes sociales como añadir fotos, videos, dar like a estos... también podemos ver información 
# relacionada con Michael Sippey, tras una búsqueda podemos concluir que esta persona fue alto cargo de Twitter pero en los meses en que hemos realizado el 
# estudio abandonó su puesto. 

