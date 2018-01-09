
#****************************************************************************
# Análisis exploratorio de los datos
#****************************************************************************

#****************************************************************************
#Librerias

library(RColorBrewer)
library(wordcloud)
library(tm)
library(slam)
library(ggplot2)
library(RWeka)
library(reshape2)
#****************************************************************************

barplot(c(length(myCorpus$content),length(finalCorpus$content)))
#Vemos que la proporción de tuits que referencia a personas segun nuestro proceso de NER  es bastante reducida frente al conjunto del dataset

#Concretamente el % de tuits que referencia a entidades de tipo persona son:

(length(finalCorpus$content)/length(myCorpus$content))*100

#8,3024

#****************************************
# Obtenemos la matriz de frecuencias
#****************************************

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
tdm1_15 <- TermDocumentMatrix(finalCorpus,control = list(wordLengths = c(1, 15)))
tdm1_20 <- TermDocumentMatrix(finalCorpus,control = list(wordLengths = c(1, 20)))
tdm1_Inf <- TermDocumentMatrix(finalCorpus,control = list(wordLengths = c(1, Inf)))

terms<-c(135149, 123658, 117418, 86729)
names(terms)<-c("Todas Palabras", "Tam [1-20]", "Tam [1-15]", "Tam [1-10]")

barplot(terms)


# Podemos ver como si cortamos el rango de palabras en las que tienen un tamaño entre 1 y 10 quizá estemos perdiendo contenido relevante, ya que 
# bajamos mucho en la variedad de términos. Por encima de 15 no es ncesario coger ya que la variedad es poca y probablemente seán errores, por lo
# que el número optimo lo situaremos en el rango de 1-13

tdm <- TermDocumentMatrix(finalCorpus,control = list(wordLengths = c(1, 13)))


# Dado que el estudio se basa en reglas de asociacion, dificilmente encontraremos alguna regla útil en palabras con frecuencias por debajo de 20,
# ya que usaremos valores de soporte aceptables, por ello, eliminaremos los terminos poco frecuentes, es decir, nos quedaremos con valores de frecuencia 
# mayores o iguales a 20

maxFrequent<-findFreqTerms(tdm, 20)

tdm.new<-tdm[maxFrequent,]

#Tenemos 7322 terminos distintos, por lo que ahora podremos obtener nuestra matriz

m <- as.matrix(tdm.new)

#***************************************
# Graficos de la matriz en nube de terminos
#***************************************

#Con la nube de terminos podemos hacernos una idea de que se hablaba
#en Twitter durante estos meses.

word.freq <- sort(rowSums(m), decreasing = T)
pal <- brewer.pal(9, "BuGn")[-(1:4)]

# Tenemos 140718 tuits que hablan de personas, para poder pintar la nube de palabras y hacernos una idea de lo que se
# habla en twitter durante esos meses, nos quedaremos con las palabras que al menos aparecen por encima de 300 veces en varios intervalos 

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 300, random.order = F, colors = pal)

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 400, random.order = F, colors = pal)

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 500, random.order = F, colors = pal)

# Parece que debido a nuestro proceso NER  hemos conseguido acotar la información a personas, y quitarnos información sobre marcas o lugares de en medio. 
# Vamos a crear por otro lado, un histograma para ver la frecuencia de las palabras que más se usan en nuestro dataset.


palabrasMasUsadas<-data.frame(cbind(names(word.freq[word.freq>1700]),word.freq[word.freq>1700]))
colnames(palabrasMasUsadas)<-c("Palabra", "Frecuencia")

palabrasMasUsadas$Palabra<-factor(palabrasMasUsadas$Palabra, levels = palabrasMasUsadas$Palabra)

ggplot(data=palabrasMasUsadas, aes(x=Palabra,y=Frecuencia)) +
      geom_bar(stat="identity", position="stack") +
      coord_flip() 

# Si usaramos el dataset completo el aumento de palabras y frecuencias harán del gráfico una mancha de la que dificilmente podrámos obtener información
# relevante. Lo que si podemos concluir es que para las palabras con mucha frecuencia, usadas mas de 1700 veces, ya aparecen ciertos terminos interesantes 
# como trump, clinton, drake... estos referencias a personas que probablemente fueron tendencia en estos meses.

# Viendo las frecuencias de este gráfico podemos concluir también que tendremos que utilizar soportes muy bajos en nuestras reglas de asociación, ya que de
# otro modo obtendremos pocos resultados o estos no serán apropiados. 

# Dado que parece que encontramos datos y palabras relacionados entre si, usaremos un estudio de 2-gramas para comprobar que palabras aparecen juntas con 
# más frecuencia.



ngram_tokenizer <- function(n = 1L, skip_word_none = TRUE) {
  stopifnot(is.numeric(n), is.finite(n), n > 0)
  options <- stringi::stri_opts_brkiter(type="word", skip_word_none = skip_word_none)
  
  function(x) {
    stopifnot(is.character(x))
    
    # Split into word tokens
    tokens <- unlist(stringi::stri_split_boundaries(x, opts_brkiter=options))
    len <- length(tokens)
    
    if(all(is.na(tokens)) || len < n) {
      # If we didn't detect any words or number of tokens is less than n return empty vector
      character(0)
    } else {
      sapply(
        1:max(1, len - n + 1),
        function(i) stringi::stri_join(tokens[i:min(len, i + n - 1)], collapse = "-")
      )
    }
  }
}

bigramtokenizer<- ngram_tokenizer(2)
trigramtokenizer <- ngram_tokenizer(3)

bigramslist<-lapply(finalCorpus$content, bigramtokenizer)
trigramlist <- lapply(finalCorpus$content, trigramtokenizer)


#Creamos de nuevo el corpus con los bi y trigramas para estudiarlo. 

bigramslist<-lapply(bigramslist, paste, sep="", collapse = " ")

vs <- VectorSource(bigramslist)

bigramCorpus<-VCorpus(vs, readerControl=list(readPlain, language="en", load=TRUE))

trigramlist<-lapply(trigramlist, paste, sep="", collapse = " ")

vs <- VectorSource(trigramlist)

trigramCorpus<-VCorpus(vs, readerControl=list(readPlain, language="en", load=TRUE))

rm(vs)

#Obtenemos las frecuencias de los terminos dobles y triples

dtm_bigram <- DocumentTermMatrix(bigramCorpus, control = list(tokenize = BigramTokenizer))

inspect(dtm_bigram)

tdm2 <- TermDocumentMatrix(bigramCorpus,control = list(wordLengths = c(1, Inf)))
tdm3 <- TermDocumentMatrix(trigramCorpus,control = list(wordLengths = c(1, Inf)))


maxFrequent2grams<-findFreqTerms(tdm2, 20)
maxFrequent3grams<-findFreqTerms(tdm3, 20)

tdm2.new<-tdm[maxFrequent2grams,]
tdm3.new<-tdm[maxFrequent3grams,]


#Tenemos 7322 terminos distintos, por lo que ahora podremos obtener nuestra matriz

m <- as.matrix(tdm.new)

