#**********************************************************
# REGLAS DE ASOCIACIÓN
#**********************************************************

#**********************************************************
#Creamos las transacciones para las reglas de asociación
#**********************************************************

# Marco Formal: Los Items serán las Palabras y las Transacciones los Tuits

items <- strsplit(as.character(finalCorpus$content), " ")


# En nuestro proceso de EDA hemos visto que ibamos a tener nombres propios compuestos interesantes. Uniremos los más conocidos para evitar en suciar el proceso.
# con reglas del tipo Justin => Bieber, Donald => Trump

fusion <- function(vector,nombre1,nombre2)
  {
    vector<-vector[vector!=""]
    final<-length(vector)-1
    if(length(vector)>1)
    {
        for(i in 1:final)
        {
          if(vector[i]==nombre1 && vector[i+1]==nombre2)
          {
            nombre<-paste(nombre1, nombre2, sep="-")
            vector[i]<-nombre
            vector[i+1]<-"borrar"
          }
        }
    }
    vector<-vector[vector!="borrar"]
    return(vector)
  }


for (i in 1:length(items))
{
  items[[i]]<-fusion(items[[i]], "ben", "simmons")
  items[[i]]<-fusion(items[[i]], "donald", "trump")
  items[[i]]<-fusion(items[[i]], "hillary", "clinton")
  items[[i]]<-fusion(items[[i]], "bill", "clinton")
  items[[i]]<-fusion(items[[i]], "barack", "obama")
  items[[i]]<-fusion(items[[i]], "justin", "bieber")
  items[[i]]<-fusion(items[[i]], "bernie", "sanders")
  items[[i]]<-fusion(items[[i]], "ted", "cruz")
  items[[i]]<-fusion(items[[i]], "buddy", "hield")
  print(i)
}

transactions <- as(items, "transactions")

summary(transactions)

#**********************************************************
#Itemsets Frecuentes
#**********************************************************

# Fijaremos el valor de soporte en 0.0005, en base al proceso de EDA, este es un valor aceptable ya que indicará que casi el 1% por ciento de Twitter como mínimo 
# constata algo, teniendo en cuenta que podremos considerar la muestra como aleatoria y que los 14 tuits que relacionen terminos en concreto sobre la muestra de
# 140000 tuits, podrá extrapolarse a una gran tendencia si lo llevamos al resto de tuiter, más, teniendo en cuenta que los temas de los que se podría hablar 
# son infinitos. 


itemsets <- apriori(transactions, parameter = list(sup = 0.0001, target="frequent itemsets",minlen=1))
itemsets <- sort(itemsets, by="support")
inspect(head(itemsets,50))

# Creamos ahora un gráfico para ver la distribución del número de items en los itemsetsfrecuentes

barplot(table(size(itemsets)), xlab="item size", ylab="count")


# Vemos una explosión e los itemsets a partir de 4, por lo que vamos a explorar los itemsets por encima y debajo de este rango. 

inspect(itemsets[size(itemsets)==1])
inspect(itemsets[size(itemsets)==2])
inspect(itemsets[size(itemsets)==3])
inspect(itemsets[size(itemsets)==4])


inspect(itemsets[size(itemsets)==7])
inspect(itemsets[size(itemsets)==8])
inspect(itemsets[size(itemsets)==9])
inspect(itemsets[size(itemsets)==10])

#Vamos a realizar un estudio  de los itemsets maximales. 

maximalItemsets <- itemsets[is.maximal(itemsets)]
maximalItemsets <- sort(maximalItemsets, by="support")

# Volvemos a dibujar el gráfico

barplot(table(size(maximalItemsets)), xlab="item size", ylab="count")

# Por último obtendremos los itemsets cerrados para obtener un gráfico con una comparativa entre ambos.

closedItemsets <- itemsets[is.closed(itemsets)]
closedItemsets <- sort(closedItemsets, by="support")


# Ahora podemos ver cuantos itemsets, frecuentes cerrados y maximales tenemos.

barplot( c(frecuentes=length(itemsets), cerrados=length(closedItemsets), maximales=length(maximalItemsets)), ylab="count", xlab="itemsets")


#*************************************************
#Reglas de asociación
#*************************************************

t <- proc.time()

rules <- apriori(transactions, parameter = list(sup = 0.0001, conf = 0.7, target="rules", minlen=2, maxtime=Inf))

proc.time()-t  

rules

# Tenemos casi 3M de reglas de asociación

# Vemos un resumen de parámetros de confianza de las reglas. 

summary(rules)

top.rules.confidence <- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")
top.rules.support <- sort(rules, drecreasing= TRUE, na.last=NA, by="support")
inspect(head(top.rules.confidence,100))
inspect(head(top.rules.support,100))


# Vamos a inspeccionar las reglas sobre algunos nombres propios interesantes que hemos podido ver en nuestro proceso de EDA

# Trump

rulesFilterTrump <- subset(rules, subset = rhs %in% "donald-trump")
rulesFilterTrump <- sort(rulesFilterTrump, by="support")
inspect(head(rulesFilterTrump, 200))

# Hillary Clinton

rulesFilterHC <- subset(rules, subset = rhs %in% "hillary-clinton")
rulesFilterHC <- sort(rulesFilterHC, by="support")
inspect(head(rulesFilterHC, 200))

# Vamos a intentar crear algun gráfico  para visualizar estos conjuntos de reglas. 

#plot(rulesFilterTrump, method="graph")
#plot(rulesFilterHC, method="graph")


# No son muy reveladores, tendremos que visualizarlo posteriormente de otra manera. 

# Vamos a eliminar las reglas redundantes para aclarar los resultados

rulesFilterTrump
rulesFilterHC

#rulesSorted <- sort(rulesFilterTrump, by="confidence")
rulesSorted<-sort(rulesFilterHC, by="confidence")

subsetMatrix <- is.subset(rulesSorted, rulesSorted)
subsetMatrix[lower.tri(subsetMatrix, diag=TRUE)] <- FALSE
redundant <- colSums(subsetMatrix, na.rm=TRUE) >= 1

#rulesPrunedTrump <- rulesSorted[!redundant] 
rulesPrunedHC <- rulesSorted[!redundant] 


#Ahora podemos analizar aquellas que no son redundantes, las ordenamos por soporte.


rulesPrunedTrump <- sort(rulesPrunedTrump,by="support")
inspect(head(rulesPrunedTrump,64))


rulesPrunedHC<-sort(rulesPrunedHC,by="support")
inspect(head(rulesPrunedHC,69))

# Vamos a intentar obtener las reglas solo para Trump, lo que nos permitirá valores de soporte muy bajos sin tener problemas de memoria
# ya que solo se generarán las reglas que contengan en el consecuente a trump.

rulesDonalTrump <- apriori (data=transactions, 
                  parameter=list (supp=0.00007,conf = 0.7, minlen=2), 
                  appearance = list(default="lhs",rhs="donald-trump"), 
                  control = list (verbose=F)) 


rulesSorted<-sort(rulesDonalTrump, by="support")

subsetMatrix <- is.subset(rulesSorted, rulesSorted)
subsetMatrix[lower.tri(subsetMatrix, diag=TRUE)] <- FALSE
redundant <- colSums(subsetMatrix, na.rm=TRUE) >= 1

rulesDonalTrump <- rulesSorted[!redundant] 
rulesDonalTrump

inspect(head(rulesDonalTrump, 200))
inspect(tail(rulesDonalTrump, 58))


# Hemos encontrado algunas reglas interesantes como: 

# caucus=>donald-trump: En esta regla, y demás varianes de esta regla que añeden algun termino más en el consecuente podemos ver, la tendencia de la
#                       raza caucasica a votar a trump ya que en estos era donde el tenia una mayoria.
# unfavorable,viewed => donald-trump: Casi siempre que aparecen estas dos palabras, donald trump también aparece, constata cierta tendencia en twitter a
#                       a ir encontra de ciertas opiniones del político o ver mal ciertas politicas o acciones del mismo., 
# ignored,rape => donald-trump: Esta es interesante, ya que habla de victimas de violación o violaciones ignoradas y lo relacionan con buen soporte y buena
#                       y buena confianza con donald-trump. Tras unas busquedas en twitter, podemos constar que hay innumerables tuits y noticias que hablan de 
#                       trump y lo relacionan con estos temas, por lo que parece que en 2016, hubo ciertos sectores que hablaban de esto bien como noticias, o 
#                       o bien para intentar desacreditar al politico ya que por la fecha de obtencion de los tuits estaban inmersos en campaña. 
# bans,serving,transgender => donald-trump: Esta es una de las reglas que más fuerza tiene y que aparece en más vertientes sin algun termino o con la combinación 
#                       de otros en el antecedente. Por 2016 ya se hablaba de la ley que permitiria a los transgenero entrar en el ejercito y las reglas constatan
#                       la tendencia a hablar de aquello y la postura de prohibir de Trump. En 2017, esta tendencia que las reglas constatan se hizo realidad. 



# Vamos a intentar ver que sentimientos se relacionan con estas palabras de las reglas, para obtener de una manera mas objetiva si las reglas aciertan

stdm["rape",]
stdm["donald",]
stdm["bans",]
stdm["serving",]
stdm["transgender",]




#Realizamos el mismo proceso para hillary-clinton


rulesHillaryClinton <- apriori (data=transactions, 
                            parameter=list (supp=0.00007,conf = 0.7, minlen=2), 
                            appearance = list(default="lhs",rhs="hillary-clinton"), 
                            control = list (verbose=F)) 


rulesSorted<-sort(rulesHillaryClinton, by="support")

subsetMatrix <- is.subset(rulesSorted, rulesSorted)
subsetMatrix[lower.tri(subsetMatrix, diag=TRUE)] <- FALSE
redundant <- colSums(subsetMatrix, na.rm=TRUE) >= 1

rulesHillaryClinton <- rulesSorted[!redundant] 


#**********************************************************
# VISUALIZACION
#**********************************************************

#Para ver las reglas en función en forma de gráfico

plot(rulesDonalTrump, method="graph")
plot(rulesHillaryClinton, method="scatterplot")
#Para ver como se distribuyen las reglas en funcion de los parámetros

plot(rulesDonalTrump, method="scatterplot")
plot(rulesHillaryClinton, method="scatterplot")
#Para ver como se distribuyen las reglas en funcion de los parámetros y el tamaño (orden) de las mismas

plot(rulesDonalTrump, method="two-key plot")
plot(rulesHillaryClinton, method="two-key plot")

#Vamos a generar una forma interesante de ver los gráficos, montaremos una nube de palabras con cada uno de los terminos en los antedecedentes de estas reglas
# y así sabremos en funcion del tamaño que palabras son las que mas representan una tendencia en funcion de trump o hillary. 

# Escribimos las reglas en un dataframe y las limpiamos de los caracteres propios de las reglas

df.rulesDonalTrump <- as(rulesDonalTrump, "data.frame")
string <- as.String(df.rulesDonalTrump$rules)
string <- gsub("=>", "", string)
string <- gsub("\\{", "", string)
string <- gsub("}", "", string)
string <- gsub(",", " ", string)
string <- gsub("donald-trump", "", string)

# Creamos la nube de palabras, para ello primero necesitamos crear de nuevo una matriz de frecuencias

corpusReglasTrump<-Corpus(VectorSource(string))

tdmReglasTrump <- TermDocumentMatrix(corpusReglasTrump,control = list(wordLengths = c(1, Inf)))
mTrump <- as.matrix(tdmReglasTrump)

# Pintamos la nube de términos

word.freq <- sort(rowSums(mTrump), decreasing = T)
pal <- brewer.pal(9, "BuGn")[-(1:4)]
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 1, random.order = F, colors = pal)



#Realizamos el mismo gráfico pero para hillary

df.rulesHillaryClinton <- as(rulesHillaryClinton, "data.frame")
string <- as.String(df.rulesHillaryClinton$rules)
string <- gsub("=>", "", string)
string <- gsub("\\{", "", string)
string <- gsub("}", "", string)
string <- gsub(",", " ", string)
string <- gsub("hillary-clinton", "", string)

# Creamos la nube de palabras, para ello primero necesitamos crear de nuevo una matriz de frecuencias

corpusReglasHillary<-Corpus(VectorSource(string))

tdmReglasHillary <- TermDocumentMatrix(corpusReglasHillary,control = list(wordLengths = c(1, Inf)))
mHillary <- as.matrix(tdmReglasHillary)

# Pintamos la nube de términos

word.freq <- sort(rowSums(mHillary), decreasing = T)
pal <- brewer.pal(9, "BuGn")[-(1:4)]
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 1, random.order = F, colors = pal)



#*************************************************
# Aproximación jerarquica basada en sentimientos
#*************************************************

# Las reglas de asociacion pueden estudiarse desde un punto de vista jerarquico, es decir, en el ejemplo de cesta de la compra: 
# {Manzanas, platanos} => {Yogurt} podría sustituitse por {Fruta} => {Yogurt}
# Dado que gracias a nuestro análisis de sentimientos tenemos polarizadas las palabras vamos jerarquizar estas reglas en función de los sentimientos. 

# Primero nos quedamos con aquellos tuits que hacen referencia a los dos personajes estudiados


encuentraNombres <- function(vector,nombre)
{
  final<-length(vector)
  for(i in 1:final)
  {
    if(vector[i]==nombre)
    {
      return(vector)
    }
  }
  return("borrar")
}

# Creamos listas vacias para añadir los tuits o "borrar" en caso de que el tuit no contenga los nombres

listTrump <- list()
listHillary <- list()

for (i in 1:length(items))
{
  listTrump[[i]]<-encuentraNombres(items[[i]], "donald-trump")
  listHillary[[i]]<-encuentraNombres(items[[i]], "hillary-clinton")
  print(i)
}

# Limpiamos las listas aquellos que referencian a borrar

listTrump<-listTrump[listTrump[1:length(listTrump)]!="borrar"]
listHillary<-listHillary[listHillary[1:length(listHillary)]!="borrar"]


# Llegados a este punto solo tenemos que intercambiar cada termino por su sentimiento asociado
# Para ello, localizamos el término en la matriz de sentimientos, obtenemos su mayor valor de sentimiento y lo sustituimos 

cambiaPalabraPorSentimiento <- function(vector,nombre,matrixsentimientos)
{
  final <- length(vector)
  for(i in 1:final)
  {
    if(vector[i]  %in% rownames(matrixsentimientos))
    {
      vector[i] <- names(which.max(matrixsentimientos[vector[i],]))
    }
    else if(vector[i]==nombre)
    {
      vector[i]<-nombre
    }
    else
    {
      # Si no es ni el nombre si está en la lista de sentimiento será raro, y lo borramos
      vector[i]<-"borrar"
    }
  }
  return(vector[vector!="borrar"])
}

for(i in 1:length(listTrump))
{
  listTrump[[i]] <- cambiaPalabraPorSentimiento(listTrump[[i]],"donald-trump",stdm)
}


for(i in 1: length(listHillary))
{
  listHillary[[i]] <- cambiaPalabraPorSentimiento(listHillary[[i]],"hillary-clinton",stdm)
}


#Llegados a este punto obtenemos las transacciones para cada uno de los candidadtos. 


transactionsTrump <- as(listTrump, "transactions")
transactionsHillary  <- as(listHillary, "transactions")


#Obtenemos las reglas de asociación. Ahora subiremos el soporte ya que al ser jerarquicas pueden ser mejores.

rulesJerarquicaTrump <- apriori(transactionsTrump, parameter = list(sup = 0.01, conf = 0.8, target="rules", minlen=2, maxtime=Inf))
rulesJerarquicaHillary <- apriori(transactionsHillary, parameter = list(sup = 0.01, conf = 0.8, target="rules", minlen=2, maxtime=Inf))

#Eliminamos reglas redundantes

rulesSorted <- sort(rulesJerarquicaTrump, by="confidence")
subsetMatrix <- is.subset(rulesSorted, rulesSorted)
subsetMatrix[lower.tri(subsetMatrix, diag=TRUE)] <- FALSE
redundant <- colSums(subsetMatrix, na.rm=TRUE) >= 1
rulesJerarquicasPrunedTrump <- rulesSorted[!redundant] 


rulesSorted<-sort(rulesJerarquicaHillary, by="confidence")
subsetMatrix <- is.subset(rulesSorted, rulesSorted)
subsetMatrix[lower.tri(subsetMatrix, diag=TRUE)] <- FALSE
redundant <- colSums(subsetMatrix, na.rm=TRUE) >= 1
rulesJerarquicasPrunedHC <- rulesSorted[!redundant] 


#Las ordenamos y visualizamos

sorted.rulesJerarquicasPrunedTrump<-sort(rulesJerarquicasPrunedTrump, by="support")
sorted.rulesJerarquicasPrunedHC<-sort(rulesJerarquicasPrunedHC, by="support")


#Como tenemos pocas reglas las pasamos a un dataframe

dftrumpsents   <- as(sorted.rulesJerarquicasPrunedTrump, "data.frame")
dfhillarysents <- as(sorted.rulesJerarquicasPrunedHC, "data.frame")

filter(dftrumpsents, grepl("donald-trump", dftrumpsents$rules))

filter(dfhillarysents, grepl("hillary-clinton", dfhillarysents$rules))

