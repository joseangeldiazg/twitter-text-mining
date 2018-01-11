#*************************************************
#REGLAS DE ASOCIACION
#*************************************************

library(arules)
library(ggplot2)
library (arulesViz)

#Vamos a probar primero con Apriori, ya que es exahustivo

#*************************************************
#Creamos las transacciones para las reglas de asociación
#*************************************************

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


for(i in 1:length(items))
{
  items[[i]]<-fusion(items[[i]], "ben", "simmons")
  items[[i]]<-fusion(items[[i]], "donald", "trump")
  items[[i]]<-fusion(items[[i]], "hillary", "clinton")
  items[[i]]<-fusion(items[[i]], "bill", "clinton")
  items[[i]]<-fusion(items[[i]], "barack", "obama")
  items[[i]]<-fusion(items[[i]], "justin", "bieber")
  items[[i]]<-fusion(items[[i]], "bernie", "sanders")
  items[[i]]<-fusion(items[[i]], "ted", "cruz")
  print(i)
}


transactions <- as(items, "transactions")

#*************************************************
#Itemsets Frecuentes
#*************************************************

# Fijaremos el valor de soporte en 0.0001, en base al proceso de EDA, este es un valor aceptable ya que indicará que casi el 1% por ciento de Twitter como minimo 
# constata algo, teniendo en cuenta que podremos considerar la muestra como aleatoria y que los 14 tuits que relacionen terminos en concreto sobre la muestra de
# 140000 tuits, podrá extrapolarse a una gran tendencia si lo llevamos al resto de tuiter, más, teniendo en cuenta que los temas de los que se podría hablar 
# son infinitos. 


itemsets <- apriori(transactions, parameter = list(sup = 0.0005, target="frequent itemsets",minlen=1))
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

barplot( c(frequent=length(itemsets), closed=length(closedItemsets), maximal=length(maximalItemsets)), ylab="count", xlab="itemsets")


#*************************************************
#Reglas de asociación
#*************************************************

rules <- apriori(transactions, parameter = list(sup = 0.0001, conf = 0.7, target="rules", minlen=2))
rules

# Tenemos casi 3M de reglas de asociación

# Vemos un resumen de parámetros de confianza de las reglas. 

summary(rules)

# Vamos a inspeccionarlas por sus valores de soporte y confianza

top.rules.confidence <- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")
top.rules.support <- sort(rules, drecreasing= TRUE, na.last=NA, by="support")

inspect(head(top.rules.confidence,100))

inspect(head(top.rules.support,100))

# Vamos a inspeccionar las reglas sobre algunos nombres propios interesantes que hemos podido ver en nuestro proceso de EDA


# Trump

rulesFilterTrump <- subset(rules, subset = lhs %in% "donald-trump")
rulesFilterTrump <- sort(rulesFilterTrump, by="support")

inspect(head(rulesFilterTrump, 200))

# Hillary Clinton

rulesFilterHC <- subset(rules, subset = lhs %in% "hillary-clinton")
rulesFilterHC <- sort(rulesFilterHC, by="support")

inspect(head(rulesFilterHC, 200))

# Vamos a intentar crear algun gráfico  para visualizar estos conjuntos de reglas. 

plot(rulesPruned[1:6], method="graph")
