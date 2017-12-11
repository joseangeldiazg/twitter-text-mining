#*************************************************
#REGLAS DE ASOCIACION
#*************************************************

#Vamos a probar primero con Apriori, ya que es exahustivo

#*************************************************
#Creamos las transacciones para las reglas de asociación
#*************************************************

items <- strsplit(as.character(finalCorpus$content), " ")
transactions <- as(items, "transactions")

help(apriori)

itemsets <- apriori(transactions, parameter = list(sup = 0.0001, conf = 0.1, target="frequent itemsets",minlen=1))
inspect(itemsets)


rules <- apriori(transactions, parameter = list(sup = 0.0001, conf = 0.7, target="rules",minlen=1))
inspect(rules)


top.confidence <- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 10))


#Apriori es muy lento, vamos a usar una aproximación por Spark para ver si funciona mejor


#*************************************************
# FP-GROWTH - SPARK FRAMEWORK
#*************************************************


#Iniciamos sesión en Spark

if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = "/Users/joseadiazg/spark-2.2.0-bin-hadoop2.7")
}

library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
sparkR.session(master = "local[*]", sparkConfig = list(spark.driver.memory = "7g"))


#Creamos un dataset para que FP-GROWTH pueda entenderlo

#No podemos tener items repetidos en una transaccion, por lo que haremos una nueva versión


items <- strsplit(as.character(finalCorpus$content), " ")

reduce_row = function(i) {
  split = strsplit(i, split=" ")
  paste(unique(split[[1]]), collapse = " ") 
}

itemsUnique<-lapply(finalCorpus$content[1:length(finalCorpus$content)],reduce_row)

listUnique<-strsplit(as.character(itemsUnique[1:855]), split=" ")

#Ya tenemos items únicos, ahora los pasamos a lista de elementos

lapply(listUnique, write, "test.txt", append=TRUE, ncolumns=1000)


#Vamos a crear el tipo de datos para Spark Fp-Growth

raw_data <- read.df(
  "./test.txt",
  source = "csv",
  schema = structType(structField("raw_items", "string")))

data <- selectExpr(raw_data, "split(raw_items, ' ') as items")

fpm <- spark.fpGrowth(data, itemsCol="items", minSupport=0.001, minConfidence=0.6)

# Extracting frequent itemsets

frequent_itemsets<-spark.freqItemsets(fpm)
showDF(frequent_itemsets)

# Extracting association rules

association_rules <- spark.associationRules(fpm)
showDF(association_rules)


