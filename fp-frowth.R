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


