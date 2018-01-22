#*************************************************
# FP-GROWTH - SPARK FRAMEWORK
#*************************************************

# En este script se usa el algoritmo de obtencion de reglas FP-GROWTH 

# El motivo es realizar una comparación con los resultados obtenidos en APRIORI


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
length(itemsUnique)

listUnique<-strsplit(as.character(itemsUnique[1:length(itemsUnique)]), split=" ")

for (i in 1:length(listUnique))
{
  listUnique[[i]]<-fusion(listUnique[[i]], "ben", "simmons")
  listUnique[[i]]<-fusion(listUnique[[i]], "donald", "trump")
  listUnique[[i]]<-fusion(listUnique[[i]], "hillary", "clinton")
  listUnique[[i]]<-fusion(listUnique[[i]], "bill", "clinton")
  listUnique[[i]]<-fusion(listUnique[[i]], "barack", "obama")
  listUnique[[i]]<-fusion(listUnique[[i]], "justin", "bieber")
  listUnique[[i]]<-fusion(listUnique[[i]], "bernie", "sanders")
  listUnique[[i]]<-fusion(listUnique[[i]], "ted", "cruz")
  print(i)
}


#Ya tenemos items únicos, ahora los pasamos a lista de elementos

lapply(listUnique, write, "test.txt", append=TRUE, ncolumns=1000)


#Vamos a crear el tipo de datos para Spark Fp-Growth

raw_data <- read.df(
  "./test.txt",
  source = "csv",
  schema = structType(structField("raw_items", "string")))

data <- selectExpr(raw_data, "split(raw_items, ' ') as items")


# Vamos a probar como se comporta el algoritmo para los valores de soporte de: 0.01, 0.001, 0.0001

t <- proc.time() # Inicia el cronómetro
fpm <- spark.fpGrowth(data, itemsCol="items", minSupport=0.0001, minConfidence=0.7)
association_rules <- spark.associationRules(fpm)
proc.time()-t    # Detiene el cronómetro

# Para cada experimento pasamos el dataframe de Spark a DataFrame de R ya que este permite más acciones

ar00001<-collect(association_rules)

object.size(ar0001)

#Obtenemos itemsets frecuentes

frequent_itemsets<-spark.freqItemsets(fpm)
showDF(frequent_itemsets)

# Obtenemos reglas de asociación

association_rules <- spark.associationRules(fpm)
showDF(association_rules)


