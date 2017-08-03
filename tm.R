install.packages("devtools")
install.packages("dplyr")
install.packages("SnowballC")
install.packages("tm")

#Carga de datos en Dataframes con Spark

#Iniciamos sesión en Spark


if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = "/Users/joseadiazg/spark-2.2.0-bin-hadoop2.7")
}
library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
sparkR.session(master = "local[*]", sparkConfig = list(spark.driver.memory = "2g"))


#Cargamos Dataframe  

tweets <- read.df("/Users/joseadiazg/Desktop/output.json", "json")


#Filtramos los que no sean RT ya que los RT estan repetidos y pueden hacernos falsear el modelo

head(filter(tweets, tweets$is_retweet==FALSE))


#Cargamos los datos en un dataframe filtrado

filterdf<-filter(tweets, tweets$is_retweet==FALSE)

#Traemos los datos de la sesion Spark a una sesión en local

localdf<-collect(tweets)

#***********************************************************
#Minería de textos
#***********************************************************

