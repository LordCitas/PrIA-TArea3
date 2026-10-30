#Preparamos el directorio de trabajo
setwd("C:/Users/dantr/OneDrive/Escritorio/IABD/Programación de IA/Tarea 3")

#Creamos un dataframe para cada archivo y los rellenamos leyéndolos
poblacion <- read.csv("poblacion.csv",
                      sep = ";",
                      header = TRUE,
                      dec = ".")

renta <- read.csv("renta.csv",
                  sep = ";",
                  header = TRUE,
                  dec = ".")

tasas <- read.csv("tasas.csv",
                  sep = ";",
                  header = TRUE,
                  dec = ".")
