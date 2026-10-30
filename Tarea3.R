#Ejercicio 1
#Preparamos el directorio de trabajo
setwd("C:/Users/dantr/OneDrive/Escritorio/IABD/Programación de IA/PrIA-TArea3")

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

#Definimos la función que va a analizar un dataframe para mostrar datos relevantes y genéricos
estudiar_conjunto <- function(titulo, datos){
  print(titulo)
  print(paste("Registros:", nrow(datos), "---- N. Atributos:", ncol(datos)))
  print("Atributos:")
  print(colnames(datos))
  print(head(datos))
}

#Invocamos la función para cada dataframe
estudiar_conjunto("Conjunto Poblacion:", poblacion)
estudiar_conjunto("Conjunto Renta:", renta)
estudiar_conjunto("Conjunto Tasas:", tasas)





#Ejercicio 2
#Definimos la función para pasar los cuantitativos de carácter a numérico
#No podemos hacer as.numeric() directamente porque da error
a_numerico <- function(columna){
  #Nos aseguramos de que el formato es texto y eliminamos espacios en blanco
  columna <- trimws(as.character(columna))
  
  #Eliminamos puntos de separación de millares
  columna <- gsub("\\.", "", columna)
  
  #Cambiamos comas decimales por puntos
  columna <- gsub(",", ".", columna)
  
  #Convertimos a numérico
  return(as.numeric(columna))
}

#Definimos la función para limpiar el inicio de cada una de las entradas de una columna de datos
limpiar_codigo <- function(texto){
  #^\\d+\\s* busca uno o más dígitos al principio de la cadena seguidos de espacios
  #Lo quitamos y devolvemos el resultado
  texto_limpio <- sub("^\\d+\\s*", "", texto)
  return(trimws(texto_limpio))
}

#Apartado A
#Filtramos por el año 2023, cogiendo sólo hombres y mujeres (sin "Total"), y registros provinciales (sin NA ni vacíos)
poblacion <- subset(poblacion, 
                    Periodo == 2023 & 
                    Sexo %in% c("Hombres", "Mujeres") & 
                    !is.na(Provincias) & Provincias != "")

#Eliminamos la columna "Total Nacional" y seleccionamos las que queremos
poblacion <- poblacion[, c("Comunidades.y.Ciudades.Autónomas", "Provincias", "Periodo", "Sexo", "Total")]

#Renombramos las columnas
colnames(poblacion) <- c("comunidad", "provincia", "anio", "sexo", "poblacion")

#Limpiamos los códigos numéricos iniciales con la función que ya hemos definido
poblacion$comunidad <- limpiar_codigo(poblacion$comunidad)
poblacion$provincia <- limpiar_codigo(poblacion$provincia)

#Convertimos la población a numérico con la función que ya hemos definido
poblacion$poblacion <- a_numerico(poblacion$poblacion)

#Reiniciamos los índices de las filas y comprobamos los datos
rownames(poblacion) <- NULL
print(head(poblacion))
























