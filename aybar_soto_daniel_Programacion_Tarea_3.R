

#Daniel Aybar Soto
#Programación de Inteligencia Artificial

#Ejercicio 1:
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





#Ejercicio 2:
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

#Apartado A:
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

#Apartado B:
#Filtramos por los trimestres de 2023, sexo (Hombres y Mujeres), y excluimos las entradas de "Total Nacional"
tasas <- subset(tasas, 
                Periodo %in% c("2023T1", "2023T2", "2023T3", "2023T4") & 
                Sexo %in% c("Hombres", "Mujeres") & 
                Provincias != "Total Nacional")

#Renombramos las columnas mientras limpiamos el formato de sus datos
tasas$provincia <- limpiar_codigo(tasas$Provincias)
tasas$valor <- a_numerico(tasas$Total)

#Comprobamos las primeras 8 filas
print(head(tasas, 8))

#Apartado C:
#Filtrarmos el año 2023, con indicador "Renta neta media por persona", filas con provincia y sin diferenciar islas
renta <- subset(renta, 
                Periodo == 2023 & 
                `Indicadores.de.renta.media` == "Renta neta media por persona" & 
                !is.na(Provincias) & Provincias != "" & 
                (is.na(Islas) | Islas == ""))

#Conservamos únicamente las columnas necesarias
renta <- renta[, c("Provincias", "Total")]

#Renombramos las columnas
colnames(renta) <- c("provincia", "renta_media")

#Limpiamos los datos
renta$provincia   <- limpiar_codigo(renta$provincia)
renta$renta_media <- a_numerico(renta$renta_media)

#Reiniciamos los índices de las filas y comprobamos
rownames(renta) <- NULL
print(head(renta))




#Ejercicio 3:
#Comprobamos que cada combinación dispone de exactamente los 4 trimestres de 2023
conteo_trimestres <- aggregate(Periodo ~ provincia + Sexo + Tasas, data = tasas, FUN = length)

#Lanzamos un error si en algún caso nos faltase un trimestre
stopifnot(all(conteo_trimestres$Periodo == 4))

#Calculamos la media aritmética de los cuatro trimestres aplicando la función "mean"
medias <- aggregate(valor ~ provincia + Sexo + Tasas, data = tasas, FUN = mean)

#Reestructuramos el dataframe para que tenga una fila por provincia y sexo
epa <- reshape(medias, 
               idvar = c("provincia", "Sexo"), 
               timevar = "Tasas", 
               direction = "wide")

#Asignamos los nombres correspondientes a cada columna
colnames(epa) <- c("provincia", "sexo", "tasa_actividad", "tasa_empleo", "tasa_paro")

#Ordenamos las columnas: provincia, sexo, tasa_actividad, tasa_paro, tasa_empleo
epa <- epa[, c("provincia", "sexo", "tasa_actividad", "tasa_paro", "tasa_empleo")]

#Ordenamos alfabéticamente por provincia y sexo
epa <- epa[order(epa$provincia, epa$sexo),]

#Reiniciamos los números de fila
rownames(epa) <- NULL

#MOstramos los resultados
print(head(epa))




#Ejercicio 4:
#Primero unimos poblacion y epa, que comparten provincia y sexo
datos_temp <- merge(poblacion, epa, by = c("provincia", "sexo"))

#Luego incorporamos renta a través de la clave provincia
datos <- merge(datos_temp, renta, by = "provincia")

#Definimos el orden exacto de las columnas
columnas_orden <- c("comunidad", "provincia", "anio", "sexo", "poblacion", 
                    "tasa_actividad", "tasa_paro", "tasa_empleo", "renta_media")
datos <- datos[, columnas_orden]

#Ordenamos por provincia y sexo
datos <- datos[order(datos$provincia, datos$sexo), ]
rownames(datos) <- NULL

#Mostramos las primeras 6 filas para comprobar que lo tenemos bien
print(head(datos))

#Comprobamos que no se nos escape nada:
#Verificamos que datos contiene exactamente 104 registros (52 provincias x 2 sexos)
nrow(datos) == 104

#Confirmamos que no existen duplicados por provincia y sexo (debe devolver FALSE)
any(duplicated(datos[, c("provincia", "sexo")]))

#Comprobarmos los tipos de datos de las variables
str(datos)

#Comprobamos que no se perdieron provincias (debe haber 52 provincias únicas)
length(unique(datos$provincia)) == 52

#Verificamos que no hay valores vacíos después de las uniones
sum(is.na(datos)) == 0

#Comprobamos que la renta media dentro de una misma provincia es igual para hombres y mujeres
all(tapply(datos$renta_media, datos$provincia, function(x) length(unique(x)) == 1))

#Exportamos el conjunto integrado a CSV
write.csv(datos, file = "datos_integrados_2023.csv", row.names = FALSE)




#Ejercicio 5:
#Definimos la función que va a consultar la información sobre una determinada provincia en un dataframe
consultar_provincia <- function(df, nombre_provincia){
  
  #Primero comprobamos que los parámetros recibidos son válidos
  if(!is.data.frame(df)){
    stop("El primer parámetro debe ser un dataframe válido.")
  }
  if(!is.character(nombre_provincia) || length(nombre_provincia) != 1 || is.na(nombre_provincia)){
    stop("El segundo parámetro debe ser una cadena de texto válida con el nombre de la provincia.")
  }
  if(!("provincia" %in% colnames(df))){
    stop("El dataframe proporcionado no contiene la columna 'provincia'.")
  }
  
  #La última comprobación es verificar si la provincia existe en el conjunto de datos
  if(!(nombre_provincia %in% df$provincia)){
    stop(paste("Provincia no encontrada:", nombre_provincia))
  }
  
  #Una vez pasado el filtro, buscamos los registros correspondientes a la provincia introducida
  resultado <- df[df$provincia == nombre_provincia,]
  
  #Reiniciamos el índice de las filas para que se vean ordenadas al mostrarlas por pantalla
  rownames(resultado) <- NULL
  
  #Devolvemos los resultados
  return(resultado)
}

#Comprobamos que funciona
consultar_provincia(datos, 'Granada')

#...o que no funciona
consultar_provincia(datos, 'Graná')




#Ejercicio 6:
#Apartado A: Identifique las cinco provincias con mayor y menor tasa de paro femenina.

#Filtramos los registros de mujeres y ordenamos por tasa de paro
paro_mujeres <- datos[datos$sexo == "Mujeres", c("provincia", "tasa_paro")]

#Seleccionamos las cinco provincias con mayor tasa de paro femenina y las ordenamos
mayor_paro_fem <- head(paro_mujeres[order(-paro_mujeres$tasa_paro), ], 5)
rownames(mayor_paro_fem) <- NULL

#Seleccionamos las cinco provincias con menor tasa de paro femenina y las ordenamos
menor_paro_fem <- head(paro_mujeres[order(paro_mujeres$tasa_paro), ], 5)
rownames(menor_paro_fem) <- NULL

print("Mayor tasa de paro femenina:")
print(mayor_paro_fem)

print("Menor tasa de paro femenina:")
print(menor_paro_fem)


#Apartado B: Determine las provincias con mayores diferencias entre las tasas de paro masculina y femenina.

#Separamos las tasas por sexo
paro_hombres <- datos[datos$sexo == "Hombres", c("provincia", "tasa_paro")]
colnames(paro_hombres)[2] <- "paro_hombres"

paro_mujeres <- datos[datos$sexo == "Mujeres", c("provincia", "tasa_paro")]
colnames(paro_mujeres)[2] <- "paro_mujeres"

#Unimos y calculamos la diferencia absoluta
dif_paro <- merge(paro_hombres, paro_mujeres, by = "provincia")
dif_paro$diferencia <- abs(dif_paro$paro_mujeres - dif_paro$paro_hombres)

#Obtenemos las 5 provincias con mayor diferencia
top_diferencias <- head(dif_paro[order(-dif_paro$diferencia), ], 5)
rownames(top_diferencias) <- NULL

#Mostramos el resultado
print(top_diferencias)

#Apartado C: Calcule la tasa media de paro de hombres y mujeres en las provincias andaluzas.

#Seleccionamos los datos pertenecientes a la comunidad andaluza
datos_andalucia <- subset(datos, comunidad == "Andalucía")

#Calculamos la media por sexo usando aggregate y mean
media_paro_andalucia <- aggregate(tasa_paro ~ sexo, data = datos_andalucia, FUN = mean)
colnames(media_paro_andalucia) <- c("sexo", "media_tasa_paro")

#Mostramos el resultado
print(media_paro_andalucia)

#Apartado D: Identifique las cinco provincias con mayor renta media por persona.

#Extraemos los registros únicos de provincia y renta
renta_provincias <- unique(datos[, c("provincia", "renta_media")])

#Seleccionamos las 5 entradas más altas ordenadas
top5_renta <- head(renta_provincias[order(-renta_provincias$renta_media), ], 5)
rownames(top5_renta) <- NULL

#Imprimimos el resultado
print(top5_renta)

#Apartado E: Determine si las provincias con mayor renta presentan también menores tasas de paro. Investigue con que indicador podríamos relacionar estas dos magnitudes. (Tip: Correlación entre datos)

#Calculamos la tasa de paro media para cada provincia combinando ambos sexos
paro_provincial <- aggregate(tasa_paro ~ provincia + renta_media, data = datos, FUN = mean)

#Calculamos los coeficientes de correlación lineal de Pearson y Spearman
cor_pearson <- cor(paro_provincial$renta_media, paro_provincial$tasa_paro, method = "pearson")
cor_spearman <- cor(paro_provincial$renta_media, paro_provincial$tasa_paro, method = "spearman")

#Imprimimos los coeficientes
print(paste("Correlación de Pearson:", round(cor_pearson, 4))) # = -0.594
print(paste("Correlación de Spearman:", round(cor_spearman, 4))) # = -7878

#Calculamos la correlación por sexo
cor_hombres <- cor(datos$renta_media[datos$sexo == "Hombres"], 
                   datos$tasa_paro[datos$sexo == "Hombres"])
cor_mujeres <- cor(datos$renta_media[datos$sexo == "Mujeres"], 
                   datos$tasa_paro[datos$sexo == "Mujeres"])

#Imprimimos los coeficientes de correlación para cada sexo
print(paste("Correlación en Hombres:", round(cor_hombres, 4)))
print(paste("Correlación en Mujeres:", round(cor_mujeres, 4)))

#Los coeficientes de correlación son muy negativos, lo cual quiere decir que hay una correlación muy fuerte:
#la renta media alta está relacionada con un mayor nivel de actividad y empleabilidad













