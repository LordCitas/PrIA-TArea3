

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
colnames(epa) <- c("provincia", "sexo", "tasa_actividad", "tasa_paro", "tasa_empleo")

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

# Segunda unión: incorporamos renta a través de la clave provincia
datos <- merge(datos_temp, renta, by = "provincia")

# ==========================================
# 2. Reordenación de columnas y filas
# ==========================================

# Definir el orden exacto de las columnas solicitado
columnas_orden <- c("comunidad", "provincia", "anio", "sexo", "poblacion", 
                    "tasa_actividad", "tasa_paro", "tasa_empleo", "renta_media")
datos <- datos[, columnas_orden]

# Ordenar por provincia y sexo (reproduce la salida exacta mostrada en la captura)
# Nota: Si el docente pide estrictamente por comunidad autónoma, se usaría:
# datos <- datos[order(datos$comunidad, datos$provincia, datos$sexo), ]
datos <- datos[order(datos$provincia, datos$sexo), ]
rownames(datos) <- NULL

# Visualizar las primeras 6 filas para verificar la coincidencia con la captura
print(head(datos))

# ==========================================
# 3. Comprobaciones requeridas
# ==========================================

# A. Verificar que contiene exactamente 104 registros (52 provincias x 2 sexos)
nrow(datos) == 104

# B. Confirmar que no existen duplicados por provincia y sexo (debe devolver FALSE)
any(duplicated(datos[, c("provincia", "sexo")]))

# C. Comprobar tipos de datos de las variables
str(datos)

# D. Comprobar que no se perdieron provincias (debe haber 52 provincias únicas)
length(unique(datos$provincia)) == 52

# E. Verificar que no existen valores faltantes (NA) producidos durante las uniones
sum(is.na(datos)) == 0

# F. Comprobar que la renta media es idéntica para hombres y mujeres dentro de la misma provincia
all(tapply(datos$renta_media, datos$provincia, function(x) length(unique(x)) == 1))

# ==========================================
# 4. Exportar el conjunto integrado a CSV
# ==========================================

write.csv(datos, file = "datos_integrados_2023.csv", row.names = FALSE)













