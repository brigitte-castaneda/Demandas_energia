####################################################
####        Demandas de energia transporte      ####
####          Linea base - 02/11/2023           ####
####################################################

# initial configuration
#rm(list=ls()) # limpiar entorno
#R.Version()

# load packages
require(pacman)
p_load(tidyverse , rio , data.table , png , grid, lubridate, ggplot2,
       hrbrthemes,dplyr,plotly,tseries,fUnitRoots,forecast, 
       stargazer,modelsummary,haven,lmtest,fastDummies,readr, readxl)

# directory
getwd()

#load data
Servicios_del_hogar <- read_delim("ECV/Servicios del hogar/Servicios del hogar.csv", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

Fuerza_de_trabajo <- read_delim("ECV/Fuerza de trabajo/Fuerza de trabajo.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

Educacion <- read_delim("ECV/Educacion/Educacion.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

#revision bases
colnames(Servicios_del_hogar) #variables de interes 1:3  I_HOGAR CANT_PERSONAS_HOGAR PERCAPITA 
glimpse(Servicios_del_hogar)

colnames(Fuerza_de_trabajo) #variables de interes 1:3, P6615, P6615S1, P8628, P8626S1, P8634, P6885, P6886
glimpse(Fuerza_de_trabajo)

colnames(Educacion) #variables de interes 1:3, P4693, P6167
glimpse(Educacion)

unique(Servicios_del_hogar$DIRECTORIO) #92161 
unique(Servicios_del_hogar$SECUENCIA_ENCUESTA) #1 2 3 4 5
unique(Servicios_del_hogar$SECUENCIA_P) #1

unique(Fuerza_de_trabajo$DIRECTORIO) #92161 
unique(Fuerza_de_trabajo$SECUENCIA_ENCUESTA) # 1  2  3  4  5  6  7  9  8 10 12 11 13 14 16 15 17 19 18
unique(Fuerza_de_trabajo$SECUENCIA_P) #1 2 3 4 5

unique(Educacion$DIRECTORIO) #92161 
unique(Educacion$SECUENCIA_ENCUESTA) #1  2  3  4  5  6  7  9 10  8 12 11 13 14 16 15 17 18 19
unique(Educacion$SECUENCIA_P) # 1 2 3 4 5

#selecciono las columnas de interes
Servicios_del_hogar = Servicios_del_hogar[,c(1:3, 59, 63, 61)]
Fuerza_de_trabajo = Fuerza_de_trabajo[,c(1:3, 33,34, 37,38, 57:59)]
Educacion = Educacion[,c(1:3, 24, 25)]

#DIRECTORIO identifica la vivienda a la que pertenece el hogar 
#SECUENCIA_ENCUESTA identifica el número de hogar dentro de la vivienda. 
#SECUENCIA_P identifica a la persona dentro del hogar.

#--compilo tabla----------------------------------------------------------------
#creo llaves
Servicios_del_hogar$key_DIR_E = paste(Servicios_del_hogar$DIRECTORIO, Servicios_del_hogar$SECUENCIA_ENCUESTA, sep = "_") 
#Llave entre personas
Fuerza_de_trabajo$key_DIR_P_E = paste(Fuerza_de_trabajo$DIRECTORIO, Fuerza_de_trabajo$SECUENCIA_P, Fuerza_de_trabajo$SECUENCIA_ENCUESTA, sep = "_") 
Educacion$key_DIR_P_E = paste(Educacion$DIRECTORIO, Educacion$SECUENCIA_P, Educacion$SECUENCIA_ENCUESTA, sep = "_") 

#Uno las tablas de personas
resultado <- merge(Fuerza_de_trabajo, Educacion[,-c(1:3)], by = "key_DIR_P_E")

#creo llave
resultado$key_DIR_E = paste(resultado$DIRECTORIO, resultado$SECUENCIA_P, sep = "_") 

#Uno las tablas de hogar y personas
resultado_H_P <- merge(Servicios_del_hogar, resultado[,-c(2:4)], by = "key_DIR_E")

#crear quintiles segun ingreso
quintiles <- quantile(resultado_H_P$I_HOGAR , probs = seq(0, 1, 0.2))
quintiles <- quantile(resultado_H_P$I_HOGAR, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))

resultado_H_P$quintil <- cut(resultado_H_P$I_HOGAR, breaks = quintiles, labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
summary(resultado_H_P$I_HOGAR)
glimpse(resultado_H_P)
hist(resultado_H_P$I_HOGAR)

# Cambiar la variable "mi_variable" a tipo character
resultado_H_P$quintil <- as.character(resultado_H_P$quintil)

#--------------------reviso NAS-----------------------------------
# Count NA values in the entire dataframe
na_count <- sum(is.na(resultado_H_P$quintil))

# Count NA values in each column of the dataframe
na_counts <- colSums(is.na(resultado_H_P))
na_counts_data <- subset(resultado_H_P, is.na(resultado_H_P$quintil))

na_counts_data <- subset(resultado_H_P, resultado_H_P$quintil=="Q1" )
unique(na_counts_data$I_HOGAR)
nrow(na_counts_data)

# Asignar el valor "Q1" a las observaciones con ingresos iguales a cero
resultado_H_P$quintil[resultado_H_P$I_HOGAR == 0] <- "Q1"


#------------------graficas de analisis-----------------------------------------
# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggplot2)

# Plot ingreso
resultado_H_P %>%
  ggplot(aes(x=quintil, y=log(I_HOGAR), fill=quintil)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Ingreso por quintiles") +
  xlab("")

# Plot CANTIDAD PERSONAS POR HOGAR
resultado_H_P %>%
  ggplot(aes(x=quintil, y=CANT_PERSONAS_HOGAR , fill=quintil)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("CANTIDAD PERSONAS POR HOGAR") +
  xlab("")

# Plot P6167
#¿Cuántos minutos gasta para ir a la institución a la que asiste?
resultado_H_P %>%
  ggplot(aes(x=quintil, y=P6167 , fill=quintil)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Educacion: minutos en transporte") +
  xlab("")

# Plot P6886
#¿cuánto tiempo se demora usted en su viaje de ida al trabajo? (incluya tiempo de espera del medio de transporte)
resultado_H_P %>%
  ggplot(aes(x=quintil, y=P6886 , fill=quintil)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Fuerza de trabajo: minutos en transporte") +
  xlab("")


# Tabla de resumen de variables por quintil
tabla_promedios <- resultado_H_P %>%
  group_by(quintil) %>%
  summarise(CANT_PERSONAS_HOGAR = mean(CANT_PERSONAS_HOGAR),
            I_HOGAR = mean(I_HOGAR),
            PERCAPITA = mean(PERCAPITA),
            transporte_priv = mean(P6615S1, na.rm = TRUE),
            transporte_aux = mean(P8628S1, na.rm = TRUE),
            tiempo_trabajo = mean(P6886, na.rm = TRUE), 
            tiempo_estudio = mean(P6167, na.rm = TRUE))

# Imprimir la tabla de promedios
print(tabla_promedios)

#-----------------------------------------
# formato de variables
glimpse(resultado_H_P)

# Lista de nombres de variables a convertir en character
variables_a_convertir <- c("CANT_PERSONAS_HOGAR", "P6615", "P8628",  "P8634", "P6885", "P4693")

# Loop para cambiar las variables a tipo character
for (variable in variables_a_convertir) {
  resultado_H_P[[variable]] <- as.character(resultado_H_P[[variable]])
}


#Analisis de variables categoricas de interes

# Calcular la frecuencia de las respuestas
frecuencia_respuestas <- table(resultado_H_P$P6885)
# Calcular la frecuencia de las respuestas por quintil, omitiendo los NA
frecuencia_respuestas <- aggregate(P6885 ~ quintil, data = resultado_H_P, FUN = function(x) table(x, useNA = "always"))
print(frecuencia_respuestas)

#----------- convierto matriz en tabla frecuencia_respuestas
# Crear un dataframe vacío
frecuencia_respuestas_df <- data.frame()

# Iterar a través de las filas de la tabla
for (i in 1:nrow(frecuencia_respuestas)) {
  fila <- frecuencia_respuestas[i, ]
  quintil <- fila$quintil
  matriz_fila <- as.matrix(fila$P6885)
  
  # Convertir la matriz en un dataframe y agregar una columna para el quintil
  df_fila <- as.data.frame(matriz_fila)
  df_fila$quintil <- quintil
  
  # Combinar el dataframe de la fila con el dataframe general
  frecuencia_respuestas_df <- rbind(frecuencia_respuestas_df, df_fila)
}

# Imprimir el dataframe resultante
print(frecuencia_respuestas_df_)

#elimino col nas
frecuencia_respuestas_df <- frecuencia_respuestas_df[,-c(14)]
#convierto de wide a long
frecuencia_respuestas_df <- pivot_longer(frecuencia_respuestas_df, cols = -quintil, names_to = "Tipo de transporte", values_to = "Frecuencia")
#cambio nombres de vaiable de transporte
frecuencia_respuestas_df_ <- frecuencia_respuestas_df %>%
  mutate(`Tipo de transporte` = recode(`Tipo de transporte`,
                             "1" = "Bus intermunicipal",
                             "2" = "Bus urbano",
                             "3" = "A pie",
                             "4" = "Metro",
                             "5" = "Transporte articulado",
                             "6" = "Taxi",
                             "7" = "Transporte de la empresa",
                             "8" = "Automovil de uso particular",
                             "9" = "Lancha, planchon, canoa",
                             "10" = "Caballo",
                             "11" = "Moto",
                             "12" = "Bicicleta",
                             "13" = "Otro"
  ))


# library
library(ggplot2)

glimpse(frecuencia_respuestas_df)

# Grouped
ggplot(frecuencia_respuestas_df_, aes(fill=`Tipo de transporte`, y=Frecuencia, x=quintil)) + 
  geom_bar(position="dodge", stat="identity")

# Stacked + percent
porcentaje_tipo_transporte <- ggplot(frecuencia_respuestas_df_, aes(fill=`Tipo de transporte`, y=Frecuencia, x=quintil)) + 
  geom_bar(position="fill", stat="identity")
porcentaje_tipo_transporte

# ---------------------------
#agregar el tiempo a la tabla.

#clasificar los medios de transporte segun los requiera
