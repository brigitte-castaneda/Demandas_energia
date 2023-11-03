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

resultado_H_P$quintil <- cut(resultado_H_P$I_HOGAR, breaks = quintiles, labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
hist(resultado_H_P$CANT_PERSONAS_HOGAR)

# Cambiar la variable "mi_variable" a tipo character
resultado_H_P$quintil <- as.character(resultado_H_P$quintil)

#--------------------reviso NAS-----------------------------------


#------------------graficas de analisis-----------------------------------------
# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggplot2)

# Plot ingreso
resultado_H_P %>%
  ggplot(aes(x=quintil, y=I_HOGAR, fill=quintil)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Ingreso por quintiles") +
  xlab("")


# Plot ingreso
resultado_H_P %>%
  ggplot(aes(x=quintil, y=I_HOGAR, fill=quintil)) +
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
  ggtitle("Ingreso por quintiles") +
  xlab("")

# Plot P6167
resultado_H_P %>%
  ggplot(aes(x=quintil, y=P6167 , fill=quintil)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Ingreso por quintiles") +
  xlab("")
