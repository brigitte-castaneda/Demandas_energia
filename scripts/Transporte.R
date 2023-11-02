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
                                  +     delim = ";", escape_double = FALSE, trim_ws = TRUE)

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

#crear quintiles segun ingreso
