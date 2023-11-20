################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

##############################################
# SCRIPT CURSO ANALISIS ESTADISTICO
##############################################

# R version 4.1.0
# Ordenador: Zeus

rm(list=ls()) # borrar todos los objetos del workspace
gc()

ls()

############
# Windows:
############

# setwd("C:\\Users\\gargu\\OneDrive\\Escritorio\\Analisis estadistico R\\datos\\")
# Otra manera: setwd("C:/Users/gargu/OneDrive/Escritorio/Analisis estadistico R/datos/")
# load("datos.curso1.RData")

# OneDrive:
# setwd("C:\\Users\\catlu\\OneDrive - Universidad de Córdoba (1)\\Ana\\Cursos Rstudio\\Análisis estadístico con Rstudio (ICIII)\\material_curso\\datos\\")
# setwd("C:/Users/catlu/OneDrive - Universidad de Córdoba (1)/Ana/Cursos Rstudio/Análisis estadístico con Rstudio (ICIII)/material_curso/datos/")
# load("datos.curso1.RData")


setwd("/Users/pfernandezn/Desktop/material_curso/datos")

load("datos.curso1.RData")

ls() # listar los nombres de los objetos creados o importados

class(datos) # data.frame

dim(datos) # 200  11
str(datos)

head(datos)

View(datos)


###################################################
###################################################
# ESTADISTICA DESCRIPTIVA
###################################################
###################################################


# Variable cuantitativa

datos$"peso"

datos[   , c(6) ]

datos[,6]

class(datos$"peso")

datos$"peso" <- as.numeric(datos$"peso")

class(datos$"peso")

mean(datos$"peso") # 69.94747
median(datos$"peso") # 69.75524
exp(mean(log(datos$"peso"))) # 69.21468 # media geometrica

quantile(datos$"peso",prob=seq(0,1,1/4))
#       0%      25%      50%      75%     100% 
# 56.56025 59.98709 69.75524 80.00352 82.28081 

?quantile
quantile(datos$"peso",prob=seq(0,1,1/4),type=2) # ?STATA

datos$"peso.gr" <- cut(datos$"peso",breaks=quantile(datos$"peso",prob=seq(0,1,1/3)),
                       right=TRUE, include.lowest=TRUE)
table(datos$"peso.gr",exclude=NULL)


sd(datos$"peso") # 10.11981

var(datos$"peso") # 102.4105

IQR(datos$"peso")# 20.01642  Q75%-Q25%

range(datos$"peso")
min(datos$"peso")
max(datos$"peso")

hist(datos$"peso")
hist(datos$"peso",xlab="Peso (kg)",ylab="N",main="Peso",ylim=c(0,60))

boxplot(datos$"peso")

qqnorm(datos$"peso")

summary(datos[,c("peso","edad")])

# install.packages("crosstable")
# install.packages("knitr")

library("crosstable")
library("knitr")

res<-crosstable(datos, c(peso,altura))
kable(res)

qqnorm(datos$"edad")

qqnorm(datos$"altura")
qqnorm(datos$"peso")

# Tabla a la antigua usanza

tabla <- data.frame(Estadistico=c("media","mediana","media.geometrica",
                          "cuartiles","coeficiente de variacion",
                          "desviación típica","minimo","maximo"),
                  Edad=NA,Peso=NA,Altura=NA,
                  stringsAsFactors=FALSE)


tabla[,"Edad"]<-c(mean(datos$"edad"), median(datos$"edad"),
                  round(exp(mean(log(datos$"edad"))),digits=2),
                  paste(round(quantile(datos$"edad"),digits=2),collapse=";"),
                  c(sd(datos$"edad")/mean(datos$"edad"))*100,
                  sd(datos$"edad"),
                  range(datos$"edad")[1],range(datos$"edad")[2])

tabla[,"Peso"]<-c(mean(datos$"peso"), median(datos$"peso"),
                  exp(mean(log(datos$"peso"))),
                  paste(round(quantile(datos$"peso"),digits=2),collapse=";"),
                  c(sd(datos$"peso")/mean(datos$"peso"))*100,
                  sd(datos$"peso"),
                  range(datos$"peso")[1],range(datos$"peso")[2])

tabla[,"Altura"]<-c(mean(datos$"altura"), median(datos$"altura"),
                    exp(mean(log(datos$"altura"))),
                    paste(round(quantile(datos$"altura"),digits=2),collapse=";"),
                    c(sd(datos$"altura")/mean(datos$altura))*100,
                    sd(datos$altura),
                    range(datos$altura)[1],range(datos$altura)[2])


library("openxlsx")
write.xlsx(tabla,file="/Users/pfernandezn/Desktop/tabla_descriptiva_curso.xlsx")




