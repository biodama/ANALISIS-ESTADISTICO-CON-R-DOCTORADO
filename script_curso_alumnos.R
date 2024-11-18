



rm(list=ls())
gc()



setwd("/Users/pfernandezn/Desktop/CURSO_AER/datos/")


load("datos.curso1.RData")

# Estadisticos de tendencia central


mean(datos$"peso")

median(datos$"peso")

exp(mean(log(datos$"peso")))


# Medidas de posicion


quantile(datos$"peso")

quantile(datos$"peso",prob=seq(0,1,1/4)) # esta es la estimacion que viene por defecto

datos$"peso.gr" <- cut(datos$"peso",breaks=quantile(datos$"peso",
prob=seq(0,1,1/3)),right=TRUE, include.lowest=TRUE)

table(datos$"peso.gr",exclude=NULL)

table(datos$"peso_cat")

quantile(datos$"peso",prob=seq(0,1,1/3))

quantile(datos$"peso",prob=seq(0,1,1/10))

# Medidas de dispersion

sd(datos$"peso")

var(datos$"peso")

IQR(datos$"peso")

?quantile

min(datos$"peso")

max(datos$"peso")

range(datos$"peso") # minimo y maximo

# Graficos

hist(datos$"peso")

hist(datos$"peso",xlab="Peso (kg)",ylab="N",main="Peso",ylim=c(0,60))

qqnorm(datos$"peso")

boxplot(datos$"peso")


# Tablas

library("crosstable")

res<-crosstable(datos, c(peso,edad))


library("knitr")

kable(res)


# Variables cuantitativas agrupadas


tapply(datos$"peso",datos$"sexo",mean)

tapply(datos$"peso",datos$"sexo",sd)

tapply(datos$"peso",datos$"sexo",IQR)


library(crosstable)

res<-crosstable(datos, c(peso), by=sexo)

res

# Graficos

par(mfrow=c(1,2)) # (Numero de filas y numero de columnas del grafico)

hist(datos$"peso"[datos$"sexo"%in%"Hombre"],
main="Peso Hombres",xlab="Peso (Kg)")

hist(datos$"peso"[datos$"sexo"%in%"Mujer"],
main="Peso Mujeres",xlab="Peso (Kg)")


shapiro.test(datos$"peso"[datos$"sexo"%in%"Hombre"])
shapiro.test(datos$"peso"[datos$"sexo"%in%"Mujer"])



res<-crosstable(datos, c(peso,edad), by=sexo,total="both")

library("openxlsx")
# require("openxlsx")

write.xlsx(res,
	file="/Users/pfernandezn/Desktop/CURSO_AER/res/tabla_des_muestral_cuanti.xlsx")


# Edad (peticion Virginia)

mean(datos$"edad")

quantile(datos$"edad")

hist(datos$"edad")

qqnorm(datos$"edad")



# VARIABLES CUALITATIVAS (character o factor en R; grupos de edad, nivel de estudios...)


# Frecuencias absolutas (N, numero de registros en cada categoria o valor)

table(datos$"estado.civil",exclude=NULL) # exclude=NULL permite contabilizar los NA values

as.data.frame(table(datos$"estado.civil",exclude=NULL))

prop.table(table(datos$"estado.civil",exclude=NULL))

prop.table(table(datos$"estado.civil",exclude=NULL))*100

# Graficos cualitativas

barplot(table(datos$"estado.civil",exclude=NULL)) # frecuencias absolutas (N)

barplot(prop.table(table(datos$"estado.civil"))*100,
ylim=c(0,50)) # frecuencias relativas (%)

labels_pie<-paste0(names(table(datos$"estado.civil")),"; N=",
as.numeric(table(datos$"estado.civil")))

pie(table(datos$"estado.civil"),
labels=labels_pie)


# Variables cualitativas agrupadas

table(datos$"estado.civil",datos$"sexo",exclude=NULL)

margin.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),1)

margin.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),2)



# Tabla descriptiva


library(crosstable)

crosstable(datos,c(peso),by=sexo)

crosstable(datos,c(estado.civil),by=sexo)


# Visualizacion tipo STATA.....

gmodels::CrossTable(datos$"estado.civil",datos$"sexo",
prop.r=TRUE, prop.c=TRUE,prop.chisq=FALSE)








