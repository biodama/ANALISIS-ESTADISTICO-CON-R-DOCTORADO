rm(list=ls())
gc()

setwd('/Users/pfernandezn/Desktop/ANALISIS-ESTADISTICO-CON-R-DOCTORADO-main 2')

getwd()

curso<-"Curso 2026"
curso<-2026

curso_2026 <- 2026
curso_2026 =2026

x <- 1:15
length(x)
class(x)


y <- matrix(5, nrow = 3, ncol = 4)
dim(y)
class(y)


datos<-data.frame(ID=c("a1", "a2", "a3"), edad=c(15,22,7))
as.factor(x)


demo(graphics)
library(ggplot2)

openxlsx::read.xlsx()
ggplot2::ggplot()

my.data.frame<-data.frame(ID=c("paciente1","paciente2","paciente3"), edad=c(10,25,33),altura=c(NA,150,180), cancer=c(TRUE,TRUE,FALSE),bmi=c(22,25,20))

load("datos/datos.curso1.RData")
#library(data.table)

#datos<-data.table(datos)
class(datos)

datos$"sexo"
length(datos$"sexo")

datos[,3]

datos[1:3,3]
datos[1:10,1:3]
datos[datos$"sexo"=="Mujer",] 
datos[datos$"sexo"=="Mujer"&datos$"estado.civil"=="Casado",1:10]
datos[datos$"sexo"%in%"Mujer"&datos$"estado.civil"%in%"Casado",1:10]


datos.new2<-datos 
datos.new2[datos.new2$”ID”==200,sexo]<-"Mujer"
datos.new2$”sexo”[datos.new2$”ID”==200]<-"Mujer"

datos.nuevos1 <-datos[1 ,1:3]

datos.union1<-cbind(datos[1:20,],datos.nuevos1)


datos.nuevos2 <-datos[1:5, ]
datos.union2 <-rbind(datos,datos.nuevos2)

indice5 <-datos$"sexo"!="Mujer"
datos[indice5,]
datos[ , names(datos)!="peso"]

indice7 <-which(datos$"sexo"=="Mujer")
datos[indice7,]


datos$cancer.mama[is.na(datos$cancer.mama)]<-"No"

prueba<-datos[indice7,]
row.names(prueba)<-NULL


x<-c(1:10); y<-x/2 
x+15; x2<-c(1:5)

datos.con.missing<-datos
datos.con.missing$peso[datos.con.missing$peso<60]<-NA

mean(datos.con	.missing$peso)
sum(is.na(datos.con.missing$"peso"))

summary(datos)

datos$ratio<-datos$altura/datos$edad

?openxlsx::write.xlsx

a1 <-rnorm(10); a2 <-1:10; a3 <-letters[10:20]
save(a1, a2, file="datos/unos.datos.guardados.RData")

?median

install.packages("data.table")
library(data.table)
library(SNPannot)

datos$"ratio"<-datos$"altura"/datos$"edad"

x<-c(2:6)
lista<-list(vector1=x,datos=datos)
lista$vector1 # lista[[1]]
lista$datos

########################################################
########################################################
# Estadistica descriptiva
########################################################
########################################################
rm(list=ls())
gc()

setwd('/Users/pfernandezn/Desktop/ANALISIS-ESTADISTICO-CON-R-DOCTORADO-main 2')

load("datos/datos.curso1.RData")

mean(datos$"peso")
median(datos$"peso")
exp(mean(log(datos$"peso")))
quantile(datos$"peso")

quantile(datos$"peso",prob=seq(0,1,1/4))
quantile(datos$"peso",prob=seq(0,1,1/3))
quantile(datos$"peso",prob=seq(0,1,1/10))

datos$"peso.gr" <- cut(datos$"peso",breaks=quantile(datos$"peso", prob=seq(0,1,1/3)),right=TRUE, include.lowest=TRUE)

sd(datos$"peso")
IQR(datos$"peso")

range(datos$"peso")
hist(datos$"peso",xlab="Peso (kg)",ylab="N",main="Peso",ylim=c(0,60))
qqnorm(datos$"peso")

library(crosstable)library(knitr)
res<-crosstable(datos, c(peso))kable(res)


datos.hombre<-datos[datos$"sexo"%in%"Hombre",]
datos.mujer<-datos[datos$"sexo"%in%"Mujer",]
tapply(datos$"peso",datos$"sexo",mean)

tapply(datos$"peso",datos$"sexo",mean)[1]

res<-crosstable(datos, c(peso), by=sexo)kable(res)

par(mfrow=c(1,2))hist(datos$"peso"[datos$"sexo"%in%"Hombre"],main="Peso Hombres",xlab="Peso (Kg)")hist(datos$"peso"[datos$"sexo"%in%"Mujer"],main="Peso Mujeres",xlab="Peso (Kg)")

library(ggplot2)

ggplot(datos, aes(x = peso)) +geom_histogram() +facet_wrap(~sexo, nrow = 1,scales="free_x") +labs(x = "Peso (Kg)",y = "Frecuencia")+theme_minimal()

boxplot(datos$"peso" ~ datos$"sexo", main="Peso",xlab="Sexo",ylab="Peso (Kg)")

table(datos$"estado.civil",exclude=NULL)
as.data.frame(table(datos$"estado.civil",exclude=NULL))

margin.table(table(datos$"estado.civil",exclude=NULL))

prop.table(table(datos$"estado.civil",exclude=NULL))

prop.table(table(datos$"estado.civil",exclude=NULL))*100


barplot(table(datos$"estado.civil"))

barplot(prop.table(table(datos$"estado.civil"))*100,ylim=c(0,50))

table(datos$"estado.civil",datos$"sexo",exclude=NULL)

margin.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),1)

margin.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),2)


prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL))*100

prop.table(table(datos$"estado.civil",datos$"fumador",exclude=NULL))*100
table(datos$"estado.civil",datos$"fumador",exclude=NULL)

prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),1)*100

prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),2)*100

par(mfrow=c(2,1))barplot(table(datos$"estado.civil"[datos$"sexo"%in%"Hombre"]),main="Hombres",ylim=c(0,50))barplot(table(datos$"estado.civil"[datos$"sexo"%in%"Mujer"]),main="Mujeres",ylim=c(0,50))


gmodels::CrossTable(datos$"estado.civil",datos$"sexo",prop.r=TRUE, prop.c=TRUE,prop.chisq=FALSE)
library(crosstable)crosstable(datos,c(estado.civil),by=sexo)%>%as_flextable(keep_id=TRUE)


crosstable(datos,c(peso,estado.civil),by=sexo)%>%
as_flextable(keep_id=TRUE)


#############################################
# Ejercicios
#############################################
d<-crosstable(
 datos,
	c(edad, peso, altura),
funs = c(
"Media" = mean,
"Mediana" = median,
 "Media geométrica" = function(x) exp(mean(log(x))),
 "Q1" = function(x) quantile(x, 0.25),
"Q2" = function(x) quantile(x, 0.50),
"Q3" = function(x) quantile(x, 0.75),
 "Coef. variación (%)" = function(x) sd(x) / mean(x) * 100,
"Desv. típica" = sd,
"Máximo" = max,
 "Mínimo" = min
  )
 )

kable(d)

d1<-crosstable(
  datos,
  c(edad, peso, altura),
  by = sexo,
  funs = c(
    "Media" = mean,
    "Mediana" = median,
    "Media geométrica" = function(x) exp(mean(log(x))),
    "Q1" = function(x) quantile(x, 0.25),
    "Q2" = function(x) quantile(x, 0.50),
    "Q3" = function(x) quantile(x, 0.75),
    "Coef. variación (%)" = function(x) sd(x) / mean(x) * 100,
    "Desv. típica" = sd,
    "Máximo" = max,
    "Mínimo" = min
  )
)

kable(d1)

stripchart(datos$peso~datos$fumador)
stripchart(datos$peso~datos$fumador,method="jitter")

margin.table(table(datos$nivel.estudios),margin=1)

prop.table(table(datos$nivel.estudios))







