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
kable(res) # para cambiar el formato de la salida

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

par(mfrow=c(1,2))
hist(datos$"peso"[datos$"sexo"%in%"Hombre"],
     main="Peso Hombres",xlab="Peso (Kg)")
hist(datos$"peso"[datos$"sexo"%in%"Mujer"],
     main="Peso Mujeres",xlab="Peso (Kg)")

tapply(datos$"peso",datos$"sexo",mean)

tapply(datos$"peso",datos$"sexo",sd)

tapply(datos$"peso",datos$"sexo",mi.funcion)

library(crosstable)
res<-crosstable(datos, c(peso,edad), 
                by=sexo, num_digits = 3,total="both")


par(mfrow=c(1,2))
hist(datos$"edad"[datos$"sexo"%in%"Hombre"],
     main="Edad Hombres",xlab="Edad")
hist(datos$"edad"[datos$"sexo"%in%"Mujer"],
     main="Edad Mujeres",xlab="Edad")


# Otras tablas

crosstable(datos, c(peso, altura), funs=c(median, meanCI, sd))

crosstable(datos, c(peso, altura), funs=c("MEDIANA"=median, 
                                          "MEDIA (IC)"=meanCI, "DES"=sd))

# MIRARRRRR https://cran.r-project.org/web/packages/crosstable/vignettes/crosstable.html


# Variables cualitativas

table(datos$"estado.civil",exclude=NULL)

margin.table(table(datos$"estado.civil",exclude=NULL))

prop.table(table(datos$"estado.civil",exclude=NULL))*100



table(datos$"sexo",exclude=NULL)

margin.table(table(datos$"sexo",exclude=NULL))

prop.table(table(datos$"sexo",exclude=NULL))*100


# Graficos variable cualitativa

barplot(table(datos$"estado.civil"),ylim=c(0,90),xlab="Estado Civil",
        ylab="Frecuencia Absoluta (N)")


pie(table(datos$"estado.civil"),
    labels=paste(c("Casado","Divorciado","Soltero"), 
                                sep = " ", table(datos$"estado.civil")))

library("plotrix")
slices <- table(datos$"estado.civil")
lbls <- c("Casado","Divorciado","Soltero")
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart Estado civil")


# Variable cualitativa agrupada

table(datos$"estado.civil",datos$"sexo",exclude=NULL)

prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL))*100

prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),1)*100

prop.table(table(datos$"estado.civil",datos$"sexo",exclude=NULL),2)*100

# Tabla clasica

# Es dificil                                                                                                                                                                                                              tabla_fre

# Tabla mejor

library("gmodels")

CrossTable(datos$"estado.civil",datos$"sexo",
                    prop.r=TRUE, prop.c=TRUE,prop.chisq=FALSE)


library(crosstable)
crosstable(datos,c(estado.civil),by=sexo)%>%
  as_flextable(keep_id=TRUE)


crosstable(datos,c(peso,edad,estado.civil),by=cancer.mama,total="both")

############################
# Tabla total descriptiva
############################

crosstable(datos,c(peso,edad,estado.civil),by=sexo)

# Y si tengo yo mi propia funcion descriptiva (variables cuantitativas)

library("beepr")
beep(sound = 3, expr = NULL)

descript<-function(x,nombre){
  
  tabla<-data.frame(Estadistico=c("media","mediana","media.geometrica","cuartiles",
                                  "coeficiente de variacion","desviación típica","maximo","minimo"),
                    Variable=NA,
                    stringsAsFactors=FALSE)
  
  tabla[,"Variable"]<-c(mean(x), median(x),exp(mean(log(x))),paste(quantile(x),
                            collapse=";"),(sd(x)/mean(x))*100,sd(x),range(x)[1],
                        range(x)[2])
  
  names(tabla)[2]<-nombre
  
  beepr::beep(sound = 3, expr = NULL)
  
  tabla
  
}


descript(x=datos$"edad",nombre="edad")


tapply(datos$"edad",datos$"sexo",descript,nombre="edad")

#########################################
#########################################
#  INFERENCIA
#########################################
#########################################

mujeres <- datos[datos$"sexo"%in%"Mujer", ]

hombres <- datos[datos$"sexo"%in%"Hombre" , ]



res<-t.test(hombres$"peso",conf.level = 0.95)

res

names(res)

res$"conf.int"
res$"estimate"

mean_ic95<-paste(round(res$"estimate",2)," (",round(res$"conf.int"[1],2),"-",
                 round(res$"conf.int"[2],2),")",sep="")
mean_ic95

################################################################################


t.test(mujeres$"peso",hombres$"peso",var.equal = FALSE)

t.test(mujeres$"peso",hombres$"peso",var.equal = TRUE)

res_alternativo<-t.test(datos$"peso" ~ datos$"sexo")

res<-t.test(hombres$"peso",mu = 90,sd=1, conf.level = 0.95)
res

wilcox.test(mujeres$"peso",hombres$"peso")

# test parametrico

anova(lm(hombres$"peso"~hombres$"nivel.estudios"))

pairwise.t.test(x=hombres$"peso",g=as.factor(hombres$"nivel.estudios"),
                p.adj="BH")

# test no parametrico

kruskal.test(hombres$"peso"~hombres$"nivel.estudios")

pairwise.wilcox.test(x=hombres$"peso",g=as.factor(hombres$"nivel.estudios"),
                     p.adj="BH")


# VARIABLES CUALITATIVAS (prop.test prop.trend.test)


# Comparar con un valor 0.40

table(datos$"fumador",exclude=NULL)
prop.table(table(datos$"fumador",exclude=NULL))


prop.test(as.numeric(table(datos$"fumador"))[2] , dim(datos)[1] ,0.40,alternative="greater")

prop.test(92,200, 0.40 ,alternative="greater")

# Comparar la proporcion de fumadores entre hombres y mujeres

table(datos$sexo,datos$fumador)[,c(2,1)]

prop.table(table(datos$sexo,datos$fumador)[,c(2,1)],1)

#suc<-table(datos$"fumador",datos$"sexo")[2,]
#tot<-margin.table(table(datos$"fumador",datos$"sexo"),2)
#prop.test(suc,tot)

prop.test(table(datos$sexo,datos$fumador)[,c(2,1)])

prop.test(table(datos$sexo,datos$fumador)[,c(2,1)],correct=FALSE)



# Compara la proporcion de fumadores entre los distintos niveles de estudios
table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)]
margin.table(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)],1)

# igual que el anova en variables cuantitativas
prop.test(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)])

prop.trend.test(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2)],
                margin.table(table(datos$nivel.estudios,datos$fumador)[c(2,3,1),c(2,1)],1))

# TABLA 0 DESCRIPTIVA CON COMPARACIONES

require("gtsummary")
require("kableExtra")

datos0<- subset(datos,select=c(edad,estado.civil,peso,cancer.prostata))

datos0 %>%
  tbl_summary(by="cancer.prostata") %>%
  add_overall() %>%
  add_p()

datos0 %>%
  tbl_summary(by="cancer.prostata") %>%
  add_overall() %>%
  add_p() %>%
  as_kable_extra(booktabs = TRUE, longtable = FALSE, linesep = "")



# TABLA 1 DESCRIPTIVA CON COMPARACIONES

require("gtsummary") # library

datos_1<-subset(datos,select=-c(ID,sexo,cancer.mama))

datos_1 %>%    
tbl_summary(by="cancer.prostata") %>%
  add_overall() %>%
  add_p()


# Exportacion (Patricia):

setwd("C:/Users/gargu/OneDrive/Escritorio/Analisis estadistico R/datos")

load("datos.curso1.RData")

require("gtsummary")

datos_1<-subset(datos,select=-c(ID,sexo,cancer.mama))

tabla_exportar<-datos_1 %>%    
  tbl_summary(by="cancer.prostata") %>%
  add_overall() %>%
  add_p()

library("flextable")
tf <- tempfile(fileext = ".docx")
tf<-("tabla_exportar.docx")
ft1 <- as_flex_table(tabla_exportar)
save_as_docx(ft1, path = tf)

getwd() # el archivo se ha guardado aqui


# TABLA 2 DESCRIPTIVA CON COMPARACIONES varios test

datos1<-subset(datos,select=c(ID,altura,edad,sexo))

require("gtsummary")

tbl0<-subset(datos1,select=-c(ID,sexo)) %>%
  tbl_summary(statistic = all_continuous() ~ "{mean}") %>%
  add_ci(pattern = "{stat} ({ci})")

tbl1<-subset(datos1,select=-c(ID)) %>%
  tbl_summary(by="sexo",
              statistic = all_continuous() ~ "{mean}")%>%
  add_ci(pattern = "{stat} ({ci})")

tbl2<-subset(datos1,select=-c(ID)) %>%
  tbl_summary(by="sexo") %>%
  add_p(test = everything() ~ "t.test")%>%
  modify_column_hide(all_stat_cols())

tbl3<-subset(datos1,select=-c(ID)) %>%
  tbl_summary(by="sexo") %>%
  add_p(test = everything() ~ "wilcox.test")%>%
  modify_column_hide(all_stat_cols())

tbl_final <- 
  tbl_merge(list(tbl0,tbl1, tbl2, tbl3)) %>%
  modify_spanning_header(everything() ~ NA)

tbl_final

# Exportacion en png

setwd("/Users/pfernandezn/Desktop/")

library(flextable)
tf <- tempfile(fileext = ".png")
tf<-("tabla_resultados.png")
ft1 <- as_flex_table(tbl_final)
save_as_image(ft1, path = tf)


# TABLA 3 DESCRIPTIVA CON COMPARACIONES

datos3<-subset(datos,select=c(altura,edad,nivel.estudios,cancer.prostata))

tabla_3<-datos3 %>%    
  tbl_summary(by="cancer.prostata",
              statistic=list(all_continuous() ~ "{mean}", 
                             all_categorical() ~ "{n} ({p}%)")) %>%
  add_ci()%>%
  add_overall() %>%
  add_p(test=list(all_continuous() ~ "t.test", 
                  all_categorical() ~ "chisq.test"))


# Caso evaluacion

tabla_1<-datos %>%    
  tbl_summary(by="cancer.mama")


tabla_1<-datos %>%    
  tbl_summary(by="fumador") # quitais el cancer de mama





#####################
# CURIOSIDADES
#####################

# APA style 
# Test: https://cran.r-project.org/web/packages/apa/vignettes/introduction.html
# Graficos: https://search.r-project.org/CRAN/refmans/jtools/html/theme_apa.html

mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V-shaped", "Straight"))
  am <- factor(am, labels = c("Automatic", "Manual"))
  cyl  <- factor(cyl)
  gear <- factor(gear)
})

library("ggplot2")
library("jtools")
p1 <- ggplot(mtcars2) +
  geom_point(aes(x = wt, y = mpg, colour = gear)) +
  labs(
    title = "Fuel economy declines as weight increases",
    subtitle = "(1973-1974)",
    x = "Weight (1000 lbs)",
    y = "Fuel economy (mpg)",
    colour = "Gears"
  )

p1
p1 + theme_apa()



