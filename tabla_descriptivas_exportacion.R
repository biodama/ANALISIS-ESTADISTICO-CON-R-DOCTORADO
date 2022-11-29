
rm(list=ls())

setwd("/Users/pfernandezn/Desktop/CURSOS_R/ISCIII_UNED_DOCTORADO/Analisis_estadistico_con_R/datos")

load("datos.curso1.RData")


tabla<-data.frame(Estadistico=c("media","mediana","media.geometrica","cuartiles",
"coeficiente de variacion","desviación típica","minimo","maximo"),
Edad=NA,Peso=NA,Altura=NA,
stringsAsFactors=FALSE)

tabla[,"Edad"]<-c(mean(datos$edad), median(datos$edad),exp(mean(log(datos$edad))),
paste(round(quantile(datos$edad),digits=2),collapse=";"),(sd(datos$edad)/mean(datos$edad))*100,
sd(datos$edad),range(datos$edad)[1],range(datos$edad)[2])

tabla[,"Peso"]<-c(mean(datos$peso), median(datos$peso),exp(mean(log(datos$peso))),
paste(round(quantile(datos$peso),digits=2),collapse=";"),(sd(datos$peso)/mean(datos$peso))*100,
sd(datos$peso),range(datos$peso)[1],range(datos$peso)[2])

tabla[,"Altura"]<-c(mean(datos$altura), median(datos$altura),
exp(mean(log(datos$altura))),paste(round(quantile(datos$altura),digits=2),collapse=";"),
c(sd(datos$altura)/mean(datos$altura))*100,sd(datos$altura),
range(datos$altura)[1],range(datos$altura)[2])

tabla

library("openxlsx")
write.xlsx(tabla,file="/Users/pfernandezn/Desktop/tabla_descriptiva.xlsx")

############################
# Otra tabla descriptiva
############################

library("crosstable")
res1<-crosstable(datos, c(altura,peso))

library("openxlsx")
write.xlsx(res1,file="/Users/pfernandezn/Desktop/tabla_descriptiva1.xlsx")


############################
# Otra tabla descriptiva
############################

library("knitr")
res2<-kable(res1)
res2

############################
# Otra tabla descriptiva
############################

library("kableExtra")
res3<-kbl(res1) 
res3

res3 %>%
kable_paper() %>%
save_kable(file = "/Users/pfernandezn/Desktop/tabla_descriptiva3.html", self_contained = T)


# Otras maneras (html)
https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html

############################
# Otra tabla descriptiva
############################

require(gtsummary)

res4<-subset(datos,select=-c(ID))%>%
tbl_summary(by="cancer.prostata") %>%
add_overall() %>%
add_p()

library(flextable)
tf <- tempfile(fileext = ".docx")
tf<-("/Users/pfernandezn/Desktop/tabla_mirar.docx")
ft1 <- as_flex_table(res4)
save_as_docx(ft1, path = tf)

