
rm(list=ls())

setwd("/Users/pfernandezn/Desktop/CURSOS_R/ISCIII_UNED_DOCTORADO/Analisis_estadistico_con_R/datos")

load("datos.curso1.RData")


datos1<-subset(datos,select=c(ID,altura,edad,sexo))

require(gtsummary)


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
  
  
# Exportacion a docx
  
setwd("/Users/pfernandezn/Desktop/")
  
library(flextable)
tf <- tempfile(fileext = ".png")
tf<-("tabla_resultados.png")
ft1 <- as_flex_table(tbl_final)
save_as_image(ft1, path = tf)


