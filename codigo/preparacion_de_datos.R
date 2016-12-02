
rm(list =ls())

install.packages("reshape")

library(dplyr)
library(lubridate)
library(viridis)
library(ggthemes)
library(RColorBrewer)
library(sm)
library(RCurl)
library(ggplot2)
library(reshape)
library(rgeos)
library(readr)

setwd("/home/stuka/MexEval/data/")
dir()
mexdel = readOGR(dsn = ".","homicidios_join_agebs", encoding = "utf-8") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
head(mexdel@data)
homi <- mexdel@data %>% select(CVEGEO,modalidad,anio) %>% group_by(CVEGEO,modalidad,anio) %>% summarize(homi_count = n())
summary(homi)
glimpse(homi)
for(i in 1:ncol(homi)){
  homi[[i]] <- as.character(homi[[i]])
}




agebs = readOGR(dsn = ".","AGEBS_DF_Centroids", encoding = "utf-8") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
agebs_code <- as.character(unique(agebs@data$CVEGEO))
anios <- as.character(unique(homi$anio))
modalidades <- as.character(unique(homi$modalidad))
homi_full <- expand.grid(agebs_code,anios,modalidades,stringsAsFactors = FALSE)
names(homi_full) <- c("CVEGEO","anio","modalidad","homi_count")
for(i in 1:ncol(homi_full)){
  homi_full[[i]] <- as.character(homi_full[[i]])
}
homi_full <- left_join(homi_full,homi,by = c("CVEGEO"="CVEGEO","anio"="anio","modalidad"="modalidad"))
homi_full[which(is.na(homi_full$homi_count.y)),4] <- 0
homi_full$homi_count.y <- as.numeric(homi_full$homi_count.y) 
sum(homi_full$homi_count.y)
agebs@data$CVEGEO <- as.character(agebs@data$CVEGEO)
df_indices_ageb <- agebs@data
names(df_indices_ageb) <- names[c(1,5:length(names))]
df_indices_ageb[,c(1,3:13)] <- NULL
glimpse(df_indices_ageb)
homi_full <- left_join(homi_full,agebs@data,by = c("CVEGEO"="CVEGEO"))
head(homi_full)
homi_full[,c(5:16)] <- NULL
names <- c(names(homi_full)[1:4],
"Municipio",
"POBTOT",
"P_18YMAS",
"P18YM_PB",
"GRAPROES",
"VIVPAR_HAB",
"VPH_PISODT",
"PROM_OCUP",
"VPH_TV",
"VPH_AUTOM",
"VPH_PC",
"VPH_INTER",
"INDICE_GLOBAL",
"NSE_num",
"NSE_char")
names(homi_full) <- names
head(homi_full)
agebs@data[which(agebs@data$CVEGEO == "0900200010769"),]
homi_full[which(homi_full$CVEGEO == "0900200010769"),][1,]
summary(homi_full)
raw_data_final <- homi_full[complete.cases(homi_full),]
length(unique(raw_data_final$CVEGEO))
summary(raw_data_final)
for(i in 1:3){
  raw_data_final[[i]] <- factor(raw_data_final[[i]])
}
write_csv(raw_data_final,"/home/stuka/RegresionAvanzada/ProyFin/raw_data.csv")



#### Tabla 4 años
tabla_4anios <- raw_data_final %>% select(CVEGEO,homi_count.y) %>% group_by(CVEGEO) %>% summarise(homi_count = sum(homi_count.y))
tabla_4anios$CVEGEO <- as.character(tabla_4anios$CVEGEO)
names(homi_full)
tmp <- homi_full[,c(1,5:ncol(homi_full))]
tmp$CVEGEO <- as.character(tmp$CVEGEO)
tabla_4anios <- left_join(tabla_4anios,df_indices_ageb,by = c("CVEGEO"="CVEGEO"))
summary(tabla_4anios)
hist(tabla_4anios$homi_count,breaks = 50)
t <- table(tabla_4anios$homi_count)
quantile(tabla_4anios$homi_count,seq(0,1,0.05))
?quantile
100*table(tabla_4anios$homi_count)/nrow(tabla_4anios)


###################################################################################
library(dplyr)


setwd("/home/denny/itam/modelos_lgeneralizados/ago_2016/proyecto_final")
tabla_4anios <- read.table("tabla_4_anios.csv", sep=",", header=T,stringsAsFactors = FALSE) #REALMENTE ESTA TABLA NO AGREGA LOS 4 ANIOS

length(unique(tabla_4anios$CVEGEO)) #2,411 agebs


#CREO UNA TABLA PARA LAS VARIABLES ECONOMICAS NIVEL AGEB
var_eco <- tabla_4anios %>% 
  select(CVEGEO,POBTOT,P_18YMAS,P18YM_PB,GRAPROES,VIVPAR_HAB,VPH_PISODT,PROM_OCUP,VPH_TV,VPH_AUTOM,VPH_PC,VPH_INTER,INDICE_GLOBAL,NSE_num,NSE_char)%>% 
  group_by(CVEGEO) %>% 
  summarize(POBTOT=unique(POBTOT),P_18YMAS=unique(P_18YMAS),P18YM_PB=unique(P18YM_PB),GRAPROES=unique(GRAPROES),VIVPAR_HAB=unique(VIVPAR_HAB),VPH_PISODT=unique(VPH_PISODT),PROM_OCUP=unique(PROM_OCUP),VPH_TV=unique(VPH_TV),VPH_AUTOM=unique(VPH_AUTOM),VPH_PC=unique(VPH_PC),VPH_INTER=unique(VPH_INTER),INDICE_GLOBAL=unique(INDICE_GLOBAL),NSE_num=unique(NSE_num),NSE_char=unique(NSE_char))

length(unique(var_eco$CVEGEO)) #comprobamos que seguimos con 2,411 agebs


#CREO LA TABLA QUE AGREGA LOS 4 ANIOS
unique(tabla_4anios$anio) #tenemos que agregar los 4 anios
tabla_4anios_ok <- tabla_4anios %>% select(CVEGEO,anio,modalidad,homi_count.y)%>% group_by(CVEGEO)%>% summarize(homi_count=sum(homi_count.y))
nrow(tabla_4anios_ok) #2411 agebs

tabla_4anios_ok$prom_homi <- tabla_4anios_ok$homi_count / 4

tabla_4anios_ok <- left_join(tabla_4anios_ok,var_eco,by=c("CVEGEO"="CVEGEO")) #TABLA PARA EL PRIMER MODELO NAIVE
head(tabla_4anios_ok,20)


#VAMOS A CREAR LA SEGUNDA TABLA DONDE AGRUPAMOS LOS 4 ANIOS PERO SEPARAMOS POR MODALIDAD DE ARMA BLANCA, ARMA DE FUEGO Y OTROS
names(tabla_4anios)

a <- tabla_4anios %>% select(CVEGEO,anio,modalidad,homi_count.y)%>% group_by(CVEGEO,modalidad)%>% summarize(homi_count=sum(homi_count.y))
unique(a$modalidad)

for(i in 1:nrow(a)){
  if(a$modalidad[i]=="Arma blanca"){
    a$modalidad[i] <- "Arma blanca"
  }else if(a$modalidad[i]=="Arma de fuego"){
    a$modalidad[i] <- "Arma de fuego"
  }else{a$modalidad[i] <- "Otros"}
}

unique(a$modalidad)

a <- a %>% select(CVEGEO,modalidad,homi_count)%>% group_by(CVEGEO,modalidad)%>% summarize(homi_count=sum(homi_count))


a$bin_ab <- rep(0,nrow(a))
a$bin_af <- rep(0,nrow(a))
a$bin_o <- rep(0,nrow(a))

for(i in 1:nrow(a)){
  if(a$modalidad[i] == "Arma blanca"){
    a$bin_ab[i] <- 1
  }else if(a$modalidad[i] == "Arma de fuego"){
    a$bin_af[i] <- 1
  }else if(a$modalidad[i] == "Otros"){
    a$bin_o[i] <- 1
  }
}

a$prom_homi <- a$homi_count / 4
#TABLA PARA EL 2DO MODELO AGRUPANDO LOS 4 ANIOS, PERO SEPARANDO POR 3 MODALIDADES
tabla_4anios_mod <- left_join(a,var_eco,by=c("CVEGEO"="CVEGEO"))
head(tabla_4anios_mod,10)


#CREAMOS TABLA PARA 3ER MODELO
#SÓLO TRABAJAREMOS CON 2014 VS 2015 SEPARADO POR MODALIDADES

b <- tabla_4anios
b <- subset(b, anio>="2014")
unique(b$anio)

b <- b %>% select(CVEGEO,anio,modalidad,homi_count.y)%>% group_by(CVEGEO,anio,modalidad)%>% summarize(homi_count=sum(homi_count.y))

for(i in 1:nrow(b)){
  if(b$modalidad[i]=="Arma blanca"){
    b$modalidad[i] <- "Arma blanca"
  }else if(b$modalidad[i]=="Arma de fuego"){
    b$modalidad[i] <- "Arma de fuego"
  }else{b$modalidad[i] <- "Otros"}
}

unique(b$modalidad)
names(b)
b <- b %>% select(CVEGEO,anio,modalidad,homi_count)%>% group_by(CVEGEO,anio,modalidad)%>% summarize(homi_count=sum(homi_count))
head(b,12)

b$bin_ab <- rep(0,nrow(b))
b$bin_af <- rep(0,nrow(b))
b$bin_o <- rep(0,nrow(b))
b$bin_2014 <-rep(0,nrow(b))
b$bin_2015 <-rep(0,nrow(b)) 

for(i in 1:nrow(b)){
  if(b$modalidad[i] == "Arma blanca"){
    b$bin_ab[i] <- 1
  }else if(b$modalidad[i] == "Arma de fuego"){
    b$bin_af[i] <- 1
  }else if(b$modalidad[i] == "Otros"){
    b$bin_o[i] <- 1
  }
}

for(i in 1:nrow(b)){
  if(b$anio[i]==2014){
    b$bin_2014[i] <- 1
  }else if(b$anio[i]==2015){
    b$bin_2015[i] <- 1
  }
}


tabla_2014_2015_mod <- left_join(b,var_eco, by=("CVEGEO"="CVEGEO")) #TABLA PARA MODELO DE INTERACCIONES
head(tabla_2014_2015_mod,20)




