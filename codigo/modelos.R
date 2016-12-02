#-Working directory-

wdir<-"/home/denny/itam/modelos_lgeneralizados/ago_2016/proyecto_final"
wdir<-"/home/stuka/RegresionAvanzada/ProyFin/Aquisetrabaja/regresion-avanzada/"
setwd(wdir)

library(R2OpenBUGS)
library(dplyr)

#--- Funciones utiles ---
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

tabla_4anios_ok <- read_csv("./datos/tabla_4anios_naive.csv")
#MODELO NAIVE
n <- nrow(tabla_4anios_ok)*1
hist(tabla_4anios_ok$homi_count,breaks = 30)
summary(tabla_4anios_ok$homi_count)
table(tabla_4anios_ok$homi_count)

mean(tabla_4anios_ok$homi_count)


plot(tabla_4anios_ok$homi_count,tabla_4anios_ok$INDICE_GLOBAL)
plot(tabla_4anios_ok$homi_count,tabla_4anios_ok$GRAPROES)
plot(tabla_4anios_ok$homi_count,tabla_4anios_ok$VPH_INTER)

#ANALISIS DE CORRELACION, ELIMINO LAS VARIABLES NSE
subdata <- tabla_4anios_ok[2:15]
cor(subdata)

#HAY UNA FUERTE CORRELACION ENTRE LAS VARIABLES, TENDRÉ QUE QUITAR VARIABLES
subdata1 <- subdata[,c(1,5,12)]
cor(subdata1)



# Y =numero de homicidios agrupado por ageb -> R+

#VARIABLES EXPLICATIVAS:
# POBTOT    Poblacion total en el AGEB en 2010
# P_18YMAS    Poblacion de mas de 18 años en el AGEB en 2010
# P18YM_PB    Poblacion de mas de 18 años en el AGEB con al menos nivel preparatoria
# GRAPROES    Grado promedio de escolaridad en el AGEB - en años (1-6 primaria, 7-9 secundaria, 9-12 preparatoria, +12 algun nivel de licenciatura)
# VIVPAR_HAB    Numero de Viviendas Particulares Habitadas en el AGEB
# VPH_PISODT    Numero de Viviendas Particulares Habitadas que reportaron tener piso otro que tierra
# PROM_OCUP    Promedio de Cuartos por Vivienda
# VPH_TV    Viviendas Particulares Habitadas con Television
# VPH_AUTOM    Viviendas Particulares Habitadas con Automóvil
# VPH_PC    Vivienda Particulares Habitadas con PC
# VPH_INTER    Viviendas Particulares con Internet

#Transformamos las variables explicativas en %

tabla_4anios_ok$POR_P_18YMAS <- tabla_4anios_ok$P_18YMAS/tabla_4anios_ok$POBTOT
tabla_4anios_ok$POR_P18YM_PB <- tabla_4anios_ok$P18YM_PB/tabla_4anios_ok$POBTOT
tabla_4anios_ok$POR_VPH_PISODT <- tabla_4anios_ok$VPH_PISODT/tabla_4anios_ok$VIVPAR_HAB
tabla_4anios_ok$POR_VPH_TV <- tabla_4anios_ok$VPH_TV/tabla_4anios_ok$VIVPAR_HAB
tabla_4anios_ok$POR_VPH_AUTOM <- tabla_4anios_ok$VPH_AUTOM/tabla_4anios_ok$VIVPAR_HAB
tabla_4anios_ok$POR_VPH_PC <- tabla_4anios_ok$VPH_PC/tabla_4anios_ok$VIVPAR_HAB
tabla_4anios_ok$POR_VPH_INTER <- tabla_4anios_ok$VPH_INTER/tabla_4anios_ok$VIVPAR_HAB
tabla_4anios_ok$GRAPROES_1 <- tabla_4anios_ok$GRAPROES

#################################### PRIMER MODELO homi ~ Po(POR_VPH_INTER,beta)
########################### Esta tecnica para seleccionar variables permite que el codigo despues se ppueda automatizar
tabla <- tabla_4anios_ok
variables <- c("homi_count","POBTOT","GRAPROES","POR_VPH_INTER")
datos <- tabla %>% select(one_of(variables))
#-Defining data-

#poisson y bin neg - exposure es el offset de POP_TOT
data<-list("n"=n,"y"=datos[[1]],"exposure"=datos[[2]],"x"=datos[[4]])

#binomial
data<-list("n"=n,"ne"=tabla_4anios_ok$POBTOT,"y"=tabla_4anios_ok$prom_homi,"x"=tabla_4anios_ok$INDICE_GLOBAL)


#-Defining inits-
inits<-function(){list(beta=rep(0,2),ypred=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("beta","ypred")

#-Running code-
#OpenBUGS
m1_poisson_log.sim<-bugs(data,inits,parameters,model.file="m1_poisson_log.txt",
               n.iter=20000,n.chains=1,n.burnin=2000)

m1_poisson_lin.sim<-bugs(data,inits,parameters,model.file="m1_poisson_lineal.txt",
                         n.iter=20000,n.chains=1,n.burnin=2000)


#RESPUESTAS MODELO 1 POISSON LIGA LOG
#OpenBUGS
out_m1_poisson_log.sim<-m1_poisson_log.sim$sims.list

#Resumen (estimadores)
#OpenBUGS
out.sum_m1_poisson_log.sim<-m1_poisson_log.sim$summary
print(out.sum_m1_poisson_log.sim)
head(out.sum_m1_poisson_log.sim)

#DIC
m1_poisson_log.dic<-m1_poisson_log.sim$DIC
print(m1_poisson_log.dic) #8295



#RESPUESTAS MODELO 1 POISSON LIGA LINEAL
#OpenBUGS
out_m1_poisson_lin.sim<-m1_poisson_lin.sim$sims.list

#Resumen (estimadores)
#OpenBUGS
out.sum_m1_poisson_lin.sim<-m1_poisson_lin.sim$summary
print(out.sum_m1_poisson_lin.sim)
head(out.sum_m1_poisson_lin.sim)

#DIC
m1_poisson_lin.dic<-m1_poisson_lin.sim$DIC
print(m1_poisson_lin.dic) #8268




################### MODELO 2, SEPARA POR MODALIDAD ##############
n <- nrow(tabla_4anios_mod)

#CORRELACION
#ANALISIS DE CORRELACION, ELIMINO LAS VARIABLES NSE
subdata <- tabla_4anios_mod[3:19]
cor(subdata)

plot(tabla_4anios_mod$homi_count,tabla_4anios_mod$INDICE_GLOBAL)
summary(tabla_4anios_mod$INDICE_GLOBAL)
max(tabla_4anios_mod$INDICE_GLOBAL)
min(tabla_4anios_mod$INDICE_GLOBAL)

#-Defining data-

#poisson
data<-list("n"=n,"y"=tabla_4anios_ok$homi_count,"x1"=tabla_4anios_ok$INDICE_GLOBAL, "x2"=tabla_4anios_mod$bin_ab, "x3"=tabla_4anios_mod$bin_af)

#-Defining inits-
inits<-function(){list(beta=rep(1,4),yf1=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("beta","yf1")

#-Running code-
#OpenBUGS
m2_poisson_log.sim<-bugs(data,inits,parameters,model.file="m2_poisson_log.txt",
                         n.iter=20000,n.chains=1,n.burnin=2000)








