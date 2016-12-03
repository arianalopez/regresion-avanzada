#-Working directory-

wdir<-""
wdir<-"/home/stuka/RegresionAvanzada/ProyFin/Aquisetrabaja/regresion-avanzada/codigo/"
setwd(wdir)

library(R2OpenBUGS)
library(dplyr)
library(readr)
library(R2jags)
#--- Funciones utiles ---
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

tabla_4anios_ok <- read_csv("../datos/tabla_4anios_naive.csv")
#MODELO NAIVE

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
########### El ifelse checa si el denominador es 0 y en ese caso pone 0 a la proporción
tabla_4anios_ok$POR_P_18YMAS <- ifelse(tabla_4anios_ok$POBTOT==0,0,tabla_4anios_ok$P_18YMAS/tabla_4anios_ok$POBTOT)
tabla_4anios_ok$POR_P18YM_PB <- ifelse(tabla_4anios_ok$POBTOT==0,0,tabla_4anios_ok$P18YM_PB/tabla_4anios_ok$POBTOT)
tabla_4anios_ok$POR_VPH_PISODT <- ifelse(tabla_4anios_ok$VIVPAR_HAB==0,0,tabla_4anios_ok$VPH_PISODT/tabla_4anios_ok$VIVPAR_HAB)
tabla_4anios_ok$POR_VPH_TV <- ifelse(tabla_4anios_ok$VIVPAR_HAB==0,0,tabla_4anios_ok$VPH_TV/tabla_4anios_ok$VIVPAR_HAB)
tabla_4anios_ok$POR_VPH_AUTOM <- ifelse(tabla_4anios_ok$VIVPAR_HAB==0,0,tabla_4anios_ok$VPH_AUTOM/tabla_4anios_ok$VIVPAR_HAB)
tabla_4anios_ok$POR_VPH_PC <- ifelse(tabla_4anios_ok$VIVPAR_HAB==0,0,tabla_4anios_ok$VPH_PC/tabla_4anios_ok$VIVPAR_HAB)
tabla_4anios_ok$POR_VPH_INTER <- ifelse(tabla_4anios_ok$VIVPAR_HAB==0,0,tabla_4anios_ok$VPH_INTER/tabla_4anios_ok$VIVPAR_HAB)
tabla_4anios_ok$GRAPROES_1 <- tabla_4anios_ok$GRAPROES

par(mfrow=c(1,3))
plot(tabla_4anios_ok$homi_count,tabla_4anios_ok$POR_VPH_INTER,pch=19)
plot(tabla_4anios_ok$homi_count,tabla_4anios_ok$GRAPROES_1,pch=19)
plot(tabla_4anios_ok$homi_count,tabla_4anios_ok$PROM_OCUP,pch=19)

write_csv(as_data_frame(cor(tabla_4anios_ok[,c(2,10,18:25)])),"../datos/corrs.csv")
cor(tabla_4anios_ok[,c("POR_VPH_INTER","GRAPROES_1","PROM_OCUP")])
#################################### PRIMER MODELO homi ~ Po(POR_VPH_INTER,beta)
########################### Esta tecnica para seleccionar variables permite que el codigo despues se ppueda automatizar
tabla <- tabla_4anios_ok
variables <- c("homi_count","POBTOT","PROM_OCUP","POR_VPH_PISODT","POR_VPH_AUTOM")
datos <- tabla %>% select(one_of(variables))
######################################Se vale escalar las variables - dejo aqui el escalador por si queremos probar que pasa
scale2 <- function(x) {
  sdx <- sqrt(var(x))
  meanx <- mean(x)
  return((x - meanx)/sdx)
}
#Se puede escalar
#datos$POR_VPH_INTER <- scale2(datos$POR_VPH_INTER)

# Tiro las agebs con POBTOT = 0
#datos <- datos[which(datos$POBTOT!=0),]
############################################### GLM frecuentista
coefini=coef(glm(homi_count ~ POR_VPH_INTER,  family = "poisson",data=tabla_4anios_ok))
#Hacemos POBTOT+1 para no tener log(0)
m.glm <- glm(homi_count ~ POR_VPH_INTER, offset=log(POBTOT+1), family = "poisson",data=datos)
summary(m.glm)

# Call:
#   glm(formula = homi_count ~ POR_VPH_INTER, family = "poisson", 
#       data = datos, offset = log(POBTOT + 1))
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -3.3444  -1.1322  -0.4349   0.5132  19.3544  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -7.36907    0.03934 -187.30   <2e-16 ***
#   POR_VPH_INTER -1.40469    0.10555  -13.31   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 4797.4  on 2410  degrees of freedom
# Residual deviance: 4605.8  on 2409  degrees of freedom
# AIC: 8370.3

####################################Ahora si el GIBBS en OpenBUGS
#-Defining data-
n <- nrow(datos)*1
#poisson - exposure es el offset de POP_TOT
data<-list("n"=n,"y"=datos$homi_count,"exposure"=datos$POBTOT,"x1"=datos$POR_VPH_AUTOM,"x2"=datos$PROM_OCUP)

#-Defining inits-
inits<-function(){list(beta=rep(0,3),ypred=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("beta","ypred","eta","mu")

#-Running code-
#OpenBUGS
m1_poisson_log.sim<-bugs(data,inits,parameters,model.file="m1_poisson_log_2covars.txt",
               n.iter=20000,n.chains=1,n.burnin=2000)

m1_poisson_lin.sim<-bugs(data,inits,parameters,model.file="m1_poisson_lineal.txt",
                         n.iter=20000,n.chains=1,n.burnin=2000)


#RESPUESTAS MODELO 1 POISSON LIGA LOG
#OpenBUGS
out_m1_poisson_log.sim<-m1_poisson_log.sim$sims.list

#############################################################################################
#Inicio del analisis Bayesiano

# Con 5000 iteraciones las cadenas no se mezclan bien :(
traceplot(m1_poisson_log.sim)
plot(m1_poisson_log.sim)


#Resumen (estimadores)
#OpenBUGS
out.sum_m1_poisson_log.sim<-m1_poisson_log.sim$summary
print(out.sum_m1_poisson_log.sim)
################## Claramente los coeficientes Betas son significativos! Y el ser negativos habla de que hay menos homicidios en zonas donde hay mas proporción de viviendas con internet 
head(out.sum_m1_poisson_log.sim)
# mean         sd    2.5%     25%     50%       75%   97.5%
# beta[1]  -5.9425547 0.14531434 -6.2080 -6.0610 -5.9260 -5.836000 -5.6720
# beta[2]  -2.2160482 0.13339709 -2.4620 -2.3170 -2.2330 -2.106000 -1.9760
# beta[3]  -0.2696281 0.03194769 -0.3286 -0.2912 -0.2703 -0.246175 -0.2062
# ypred[1]  0.8475000 0.92259413  0.0000  0.0000  1.0000  1.000000  3.0000
# ypred[2]  2.6732222 1.64496874  0.0000  1.0000  2.0000  4.000000  6.0000
# ypred[3]  1.7693889 1.33814806  0.0000  1.0000  2.0000  3.000000  5.0000
####### Un incremento de una unidad de proporción de internet se traduce en una reducción de 22% en el conteo de homicidios
exp(-1.2252) # = 0.2936 si beta es > 0 la tasa es de crecimiento si beta < 0 la tasa es de decrecimiento
exp(-0.2696)
#DIC
m1_poisson_log.dic<-m1_poisson_log.sim$DIC
print(m1_poisson_log.dic) #8.4e+11

######################## Analisis MCMC para beta1
beta_num <- 1
z<-out_m1_poisson_log.sim$beta[,beta_num]
par(mfrow=c(2,2))
plot(z,type="l",main="Mezcla de la cadena")
plot(cumsum(z)/(1:length(z)),type="l",main="Gráfica del promedio ergódico")
hist(z,freq=FALSE,main=paste0("Distribución final de beta ",beta_num))
acf(z,main="Gráfica de autocorrelación")

############## p-value
prob(z)

######################## Analisis MCMC para beta2
beta_num <- 2
z<-out_m1_poisson_log.sim$beta[,beta_num]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)
############### p-value
prob(z)

######################## Analisis MCMC para beta2
beta_num <- 3
z<-out_m1_poisson_log.sim$beta[,beta_num]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)
############### p-value
prob(z)


########################################## Predictions
#Predictions
out.ypred<-out.sum_m1_poisson_log.sim[grep("ypred",rownames(out.sum_m1_poisson_log.sim)),]
or<-order(datos$POR_VPH_AUTOM)
ymin<-min(datos$homi_count,out.ypred[,c(1,3,7)])
ymax<-max(datos$homi_count,out.ypred[,c(1,3,7)])
par(mfrow=c(1,1))
plot(datos$POR_VPH_AUTOM,datos$homi_count,ylim=c(ymin,ymax))
lines(datos$POR_VPH_AUTOM[or],out.ypred[or,1],lwd=2,col=2)
lines(datos$POR_VPH_AUTOM[or],out.ypred[or,3],lty=2,col=3)
lines(datos$POR_VPH_AUTOM[or],out.ypred[or,7],lty=2,col=3)


out.ypred<-out.sum_m1_poisson_log.sim[grep("ypred",rownames(out.sum_m1_poisson_log.sim)),]
or<-order(datos$PROM_OCUP)
ymin<-min(datos$homi_count,out.ypred[,c(1,3,7)])
ymax<-max(datos$homi_count,out.ypred[,c(1,3,7)])
par(mfrow=c(1,1))
plot(datos$PROM_OCUP,datos$homi_count,ylim=c(ymin,ymax))
lines(datos$PROM_OCUP[or],out.ypred[or,1],lwd=2,col=2)
lines(datos$PROM_OCUP[or],out.ypred[or,3],lty=2,col=3)
lines(datos$PROM_OCUP[or],out.ypred[or,7],lty=2,col=3)

plot(datos$homi_count,out.ypred[,1])
##########################################################################################
# Hasta aqui mis avances - falta la pseudo-R
cor(datos$homi_count,out.ypred[,1])

############################################################################################
# 3 covars
#-Defining data-
n <- nrow(datos)*1
#poisson - exposure es el offset de POP_TOT
data<-list("n"=n,"y"=datos$homi_count,"exposure"=datos$POBTOT,"x1"=datos$POR_VPH_AUTOM,"x2"=datos$PROM_OCUP,"x3"=datos$POR_VPH_PISODT)

#-Defining inits-
inits<-function(){list(beta=rep(0,4),ypred=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("beta","ypred","eta","mu")

#-Running code-
#OpenBUGS
m1_poisson_log.sim<-bugs(data,inits,parameters,model.file="m1_poisson_log_3covars.txt",
                         n.iter=20000,n.chains=1,n.burnin=5000)

#RESPUESTAS MODELO 1 POISSON LIGA LOG
#OpenBUGS
out_m1_poisson_log.sim<-m1_poisson_log.sim$sims.list

#############################################################################################
#Inicio del analisis Bayesiano

# Con 5000 iteraciones las cadenas no se mezclan bien :(
traceplot(m1_poisson_log.sim)
plot(m1_poisson_log.sim)


#Resumen (estimadores)
#OpenBUGS
out.sum_m1_poisson_log.sim<-m1_poisson_log.sim$summary
print(out.sum_m1_poisson_log.sim)
################## Claramente los coeficientes Betas son significativos! Y el ser negativos habla de que hay menos homicidios en zonas donde hay mas proporción de viviendas con internet 
head(out.sum_m1_poisson_log.sim)
# mean         sd    2.5%     25%     50%       75%   97.5%
# beta[1]  -5.9425547 0.14531434 -6.2080 -6.0610 -5.9260 -5.836000 -5.6720
# beta[2]  -2.2160482 0.13339709 -2.4620 -2.3170 -2.2330 -2.106000 -1.9760
# beta[3]  -0.2696281 0.03194769 -0.3286 -0.2912 -0.2703 -0.246175 -0.2062
# ypred[1]  0.8475000 0.92259413  0.0000  0.0000  1.0000  1.000000  3.0000
# ypred[2]  2.6732222 1.64496874  0.0000  1.0000  2.0000  4.000000  6.0000
# ypred[3]  1.7693889 1.33814806  0.0000  1.0000  2.0000  3.000000  5.0000
####### Un incremento de una unidad de proporción de internet se traduce en una reducción de 22% en el conteo de homicidios
exp(-1.2252) # = 0.2936 si beta es > 0 la tasa es de crecimiento si beta < 0 la tasa es de decrecimiento
exp(-0.2696)
#DIC
m1_poisson_log.dic<-m1_poisson_log.sim$DIC
print(m1_poisson_log.dic) #8.4e+11

######################## Analisis MCMC para beta1
beta_num <- 1
z<-out_m1_poisson_log.sim$beta[,beta_num]
par(mfrow=c(2,2))
plot(z,type="l",main="Mezcla de la cadena")
plot(cumsum(z)/(1:length(z)),type="l",main="Gráfica del promedio ergódico")
hist(z,freq=FALSE,main=paste0("Distribución final de beta ",beta_num))
acf(z,main="Gráfica de autocorrelación")

############## p-value
prob(z)

######################## Analisis MCMC para beta2
beta_num <- 2
z<-out_m1_poisson_log.sim$beta[,beta_num]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)
############### p-value
prob(z)

######################## Analisis MCMC para beta2
beta_num <- 3
z<-out_m1_poisson_log.sim$beta[,beta_num]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)
############### p-value
prob(z)


########################################## Predictions
#Predictions
out.ypred<-out.sum_m1_poisson_log.sim[grep("ypred",rownames(out.sum_m1_poisson_log.sim)),]
or<-order(datos$POR_VPH_AUTOM)
ymin<-min(datos$homi_count,out.ypred[,c(1,3,7)])
ymax<-max(datos$homi_count,out.ypred[,c(1,3,7)])
par(mfrow=c(1,1))
plot(datos$POR_VPH_AUTOM,datos$homi_count,ylim=c(ymin,ymax))
lines(datos$POR_VPH_AUTOM[or],out.ypred[or,1],lwd=2,col=2)
lines(datos$POR_VPH_AUTOM[or],out.ypred[or,3],lty=2,col=3)
lines(datos$POR_VPH_AUTOM[or],out.ypred[or,7],lty=2,col=3)


out.ypred<-out.sum_m1_poisson_log.sim[grep("ypred",rownames(out.sum_m1_poisson_log.sim)),]
or<-order(datos$PROM_OCUP)
ymin<-min(datos$homi_count,out.ypred[,c(1,3,7)])
ymax<-max(datos$homi_count,out.ypred[,c(1,3,7)])
par(mfrow=c(1,1))
plot(datos$PROM_OCUP,datos$homi_count,ylim=c(ymin,ymax))
lines(datos$PROM_OCUP[or],out.ypred[or,1],lwd=2,col=2)
lines(datos$PROM_OCUP[or],out.ypred[or,3],lty=2,col=3)
lines(datos$PROM_OCUP[or],out.ypred[or,7],lty=2,col=3)

plot(datos$homi_count,out.ypred[,1])
##########################################################################################
# Hasta aqui mis avances - falta la pseudo-R
cor(datos$homi_count,out.ypred[,1])






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

########################################################################################################
####################  Modelos Zero-Inflated Poisson
#-Defining data-
n <- nrow(datos)*1
z <- rep(1,n)
#poisson y bin neg - exposure es el offset de POP_TOT
data<-list("n"=n,"y"=datos$homi_count,"exposure"=datos$POBTOT,"x"=datos$POR_VPH_AUTOM,"z"=z)

#-Defining inits-
inits<-function(){list(beta=rep(0,2),gamma=rep(0,1),ypred=rep(1,n))}

#-Selecting parameters to monitor-
parameters<-c("beta","gamma","p","ypred")

#-Running code-
#OpenBUGS
m1_poisson_log.sim<-bugs(data,inits,parameters,model.file="m1_poisson_log_2covars.txt",
                         n.iter=20000,n.chains=1,n.burnin=2000)




