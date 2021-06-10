
# setwd("C:/Users/saraa/Desktop/TFG/R/TFG")
# install.packages("remotes")
# remotes::install_github("DavZim/varsExtra")
# Packages####
library(haven)
library(readxl)
library(zoo)
library(lmtest)
library(urca)
library(sandwich)
library(gridExtra)
library(ggplot2)
library(MASS)
library(strucchange)
library(vars)
library(forecast)
library(moments)
library(xts)
library(readr)
library(dynlm)
library(carData)
library(car)
library(survival)
library(AER)
library(statsr)
library(plyr)
library(mFilter)
library(tseries)
library(stats)
library(UnitCircle)
library(ARDL)
library(dLagM)
library(egcm)
library(tsDyn)
library(scatterplot3d)
library(strucchange)
library(rgl)
library(magick)
library(RColorBrewer)
library(varsExtra)
library(xtable)

### DATA ####


library(readxl)
GDP <- read_excel("Uni/TFG-LAPTOP-UA5JRB0S/data/1947-2017 annual/GDP.xls", + skip = 10)
View(GDP)

Household_Production <- read_excel("Uni/TFG-LAPTOP-UA5JRB0S/data/1947-2017 annual/Household Production Quantification (1947-2017).xlsx")
View(Household_Production)

Fertility_Rate <- read_excel("Uni/TFG-LAPTOP-UA5JRB0S/data/Fertility Rate US (1960-2019).xls", 
                             +     skip = 10)
View(Fertility_Rate)

#Household Production
house_ts <- ts(Household_Production$`Value of Household Production ($ billions)`, start = 1947, frequency = 1)
domestico <- autoplot(house_ts, main = "Household Production in USA")
domestico


#PIB
PIB_ts <- ts(GDP$GDPC1, start = 1947, frequency = 1)
PIB <- autoplot(PIB_ts, main = "GDP - USA")
PIB


#Fertility Rate
fertility_ts <- ts(Fertility_Rate$SPDYNTFRTINUSA, start=1960, end=2017, frequency=1)
fertility <- autoplot(fertility_ts, main="Fertility Rate - USA")
fertility


# SEPARANDO PERIODOS
pib60_72 <- window(PIB_ts, start=1960, end=1972)
pib72_17 <- window(PIB_ts, start=1972, end=2017)

domest60_72 <- window(house_ts, start=1960, end=1972)
domest72_17 <- window(house_ts, start=1972, end=2017)

fert60_72 <- window(fertility_ts, start=1960, end=1972)
fert72_17 <- window(fertility_ts, start=1972, end=2017)

ingresos72_17 <- window(Ingresos_anuales, start=1972, end=2017)
# Periodo 60-75 ####

autoplot(diff(log(pib60_72)))
acf(diff(log(pib60_72)))

autoplot(diff(diff(log(domest60_72))))
acf(diff(diff(log(domest60_72))))

autoplot(diff(fert60_72))
acf(diff(fert60_72))

# Periodo 75-2017 ####

pib72_17_log <- log(pib72_17)
autoplot(pib72_17_log)
autoplot(diff(log(pib72_17)))
acf_pib<-autoplot(acf(diff(log(pib72_17))))
pacf_pib<-autoplot(pacf(diff(log(pib72_17))))

pp.test(diff(log(pib72_17)))
kpss.test(diff(log(pib72_17)))


autoplot(domest72_17_log)
domest72_17_log <- log(domest72_17)
autoplot(diff(domest72_17))
acf_domest <- autoplot(acf(diff(domest72_17)))
pacf_domest <- autoplot(pacf(diff(domest72_17)))

pp.test(diff(domest72_17))
kpss.test(diff(domest72_17))


autoplot(fert72_17_log)
fert72_17_log <- log(fert72_17)
autoplot(diff(fert72_17))
acf_fert<- autoplot(acf(diff(fert72_17)))
pacf_fert<- autoplot(pacf(diff(fert72_17)))

pp.test(diff(fert72_17))
kpss.test(diff(fert72_17))


acf_ing <- autoplot(acf(diff(ingresos72_17)))
pacf_ing <- autoplot(pacf(diff(ingresos72_17)))


ACF_PACF <- grid.arrange(acf_pib, pacf_pib, acf_domest, pacf_domest, acf_fert, pacf_fert, acf_ing, pacf_ing, nrow=4, ncol=2)

# Hodrick-Prescott decomposition ####
pib_de <- hpfilter(log(pib72_17))
plot(pib_de)
cycle_pib <- autoplot(pib_de$cycle, xlab="Año", main="Ciclo del PIB en logs", ylab="")

house_de <- hpfilter(log(domest72_17))
plot(house_de)
cycle_house <- autoplot(house_de$cycle, main ="Ciclo de la Producción Doméstica en logs", xlab = "Año", ylab="")

fert_de <- hpfilter(fert72_17)
plot(fert_de)
cycle_rate <- autoplot(fert_de$cycle, main = "Ciclo de la Tasa de Fertilidad", xlab="Año", ylab="")

ingres_de <- hpfilter(log(ingresos72_17))

ciclos_fert_ingr <- cbind(pib_de$cycle,house_de$cycle, fert_de$cycle, ingres_de$cycle)
grid.arrange(autoplot(cbind(pib_de$cycle, house_de$cycle), ylab="Ciclos", xlab = "Año",
                      main="Ciclos descompuestos del PIB y la Producción Doméstica"), 
             autoplot(cbind(pib_de$cycle, fert_de$cycle), ylab="Ciclos", xlab = "Año", 
                      main="Ciclos descompuestos del PIB y la Tasa de Fertilidad"),
             autoplot(cbind(pib_de$cycle, ingres_de$cycle), ylab="Ciclos", xlab = "Año",
                      main="Ciclos descompuestos del PIB y los Ingresos medios de las mujeres"),
             nrow=3, ncol=1)
CYCLES <- grid.arrange(cycle_pib, cycle_house, cycle_rate, nrow=3, ncol=1)

#Aquí podemos ver que pueden tener una relación, y que si se incluyese 
# la producción doméstica, puede ser que se absorba estos ciclos del pib. 
# Además vemos que siguen un mismo ciclo. Esperamos que el PIB afecte en la producción
# doméstica futura, ya que si el PIB baja estamos en recesión, por lo q menos gente 
# trabajará, y más gente hará tareas domésticas.

# Analizamos el cross-correlogram ####
pib_house <- autoplot(ccf(pib_de$cycle,house_de$cycle))#Procíclico
pib_fert <- autoplot(ccf(pib_de$cycle, fert_de$cycle)) #Sí, PIB lagea la Fertilidad. Procíclico

ccf(house_de$cycle, pib_de$cycle) #Procíclico
domest_fert <- autoplot(ccf(house_de$cycle, fert_de$cycle)) #Procíclico

ccf(fert_de$cycle, pib_de$cycle) #Fertilidad guía al pib
ccf(fert_de$cycle, house_de$cycle) #Procíclico

par(mfrow=c(1,1))

print(ccf(pib_de$cycle,fert_de$cycle, ylab = "Cross-correlation"), title="Cross correlogram of cycles")
print(ccf(house_de$cycle, pib_de$cycle, ylab = "Cross-correlation"), title="Cross correlogram of cycles")
ciclos_fert <- cbind(pib_de$cycle,house_de$cycle, fert_de$cycle)
autoplot(ciclos_fert, ylab="Ciclos", xlab = "Año")

matplot(cbind(pib_de$cycle,house_de$cycle, fert_de$cycle),type="l",col=c("red","green", "blue"),lty=c(1,1), ylab = "ciclos")
# Vemos que el PIB laggea la producción doméstica, con la fertilidad no vemos relación clara.

# Causalidad de Granger test ####
grangertest(pib_de$cycle, house_de$cycle) #SI causalidad, RH0
grangertest(pib_de$cycle, fert_de$cycle) #SI
grangertest(pib_de$cycle, ingresos_de$cycle) #No


grangertest(house_de$cycle, pib_de$cycle) #No
grangertest(house_de$cycle, fert_de$cycle) #NO
grangertest(house_de$cycle, ingresos_de$cycle) #No

grangertest(fert_de$cycle, pib_de$cycle) #NO
grangertest(fert_de$cycle, house_de$cycle) #NO
grangertest(fert_de$cycle, ingresos_de$cycle) #NO

grangertest(ingresos_de$cycle, pib_de$cycle) #No
grangertest(ingresos_de$cycle, house_de$cycle) #No
grangertest(ingresos_de$cycle, fert_de$cycle) #No

# Cointegración ####
levels_data72_17 <- data.frame(fert72_17_log, pib72_17_log, domest72_17_log)
levels_data75_4 <- data.frame(fert75, pib75_log, house75, Ingresos_anuales75)

Joh5_trace <- ca.jo(levels_data72_17, type="trace") #Hay relación de cointegración
summary(Joh5_trace) 

Joh5 <- ca.jo(levels_data72_17, type="eigen") #There are 2 cointegrating equations
summary(Joh5) #Hay 1 relación de cointegración al 99%


# VAR ####
selectionvar72_17 <- VARselect(levels_data72_17, lag.max = 8, type = "none", season = NULL, exogen = NULL)
selectionvar72_17$selection # VAR(2) 2 lags 

var_model4 <- VAR(levels_data72_17, p=1, lag=2, type="none", season=NULL)
summary(var_model4)
serial.test(var_model4, lags.pt=10, type="PT.asymptotic")
# There is no serial correlation as pvalue is 0.8698

arch.test(var_model4, lags.single = 16, lags.multi = 5, multivariate.only = TRUE)
#No arch efects as pvalue is 0.4001

#Normality of residuals
normality.test(var_model4) #H0 de JB es que son normales, 
#no rechazamos por lo q son normales. (0.7312)

plot(stability(var_model4, type = "OLS-CUSUM")) #El modelo es estable
par(mar=c(3,2,1.5,1))

causality(var_model4, cause = "pib72_17_log") #SI Granger Causality long term, RH0
causality(var_model4, cause = "domest72_17_log") #No Granger Causality long term
causality(var_model4, cause = "fert72_17_log") #No Granger Causality

varest(levels_data4)
cov(var_model4$y) #TRIANGULAR - DIAGONAL
cor(var_model4$y) #Alta correlación
var(levels_data4)

#Descomposición de la varianza
vardecomp4 <- fevd(var_model4)
plot(vardecomp4, title="Descomposición de la varianza del VAR(1) con variables cointegradas")

# IMPULSE RESPONSE GRAPH  ####


IR_4           = irf(var_model4, n.ahead=20,ci=0.95, cumulative =FALSE)
IR_4fert = IR_4$irf$fert72_17_log
IR_4fert_Low          = IR_4$Lower$fert72_17_log
IR_4fert_sup          = IR_4$Upper$fert72_17_log
IR_4pib              = IR_4$irf$pib72_17_log
IR_4pib_Low          = IR_4$Lower$pib72_17_log
IR_4pib_sup          = IR_4$Upper$pib72_17_log
IR_4domest              = IR_4$irf$domest72_17_log
IR_4domest_Low          = IR_4$Lower$domest72_17_log
IR_4domest_sup          = IR_4$Upper$domest72_17_log



#par(mfrow=c(Model$K/2,Model$K/2))
par(mfrow=c(3,3))
for(i in 1:var_model4$K){
  plot.ts(IR_4fert[,i],ylim=c(min(IR_4fert_Low[,i]),max(IR_4fert_sup[,i])),
          main=paste(colnames(var_model4$y)[i], "- Choque en la Tasa de Fertilidad"))
  abline(h=0, col=1)
  lines(IR_4fert_Low[,i],col=2,lty=2)
  lines(IR_4fert_sup[,i],col=2,lty=2)
  plot.ts(IR_4pib[,i],ylim=c(min(IR_4pib_Low[,i]),max(IR_4pib_sup[,i])),
          main=paste(colnames(var_model4$y)[i], "- Choque en PIB"))
  abline(h=0, col=1)
  lines(IR_4pib_Low[,i],col=2,lty=2)
  lines(IR_4pib_sup[,i],col=2,lty=2)
  plot.ts(IR_4domest[,i],ylim=c(min(IR_4domest_Low[,i]),max(IR_4domest_sup[,i])),
          main=paste(colnames(var_model4$y)[i], "- Choque en Producción Doméstica"))
  abline(h=0, col=1)
  lines(IR_4domest_Low[,i],col=2,lty=2)
  lines(IR_4domest_sup[,i],col=2,lty=2)
 
  
}
mtext("Funciones de Impulso-Respuesta de Choleski", side = 3, line = 0, outer = TRUE)

par("mar")
par(mar=c(3,2,1.5,1))

# VECM ####

VECM4 <- VECM(levels_data72_17,lag=1, r=1, estim="ML")
VECM4_summary <-summary(VECM4) # se puede sacar para latex
print(VECM4)
toLatex(VECM4_summary)
vecm_coint <- ca.jo(levels_data72_17, type="trace", K=2)
summary(vecm_coint) # 1 relacion de cointegracion
toLatex(VECM4)
VARrep(VECM4)

vectovar4 <- vec2var(vecm_coint, r=1)
serial.test(vectovar4) # WN
arch.test(vectovar4, lags.multi = 15, multivariate.only = TRUE) # No ARCH efects
normality.test(vectovar4) #sí siguen normalidad de errores

vecdecomp4 <- fevd(vectovar4)
plot(vecdecomp4, title="Descomposición de la varianza del VAR(1) con variables cointegradas")
print(vecdecomp4)


# IRF VECM ####



IR_4           = irf(vectovar4, n.ahead=20,ci=0.95, cumulative =FALSE, )
IR_4fert    = IR_4$irf$fert72_17_log
IR_4fert_Low          = IR_4$Lower$fert72_17_log
IR_4fert_sup          = IR_4$Upper$fert72_17_log
IR_4pib              = IR_4$irf$pib72_17_log
IR_4pib_Low          = IR_4$Lower$pib72_17_log
IR_4pib_sup          = IR_4$Upper$pib72_17_log
IR_4domest              = IR_4$irf$domest72_17_log
IR_4domest_Low          = IR_4$Lower$domest72_17_log
IR_4domest_sup          = IR_4$Upper$domest72_17_log


#par(mfrow=c(Model$K/2,Model$K/2))
par(mfrow=c(3,3))
for(i in 1:vectovar4$K){
  plot.ts(IR_4fert[,i],ylim=c(min(IR_4fert_Low[,i]),max(IR_4fert_sup[,i])),
          main=paste(colnames(vectovar4$y)[i], "- Choque en la Tasa de Fertilidad"))
  abline(h=0, col=1)
  lines(IR_4fert_Low[,i],col=2,lty=2)
  lines(IR_4fert_sup[,i],col=2,lty=2)
  plot.ts(IR_4pib[,i],ylim=c(min(IR_4pib_Low[,i]),max(IR_4pib_sup[,i])),
          main=paste(colnames(vectovar4$y)[i], "- Choque en PIB"))
  abline(h=0, col=1)
  lines(IR_4pib_Low[,i],col=2,lty=2)
  lines(IR_4pib_sup[,i],col=2,lty=2)
  plot.ts(IR_4domest[,i],ylim=c(min(IR_4domest_Low[,i]),max(IR_4domest_sup[,i])),
          main=paste(colnames(vectovar4$y)[i], "- Choque en Producción Doméstica"))
  abline(h=0, col=1)
  lines(IR_4domest_Low[,i],col=2,lty=2)
  lines(IR_4domest_sup[,i],col=2,lty=2)
  
  
}
mtext("Funciones de Impulso-Respuesta de Choleski", side = 3, line = 0, outer = TRUE)

par("mar")
par(mar=c(3,2,1.5,1))




