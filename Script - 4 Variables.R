# Añadiendo Annual Earnings a fertilidad (Análisis variable) ####

Ingresos_anuales <- ts(Tablitas$`All Women`, start=1960, end=2017, frequency=1)
Ingresos_anuales75 <- window(Ingresos_anuales, start=1975, end=2017)
Ingresos_an <- autoplot(Ingresos_anuales, main = "Ingresos Medios Anuales de las mujeres", xlab="Año", ylab="Ingresos medios ($)")

ingresos72_17 <- window(Ingresos_anuales, start=1972, end=2017)

autoplot(ingresos72_17)
cuatrovariables <- grid.arrange(cuatr,  nrow=2, ncol=2)

adf.test(ingresos72_17) #No recahzamos H0, no es estacionaria
ingresos72 <- diff(ingresos72_17)
ingresos72_log <- log(ingresos72_17)
autoplot(ingresos72)
pp.test(diff(ingresos72_17))


adf.test(Ingresos_anuales) #Sí unit root
pp.test(Ingresos_anuales) #Sí unit root
kpss.test(Ingresos_anuales) #RH0 de level stationary, por lo tanto es Trend stationary = No stationary

adf.test(ingresos72) #RH0 (0.0346)= Stationary = I(1) = no unit root
pp.test(ingresos, lshort = TRUE) #RH0 (<0.01) = No unit root
kpss.test(ingresos) #No RH0 = level stationary

# Structural break (nada) ####
bp.ingresos <- breakpoints(diff(ingresos72_17)~1) ## Bai and Perron Test
summary(bp.ingresos)
plot(bp.ingresos)
plot(diff(ingresos72_17))
lines(bp.ingresos)
ci.ingresos <- confint(bp.ingresos)
ci.ingresos
lines(ci.ingresos)
title(main="Cambio estructural en la serie temporal de los Ingresos", cex=0.1)

par(mfrow=c(1,1))



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
plot(ingres_de)
cycle_ingres <- autoplot(ingres_de$cycle, main= "Ciclo de los Ingresos medios de las mujeres", xlab="Año", ylab="")

CYCLES <- grid.arrange(cycle_pib, cycle_house, cycle_rate, cycle_ingres, nrow=4, ncol=1)

#Aquí podemos ver que pueden tener una relación, y que si se incluyese 
# la producción doméstica, puede ser que se absorba estos ciclos del pib. 
# Además vemos que siguen un mismo ciclo. Esperamos que el PIB afecte en la producción
# doméstica futura, ya que si el PIB baja estamos en recesión, por lo q menos gente 
# trabajará, y más gente hará tareas domésticas.

# Analizamos el cross-correlogram ####
par(mfrow=c(1,1))

pib_domest <- autoplot(ccf(pib_de$cycle,house_de$cycle), main="PIB y Producción Doméstica") #Procíclico
pib_fert <- autoplot(ccf(pib_de$cycle, fert_de$cycle), main="PIB y Tasa de Fertilidad") #Sí, PIB lagea la Fertilidad. Procíclico
pib_ingr <- autoplot(ccf(pib_de$cycle, ingres_de$cycle), main="PIB e Ingresos") #Nada



ccf(house_de$cycle, pib_de$cycle) #Procíclico
domest_fert <- autoplot(ccf(house_de$cycle, fert_de$cycle), main="Producción Doméstica y Tasa de Fertilidad") #Procíclico

ccf(fert_de$cycle, pib_de$cycle) #Fertilidad guía al pib
ccf(fert_de$cycle, house_de$cycle) #Procíclico

ccf(ingres_de$cycle,pib_de$cycle)
ingres_domest <- autoplot(ccf(ingres_de$cycle, house_de$cycle), main="Ingresos y Producción Doméstica")
ingres_fert <- autoplot(ccf(ingres_de$cycle, fert_de$cycle), main= "Ingresos y Tasa de Fertilidad")

grid.arrange(pib_domest, pib_fert, pib_ingr, domest_fert, ingres_domest, ingres_fert,
             nrow=3, ncol=2)


print(ccf(pib_de$cycle,fert_de$cycle, ylab = ""), title="Correlograma cruzado del PIB y Tasa de Fertilidad")
print(ccf( pib_de$cycle, house_de$cycle, ylab = ""), title="Correlograma cruzado del PIB y Producción Doméstica")


ciclos_fert_ingr <- cbind(pib_de$cycle,house_de$cycle, fert_de$cycle, ingres_de$cycle)
grid.arrange(autoplot(cbind(pib_de$cycle, house_de$cycle), ylab="Ciclos", xlab = "Año",
                      main="Ciclos descompuestos del PIB y la Producción Doméstica"), 
             autoplot(cbind(pib_de$cycle, fert_de$cycle), ylab="Ciclos", xlab = "Año", 
                      main="Ciclos descompuestos del PIB y la Tasa de Fertilidad"),
             autoplot(cbind(pib_de$cycle, ingres_de$cycle), ylab="Ciclos", xlab = "Año",
                      main="Ciclos descompuestos del PIB y los Ingresos medios de las mujeres"),
             nrow=3, ncol=1)

matplot(cbind(pib_de$cycle,house_de$cycle, fert_de$cycle),type="l",col=c("red","green", "blue"),lty=c(1,1), ylab = "ciclos")
# Vemos que el PIB laggea la producción doméstica, con la fertilidad no vemos relación clara.

# Causalidad de Granger test ####
grangertest(pib_de$cycle, house_de$cycle) #SI
grangertest(pib_de$cycle, fert_de$cycle) #SI
grangertest(pib_de$cycle, ingresos_de$cycle) #No


grangertest(house_de$cycle, pib_de$cycle) #SI
grangertest(house_de$cycle, fert_de$cycle) #NO
grangertest(house_de$cycle, ingresos_de$cycle) #No

grangertest(fert_de$cycle, pib_de$cycle) #NO
grangertest(fert_de$cycle, house_de$cycle) #NO
grangertest(fert_de$cycle, ingresos_de$cycle) #NO

grangertest(ingresos_de$cycle, pib_de$cycle) #No
grangertest(ingresos_de$cycle, house_de$cycle) #No
grangertest(ingresos_de$cycle, fert_de$cycle) #No

# Cointegración ####
levels_4data72_17 <- data.frame(fert72_17_log, ingresos72_log, pib72_17_log, domest72_17_log)
levels_data75_4 <- data.frame(fert75, pib75_log, house75, Ingresos_anuales75)

Joh6_trace <- ca.jo(levels_4data72_17, type="trace") #Hay relación de cointegración
summary(Joh6_trace) 

Joh6 <- ca.jo(levels_4data72_17, type="eigen") #There are 2 cointegrating equations
summary(Joh6) #Hay 1 relación de cointegración al 99%, 2 al 90%.


# VAR ####
selectionvar472_17 <- VARselect(levels_4data72_17, lag.max = 7, type = "none", season = NULL, exogen = NULL)
selectionvar472_17$selection # VAR(2) 2 lags 

var_model5 <- VAR(levels_4data72_17, p=1, lag=2, type="none", season=NULL)
summary(var_model5)
serial.test(var_model5, lags.pt=10, type="PT.asymptotic")
# There is no serial correlation as pvalue is 0.8058

arch.test(var_model5, lags.single = 16, lags.multi = 5, multivariate.only = TRUE)
#No arch efects as pvalue is 0.9999

#Normality of residuals
normality.test(var_model5) #H0 de JB es que son normales, 
#no rechazamos por lo q son normales. (0.4389)

plot(stability(var_model5, type = "OLS-CUSUM")) #El modelo es estable
par(mar=c(3,2,1.5,1))



causality(var_model5, cause = "pib72_17_log") #SI Granger Causality long term, RH0, tb short term al 90%
causality(var_model5, cause = "domest72_17_log") #Si Granger Causality long term RH0
causality(var_model5, cause = "fert72_17_log") #No Granger Causality, sí al 90%
causality(var_model5, cause = "ingresos72_log") #Si granger causality, RH0

cov(var_model5$y) #TRIANGULAR - DIAGONAL
toLatex(cor(var_model5$y)) #Alta correlación

#Descomposición de la varianza
vardecomp5 <- fevd(var_model5)
plot(vardecomp5, title="Descomposición de la varianza del VAR(1) con variables cointegradas")

# IMPULSE RESPONSE GRAPH  ####


IR_5           = irf(var_model5, n.ahead=20,ci=0.95, cumulative =FALSE)
IR_5fert              = IR_5$irf$fert72_17_log
IR_5fert_Low          = IR_5$Lower$fert72_17_log
IR_5fert_sup          = IR_5$Upper$fert72_17_log
IR_5ingresos = IR_5$irf$ingresos72_log
IR_5ingresos_Low          = IR_5$Lower$ingresos72_log
IR_5ingresos_sup          = IR_5$Upper$ingresos72_log
IR_5pib              = IR_5$irf$pib72_17_log
IR_5pib_Low          = IR_5$Lower$pib72_17_log
IR_5pib_sup          = IR_5$Upper$pib72_17_log
IR_5domest              = IR_5$irf$domest72_17_log
IR_5domest_Low          = IR_5$Lower$domest72_17_log
IR_5domest_sup          = IR_5$Upper$domest72_17_log




#par(mfrow=c(Model$K/2,Model$K/2))
par(mfrow=c(4,4))
for(i in 1:var_model5$K){
  plot.ts(IR_5fert[,i],ylim=c(min(IR_5fert_Low[,i]),max(IR_5fert_sup[,i])),
          main=paste(colnames(var_model5$y)[i], "- Choque en Tasa de Fertilidad"))
  abline(h=0, col=1)
  lines(IR_5fert_Low[,i],col=2,lty=2)
  lines(IR_5fert_sup[,i],col=2,lty=2)
  plot.ts(IR_5ingresos[,i],ylim=c(min(IR_5ingresos_Low[,i]),max(IR_5ingresos_sup[,i])),
          main=paste(colnames(var_model5$y)[i], "- Choque en Ingresos"))
  abline(h=0, col=1)
  lines(IR_5ingresos_Low[,i],col=2,lty=2)
  lines(IR_5ingresos_sup[,i],col=2,lty=2)
  plot.ts(IR_5pib[,i],ylim=c(min(IR_5pib_Low[,i]),max(IR_5pib_sup[,i])),
          main=paste(colnames(var_model5$y)[i], "- Choque en PIB"))
  abline(h=0, col=1)
  lines(IR_5pib_Low[,i],col=2,lty=2)
  lines(IR_5pib_sup[,i],col=2,lty=2)
  plot.ts(IR_5domest[,i],ylim=c(min(IR_5domest_Low[,i]),max(IR_5domest_sup[,i])),
          main=paste(colnames(var_model5$y)[i], "- Choque en Producción Doméstica"))
  abline(h=0, col=1)
  lines(IR_5domest_Low[,i],col=2,lty=2)
  lines(IR_5domest_sup[,i],col=2,lty=2)
}
mtext("Funciones de Impulso-Respuesta de Choleski", side = 3, line = 0, outer = TRUE)

par("mar")
par(mar=c(3,2,1.5,1))

# VECM ####

VECM5 <- VECM(levels_4data72_17,lag=2, r=1, estim="ML")
VECM5_summary <-summary(VECM5) # se puede sacar para latex
print(VECM5)
toLatex(VECM4_summary)
vecm5_coint <- ca.jo(levels_4data72_17, type="trace", K=2)
summary(vecm5_coint)
toLatex(VECM5_summary)
VARrep(VECM5)

vectovar5 <- vec2var(vecm5_coint, r=1)
serial.test(vectovar5) # WN
arch.test(vectovar5, lags.multi = 15, multivariate.only = TRUE) # No ARCH efects
normality.test(vectovar5) #sí siguen normalidad de errores

print(xtable(VECM5_summary, type = "latex"), file = "filename2.tex")


toLatex(VECM5)
options("show.signif.stars"=TRUE)


vecdecomp5 <- fevd(vectovar5)
plot(vecdecomp5, title="Descomposición de la varianza del VECM(2) con variables cointegradas")
print(vecdecomp5)

IR_5$irf$domest72_17_log

# IRF VECM ####

IR_5           = irf(vectovar5, n.ahead=20,ci=0.95, cumulative =FALSE)
IR_5fert              = IR_5$irf$fert72_17_log
IR_5fert_Low          = IR_5$Lower$fert72_17_log
IR_5fert_sup          = IR_5$Upper$fert72_17_log
IR_5ingresos = IR_5$irf$ingresos72_log
IR_5ingresos_Low          = IR_5$Lower$ingresos72_log
IR_5ingresos_sup          = IR_5$Upper$ingresos72_log
IR_5pib              = IR_5$irf$pib72_17_log
IR_5pib_Low          = IR_5$Lower$pib72_17_log
IR_5pib_sup          = IR_5$Upper$pib72_17_log
IR_5domest              = IR_5$irf$domest72_17_log
IR_5domest_Low          = IR_5$Lower$domest72_17_log
IR_5domest_sup          = IR_5$Upper$domest72_17_log

#par(mfrow=c(Model$K/2,Model$K/2))
par(mfrow=c(4,4))
for(i in 1:vectovar5$K){
  plot.ts(IR_5fert[,i],ylim=c(min(IR_5fert_Low[,i]),max(IR_5fert_sup[,i])),
          main=paste(colnames(vectovar5$y)[i], "- Choque en Tasa de Fertilidad"))
  abline(h=0, col=1)
  lines(IR_5fert_Low[,i],col=2,lty=2)
  lines(IR_5fert_sup[,i],col=2,lty=2)
  plot.ts(IR_5ingresos[,i],ylim=c(min(IR_5ingresos_Low[,i]),max(IR_5ingresos_sup[,i])),
          main=paste(colnames(vectovar5$y)[i], "- Choque en Ingresos"))
  abline(h=0, col=1)
  lines(IR_5ingresos_Low[,i],col=2,lty=2)
  lines(IR_5ingresos_sup[,i],col=2,lty=2)
  plot.ts(IR_5pib[,i],ylim=c(min(IR_5pib_Low[,i]),max(IR_5pib_sup[,i])),
          main=paste(colnames(vectovar5$y)[i], "- Choque en PIB"))
  abline(h=0, col=1)
  lines(IR_5pib_Low[,i],col=2,lty=2)
  lines(IR_5pib_sup[,i],col=2,lty=2)
  plot.ts(IR_5domest[,i],ylim=c(min(IR_5domest_Low[,i]),max(IR_5domest_sup[,i])),
          main=paste(colnames(vectovar5$y)[i], "- Choque en Producción Doméstica"))
  abline(h=0, col=1)
  lines(IR_5domest_Low[,i],col=2,lty=2)
  lines(IR_5domest_sup[,i],col=2,lty=2)
}
mtext("Funciones de Impulso-Respuesta de Choleski", side = 3, line = 0, outer = TRUE)

par("mar")
par(mar=c(3,2,1.5,1))











# prueba tablas#####
varsExtra::tabulize(var_model$varresult, format="latex")
result <- var_model$varresult

sum_model3 <- summary(var_model3, x=svarsum)
toLatex(sum_model3$covres)
