##########################################################################
## 09-11-2020 Durbin-Watson test                                        ##
## esempio tratto da: A SECOND COURSE IN STATISTIC: REGRESSION ANALYSIS ##
##                    Mendenhall W., Sincich T., capitolo 8             ##
##########################################################################

## dataset
setwd('C:/Users/marti/OneDrive/Desktop/modelli_statistici/LEZIONE7-residui')
data=read.csv2('201109 SalesData.csv',header = F,col.names=c('time','sales'))
attach(data)
time
sales=as.numeric(sales)
plot(density(sales))

par(mfrow=c(1,2))
plot(time,sales)
lines(time,sales)
scatter.smooth(time,sales, pch=16,col='red')

## modello lineare
LM1=lm(sales~time,data = data)
LM1
summary(LM1)
# cosa possiamo dire sull'intercetta?

## analisi dei residui
par(mfrow=c(3,1))

plot(time,LM1$residuals)
abline(h=0,col='red')

plot(LM1$fitted.values,LM1$residuals)
abline(h=0,col='red')

plot(sales,LM1$residuals)
abline(h=0,col='red')

# cosa notiamo nei residui?

## calcoliamo 'a mano' il valore della statistica
n=length(sales)
residui=LM1$residuals[2:n]
residui2=LM1$residuals[1:(n-1)]
db_statistic=(sum((residui-residui2)^2))/(sum(LM1$residuals^2))
db_statistic

## durbin-Watson test
library(car)
?durbinWatsonTest
durbinWatsonTest(LM1,alternative = 'two.sided')

