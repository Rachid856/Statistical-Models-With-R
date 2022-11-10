###############################################################################
## 09-11-2020                                                                ##
## modello lineare con varianza non costante                                 ##
###############################################################################

rm(list=ls())

# dataset
library(readr)
setwd('C:/Users/marti/OneDrive/Desktop/modelli_statistici/LEZIONE7-residui')
advertising <- read_csv("C:/Users/marti/OneDrive/Desktop/modelli_statistici/LEZIONE7-residui/Advertising.csv")
advertising$TV2=advertising$TV^0.5

# leggiamo le prime righe del dataset
head(advertising)
attach(advertising)

# installo le librerie
library(lmvar)
library(MASS)

# analiziamo un po' di modelli semplici per capire cosa possiamo fare: c'è qualche dipendenza rilevante?

# modello lineare multiplo
model_all=lm(Sales ~ TV+Radio+Newspaper,data=advertising)
par(mfrow=c(3,1))
plot(TV,model_all$residuals)
abline(h=0,col='red')
plot(Radio,model_all$residuals)
abline(h=0,col='red')
plot(Newspaper,model_all$residuals)
abline(h=0,col='red')

# modello lineare semplice
# plot
plot(TV2,Sales)
scatter.smooth(TV2,Sales)
model1=lm(Sales ~ TV,data=advertising)
par(mfrow=c(2,1))
plot(fitted(model1),model1$residuals)
plot(TV,model1$residuals)
shapiro.test(model1$residuals)

model2=lm(Sales ~ TV2,data=advertising)
par(mfrow=c(2,1))
plot(fitted(model2),model2$residuals)
plot(TV2,model2$residuals)
shapiro.test(model2$residuals)


# grafici a confronto
par(mfrow=c(2,2))
plot(fitted(model1),model1$residuals)
plot(TV,model1$residuals)
plot(fitted(model2),model2$residuals)
plot(TV2,model2$residuals)



################################################################################
##### parte I: modello lineare semplice                                        #
################################################################################

# creiamo le matrici del modello ETEROSCHEDASTICO
X_media=model.matrix(~-1+TV2,advertising)
X_var=model.matrix(~-1+TV2,advertising)

model_with_intercept=lmvar(Sales,X_mu=X_media,X_sigma=X_var)
model_no_intercept=lmvar(Sales,X_mu=X_media,X_sigma=X_var,intercept_mu = F,intercept_sigma = F)

summary(model_with_intercept)
summary(model_no_intercept)

residui=residuals(model_with_intercept)
sigma=fitted(model_with_intercept,mu=F)

# creiamo il modello OMOSCHEDASTICO
model=lm(Sales~TV2,advertising)
residui_lm=residuals(model)
sigma_lm=summary(model)$sigma

# confrontiamo i modelli
plot(model_with_intercept)
plot(model)


# residui 
par(mfrow=c(1,2))
plot(TV2,residui/sigma,main='modello eteroschedastico')
abline(h=0,col='red')
plot(TV2,residui_lm/sigma_lm,main='modello omoschedastico')
abline(h=0,col='red')


# confrontiamo i modelli:valori fittati
par(mfrow=c(1,2))
plot(TV2,Sales)
abline(coef(model_with_intercept)[1:2],col='red')
abline(coef(model),col='green')

plot(TV,Sales)
lines(1:300,coef(model_with_intercept)[1]+coef(model_with_intercept)[2]*sqrt(1:300),col='red')
curve(coef(model)[1]+coef(model)[2]*sqrt(x),from=0,to=300,col='green',xname = 'x',add=T)


# plot con itervalli di confidenza
intervalli=fitted(model_with_intercept,interval = "confidence",level=0.95)

lwr=intervalli[,"mu_lwr"]
upr=intervalli[,"mu_upr"]
mu=fitted(model_with_intercept,sigma=F)

par(mfrow=c(1,1))
plot(TV2,mu,xlab='TV2',ylab='Average Sales')
segments(TV2,lwr,TV2,upr)
abline(coef(model_with_intercept)[1:2],col='red')

lwr=intervalli[,"sigma_lwr"]
upr=intervalli[,"sigma_upr"]
plot(TV2,sigma,xlab='TV2',ylab='St.dev di Sales')
segments(TV2,lwr,TV2,upr)
coeff=coef(model_with_intercept)[3:4]
curve(exp(coeff[1]+coeff[2]*x),from=0,to=20,xname='x',add=T,col='red')
sigma_lm=summary(model)$sigma
abline(h=sigma_lm,col='green')

# confrontiamo l'AIC
AIC(model,model_with_intercept)
?BIC
BIC(model,model_with_intercept)

################################################################################
# parte II - regressione lineare multipla                                      #        
################################################################################

# modello ottimale trovato precedentemente

# creiamo le matrici dei  modelli ETEROSCHEDASTICO
X_media=model.matrix(~-1+(TV2+Newspaper+Radio)^2,advertising)
X_var=model.matrix(~-1+TV2,advertising)

model_with_intercept2=lmvar(Sales,X_mu=X_media,X_sigma=X_var)
model_no_intercept2=lmvar(Sales,X_mu=X_media,X_sigma=X_var,intercept_mu = F,intercept_sigma = F)

summary(model_with_intercept2)
summary(model_no_intercept2)

residui=residuals(model_with_intercept2)
sigma=fitted(model_with_intercept2,mu=F)

residui_nointercetta=residuals(model_no_intercept2)
sigma_nointercetta=fitted(model_no_intercept2,mu=F)

# creiamo il modello OMOSCHEDASTICO
model2=lm(Sales~(TV2+Newspaper+Radio)^2,advertising)
residui_lm=model2$residuals
sigma_lm=summary(model)$sigma

# confrontiamo i modelli:residui
par(mfrow=c(3,3))
plot(TV2,residui/sigma,main='modello eteroschedastico con intercetta')
abline(h=0,col='red')
plot(TV2,residui_nointercetta/sigma_nointercetta,main='modello eteroschedastico senza intercetta')
abline(h=0,col='red')
plot(TV2,residui_lm/sigma_lm,main='modello omoschedastico')
abline(h=0,col='red')

plot(Radio,residui/sigma)
abline(h=0,col='red')
plot(Radio,residui_nointercetta/sigma_nointercetta)
abline(h=0,col='red')
plot(Radio,residui_lm/sigma_lm)
abline(h=0,col='red')

plot(Newspaper,residui/sigma)
abline(h=0,col='red')
plot(Newspaper,residui_nointercetta/sigma_nointercetta)
abline(h=0,col='red')
plot(Newspaper,residui_lm/sigma_lm)
abline(h=0,col='red')

# AIC, BIC
AIC(model2,model_with_intercept2)
BIC(model2,model_with_intercept2)


detach(advertising)

################################################################################
