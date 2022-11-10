### ### ### ### ### ### ### ### ### 
### 16-11-2020 glm Poisson
### Si vuole studiare come il numero delle rotture di
### fili di lana dipendano dal tipo di
### lana e la tensione 
### ### ### ### ### ### ### ### ### 

## 
rm(list =ls())
## 

## librerie
library(datasets)
library(contrast)
library(MASS)
##

# carichiamo i dati
Data = warpbreaks
summary(Data)

# i dati si possono rappresentare come tabelle a doppia entrata
table(Data[,-1])

mytable= xtabs(~Data[,2]+Data[,3])
ftable(mytable)
### ### ### ### ### ### ### 
### Statistiche descrittive
### ### ### ### ### ### ### 

mean(Data$breaks)
var(Data$breaks) # overdispersed

tapply(Data$breaks,Data$tension,mean)
tapply(Data$breaks,Data$wool,mean)

tapply(Data$breaks,Data$tension,var)
tapply(Data$breaks,Data$wool,var)

tapply(Data$breaks,paste(Data$tension,Data$wool),mean)
tapply(Data$breaks,paste(Data$tension,Data$wool),var)

## ## ## ## ## ## ## ##  
## Modello di poisson
## ## ## ## ## ## ## ## 
Mod1 = glm(breaks ~ tension+wool, data=Data, family="poisson")
summary(Mod1)
X = model.matrix(Mod1)


# Possiamo fare dei CONTRASTI

Cont1 = contrast(Mod1 , 
                 list(tension="H", wool = "A"),
                 list(tension="L", wool = "A"), type="individual" )
print(Cont1, X=T)


Cont1 = contrast(Mod1 , 
                 list(tension="H", wool = "B"),
                 list(tension="L", wool = "B"), type="individual" )
print(Cont1, X=T)

Cont1 = contrast(Mod1 , 
                 list(tension="H", wool = levels(Data$wool)),
                 list(tension="L", wool = levels(Data$wool)), type="individual" )
print(Cont1, X=T)


Cont1 = contrast(Mod1 , 
                 list(tension="M", wool = levels(Data$wool)),
                 list(tension="H", wool = levels(Data$wool)), type="individual" )
print(Cont1, X=T)

# CALCOLIAMOLO MANUALMENTE QUESTO CONTRASTO
Cont1 = contrast(Mod1 , 
                 list(tension="M", wool = "A"),
                 list(tension="H", wool = "A"), type="individual" )
print(Cont1, X=T)

# salvo le stime dei coefficienti
coefs=matrix(coef(Mod1),ncol=1)

#calcolo la matrice di covarianza dei coefficienti
covmatJ=solve(t(X)%*%diag(Mod1$weights)%*%X)

# scelgo il vettore v della configurazione
v=array(0,dim=length(coefs))
v[c(2:3)]=c(1,-1)

##  valore del parametro sotto analsi
Diff=v%*%coefs
var=v%*%covmatJ%*%v

# Tvalue
tvalue=Diff/(var^0.5)
df=length(Data$breaks)-4

# t-test
pt(tvalue,df,lower.tail = F)+pt(-tvalue,df,lower.tail = T)
2*pnorm(tvalue,lower.tail = F)

##
Cont2 = contrast(Mod1 , 
                 list(tension="M", wool = levels(Data$wool)),
                 list(tension="H", wool = levels(Data$wool)))
print(Cont2, X=T)

## Calcoliamo gli intervalli di confidenza
confint.default(Mod1)
#o in termini di log-ratio (OO)
exp(confint.default(Mod1))


## ## ## ## ## ## ## ## ## 
## TESTS
## ## ## ## ## ## ## ## ## 

str(summary(Mod1))
D0 = summary(Mod1)$null.deviance
gdl0 = summary(Mod1)$df.null
D1 = summary(Mod1)$deviance
gdl1 = summary(Mod1)$df.residual

deltaD = D0-D1
pchisq(deltaD, gdl0-gdl1, lower.tail=F)


## vediamo se il modello e' buono come il saturo
pchisq(D1, gdl1, lower.tail=F)

# rappresentazione grafica
xseq = seq(0,220, length.out=100)
plot(xseq,dchisq(xseq,gdl1), type="l")
abline(v=D1,col=2)



## ## ## ## ## ## ## ##  
## Misure di influenza
## ## ## ## ## ## ## ## 
InfMeas1 = influence.measures(Mod1)
str(InfMeas1)
InfMeas1Mat = InfMeas1$infmat
ObsInf = InfMeas1$is.inf


# i risultati dipendono dalla parametrizzazione

s=1:54 #indici delle osservazioni
plot(InfMeas1Mat[,1])

plot(InfMeas1Mat[,2])
points(s[Data$tension=='H'],InfMeas1Mat[Data$tension=='H',2],col='red')

plot(InfMeas1Mat[,3])
points(s[Data$tension=='M'],InfMeas1Mat[Data$tension=='M',3],col='red')


plot(InfMeas1Mat[,4])

table(ObsInf)


## ## ## ## ## ## ## ##  
## introduciamo l'interazione
## ## ## ## ## ## ## ## 

Mod2 = glm(breaks ~ tension+wool+tension:wool, data=Data, family=poisson)
summary(Mod2)


# AIC
AIC(Mod1,Mod2)
# TEST SULLE DEVIANZE
D2=Mod2$deviance
gdl2=Mod2$df.residual

anova(Mod1,Mod2,test="Chisq")
pchisq(D1-D2,gdl1-gdl2,lower.tail = F)





