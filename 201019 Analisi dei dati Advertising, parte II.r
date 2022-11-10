###########################
### Advertising parte 2 ###
###########################

# Settiamo la directory di lavoro  ------------------------------------------------------
#setwd("C:/Users/marti/OneDrive/Desktop/modelli_statistici/LEZIONE4-qualitative variables, multiple regressor/")

# Carichiamo i dati  --------------------------------------------------------------

# Leggiamo il file csv
library(readr)
## caricare il pacchetto
#advertising <- read_csv("C:/Users/marti/OneDrive/Desktop/modelli_statistici/LEZIONE2-regressione lineare semplice/Advertising.csv")
attach(advertising)
## richiamiamo il modello lineare semplice migliore che avevamo trovato ##
advertising$TV2 = TV^0.5
attach(advertising)
SimpleReg4 = lm(Sales~TV2)
summary(SimpleReg4)


### ### ### ### ### ### ### ### ###
### Modello regressivo multivariato
### ### ### ### ### ### ### ### ###

# regressioni multiple
MultiReg1 = lm(Sales~TV+Radio+Newspaper)
summary.lm(MultiReg1)
MultiReg2 = lm(Sales~TV2+Radio+Newspaper)
summary.lm(MultiReg2)
# In tutti e due i modelli la variabile Newspaper non è significativa
# e va eliminata dal modello
MultiReg3 = lm(Sales~TV+Radio)
summary.lm(MultiReg3)
MultiReg4 = lm(Sales~TV2+Radio)
summary.lm(MultiReg4)

## model choice
AIC(MultiReg3,MultiReg4) # il quarto è il migliore

# CIs
paste(round(coef(MultiReg4),3), " (95%CI: [",round(confint(MultiReg4, level=0.95),3)[,1], " ; ",round(confint(MultiReg4, level=0.95),3)[,2], "])", sep="")
# standard errors
paste(round(coef(MultiReg4),3), " (SE: ",round(summary.lm(MultiReg4)$coefficients[,2],3), ")",sep="")

# Matrice di correlazione dei regressori
covmat=summary(MultiReg4)$cov.unscaled * (summary.lm(MultiReg4)$sigma^2) # covariance matrix
round(covmat,2) # per visualizzare meglio


# salviamo i residui
res2=MultiReg4$residuals
# oppure
res = rstandard(MultiReg4)
# e facciamone un plot
plot(fitted(MultiReg4),res)
abline(h=0)
# c'è una struttura nei residui che va eliminata. Per adesso ignoriamola

# plot dei residui rispetto alle variabili usate nel modello
par(mfrow=c(1,3))
plot(fitted(MultiReg4),res)
abline(h=0)
plot(TV2, res)
plot(Radio, res)
# anche qui ci sono strutture nei residui

# facciamo un'analisi di influenza
meas.inf = influence.measures(MultiReg4)$infmat
colnames(meas.inf)
#"dfb.1_"  		variazioni nel valore dell'intercetta
#"dfb.TV2" 		variazioni nel valore del coefficiente regressivo di TV2
#"dfb.Radi" 		variazioni nel valore del coefficiente regressivo di Radio
#"dffit"		  variazioni nella previsione i-esima
#"cov.r"			variazioni nella matrice di covarianza
#"cook.d"			variazione nella previsione generale
#"hat"				matrice di influenza

par(mfrow=c(3,2))
plot(meas.inf[,"dfb.1_"])
plot(meas.inf[,"dfb.TV2"])
plot(meas.inf[,"dfb.Radi"])
plot(meas.inf[,"dffit"])
#plot(meas.inf[,"cov.r"])
plot(meas.inf[,"cook.d"])
plot(meas.inf[,"hat"])
##
which(meas.inf[,"dfb.1_"]< -0.4)
which(meas.inf[,"dfb.TV2"]>0.6)

which(meas.inf[,"cook.d"]>0.2)

# l'osservazione 131 sembra essere anomala
summary.lm(MultiReg4)
summary(advertising)
advertising[131,]
res[131]
summary(res)
# l'osservazione ha residuo alto in modulo
# ha valori alti per radio e bassi per TV2
# e valore di vendita medio basso
# Vediamo il valore predetto
predict(MultiReg4)[131]



# alcune previsioni
predict(MultiReg4,newdata=data.frame(TV2=0^0.5, Radio=0, Newspaper=0), interval="confidence")
predict(MultiReg4,newdata=data.frame(TV2=300^0.5, Radio=0, Newspaper=0), interval="confidence")
predict(MultiReg4,newdata=data.frame(TV2=0^0.5, Radio=30, Newspaper=0), interval="confidence")
predict(MultiReg4,newdata=data.frame(TV2=0^0.5, Radio=0, Newspaper=100), interval="confidence")
predict(MultiReg4,newdata=data.frame(TV2=300^0.5, Radio=30, Newspaper=100), interval="confidence")



### ### ### ### ### ### ### ### ###
### Modello regressivo multivariato con interazione
### ### ### ### ### ### ### ### ###

# con tre variabili abbiamo molti modelli da testare
# effetti singoli
# effetti singoli piu' interazioni

# Non si dovrebbe mettere un'interazione se non sono presenti anche gli effetti singoli,
# diventa difficile l'interpretazione

# ci sono differenti modi per specificare l'interazione
Reg1 = lm(Sales~TV2:Radio)
Reg2 = lm(Sales~TV2*Radio)
Reg3 = lm(Sales~(TV2+Radio)^2)
summary(Reg1)
summary(Reg2)
summary(Reg3)


# Scelta automatica del miglior modello
RegAll = lm(Sales~(TV2+Radio+Newspaper)^2)
summary.lm(RegAll)
# notate come newspaper ha un effetto interattivo significativo

# Utilizziamo la funzione step
?step
RegStep = step(RegAll)
summary(RegStep)
# nel modello migliore, newspaper ha un effetto singolo significativo,
# anche se nel modello senza interazione non era significativa
# In generale (non in questo caso) step potrebbe scegliere un modello
# con alcune variabili non significative, che vanno poi eliminate


# text manipulation for a useful output: estimate and its 95%CI
paste(round(coef(RegStep),3), " (95%CI: [",round(confint(RegStep, level=0.95),3)[,1], " ; ",round(confint(RegStep, level=0.95),3)[,2], "])", sep="")
# text manipulation for a useful output: estimate and its standard error
paste(round(coef(RegStep),3), " (SE: ",round(summary.lm(RegStep)$coefficients[,2],3), ")",sep="")


# residui
res=RegStep$residuals
# plot dei residui
plot(fitted(RegStep),res)
abline(h=0)
# ci sono delle strutture nei residui (inverse U-shape) - ma è meglio di prima
# Delle strutture rimangono
par(mfrow=c(3,2))
plot(predict(RegStep), res)
plot(TV2, res)
plot(Radio, res)
plot(Newspaper, res)
plot(TV2*Newspaper, res)
plot(TV2*Radio, res)


# facciamo un'analisi di influenza
meas.inf = influence.measures(RegStep)$infmat
colnames(meas.inf)
par(mfrow=c(3,2))
plot(meas.inf[,"dfb.1_"])
plot(meas.inf[,"dfb.TV2"])
plot(meas.inf[,"dfb.Radi"])
plot(meas.inf[,"dfb.Nwsp"])
plot(meas.inf[,"dfb.TV2:R"])
plot(meas.inf[,"dfb.TV2:N"])

par(mfrow=c(3,2))
plot(meas.inf[,"dffit"])
#plot(meas.inf[,"cov.r"])
plot(meas.inf[,"cook.d"])
plot(meas.inf[,"hat"])

## le osservazioni 131 e 156 sono influenti:
which(meas.inf[,"dfb.TV2"]>0.4)
which(meas.inf[,"dfb.Radi"]< -1)
which(meas.inf[,"dfb.Nwsp"]>0.4)
which(meas.inf[,"dfb.TV2:R"]> 1)

which(meas.inf[,"dffit"]< -0.7)
which(meas.inf[,"cook.d"]>0.2)



# ripetiamo queste ultime analisi senza queste osservazioni

# Scelta automatica del miglior modello
RegAll2 = lm(Sales~(TV2+Radio+Newspaper)^2, data = advertising[-c(131,156),])
summary.lm(RegAll2)

# Utilizziamo la funzione step
?step
RegStep2 = step(RegAll2)
summary(RegStep2)


# residui
res=RegStep2$residuals
# plot dei residui
plot(fitted(RegStep2),res)
abline(h=0)
# ci sono delle strutture nei residui (inverse U-shape) - ma è meglio di prima


# Delle strutture rimangono
par(mfrow=c(3,2))
plot(predict(RegStep2), res)
plot(advertising[-c(131,156),]$TV2, res)
plot(advertising[-c(131,156),]$Radio, res)
plot(advertising[-c(131,156),]$Newspaper, res)
plot(advertising[-c(131,156),]$TV2*advertising[-c(131,156),]$Newspaper, res)
plot(advertising[-c(131,156),]$TV2*advertising[-c(131,156),]$Radio, res)
# I risultati sono migliori
