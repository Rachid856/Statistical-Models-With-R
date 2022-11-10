#####################################################
# MODELLI STATISTICI
#
# Regressione lineare
#
# Dataset: Adverstising
#
# Il dataset contiene le vendite, di migliaia di unita', di un prodotto in 200 mercati,
# insieme al budget speso per la pubblicita', in migliaia di dollari, su 3 media: Tv,
# radio e giornali.
# Lo scopo e' determinare quale strategia pubblicitaria e' migliore e prevedere
# le vendita in base al budget speso per ogni media
#
# 
#####################################################

# Puliamo il workspace ----------------------------------------------

rm(list=ls())


# LIBRARIES  -------------------------------------------------------------

# Functions
# le due funzioni riportate di sequito si trovano nella documentazione di pairs
?pairs

# mettere sulla diagonale le correlazioni
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

# mettere sulla diagonale gli istogrammi
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# Settiamo la directori di lavoro  ------------------------------------------------------
setwd("C:/Users/marti/OneDrive/Desktop/modelli_statistici/LEZIONE2-regressione lineare semplice/")

# Carichiamo i dati  --------------------------------------------------------------

# Leggiamo il file csv
library(readr)
advertising <- read_csv("C:/Users/marti/OneDrive/Desktop/modelli_statistici/LEZIONE2-regressione lineare semplice/Advertising.csv")

# quando si caricano i dataset fare attenzione a separatori, decimali e header

# leggiamo le prime righe del dataset
head(advertising)

# facciamo un summary
summary(advertising)

TV # perche' questo comando non funziona? dobbiamo fare attach del dataset

# facciamo attach del dataset, per richiamare le variabili piu¹ facilmente
attach(advertising)
# con il comando detach(advertising) si elimina l'effetto di attach


### ### ### ### ### ### ### ### ### ### ###
### Anailisi descrittive                ###
### ### ### ### ### ### ### ### ### ### ###

# Dimensioni del dataset
length(advertising) # Numero di variabili:
										# questo comando funziona solo con dataframe, ma non con matrici.
dim(advertising)		# Righe e colonne
names(advertising)	# nome variabili, potete usare anche colnames(advertising)
length(X1)					# numero di osservazioni

# altre alternative
nrow(advertising)
ncol(advertising)

# plot generale
plot(advertising)
?pairs
pairs(advertising[,2:5]) # standar plot
pairs(advertising[,2:5], upper.panel=panel.cor) # correlazione
pairs(advertising[,2:5], upper.panel=panel.cor,diag.panel=panel.hist) # istogramma
# fate attenzione alla correlazione tra predittori

# Plots
# 1 riga, tre colonne
par(mfrow=c(nrows=1,ncols=3)) # par(mfrow=c(1,3))
plot(x=TV, y=Sales, pch=16, col="red")
plot(x=Radio, y=Sales, pch=16, col="red")
plot(x=Newspaper, y=Sales, pch=16, col="red")


# stima smooth della relazione
?scatter.smooth
par(mfrow=c(nrows=1,ncols=3)) # par(mfrow=c(1,3))
scatter.smooth(x=TV, y=Sales, pch=16, col="red")
scatter.smooth(x=Radio, y=Sales, pch=16, col="red")
scatter.smooth(x=Newspaper, y=Sales, pch=16, col="red")

# aumentare lo smooth
?scatter.smooth
par(mfrow=c(1,3))
scatter.smooth(x=TV, y=Sales, pch=16, col="red",span=0.1)
scatter.smooth(x=Radio, y=Sales, pch=16, col="red",span=0.1)
scatter.smooth(x=Newspaper, y=Sales, pch=16, col="red",span=0.1)

### ### ### ### ### ### ### ### ### ### ###
### Regressione lineare - un predittore
### ### ### ### ### ### ### ### ### ### ###

### TV
n=dim(advertising)[1] # numero dei dati raccolti
m=2 # numero di coefficienti (intercetta beta1 + coeff. angolare beta2 )
SimpleReg1 = lm(Sales~TV)
modello1_summary=summary(SimpleReg1)
modelCoeffs <- modello1_summary$coefficients 
summary(SimpleReg1)

#analizziamo i risultati relativi all'intercetta
beta1.estimate <- modelCoeffs["(Intercept)", "Estimate"] 

std.error <- modelCoeffs["(Intercept)","Std. Error"]
sqrt(sum((Sales-SimpleReg1$fitted.values)^2*sum(TV^2))/(n*(n-m)*(sum(TV^2)-n*mean(TV)^2)))

t_value1 <- beta1.estimate/std.error # sotto h0
p_value1 <- 2*pt(-abs(t_value1), df=n-m) # per simmetria della distribuzione t

#analizziamo i risultati relativi al coefficiente angolare
beta2.estimate <- modelCoeffs["TV", "Estimate"] 

std.error <- modelCoeffs["TV","Std. Error"]
sqrt(sum((Sales-SimpleReg1$fitted.values)^2)/((n-m)*(sum(TV^2)-n*mean(TV)^2)))

t_value2 <- beta2.estimate/std.error # sotto h0
p_value2 <- 2*pt(-abs(t_value2), df=n-m) # per simmetria della distribuzione t

# F test
(n-m)*sum((SimpleReg1$fitted.values-mean(Sales))^2)/(sum((Sales-SimpleReg1$fitted.values)^2))



### Radio
SimpleReg2 = lm(Sales~Radio)
summary(SimpleReg2)
### Newspaper
SimpleReg3 = lm(Sales~Newspaper)
summary(SimpleReg3)

# Scelta del modello: maggiore è l'indice R^2adj migliore è il modello

RsquaredAdj = c(
	summary(SimpleReg1)$adj.r.squared,
	summary(SimpleReg2)$adj.r.squared,
	summary(SimpleReg3)$adj.r.squared
	)
Rsquared = c(
	summary(SimpleReg1)$r.squared,
	summary(SimpleReg2)$r.squared,
	summary(SimpleReg3)$r.squared
	)

Aic = AIC(SimpleReg1,SimpleReg2,SimpleReg3)[,2] # minore è l'indice AIC, migliore è il modello

# tutti e 3 gli indici preferiscono il primo modello
ModelChoice = data.frame(
	Mod=c("SimpleReg1","SimpleReg2","SimpleReg3"),
	RsquaredAdj=RsquaredAdj,
	Rsquared=Rsquared,
	Aic = Aic
	)

# Vediamo i risultati del primo modello
summary(SimpleReg1)
names(SimpleReg1)

# Stime e 95%CIs
paste(round(coef(SimpleReg1),3), " (95%CI: [",round(confint(SimpleReg1, level=0.95),3)[,1], " ; ",round(confint(SimpleReg1, level=0.95),3)[,2], "])", sep="")
# Stime e standard errors
paste(round(coef(SimpleReg1),3), " (SE: ",round(summary.lm(SimpleReg1)$coefficients[,2],3), ")",sep="")


# Plottiamo la linea di regressione
par(mfrow=c(nrows=1,ncols=1))
plot(x=TV, y=Sales, pch=16, col="red")
abline(SimpleReg1, col="blue", lwd=3)

# disegnamo i residui
segments(x0=TV,y0=Sales,x1=TV,y1=coef(SimpleReg1)[1] + coef(SimpleReg1)[2]*TV)

## Verifica delle ipotesi alla base del modello

res = SimpleReg1$residuals
# o res = summary.lm(SimpleReg1)$residuals...

# plottiamo i residui
plot(TV,res)
abline(h=0)
# C'e' eteroschedasticita'. L'ipotesi alla base del modello non sono soddisfatte
# C'e' una componente "curvilinea" residua

# Verifichiamo la normalita' dei residui
qqnorm(res/sd(res)) 		# plot quantile-quantile
abline(a=0,b=1,col=2)
# oppure
plot(hist(SimpleReg1$residuals))
#oppure
plot(SimpleReg1)
# oppure
shapiro.test(res) #  H0: i dati sono distribuiti normalmente



# Intervalli di confidenza dei parametri
confint(SimpleReg1)
Lower_bounds=confint(SimpleReg1)[,1]
Upper_bounds=confint(SimpleReg1)[,2]

# banda di previsione
plot(x=TV, y=Sales, pch=16, col="red")
abline(SimpleReg1, col="blue", lwd=3)
abline(Lower_bounds, col="blue", lwd=3, lty=2)
abline(Upper_bounds, col="blue", lwd=3, lty=2)


# Proviamo a risolvere i problemi nei residui con una trasformazione
# e' impossibile risolvere il problema dell'eteroschedasticita' con una trasformazione,
# ma possiamo provare ad eliminare, o alleviare, la componente curva residua

# trasformazione
advertising$TV2 = TV^0.5
attach(advertising)
pairs(advertising[,2:6], upper.panel=panel.cor,diag.panel=panel.hist)
scatter.smooth(x=TV2, y=Sales, pch=16, col="red")

SimpleReg4 = lm(Sales~I(TV^0.5))
# oppure
SimpleReg4 = lm(Sales~TV2) # da errore, bisogna rifare attach del dataset
attach(advertising)
SimpleReg4 = lm(Sales~TV2)

# Scelta del modello
RsquaredAdj = c(
	summary(SimpleReg1)$adj.r.squared,
	summary(SimpleReg2)$adj.r.squared,
	summary(SimpleReg3)$adj.r.squared,
	summary(SimpleReg4)$adj.r.squared
	)
Rsquared = c(
	summary(SimpleReg1)$r.squared,
	summary(SimpleReg2)$r.squared,
	summary(SimpleReg3)$r.squared,
	summary(SimpleReg4)$r.squared
	)
Aic = AIC(SimpleReg1,SimpleReg2,SimpleReg3,SimpleReg4)[,2]


ModelChoice # il nuovo modello e' il migliore


# Vediamo i risultati
summary(SimpleReg4)
names(SimpleReg4)
str(SimpleReg4)


# Plottiamo la linea di regressione
par(mfrow=c(nrows=1,ncols=1))
plot(x=TV2, y=Sales, pch=16, col="red") # fate attenzione: usare TV2
abline(SimpleReg4, col="blue", lwd=3)

# plot the least squares lines
segments(x0=TV2,y0=Sales,x1=TV2,y1=coef(SimpleReg4)[1] + coef(SimpleReg4)[2]*TV2)

## Verifica delle ipotesi alla base del modello
res = SimpleReg4$residuals
# o res = summary.lm(SimpleReg4)$residuals...

# plottiamo i residui
plot(TV2,res)
abline(h=0)
# C'e' ancora eteroschedasticita'. Proviamo a risolverla utilizzando piu¹ variabili


# Verifichiamo la normalita' dei residui
qqnorm(res/sd(res)) 		# plot quantile-quantile
abline(a=0,b=1,col=2)
# oppure
plot(hist(SimpleReg4$residuals))
#oppure
shapiro.test(res)


# Intervalli di confidenza dei parametri
confint(SimpleReg4)
Lower_bounds=confint(SimpleReg4)[,1]
Upper_bounds=confint(SimpleReg4)[,2]

# banda di previsione
plot(x=TV2, y=Sales, pch=16, col="red")
abline(SimpleReg4, col="blue", lwd=3)
abline(Lower_bounds, col="blue", lwd=3, lty=2)
abline(Upper_bounds, col="blue", lwd=3, lty=2)


# predizione
predict(SimpleReg4,data.frame(TV2=(c(0,150,300)^0.5)), interval="confidence")
predict(SimpleReg4,data.frame(TV2=(c(0,150,300)^0.5)), interval="prediction")


### ### ### ### ### ### ### ### ###
### Misure di influenza -  Quarto Modello
### ### ### ### ### ### ### ### ###

# L'idea e' quella di eliminare un'osservazione e vedere come cambia il modello
meas.inf = influence.measures(SimpleReg4)$infmat
colnames(meas.inf)
#"dfb.1_"  		variazioni nel valore dell'intercetta
#"dfb.TV2" 		variazioni nel valore del coefficiente regressivo
#"dffit"		  variazioni nella previsione i-esima
#"cov.r"			variazioni nella matrice di covarianza
#"cook.d"			variazione nella previsione generale
#"hat"				matrice di influenza

plot(meas.inf[,"dfb.1_"])
plot(meas.inf[,"dfb.TV2"])
plot(meas.inf[,"dffit"])
plot(meas.inf[,"cov.r"])
plot(meas.inf[,"cook.d"])
plot(meas.inf[,"hat"])
# non sembrano esserci osservazioni particolarmente influenti

## possiamo trovare le osservazioni con valori "estremi" tramite
summary(influence.measures(SimpleReg4))


