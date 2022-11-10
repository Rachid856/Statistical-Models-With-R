#### #### #### #### #### #### ####
#### Modello di Rasch
#### #### #### #### #### #### ####

rm(list=ls())

#### librerie
library(R2jags)
library(sjmisc)

#### #### #### #### #### ####
#### The data
#### #### #### #### #### ####
#### #### DESCRIZIONE
####  is a 5-item multiple choice test; students score 1 on each
####  item for the correct answer and 0 otherwise, giving R = 32 possible response patterns.
####  Boch and Lieberman (1970) present data on LSAT for N = 1000 students
#### #### #### #### #### ####

setwd("/Users/gianlucamastrantonio/Dropbox (Politecnico di Torino Staff)/didattica/Modelli Statistici/Codici R/WinBugs/")

### ### ### ### ### ### ###
### I dati
### ### ### ### ### ### ###

# pattern di possibili risposte
response = matrix(
	c(
							0, 0, 0, 0, 0,
							0, 0, 0, 0, 1,
							0, 0, 0, 1, 0,
							0, 0, 0, 1, 1,
							0, 0, 1, 0, 0,
							0, 0, 1, 0, 1,
							0, 0, 1, 1, 0,
							0, 0, 1, 1, 1,
							0, 1, 0, 0, 0,
							0, 1, 0, 0, 1,
							0, 1, 0, 1, 0,
							0, 1, 0, 1, 1,
							0, 1, 1, 0, 0,
							0, 1, 1, 0, 1,
							0, 1, 1, 1, 0,
							0, 1, 1, 1, 1,
							1, 0, 0, 0, 0,
							1, 0, 0, 0, 1,
							1, 0, 0, 1, 0,
							1, 0, 0, 1, 1,
							1, 0, 1, 0, 0,
							1, 0, 1, 0, 1,
							1, 0, 1, 1, 0,
							1, 0, 1, 1, 1,
							1, 1, 0, 0, 0,
							1, 1, 0, 0, 1,
							1, 1, 0, 1, 0,
							1, 1, 0, 1, 1,
							1, 1, 1, 0, 0,
							1, 1, 1, 0, 1,
							1, 1, 1, 1, 0,
							1, 1, 1, 1, 1),
							ncol= 5, byrow=T

)
### frequenze cumulate del numero di risposte dei patterns
culm = c(3, 9, 11, 22, 23, 24, 27, 31, 32, 40, 40, 56, 56, 59, 61, 76, 86, 115, 129, 210, 213, 241, 256, 336, 352, 408, 429, 602, 613, 674, 702, 1000)

# creaiamo il dataset in cui ogni riga sono le risposte di un soggetto
Data = matrix(NA, ncol=5, nrow=1000)
W1 = 1
for(i in 1:length(culm))
{
	W2 = culm[i]

	if( (W2-W1)>=0)
	{
		Data[W1:W2,] = rep(response[i,],each = W2-W1+1)
	}
	W1 = culm[i]+1
}

# spieghiamo i dati utilizzando una variabile che rappresenta
# la difficoltà della domanda
# e una che rappresenta l'abilità del soggetto

#### #### #### #### #### #### ####
#### stime
#### #### #### #### #### #### ####

# lista con tutti gli elementi che servono al modello
dataList = list(
	Y   = Data,
	n   = 1000,
	k   = 5
)

#### Modello Bayesiano
mod1_string <- 'model
{
	# Rasch model
	for(i in 1:n)
	{
		for(j in 1:k)
		{
  		logit(p[i,j]) <- theta[i] - alpha[j]
  		Y[i,j] ~ dbern(p[i,j])
		}
	}
	# Priors
	for(i in 1:n)
	{
		theta[i] ~ dnorm(0, 1/10000)
	}
	for(j in 1:(k-1))
	{
 		alpha[j] ~ dnorm(0, 1/10000)
	}
	# necessaria per identificare i parametri
	alpha[k] = 0
}'


# parametri da salvare
SavePar = c("alpha", "theta", "p")

# fittiamo il modello
set.seed(1)
rasch_model = jags(
			data 	= dataList,
			parameters.to.save 	= SavePar,
			model.file 					= textConnection(mod1_string), # il model.file dovrebbe essere un file,
			 																									# "textConnection" crea il file e lo carica
			n.chains 						= 1,
			n.iter 							= 5000,
			n.burnin 						= 2000,
			n.thin 							= 2,
			DIC 								= T
)

#### #### #### #### #### #### ####
#### stime
#### #### #### #### #### #### ####

# difficoltà
Walpha = which(substr(names(rasch_model$BUGSoutput$sims.array[1,1,]),1,5)=="alpha")
## alpha
alphaPOST = rasch_model$BUGSoutput$sims.array[,1,Walpha]
par(mfrow=c(3,2))
plot(alphaPOST[,1])
plot(alphaPOST[,2])
plot(alphaPOST[,3])
plot(alphaPOST[,4])
plot(alphaPOST[,5])

acf(alphaPOST)

alphaPOST_MCMC = as.mcmc(alphaPOST)
summary(alphaPOST_MCMC)

# vediamo le distribuzioni
boxplot(alphaPOST)

# abilità
Wtheta = which(substr(names(rasch_model$BUGSoutput$sims.array[1,1,]),1,5)=="theta")
## alpha
thetaPOST = rasch_model$BUGSoutput$sims.array[,1,Wtheta]

# medie delle abilità
thetaMeans = colMeans(thetaPOST)
plot(thetaMeans)
# ricordate che i dati sono ordinati in base al numero di 1

# probabilità
Wp = which(substr(names(rasch_model$BUGSoutput$sims.array[1,1,]),1,2)=="p[")
## alpha
pPOST = rasch_model$BUGSoutput$sims.array[,1,Wp]

# medie delle abilità
pMeans = colMeans(pPOST)
plot(pMeans, col=rep(1:5, each= 1000))
#colnames(pPOST)


## previsione
