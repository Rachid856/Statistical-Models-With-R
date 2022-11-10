#### #### #### #### #### #### ####
#### Stagnant
#### #### #### #### #### #### ####

rm(list=ls())

#### librerie
library(R2jags)
library(sjmisc)

#### #### #### #### #### ####
#### The data
#### #### #### #### #### ####
#### #### DESCRIZIONE
#### Cerchiamo di spiegare i dati Y usando due spezzate
#### i.e., cerchiamo il change-point.
#### behaviour of stagnant surface layer height in a controlled flow of water
#### down an inclined channel using different surfactants
#### (comportamento dell'altezza dello strato superficiale stagnante in un flusso d'acqua
#### controllato lungo un canale inclinato utilizzando diversi tensioattivi)
#### #### #### #### #### ####

setwd("/Users/gianlucamastrantonio/Dropbox (Politecnico di Torino Staff)/didattica/Modelli Statistici/Codici R/WinBugs/")

### ### ### ### ### ### ###
### I dati
### ### ### ### ### ### ###

# valori y
Y = c(1.12, 1.12, 0.99, 1.03, 0.92, 0.90, 0.81, 0.83, 0.65, 0.67, 0.60, 0.59, 0.51, 0.44, 0.43,
0.43, 0.33, 0.30, 0.25, 0.24, 0.13, -0.01, -0.13, -0.14, -0.30, -0.33, -0.46, -0.43, -0.65)

x = c(-1.39, -1.39, -1.08, -1.08, -0.94, -0.80, -0.63, -0.63, -0.25, -0.25, -0.12, -0.12, 0.01, 0.11, 0.11,
0.11, 0.25, 0.25, 0.34, 0.34, 0.44, 0.59, 0.70, 0.70, 0.85, 0.85, 0.99, 0.99, 1.19)

# vediamo la densità
plot(x,Y)

#### #### #### #### #### #### ####
#### stime
#### #### #### #### #### #### ####

# lista con tutti gli elementi che servono al modello
dataList = list(
	Y   = Y,
	x   = x,
	N   = length(Y)
)

#### Modello Bayesiano
mod1_string <- 'model
{
	for(i in 1:N)
	{
		Y[i] ~ dnorm(mu[i], tau)
		mu[i] <- alpha + beta[J[i]] * (x[i] - x.change)
		J[i] <- 1 + step(x[i] - x.change) ## step è 0 se <0, 1 altrimenti
	}
	tau ~ dgamma(0.001, 0.001)
	alpha ~ dnorm(0.0,1.0E-6)
	for(j in 1:2)
	{
		beta[j] ~ dnorm(0.0,1.0E-6)
	}
	sigma <- 1 / sqrt(tau)
	x.change ~ dunif(-1.3,1.1)
}'


# parametri da salvare
SavePar = c("alpha", "beta","sigma","x.change")

# inits
inits =  list(
  list(alpha = 0.47, beta = c(-0.45, -1.0), tau = 5, x.change = 0.5)
)
# fittiamo il modello
set.seed(1)
model = jags(
			data 	= dataList,
			parameters.to.save 	= SavePar,
			inits					= inits,
			model.file 		= textConnection(mod1_string),
			n.chains 						= 1,
			n.iter 						  = 25000,
			n.burnin 						= 15000,
			n.thin 							= 5,
			DIC 								= T
)

#### #### #### #### #### #### ####
#### stime
#### #### #### #### #### #### ####

# alpha
Wpar = which(substr(names(model $BUGSoutput$sims.array[1,1,]),1,1)%in%c("a","b","s","x"))
##
parPOST = as.mcmc(model$BUGSoutput$sims.array[,1,Wpar])
summary(parPOST)
plot(parPOST)
acf(parPOST)


### funzione media -
# mu[i] <- alpha + beta[J[i]] * (x[i] - x.change)
# J[i] <- 1 + step(x[i] - x.change)

# valori di x da usare
xseq = seq(min(x), max(x), by=0.1)

PosterioFunction = matrix(NA, nrow=nrow(parPOST), ncol=length(xseq))

for(imcmc in 1:nrow(parPOST))
{
	PosterioFunction[imcmc,] = parPOST[imcmc,"alpha"]+ifelse((xseq-parPOST[imcmc,"x.change"]) < 0,parPOST[imcmc,"beta[1]"],parPOST[imcmc,"beta[2]"])*(xseq-parPOST[imcmc,"x.change"])
}

## esempi delle realizzazioni della funzione a posteriori

# tutte
plot(xseq,PosterioFunction[1,], type="l", col=1,ylim=range(PosterioFunction))
for(i in 1:nrow(PosterioFunction))
{
	lines(xseq,PosterioFunction[i,],  col=i)
}

# solo alcune
plot(xseq,PosterioFunction[1,], type="l", col=1,ylim=range(PosterioFunction))
lines(xseq,PosterioFunction[150,],  col=3)
lines(xseq,PosterioFunction[250,],  col=4)

### calcoliamo la media a posteriori e intervalli di credibilità

PostMeam = colMeans(PosterioFunction)
PostQ1   = apply(PosterioFunction,2,quantile, prob=0.025)
PostQ2   = apply(PosterioFunction,2,quantile, prob=1-0.025)

plot(xseq,PostMeam, type="l", col=1, ylim=range(PosterioFunction))
lines(xseq,PostQ1, col=2, lty=3, lwd=2)
lines(xseq,PostQ2, col=2, lty=3, lwd=2)
points(x,Y)
