#### #### #### #### #### #### ####
#### Rats
#### #### #### #### #### #### ####

rm(list=ls())

#### librerie
library(R2jags)
library(sjmisc)

#### #### #### #### #### ####
#### The data
#### #### #### #### #### ####
#### #### DESCRIZIONE
#### This example is taken from section 6 of Gelfand et al (1990), and concerns 30
#### young rats whose weights were measured weekly for five weeks.
####  Y ij is the weight of the ith rat measured at age x j .
#### #### #### #### #### ####

setwd("/Users/gianlucamastrantonio/Dropbox (Politecnico di Torino Staff)/didattica/Modelli Statistici/Codici R/WinBugs/")

### ### ### ### ### ### ###
### I dati
### ### ### ### ### ### ###

# valori x
x = c(8.0, 15.0, 22.0, 29.0, 36.0)
# valori y
Y = matrix(
	c(151, 199, 246, 283, 320,
							145, 199, 249, 293, 354,
							147, 214, 263, 312, 328,
							155, 200, 237, 272, 297,
							135, 188, 230, 280, 323,
							159, 210, 252, 298, 331,
							141, 189, 231, 275, 305,
							159, 201, 248, 297, 338,
							177, 236, 285, 350, 376,
							134, 182, 220, 260, 296,
							160, 208, 261, 313, 352,
							143, 188, 220, 273, 314,
							154, 200, 244, 289, 325,
							171, 221, 270, 326, 358,
							163, 216, 242, 281, 312,
							160, 207, 248, 288, 324,
							142, 187, 234, 280, 316,
							156, 203, 243, 283, 317,
							157, 212, 259, 307, 336,
							152, 203, 246, 286, 321,
							154, 205, 253, 298, 334,
							139, 190, 225, 267, 302,
							146, 191, 229, 272, 302,
							157, 211, 250, 285, 323,
							132, 185, 237, 286, 331,
							160, 207, 257, 303, 345,
							169, 216, 261, 295, 333,
							157, 205, 248, 289, 316,
							137, 180, 219, 258, 291,
							153, 200, 244, 286, 324),
							ncol=5, byrow=T
)

#list(x = c(8.0, 15.0, 22.0, 29.0, 36.0), xbar = 22, N = 30, T = 5,

#### #### #### #### #### #### ####
#### stime
#### #### #### #### #### #### ####

# lista con tutti gli elementi che servono al modello
dataList = list(
	Y   = Y,
	x   = x,
	xbar = mean(x),
	n   = nrow(Y),
	k   = ncol(Y)
)

#### Modello Bayesiano
mod1_string <- 'model
{
	# Models
	for( i in 1 : n )
	{
     for( j in 1 : k )
		 {
        Y[i , j] ~ dnorm(mu[i , j],tau.c)
        mu[i , j] <- alpha[i] + beta[i] * (x[j] - xbar)
     }
     alpha[i] ~ dnorm(alpha.c,alpha.tau)
     beta[i] ~ dnorm(beta.c,beta.tau)
  }

	# Priors
  tau.c ~ dgamma(0.001,0.001)
  sigma <- 1 / sqrt(tau.c)


  alpha.c ~ dnorm(0.0,1.0E-6)
  alpha.tau ~ dgamma(0.001,0.001)
	alpha.sigma2 = 1/alpha.tau

  beta.c ~ dnorm(0.0,1.0E-6)
  beta.tau ~ dgamma(0.001,0.001)
	beta.sigma2 = 1/beta.tau

	#
	for( i in 1 : n )
	{
     for( j in 1 : k )
		 {
        res[i,j] = Y[i , j] -  mu[i , j]
     }
  }

}'


# parametri da salvare
SavePar = c("alpha", "beta", "alpha.c","beta.c","alpha.sigma2","beta.sigma2", "mu","res")

# fittiamo il modello
set.seed(1)
model = jags(
			data 	= dataList,
			parameters.to.save 	= SavePar,
			model.file 					= textConnection(mod1_string), # il model.file dovrebbe essere un file,
			 																									# "textConnection" crea il file e lo carica
			n.chains 						= 1,
			n.iter 							= 5000,
			n.burnin 						= 2000,
			n.thin 							= 1,
			DIC 								= T
)

#### #### #### #### #### #### ####
#### stime
#### #### #### #### #### #### ####

# alpha
Walpha = which(substr(names(model $BUGSoutput$sims.array[1,1,]),1,6)=="alpha[")
##
alphaPOST = model$BUGSoutput$sims.array[,1,Walpha]
# medie
alphaMeans = colMeans(alphaPOST)
plot(alphaMeans)

# beta
Wbeta = which(substr(names(model $BUGSoutput$sims.array[1,1,]),1,5)=="beta[")
##
betaPOST = model$BUGSoutput$sims.array[,1,Wbeta]
# medie
betaMeans = colMeans(betaPOST)
plot(betaMeans)

## possiamo anche vedere i aprametri delle distribuzioni di alpha e beta
par(mfrow=c(2,2))
plot(model $BUGSoutput$sims.array[,1,"alpha.c"], type="l")
plot(model $BUGSoutput$sims.array[,1,"beta.c"], type="l")
plot(model $BUGSoutput$sims.array[,1,"alpha.sigma2"], type="l")
plot(model $BUGSoutput$sims.array[,1,"beta.sigma2"], type="l")

### Vediamo qualche residuo
Wres = which(substr(names(model $BUGSoutput$sims.array[1,1,]),1,3)=="res")
##
resPOST = model$BUGSoutput$sims.array[,1,Wres]
resMeans = colMeans(resPOST)
plot(resMeans)



##### ##### ##### ##### ##### ##### ##### #####
##### Proviamo ad introdurre dipendenza tra i parametri
##### positive correlation would imply that initially heavy rats (high intercept)
##### tend to gain weight more rapidly (steeper slope) than lighter rats.
##### ##### ##### ##### ##### ##### ##### #####


mod2_string <- 'model
{
	# Models
	for( i in 1 : n )
	{
      for( j in 1 : k )
			{
         Y[i, j] ~ dnorm(mu[i , j], tauC)
         mu[i, j] <- beta[i, 1] + beta[i, 2] * (x[j] - xbar)
      }
  }

	# Prior
	for( i in 1 : n )
 	{
      beta[i , 1 : 2] ~ dmnorm(mu.beta[], R[ , ])
  }
	mu.beta[1 : 2] ~ dmnorm(mean[], prec[ , ])
	R[1 : 2 , 1 : 2] ~ dwish(Omega[ , ], 2) # prior per matrici di precisione
	tauC ~ dgamma(0.001, 0.001)

	Sigma = inverse(R)
	# Residui
	for( i in 1 : n )
	{
     for( j in 1 : k )
		 {
        res[i,j] = Y[i , j] -  mu[i , j]
     }
  }

}'

dataList = list(
	Y   = Y,
	x   = x,
	xbar = mean(x),
	n   = nrow(Y),
	k   = ncol(Y),
	Omega = matrix(c(200, 0, 0, 0.2),ncol=2),
	mean = c(0,0),
	prec = matrix(c(1.0E-6, 0, 0, 1.0E-6),ncol=2)
)
# parametri da salvare
SavePar = c("beta","mu.beta","Sigma", "mu","res")

# fittiamo il modello
set.seed(1)
model2 = jags(
			data 	= dataList,
			parameters.to.save 	= SavePar,
			model.file 					= textConnection(mod2_string), # il model.file dovrebbe essere un file,
			 																									# "textConnection" crea il file e lo carica
			n.chains 						= 1,
			n.iter 							= 5000,
			n.burnin 						= 2000,
			n.thin 							= 1,
			DIC 								= T
)

#### #### #### #### #### #### ####
#### stime
#### #### #### #### #### #### ####

# beta
Wbeta = which(substr(names(model2 $BUGSoutput$sims.array[1,1,]),1,5)=="beta[")
##
betaPOST = model2$BUGSoutput$sims.array[,1,Wbeta]
# medie
betaMeans = colMeans(betaPOST)
par(mfrow=c(2,1))
plot(betaMeans[1:30], main="Int")
plot(betaMeans[30+1:30], main="reg")


# media, varianza e correlazione degli effetti beta
Wmu.beta = which(substr(names(model2 $BUGSoutput$sims.array[1,1,]),1,7)=="mu.beta")
mu.betaPOST = model2$BUGSoutput$sims.array[,1,Wmu.beta]

WSigma = which(substr(names(model2 $BUGSoutput$sims.array[1,1,]),1,5)=="Sigma")
SigmaPOST = model2$BUGSoutput$sims.array[,1,WSigma]


par(mfrow=c(3,2))
plot(mu.betaPOST[,1], main="Mean Int.")
plot(mu.betaPOST[,2], main="Mean Coef.Reg.")
plot(SigmaPOST[,1], main="Var Int.")
plot(SigmaPOST[,2], main="Covariance")
plot(SigmaPOST[,4], main="Var Coef.Reg.")
plot(SigmaPOST[,2]/sqrt(SigmaPOST[,1]*SigmaPOST[,4]), main="Correlation")



### Vediamo qualche residuo
Wres = which(substr(names(model2$BUGSoutput$sims.array[1,1,]),1,3)=="res")
##
resPOST = model2$BUGSoutput$sims.array[,1,Wres]
resMeans = colMeans(resPOST)
plot(resMeans)
