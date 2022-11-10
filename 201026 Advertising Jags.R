#### #### #### #### #### #### ####
#### Advertising Bayesiano - Jags
#### #### #### #### #### #### ####

#### Libraries
library(R2jags)

#### Directory
DIR = "/Users/gianlucamastrantonio/Dropbox (Politecnico di Torino Staff)/didattica/Modelli Statistici/Codici R/Advertising - JAGS/"

# carichiamo il dataframe
load(paste(DIR, "Dataset Advertising.Rdata",sep=""))
summary(data)

#### Costruiamo il modello

# Matrice X
X = model.matrix(~ (TV+Radio+Newspaper)^2 ,data= data)

# Vettore y
Y = data$Sales[,drop=F]

# indici vari
n = nrow(data)
p = ncol(X)
nchain = 2 # stimare il modello nchain volte

# lista con tutti gli elementi che servono al modello
dataList = list(
	Y   = Y,
	X   = X,
	n   = n,
	p   = p,
	b_var_prior = 1000
)

#### Modello Bayesiano (JAGS)
# attenzione che il secondo parametro della normale è la precisione (1/varianza)
mod1_string <- 'model {
  for(i in 1:n) {
    Y[i] ~ dnorm(mu[i], tau)
    mu[i] <-  inprod(b[], X[i,])
  }
	sigma2 = 1/tau
  tau ~ dgamma(.01,.01)
  for (j in 1:p) {
    b[j] ~ dnorm(0,1/b_var_prior)
  }
}'

# inizializzazioni
inits =  list(
  list("b" = rnorm(p,0,1), "tau" = rgamma(1,1,1)),
  list("b" = rnorm(p,0,1), "tau" = rgamma(1,1,1))
)

# parametri da salvare
SavePar = c("b", "sigma2")

# fittiamo il modello
fit_lm1 = jags(
			data 								= dataList,
			inits 							= inits,
			parameters.to.save 	= SavePar,
			model.file 					= textConnection(mod1_string), # il model.file dovrebbe essere un file,
			 																									# "textConnection" crea il file e lo carica
			n.chains 						= nchain,
			n.iter 							= 12000,
			n.burnin 						= 2000,
			n.thin 							= 10,
			DIC 								= T # simile all'AIC, serve per il model selection
)
str(fit_lm1)
fit_lm1$BUGSoutput$summary

#### #### #### #### #### #### ####
#### vediamo i risultati
#### #### #### #### #### #### ####

#### beta
# summary
beta_post_chain1 = fit_lm1$BUGSoutput$sims.array[1:1000,1,1:p]
beta_post_chain2 = fit_lm1$BUGSoutput$sims.array[1:1000,2,1:p]
colnames(beta_post_chain1) = colnames(beta_post_chain1) = colnames(X)

# confrontiamo le catene per vedere se il modello è a convergenza
par(mfrow=c(3,2))
for(i in 2:p)
{
	plot(beta_post_chain1[,i], type="l", main= colnames(beta_post_chain1)[i])
	lines(beta_post_chain2[,i],col=2)
}

# stimiamo le medie e gli intervalli di credibilità
beta_mean = round(colMeans(beta_post_chain1),5)
beta_CI1  = round(apply(beta_post_chain1,2,quantile,prob=0.025),5)
beta_CI2  = round(apply(beta_post_chain1,2,quantile,prob=0.975),5)

Beta_Res = cbind(beta_mean,beta_CI1,beta_CI2)
Beta_Res

# confrontiamo i risultati con il modelli regressivo frequentista
lm_freq = lm(Sales ~ (TV+Radio+Newspaper)^2 ,data= data)
summary(lm_freq)
cbind(beta_mean,summary(lm_freq)$coefficients[,1])

#
par(mfrow=c(3,2))
for(i in 2:p)
{
	plot(density(beta_post_chain1[,i]), type="l", main= colnames(beta_post_chain1)[i])
}
str(fit_lm1$BUGSoutput)

#### sigma
sigma2_post_chain1 = fit_lm1$BUGSoutput$sims.array[1:1000,1,"sigma2"]
sigma2_post_chain2 = fit_lm1$BUGSoutput$sims.array[1:1000,2,"sigma2"]

par(mfrow=c(1,2))
plot(sigma2_post_chain1,col=1,type="l")
lines(sigma2_post_chain2,col=2)
plot(density(sigma2_post_chain1),col=1,type="l")


### vediamo come cambia la prior di beta nella posterior
xseq = seq(-100,100,by=0.5)
par(mfrow=c(3,2))
for(i in 2:p)
{
	plot(xseq, dnorm(xseq,0,dataList$b_var_prior^0.5),type="l" ,main= colnames(beta_post_chain1)[i],ylim=c(0,1)) # prior
	lines(density(beta_post_chain1[,i]), type="l", col=2) # posterior
}

#### #### #### #### #### #### #### ####
#### Residui
#### #### #### #### #### #### #### ####

### possiamo costruire le distribuzioni a posteriori dei residui
### calcolando y-Xb, per ogni campione di b
Residui = matrix(NA, ncol=n, nrow=nrow(beta_post_chain1))
colnames(Residui) = paste("e",1:n, sep="")
for(i in 1:nrow(Residui))
{
	Residui[i,] = Y-X%*%beta_post_chain1[i,]
}

### vediamo le distribuzioni a posteriori dei
# residui delle prime 12 osservazioni
par(mfrow=c(4,3))
for(i in 1:12)
{
	plot(density(Residui[,i]), main=colnames(Residui)[i])
}

# possiamo calcolare i residui medi e fare del model checking
ResiduiMedi = colMeans(Residui)
par(mfrow=c(2,2))
plot(data$TV,ResiduiMedi)
plot(data$Radio,ResiduiMedi)
plot(data$Newspaper,ResiduiMedi)


#### #### #### #### #### #### #### ####
#### Distribuzione predittiva
#### #### #### #### #### #### #### ####

# vogliamo vedere la distribuzione di una y
# che ha un set X^* qualsiasi di valori
istar = 10
Xstar = X[istar,]

# abbiamo due modi:
# 1) simuliamo dalla distribuzione di y^*|y
# 2) per un set di valori nel dominio di y (es y^*), calcoliamo
# f(y^*|y)

### Metodo 1
ypred_sim = c()
for(i in 1:nrow(beta_post_chain1))
{
	ypred_sim[i] = rnorm(1,sum(Xstar*beta_post_chain1[i,]),sigma2_post_chain1[i]^0.5 )
}
plot(density(ypred_sim))

### Metodo 2
yseq 				= seq(7,15,by=0.1)
ypred_dens_matrix	= matrix(0, nrow=nrow(beta_post_chain1), ncol=length(yseq))
for(i in 1:nrow(beta_post_chain1))
{
	ypred_dens_matrix[i,] = dnorm(yseq,sum(Xstar*beta_post_chain1[i,]),sigma2_post_chain1[i]^0.5 )
}
ypred_dens = colMeans(ypred_dens_matrix)
plot(yseq,ypred_dens, type="l")

### possiamo confrontare i due risultati
plot(yseq,ypred_dens, type="l")
lines(density(ypred_sim), col=2)

# visto che il set di covariate che abbiamo usato
# appartengono ad una osservazione, possiamo valutare se il modello
# prende bene l'osservazione graficamente
plot(yseq,ypred_dens, type="l")
abline(v = data$Sales[istar], col=2)

# oppure valutando se l'osservazione cade nell'intervallo
# di credibilità
CI = quantile(ypred_sim, prob=c(0.025,0.975))
data$Sales[istar]
(data$Sales[istar]>=CI[1]) & ((data$Sales[istar]<=CI[2]))
