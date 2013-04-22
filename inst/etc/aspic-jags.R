library(R2OpenBUGS)
library(ggplot2)
library(coda)

setwd("/home/laurie/Desktop/gcode/gbyp-sam/papers/SCRS/2013/SCRS-2013-diags/data")
getwd()

prj=function(b0,r,k,catch,year){
   stock=rep(b0*k,length(year)+1)
   
   for (y in year)
       stock[y+1]=stock[y]-catch[y]+r*stock[y]*(1-stock[y]/k)
   
   stock}

year =1:40
catch=rep(100,max(year))
stock=prj(b0=1,r=.5,k=1000,catch,year)
index=stock[year]*rlnorm(length(year),0,.1)
#ggplot(dat)+geom_point(aes(year,index))

### stock known #######################################################################
dat=list(yrs=length(year),stock=stock[-length(stock)],index=index)

aspicMdl<-function(){
  ## model
  for (j in 1:yrs){
    index[ j]  ~  dnorm(mu[j],tau)  ## Response values Y are Normally distributed
    mu[j]      <- beta*(stock[j])   ## linear model
    }
  
  ## Priors
  beta  ~dnorm( 0,    0.001) ## mean and precision (1/var)
  tau   ~dgamma(0.001,0.001)
  
  ## derived
  sigma <-1/sqrt(tau)
}
write.model(aspicMdl,"model.txt")

## Initial values for MCMC. 
inits<-function(){list(beta=1,tau=1)}

## Run the model using OpenBUGS
lineout<-bugs(data       =dat,
              inits      =inits,
              parameters =c("beta","sigma"),
              model.file ="model.txt",
              n.chains   =2,
              n.iter     =1000,
              codaPkg    =T)

## Produce a CODA object from the lineout output
line.coda<-read.bugs(lineout)
plot(line.coda)

### catch and pars known ####################################################################
dat=list(yrs=length(year),stock=stock,catch=catch) #,b0=1,r=.5,k=1000)

aspicMdl<-function(){
  ## model
  for (j in 1:yrs){
    index[ j]  ~  dnorm(mu[j],tau)  ## Response values Y are Normally distributed
    mu[j]      <- beta*(stock[j])   ## linear model
    
    }
  
  ## Priors
  beta  ~dnorm( 0,    0.001) ## mean and precision (1/var)
  tau   ~dgamma(0.001,0.001)
  
  ## derived
  sigma <-1/sqrt(tau)
  }
write.model(aspicMdl,"model.txt")

## Initial values for MCMC. 
inits<-function(){list(beta=1,tau=1)}

## Run the model using OpenBUGS
lineout<-bugs(data       =dat,
              inits      =inits,
              parameters =c("beta","sigma"),
              model.file ="model.txt",
              n.chains   =2,
              n.iter     =1000,
              codaPkg    =T)

## Produce a CODA object from the lineout output
line.coda<-read.bugs(lineout)
plot(line.coda)

