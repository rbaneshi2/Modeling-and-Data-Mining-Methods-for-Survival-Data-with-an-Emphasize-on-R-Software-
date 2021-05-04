set.seed(1)
lifetimes <- rexp( 25, rate = 0.2)
round(lifetimes,2)
set.seed(1234)
lifetimes <- rweibull( 25, shape=0.48,scale =0.98)
round(lifetimes,2)
set.seed(1)
stimes <- rweibull(10, shape=3,scale =5)
round(stimes,2)
censtimes <- rexp( 10, rate = 0.1)
round(censtimes,2)
time=pmin(stimes,censtimes)
round(time,2)
status=ifelse(stimes<=censtimes,1,0)
status
simulWeib <- function(N, lambda, rho, beta, rateC)
  {
    x <- rbinom(N,1,0.5)
    v <- runif(n=N)
    stime <- (- log(v)/(lambda*exp(x*beta)))^(1/ rho)
    censtime <- rexp(n=N, rate=rateC)
    time <- pmin(stime,censtime)
    status <- as.numeric(stime <= censtime)
    # data set
      data.frame(id=1:N,
                   time=time,
                   status=status,
                   x=x)
     }
library(survival)
set.seed(1234)
betaHat <- rep(NA, 1e3)
for(k in 1:1e3)
  {
    dat=simulWeib(N=100,lambda=0.01,rho=1,beta=-0.6,rateC=0.001)
    fit <- coxph(Surv(time, status) ~ x, data=dat)
    betaHat[k] <- fit$coef
    }
mean(betaHat)
set.seed(1234)
dat=simulWeib(N=100,lambda=0.01,rho=1,beta=-0.6,rateC=0.001)
head(dat)
mean(betaHat)
library(copula)
N=100
set.seed(1234)
myCop<-normalCopula(param=c(0.8,0.2,0.3,0.4,0.6,0.7), 
                      dim = 4, dispstr = "un")
myMvd <- mvdc(copula=myCop, margins=c("binom","norm", "norm", "norm"),
                param Margins=list(list(size = 1 , prob = 0.5),
                                     list(mean=2,sd=0.5),list(mean=40,sd=5),list(mean=10,sd=2)))
Z1=rMvdc(100, mvdc = myMvd)
colnames(Z1) <- c("x1", "x2","x3","x4")
head(Z1)
library(psych)
pairs.panels(Z1)
library(copula)
N=100
simulWeib <- function(N, lambda, rho, beta, rateC)
  {
    myCop <- normalCopula(param=c(0.2,0.3,0.5,0.4,0,0.1),dim = 4, dispstr = "un")
    myMvd <- mvdc(copula=myCop, margins=c("binom", "norm", "norm", "norm"),
                  param Margins=list(list(size = 1 , prob = 0.5),
                      list(mean=2,sd=0.5),list(mean=40,sd=5),list(mean=10,sd=2)))
    Z1=rMvdc(100, mvdc = myMvd)                  
    colnames(Z1) <- c("X1", "X2","X3","X4")
    x=cbind(Z1)
    v <- runif(n=N)
    Tlat <- (- log(v)/(lambda*exp(x%*%beta)))^(1/ rho)
    C <- rexp(n=N, rate=rateC)
    t0=runif(N,0,5)
    time <- (pmin(Tlat, C))
    t=ifelse(round(time)==0,time + t0,time)
    status <- as.numeric(Tlat <= C)
    data.frame(id=1:N,
                             time=t,
                             status=status,
                             x=x)		
     }
set.seed(1)
dat <- simulWeib(N=100, lambda=0.01, rho=0.6, beta=c(0.3,0.2,0.03,0.1), rateC=0.3)
round(head(dat))
library(survival)
set.seed(123)
betaHat1 <- rep(NA,1000)
betaHat2 <- rep(NA,1000)
betaHat3 <- rep(NA,1000)
betaHat4 <- rep(NA,1000)
for(k in 1:1000)
   {
     dat <- simulWeib(N=100, lambda=0.01, rho=0.6, beta=c(0.3,0.2,0.03,0.1), rateC=0.3)
     fit <- coxph(Surv(time, status)~ x.X1 + x.X2 + x.X3 + x.X4 , data=dat)
     betaHat1[k] <- c(fit$coef[1])
     betaHat2[k] <- c(fit$coef[2])
     betaHat3[k] <- c(fit$coef[3])
     betaHat4[k] <- c(fit$coef[4])
     }
mean(betaHat1[1:1000])
mean(betaHat2[1:1000])
mean(betaHat3[1:1000])
mean(betaHat4[1:1000])
