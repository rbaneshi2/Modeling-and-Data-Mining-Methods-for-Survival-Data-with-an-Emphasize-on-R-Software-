library(survival)
library(maxstat)
data <- DLBCL
#DLCLid DLBCL identifier
#GEG Gene Expression Group
#time survival time in month
#cens censoring: 0 cencored, 1 dead
#IPI International Prognostic Index
#MGE Mean Gene Expression

fit<- maxstat.test(Surv(time, cens) ~ MGE,
                   data=DLBCL, smethod="LogRank",
                   pmethod="condMC", B = 9999)
fit$estimate
plot(fit)


Boot.sample <- 1000 
estimated.cut <- c()
for ( i in 1 : Boot.sample){
  data.boot <- data[ sample( 1 : nrow ( data ) , replace = T ) , ]
  fit1<- maxstat.test(Surv(data.boot$time, data.boot$cens) ~ data.boot$MGE,
                      data=data.boot, smethod="LogRank",
                      pmethod="condMC", B = 9999)
  estimated.cut [i]<- fit1$estimate
}
hist (estimated.cut)

## Determination of estimated cutpoint which has maximum frequency
which.max(table (estimated.cut))

quantile (estimated.cut,c(.025,0.975))
##########################



library ( survival )
library ( mfp )
data ( GBSG )
f <- mfp(Surv(rfst, cens) ~ htreat+ age + tumsize + fp(posnodal) + prm + esm + menostat + tumgrad, family = cox, data = GBSG , select=0.05 , verbose=TRUE)
summary ( f )

Boot.sample <- 1000
power <- matrix ( nrow = Boot.sample , ncol = 2 )
for ( i in 1 : Boot.sample){
  # Providing the bootstrap sample with replacment
  data.boot <- GBSG [ sample( 1 : nrow ( GBSG ) , replace = T ) , ]
  colnames ( data.boot ) <- dimnames ( GBSG )[[2]]
  fit <- mfp(Surv(rfst, cens) ~ htreat + age + tumsize + fp(posnodal) + prm + esm
             + menostat + tumgrad, family = cox, data=data.boot , select=0.05 , verbose=FALSE)
  power [ i , ] <- fit$powers [ 1 , ] # Powers
}
( PW.fp1 <- table ( power [ , 1 ] ) )
( max1 <- as.numeric ( names ( PW.fp1 [ which.max ( as.vector ( PW.fp1 ) ) ] ) ) )
( PW.fp2 <- table ( power [ , 2 ] ) )
( max2 <- as.numeric ( names ( PW.fp2 [ which.max ( as.vector ( PW.fp2 ) ) ] ) ) )



#############################
y<- c(1,2,3,4,5,6,70,80,90,100)
t<- seq(1:10)
y1<- y+rnorm(length(y),0,1)
cut<- 6
ind<- ifelse(t>cut, 1, 0)
f<- lm(y1~t+t:ind)
f$coef
logL<- logLik(f)

logL<-c()
for (i in 1:length(t)){
  cut<- t[i]
  ind<- ifelse(t>cut,1,0)
  f<- lm(y1~t+t:ind)
  logL[i]<- logLik(f)
}
results<- cbind(t,logL)
change.point<- t[which.max(results[,2])]
results
change.point
