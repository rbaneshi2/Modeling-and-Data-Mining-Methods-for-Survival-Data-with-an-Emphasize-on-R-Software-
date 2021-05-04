addicts<-data.frame(addicts)
#exponential model
modpar1=survreg(Surv(addicts$time,addicts$status)~prison+dose +clinic
  ,data=addicts,dist="exponential")
summary(modpar1)

#Weibull AFT model
modpar2=survreg(Surv(addicts$time,addicts$status)
      ~ prison+dose+clinic,data=addicts,dist="weibull")
summary(modpar2)
#estimate the median

pattern1=data.frame(prison=1,dose=50,clinic=1)
pct=c(.25,.50,.75)
days=predict(modpar2,newdata=pattern1,type="quantile",p=pct)
cbind(pct,days)
pct2=0:100/100
days2=predict(modpar2,newdata=pattern1,
              type="quantile",p=pct2)
survival=1-pct2
plot(days2,survival,xlab="survival time in days",ylab= "survival
     probabilities",main="Weibull survival estimates for prison=0,
     dose=40,clinic=1",xlim=c(0,800))   


#log-logistic AFT model
modpar3=survreg(Surv(addicts$time,addicts$status)~
                prison+dose+clinic,data=addicts,dist="loglogistic")
summary(modpar3)





