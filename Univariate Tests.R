library(KMsurv)
data(kidney)
head(kidney)
library(survival)
logrank=survdiff(Surv(time,delta)~type,data=kidney)
logrank
plot(survfit(Surv(time,delta)~type, data=kidney),main="plot", 
       ylab ="survival",xlab="Survival Time in Month",lty = c(2, 1))
fit <- survfit(Surv(time, delta)~type,data=kidney)
summary(fit,times=c(3))
v1hat=(0.023)^2
v1hat
s1hat=0.977
s2hat=0.8882
v1hat=(0.023)^2
v2hat=(0.0376)^2
z=(s1hat-s2hat)/sqrt(v1hat+v2hat)
z
library(KMsurv)
data("bmt", package="KMsurv")
head(bmt)
library(survMisc)
b1 <- ten(Surv(time=t2, event=d3) ~ group,data=bmt)
comp(b1)
library(KMsurv)
library(survMisc)
data(larynx)
head(larynx)
trend <- ten(Surv(time, delta) ~ stage,data=larynx)
comp(trend)
attr(trend,"tft")
library(survival)
library(KMsurv)
data(drug6mp)
drug6mp
Long <- cbind( rep(drug6mp$pair, 2), c(drug6mp$t1,drug6mp$t2),rep(0:1, each=21), c( rep(1,21), drug6mp$relapse ) )
Long <- as.data.frame(Long)
names(Long) <- c( "pair", "time", "treatment", "relapse" )
head(Long)   
survdiff(Surv(time,relapse)~treatment+strata(pair), data=Long)
library(survMisc)
data(gastric)
head(gastric)
fit=survfit(Surv(time,event)~group,data=gastric)
plot(fit)
g1 <- ten(Surv(time, event) ~ group, data=gastric)
comp(g1)
library(readxl)
mortality_pop <- read_excel("C:/Users/rafati/Desktop/mortality.pop.xlsx")
View(mortality_pop)
standard.mortality<-data.frame(mortality_pop)
standard.mortality[1:6,]
library(readxl)
mydata <-read_excel("C:/Users/rafati/Desktop/mydata.xlsx")
View(mydata)
table1.7<-data.frame(mydata)
table1.7[1:6,]
merge.data.lj<-merge(table1.7,standard.mortality,by = c("sex","age"))
merge.data.lj
table1.7<-data.frame(sex=table1.7$sex,cens=table1.7$cens,interage=table1.7$age, age=table1.7$end.age,time=table1.7$time )
merge.table.tj<-merge(table1.7,standard.mortality,by = c("sex","age"))
merge.table.tj
H0.lj=-log(merge.data.lj$survival)
H0.lj
H0.tj=-log(merge.table.tj$survival)
H0.tj
expected.event=sum(H0.tj-H0.lj)
expected.event
chiq2=(sum(table1.7$cens)-expected.event)^2/expected.event
chiq2
p.value =1-pchisq(chiq2, df =  1)
p.value
