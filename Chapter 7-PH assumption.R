library(foreign)
leuk <-read.dta("http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/anderson.dta")
leuk[1,]
library(rms)
library(survival)
#-Ln(-Ln)S method for checking PH assumption
surv <- npsurv(formula = Surv(survt, status)~ rx,leuk)
survplot(fit = surv,
           conf = c("none","bands","bars")[1],
            xlab = "logt",label.curves = list (keys = "lines"),
            loglog = TRUE,
           logt = TRUE,)
surv <- npsurv(formula = Surv(survt, status)~ sex,
               leuk)
survplot(fit = surv,
           conf = c("none","bands","bars")[1],
           xlab = "",
           label.curves = list(keys = "lines"),
           loglog=TRUE,
           logt= TRUE, )
# Observed and expected survival plot

surv <- npsurv(formula = Surv(survt, status)~ sex,
               leuk)
survplot(fit = surv,
          conf = c("none","bands","bars")[1],
          xlab = "",
          label.curves = list(keys = "lines"),
          lwd = 2)
cox.fit <- coxph(Surv(survt, status) ~ sex, leuk)
newdata <- data.frame(sex = 0:1)
lines(survfit(cox.fit, newdata = newdata),col ="red", lty = 1:2)
#Goodness-of-fit test
cox.fit <- coxph(Surv(survt, status) ~ rx + logwbc +
                   sex, data = leuk)
cox.zph(cox.fit)
#Extended cox
leuk.cp.format <- survSplit(data =
                              leuk,cut=leuk$survt[leuk$status==1]
                            ,end="survt",event = "status",start = "start")
head(leuk.cp.format,35)
extended.cox <- coxph(Surv(start,survt,status==1) ~ rx +
                        logwbc + sex + rx:survt + logwbc:survt + sex:survt ,data =
                        leuk.cp.format)
summary(extended.cox)
extended.cox <- coxph(Surv(start,survt,status==1) ~ rx +
                        factor(lwbc3) + sex + sex:survt ,data = leuk.cp.format)
summary(extended.cox)
#Extended cox with heaviside function
surv <- npsurv(formula = Surv(survt, status)~ sex ,
               leuk)
survplot(surv)

leuk$id <- as.numeric(rownames(leuk))
leuk.cp.format <- survSplit(data = leuk,
                               cut = c(11),
                               end = "survt",
                               event = "status",
                               start = "start")
leuk.cp.format <- within(leuk.cp.format, {
  SurvObj <- Surv(start, survt, status)
  
    interval <- factor(start, levels = c(0,11), labels =
                           c("First","Second"))
     })
leuk.cp.format <- leuk.cp.format[with(leuk.cp.format,
                                        order(id, survt)),]
head(leuk.cp.format, 15)
res.extended.cox <- coxph(SurvObj ~ rx + factor(lwbc3) + sex +
                            sex:interval ,data = leuk.cp.format)
summary(res.extended.cox)
#stratified cox
strata.cox <- coxph(formula = Surv(survt, status) ~ rx + logwbc
                    +strata(sex),data = leuk)
summary(strata.cox)
#Additive Hazard Models
library(KMsurv)
library(timereg)
data(larynx)
larynx[,-4]
fitAalen <-aalen(Surv(time, delta)
                 ~factor(stage)+age , data=larynx)
par(mfrow=c(2,3))
plot(fitAalen)
fitAalen <-aalen(Surv(time, delta)
                 ~const(factor(stage))+const(age) , data=larynx)
summary(fitAalen)
