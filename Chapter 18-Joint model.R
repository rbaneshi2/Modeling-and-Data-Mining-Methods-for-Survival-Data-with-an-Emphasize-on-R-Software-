
library("JM")
library("lattice")
library(survival)
data(aids)
head(aids,5)
#cox model
td.Cox <- coxph(Surv(start, stop, event) ~ drug + sqrt(CD4),
                data = aids)
summary(td.Cox)
#Two Stage Regression
fitLME <- lme(sqrt(CD4) ~ obstime + obstime:drug,random
              = ~ obstime | patient, data = aids)
fitval=fitted.values(fitLME)
td.Cox3<- coxph(Surv(start, stop, event) ~ drug + fitval,
                  data = aids)
summary(td.Cox3)
#joint model
fitLME <- lme(sqrt(CD4) ~ obstime + obstime:drug,random
              = ~ obstime | patient, data = aids)
fitSURV <- coxph(Surv(Time, death) ~ drug, data =
                     aids.id, x = TRUE)
fit.JM <- jointModel(fitLME, fitSURV, timeVar =
                         "obstime",method = "piecewise-PH-GH")
summary(fit.JM)
fit.JM1 <- jointModel(fitLME, fitSURV, timeVar =
                        "obstime",method = "weibull-PH-GH")
summary(fit.JM1)