library(survival)
library(KMsurv)
data(hodg)
fit=coxph(Surv(time,delta)~factor(dtype),data=hodg)
fit=coxph(Surv(time,delta)~wtime+factor(dtype)+ factor(gtype)+score,data=hodg)
fit
library(survival)
library(KMsurv)
data(hodg)
library(MASS)
fit<-survfit(Surv(hodg$wtime,hodg$delta)~ hodg$dtype)
plot(fit,lty=2:3,fun="cloglog",col=c("blue","red"), xlab=" log wtime", ylab="cloglog")
fit=coxph(Surv(time,delta)~wtime+factor(dtype)+factor(gtype)+score,data=hodg,
          method='breslow')
resid(fit,type='martingale')
new.hodg=hodg[-c(1,29),]
new.hodg
fit.new=coxph(Surv(time,delta)~wtime+factor(dtype)+factor(gtype)+score,data=new.hodg)
fit.new
fit <- coxph(Surv(time,delta)~wtime+
               factor(dtype)+factor(gtype)+score,
             data=hodg)
fit1 <- cox.zph(fit,transform="identity", global=TRUE)
print(fit1)
fit1$y
plot(fit1,v=1)
plot(fit1,v=3)
library(KMsurv)
library(splines)
library(survival)
library(survMisc)
data(larynx)
c1<- coxph(Surv(time,delta)~factor(stage)+ age,data=larynx)
locScore(c1, hypo=c(0, 0, 0, 1))
library("survival")
library("survminer")
data("lung")
res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)
ggsurvplot(survfit(res.cox), data = lung)
library(survival)
library("SvyNom")
data(noNA)
library("rms")
library("survey")
(cox.fit=coxph(Surv(survival, surv_cens)~ ECOG + liver_only
               + Alb + Hb + Age + Differentiation +Gt_1_m1site +lymph_only,data=noNA))
dstr2 <- svydesign(id = ~1, strata = ~group, prob = ~inv_weight,fpc = ~ssize, data = noNA)
(svy.cox.fit <- svycoxph(Surv(survival, surv_cens) ~ ECOG + liver_only + Alb + Hb + Age + Differentiation +Gt_1_m1site +lymph_only, x = TRUE,design = dstr2))

svycoxph(formula = Surv(survival, surv_cens) ~ ECOG + liver_only + Alb + Hb + Age + Differentiation +Gt_1_m1site +lymph_only, x = TRUE,design = dstr2)




