library(cr17)
data(LUAD)
survivalCurves <- fitSurvival(time = LUAD$time,
                              risk = LUAD$event, 
                              group = LUAD$gender,
                              cens = "alive")

SC <- survivalCurves$death[c("time", 
                             "n.risk", 
                             "n.event", 
                             "n.censor", 
                             "surv", 
                             "strata",
                             "std.err",
                             "lower",
                             "upper")]

SC <- as.data.frame(SC)
SC <- filter(SC, strata == "male")

kable(head(SC, n = 10))

plotSurvival(fit = survivalCurves,
             target = 1500,
             ggtheme = theme_gray(),
             legendtitle = "Gender")

testSurvival(time = LUAD$time, 
             risk = LUAD$event, 
             group = LUAD$gender, 
             cens = "alive", 
             rho = 0)

coxModel <- fitCox(time = LUAD$time,
                   risk = LUAD$event,
                   group = LUAD$gender,
                   cens = "alive",
                   conf.int = 0.95)

coxModel$death$coefficients

library(knitr)
library(kableExtra)
options(kableExtra.auto_format = FALSE)
kable(testCox(fitCox = coxModel))

cuminc <- fitCuminc(time = LUAD$time,
                    risk = LUAD$event,
                    group = LUAD$gender,
                    cens = "alive")

femaleDeathCuminc <- cuminc[["female death"]]
femaleDeathCuminc <- as.data.frame(femaleDeathCuminc)
head(femaleDeathCuminc)

plotCuminc(ci = cuminc,
           cens = "alive",
           target = 1500,
           ggtheme = theme_gray(),
           legendtitle = "Gender")

testCuminc(cuminc)
reg <- fitReg(time = LUAD$time,
              risk = LUAD$event,
              group = LUAD$gender,
              cens = "alive")

reg$death$coef
testReg(reg)
riskTab(time = LUAD$time,
        risk = LUAD$event,
        group = LUAD$gender,
        cens = "alive",
        title = "Number at risk")

eventTab(time = LUAD$time,
         risk = LUAD$event,
         group = LUAD$gender,
         cens = "alive",
         title = "Number of events")

summarizeCR(time = LUAD$time, 
            risk = LUAD$event, 
            group = LUAD$gender, cens = "alive")

summarizeCR(time = LUAD$time, 
            risk = LUAD$event, 
            group = LUAD$gender, 
            cens = "alive", 
            rho = 1, 
            target = 800, 
            type = "kaplan-meier", 
            ggtheme = theme_gray(), 
            titleSurv = "Survival analysis", 
            titleCuminc = "Competing risks models", 
            xtitle = "Days", 
            ytitleSurv = "Survival curves", 
            ytitleCuminc = "Cumulative incidence functions", 
            legendtitle = "Gender")
