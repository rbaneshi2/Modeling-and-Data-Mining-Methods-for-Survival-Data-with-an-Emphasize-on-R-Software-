library(KMsurv)
data(bmt)
dataset=data.frame(bmt)
library(survival)
library(rpart)
tree=rpart(Surv(t1, d1)~., data=dataset)
tree

plot(tree, uniform=TRUE)
text(tree, use.n=TRUE)

tree$cptable
prune.tree=prune(tree, cp=.03)
prune.tree

plot(prune.tree, uniform=TRUE)
text(prune.tree, use.n=TRUE)

survival=survfit(Surv(t1, d1)~prune.tree$where, data=dataset)
survival

plot(survival, lty=1:4)
legend("topright", paste('node', c(3, 5, 6, 7)), lty=1:4)