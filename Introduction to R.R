Chapter 1

########### Restructure to start-stop data
library(readxl)
> data     
ID Time1 Time2 Status1 Status2
1     1     2     5      1      1
2     2     4    10      1      0
3     4    10    15      0      1
4     5     6     9      1      0

start<- c()
stop<- c()
status<- c()
for (i in 1:nrow(data)){
  start<- c(start,0)
  stop<- c(stop, min(Time1[i],Time2[i]))
  status<- c(status,max(Event1[i],Event2[i]))  
  start<- c(start,min(Time1[i],Time2[i]))
  stop<- c(stop,max(Time1[i],Time2[i]))
  status<- c(status,max(Event1[i],Event2[i]))
}

long.data<- cbind(id=rep(ID,each=2),start,stop,status,interval=rep(seq(1:2),nrow(data)))
long.data
#################################################

> data
ID Time1 Time2 Status1 Status2
1     1     2     3       1       1
2     2     8     4       0       1
3     3     6     7       1       0
4     4    10    15       1       1
5     5    10     6       0       1 

start<- c()
stop<- c()
event<- c()
status<- c()
for (i in 1:nrow(data)){
  start<- c(start,0)
  stop<- c(stop, min(Time1[i],Time2[i]))
  status<- c(status,c(Status1[i],Status2[i])[which.min(c(Time1[i],Time2[i]))])
  event<- c(event,which.min(c(Time1[i],Time2[i])))
  start<- c(start,min(Time1[i],Time2[i]))
  stop<- c(stop,max(Time1[i],Time2[i]))
  status<- c(status,c(Status1[i],Status2[i])[which.max(c(Time1[i],Time2[i]))])
  event<- c(event,which.max(c(Time1[i],Time2[i])))
}

long.data<- cbind(id=rep(ID,each=2),start,stop,event,status,interval=rep(seq(1:2),nrow(data)))

long.data

###########matching
library(optmatch)
library(survival)

lung.data <- cbind (id=seq(1,nrow(lung)),lung , status.new=ifelse(lung$status==2,0,1))
head (lung.data)
print(pairmatch(status.new ~ age, data = lung.data), grouped = TRUE)

print(pairmatch(status.new ~ age,control=2, data = lung.data), grouped = TRUE)

print(pairmatch(status.new ~ age+sex, data = lung.data), grouped = TRUE)

print(pairmatch(status.new ~ age+sex, control=2, data = lung.data), grouped = TRUE)

pairdata=pairmatch(status.new ~ age, data = lung.data)
my.data <- data.frame(lung.data, matches = pairdata, check.rows=TRUE)
attach(my.data)
my.data <- my.data[order(matches),]
head (my.data)
match.data=subset(my.data,is.na(matches)==FALSE)

###########Imputation

> data
Id  Time   BMI
1     1     1  19.7
2     1     2  20.2
3     1     3  20.4
4     1     4  NA  
5     1     5  21.1
6     2     1  19.1
7     2     2  19.8
8     2     3  22.3
9     2     4  NA  
10     2     5  23.4
11     3     1  NA  
12     3     2  20.8
13     3     3  20.9
14     3     4  NA  
15     3     5  22.8

imp.BMI<- c()
for( i in 1:3){
  d1<- subset(data,Id==i)
  f<- lm(BMI~Time,data=d1)
  pred.f<- f$coefficients[1]+f$coefficients[2]*d1$Time
  imp.BMI<- c(imp.BMI,ifelse (is.na(d1$BMI)==TRUE,pred.f,d1$BMI))
}
data.imp<- cbind(data,imp.BMI=imp.BMI)
#####################
##### select row corresponded to maximum bmi for each id
> data
Id  Time   BMI Gender   Age
1     1     1  22        1    32
2     1     2  21.9      1    33
3     1     3  20.7      1    34
4     1     4  21.1      1    35
5     1     5  21.2      1    36
6     2     1  22.7      0    23
7     2     2  24.3      0    24
8     2     3  23.1      0    25
9     2     4  23.4      0    26
10     2     5  19.8      0    27
11     3     1  21.6      0    25
12     3     2  20.8      0    26
13     3     3  22.1      0    27
14     3     4  22.8      0    28
15     3     5  20.9      0    29

mydata=c()
for (i in 1:3){
  ith=subset(data,Id==i)
  if(any(is.na(ith$BMI)==FALSE)){
    mydata=rbind(mydata,subset(ith,Time==which.max(ith$BMI)))
  }
}

mydata




