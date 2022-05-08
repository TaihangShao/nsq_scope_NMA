library(gemtc)
library(rjags)

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/safety")
data<-read.csv("data_gradeall.csv",header = T,sep = ",")
treatments<-read.csv("treatment.csv",sep = ",",header = T)

network<-mtc.network(data)
plot(network)

model<-mtc.model(network,type="consistency",n.chain=3,likelihood="binom",link="logit",linearModel="fixed")
results<-mtc.run(model,n.adapt = 10000,n.iter = 15000,thin = 1)
forest(relative.effect(results,"A"))

gelman.diag(results)

##计算两两比较结果
tb<-relative.effect.table(results)
tb<-round(tb,2)
tb1<-round(exp(tb),2)
tb1

tb2<-round(tb/(1-tb),2)
tb2

##结果导出为excel图，作为两两比较结果
write.csv(tb1,"res_allgrade.csv")




setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/safety")
data<-read.csv("data_grade3.csv",header = T,sep = ",")
treatments<-read.csv("treatment.csv",sep = ",",header = T)

network<-mtc.network(data)
plot(network)

model<-mtc.model(network,type="consistency",n.chain=3,likelihood="binom",link="logit",linearModel="fixed")
results<-mtc.run(model,n.adapt = 10000,n.iter = 15000,thin = 1)
forest(relative.effect(results,"A"))

gelman.diag(results)
##计算两两比较结果
tb<-relative.effect.table(results)
tb1<-round(exp(tb),2)
tb1

##结果导出为excel图，作为两两比较结果
write.csv(tb1,"res_grade3.csv")