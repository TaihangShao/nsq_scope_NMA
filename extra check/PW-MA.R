library("RStata")
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/extra check")
chooseStataBin()
options("RStata.StataPath"="\"D:\\Stata\\StataSE-64\"")
options("RStata.StataVersion"=15)

data<-read.csv("RP_PFS.csv")
stata("C:/Users/ECHO/Desktop/nsq-cea-scope/extra check/pairwise_pf.do",data.out = TRUE)

data<-read.csv("RP_OS.csv")
stata("C:/Users/ECHO/Desktop/nsq-cea-scope/extra check/pairwise_os.do",data.out = TRUE)

