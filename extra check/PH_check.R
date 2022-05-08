# Test for non-PH

# Load libraries
library(survival)
library(broom)
library(metafor)
library(survminer)


#pfs
# Start with an empty environment
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/extra check")
data <- read.csv("RP_PFS.csv")
# Set the working directory
path <- "C:/Users/ECHO/Desktop/nsq-cea-scope/plot/she-plot/pfs/"
setwd(path)
# Import the data and split into data frames for each trial
name<-c("CameL.png","cm9la.png","gs302.png","ip130.png","ip132.png","ip150.png","kn189.png","o11.png","t52.png","r304.png")

for (i in 1:10) {
  df<-data[data$studyCode==i,]
  cox <- coxph(formula = Surv(time, event) ~ arm, data=df)
  test<-cox.zph(cox)
  png(name[i],width = 600,height=400)
  fig<-ggcoxzph(test)
  print(fig)
  dev.off()
}

#os
# Start with an empty environment
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/extra check")
data <- read.csv("RP_OS.csv")
# Set the working directory
path <- "C:/Users/ECHO/Desktop/nsq-cea-scope/plot/she-plot/os/"
setwd(path)
# Import the data and split into data frames for each trial
name<-c("CameL.png","cm227.png","cm9la.png","ip130.png","ip132.png","ip150.png","kn189.png","o11.png","t52.png")

for (i in 1:9) {
  df<-data[data$studyCode==i,]
  cox <- coxph(formula = Surv(time, event) ~ arm, data=df)
  test<-cox.zph(cox)
  png(name[i],width = 600,height=400)
  fig<-ggcoxzph(test)
  print(fig)
  dev.off()
}


#pfs tps o50
# Start with an empty environment
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/extra check")
data <- read.csv("RP_tpso50_PFS.csv")
# Set the working directory
path <- "C:/Users/ECHO/Desktop/nsq-cea-scope/plot/she-plot/pfs-tpso50/"
setwd(path)
# Import the data and split into data frames for each trial
name<-c("ip132.png","kn189.png","o11.png","r304.png")

for (i in 1:4) {
  df<-data[data$studyCode==i,]
  cox <- coxph(formula = Surv(time, event) ~ arm, data=df)
  test<-cox.zph(cox)
  png(name[i],width = 600,height=400)
  fig<-ggcoxzph(test)
  print(fig)
  dev.off()
}

#pfs tps s1
# Start with an empty environment
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/extra check")
data <- read.csv("RP_tpss1_PFS.csv")
# Set the working directory
path <- "C:/Users/ECHO/Desktop/nsq-cea-scope/plot/she-plot/pfs-tpss1/"
setwd(path)
# Import the data and split into data frames for each trial
name<-c("cm9la.png","ip132.png","kn189.png","o11.png","r304.png")

for (i in 1:5) {
  df<-data[data$studyCode==i,]
  cox <- coxph(formula = Surv(time, event) ~ arm, data=df)
  test<-cox.zph(cox)
  png(name[i],width = 600,height=400)
  fig<-ggcoxzph(test)
  print(fig)
  dev.off()
}


#os tps o50
# Start with an empty environment
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/extra check")
data <- read.csv("RP_tpso50_OS.csv")
# Set the working directory
path <- "C:/Users/ECHO/Desktop/nsq-cea-scope/plot/she-plot/os-tpso50/"
setwd(path)
# Import the data and split into data frames for each trial
name<-c("cm227.png","ip132.png","ip150.png","kn189.png")

for (i in 1:4) {
  df<-data[data$studyCode==i,]
  cox <- coxph(formula = Surv(time, event) ~ arm, data=df)
  test<-cox.zph(cox)
  png(name[i],width = 600,height=400)
  fig<-ggcoxzph(test)
  print(fig)
  dev.off()
}


#os tps s1
# Start with an empty environment
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/extra check")
data <- read.csv("RP_tpss1_OS.csv")
# Set the working directory
path <- "C:/Users/ECHO/Desktop/nsq-cea-scope/plot/she-plot/os-tpss1/"
setwd(path)
# Import the data and split into data frames for each trial
name<-c("cm9la.png","ip132.png","ip150.png","kn189.png")

for (i in 1:4) {
  df<-data[data$studyCode==i,]
  cox <- coxph(formula = Surv(time, event) ~ arm, data=df)
  test<-cox.zph(cox)
  png(name[i],width = 600,height=400)
  fig<-ggcoxzph(test)
  print(fig)
  dev.off()
}


