#library
library(survHE)
library(survival)
library(survminer)

###整理文件
##-------------------------------------------------------##-------------------------------------------------------------##
###    tps over 50
##-------------------------------------------------------##-------------------------------------------------------------##

###    OS
##-------------------------------------------------------#    Checkmate 227    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps over 50%/os/checkmate227")
##OS
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]

###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("OS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps over 50%/os")
write.table(intervation1,row.names=FALSE,"IPD_cm227_tpsO50_os1.txt")
write.table(intervation0,row.names=FALSE,"IPD_cm227_tpsO50_os0.txt")

##-------------------------------------------------------#    Impower 132    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps over 50%/os/impower132")
##OS
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]

###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("OS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps over 50%/os")
write.table(intervation1,row.names=FALSE,"IPD_im132_tpsO50_os1.txt")
write.table(intervation0,row.names=FALSE,"IPD_im132_tpsO50_os0.txt")


##-------------------------------------------------------#    Impower 150    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps over 50%/os/impower150")
##OS1
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]

###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("OS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

##OS2
surv_inp <- "surv2.txt"
nrisk_inp <- "table2.txt"
km_out <- "KM2.txt"
ipd_out <- "IPD2.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM2.txt",ipd_output = "IPD2.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files2 <- list("IPD2.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD2.txt")
#实验组个体数据
intervation2 <- make.ipd(ipd_files2,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]

###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("OS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps over 50%/os")
write.table(intervation2,row.names=FALSE,"IPD_im150_tpsO50_os2.txt")
write.table(intervation1,row.names=FALSE,"IPD_im150_tpsO50_os1.txt")
write.table(intervation0,row.names=FALSE,"IPD_im150_tpsO50_os0.txt")

##-------------------------------------------------------#    Keynote 189    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps over 50%/os/kenote189")
##OS
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]

###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("OS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps over 50%/os")
write.table(intervation1,row.names=FALSE,"IPD_kn189_tpsO50_os1.txt")
write.table(intervation0,row.names=FALSE,"IPD_kn189_tpsO50_os0.txt")


##PFS
##-------------------------------------------------------#    impower 132    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps over 50%/pfs/impower132")
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]
###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("PFS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps over 50%/pfs")
write.table(intervation1,row.names=FALSE,"IPD_ip132_tpsO50_pfs1.txt")
write.table(intervation0,row.names=FALSE,"IPD_ip132_tpsO50_pfs0.txt")

##-------------------------------------------------------#    keynote 189    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps over 50%/pfs/kenote189")
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]
###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("PFS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps over 50%/pfs")
write.table(intervation1,row.names=FALSE,"IPD_kn189_tpsO50_pfs1.txt")
write.table(intervation0,row.names=FALSE,"IPD_kn189_tpsO50_pfs0.txt")

##-------------------------------------------------------#    orient 11    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps over 50%/pfs/orient11")
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]
###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("PFS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps over 50%/pfs")
write.table(intervation1,row.names=FALSE,"IPD_o11_tpsO50_pfs1.txt")
write.table(intervation0,row.names=FALSE,"IPD_o11_tpsO50_pfs0.txt")

##-------------------------------------------------------#    rationale 304    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps over 50%/pfs/rationalt304")
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]
###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("PFS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps over 50%/pfs")
write.table(intervation1,row.names=FALSE,"IPD_r304_tpsO50_pfs1.txt")
write.table(intervation0,row.names=FALSE,"IPD_r304_tpsO50_pfs0.txt")

##-------------------------------------------------------##-------------------------------------------------------------##
###    tps smaller than 1
##-------------------------------------------------------##-------------------------------------------------------------##

###    OS
##-------------------------------------------------------#    Checkmate 9LA    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/os/checkmate9la")
##OS
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]

###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("OS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/os")
write.table(intervation1,row.names=FALSE,"IPD_cm9la_tpss1_os1.txt")
write.table(intervation0,row.names=FALSE,"IPD_cm9la_tpss1_os0.txt")

##-------------------------------------------------------#    Impower 132    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/os/impower132")
##OS
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]

###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("OS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/os")
write.table(intervation1,row.names=FALSE,"IPD_im132_tpss1_os1.txt")
write.table(intervation0,row.names=FALSE,"IPD_im132_tpss1_os0.txt")

##-------------------------------------------------------#    Impower 150    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/os/impower150")
##OS1
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]

###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("OS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

##OS2
surv_inp <- "surv2.txt"
nrisk_inp <- "table2.txt"
km_out <- "KM2.txt"
ipd_out <- "IPD2.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM2.txt",ipd_output = "IPD2.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files2 <- list("IPD2.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD2.txt")
#实验组个体数据
intervation2 <- make.ipd(ipd_files2,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]

###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("OS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/os")
write.table(intervation2,row.names=FALSE,"IPD_im150_tpss1_os2.txt")
write.table(intervation1,row.names=FALSE,"IPD_im150_tpss1_os1.txt")
write.table(intervation0,row.names=FALSE,"IPD_im150_tpss1_os0.txt")

##-------------------------------------------------------#    Keynote 189    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/os/kenote189")
##OS
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]

###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("OS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/os")
write.table(intervation1,row.names=FALSE,"IPD_kn189_tpss1_os1.txt")
write.table(intervation0,row.names=FALSE,"IPD_kn189_tpss1_os0.txt")

###    PFS
##-------------------------------------------------------#    Checkmate 9LA    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/pfs/checkmate9la")
##OS
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]

###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("OS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/pfs")
write.table(intervation1,row.names=FALSE,"IPD_cm9la_tpss1_pfs1.txt")
write.table(intervation0,row.names=FALSE,"IPD_cm9la_tpss1_pfs0.txt")

##-------------------------------------------------------#    Impower 132    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/pfs/impower132")
##OS
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]

###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("OS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/pfs")
write.table(intervation1,row.names=FALSE,"IPD_ip132_tpss1_pfs1.txt")
write.table(intervation0,row.names=FALSE,"IPD_ip132_tpss1_pfs0.txt")

##-------------------------------------------------------#    Keynote 189    #-------------------------------------------------------------##
##-------------------------------------------------------#    Keynote 189    #-------------------------------------------------------------##
##-------------------------------------------------------#    Keynote 189    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/pfs/kenote189")
##OS
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]

###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("OS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/pfs")
write.table(intervation1,row.names=FALSE,"IPD_kn189_tpss1_pfs1.txt")
write.table(intervation0,row.names=FALSE,"IPD_kn189_tpss1_pfs0.txt")

##-------------------------------------------------------#    Orient 11    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/pfs/orient11")
##OS
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]

###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("OS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/pfs")
write.table(intervation1,row.names=FALSE,"IPD_o11_tpss1_pfs1.txt")
write.table(intervation0,row.names=FALSE,"IPD_o11_tpss1_pfs0.txt")

##-------------------------------------------------------#    Rationale 304    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/pfs/rationalt304")
##OS
surv_inp <- "surv1.txt"
nrisk_inp <- "table1.txt"
km_out <- "KM1.txt"
ipd_out <- "IPD1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM1.txt",ipd_output = "IPD1.txt")
surv_inp <- "surv0.txt"
nrisk_inp <- "table0.txt"
km_out <- "KM0.txt"
ipd_out <- "IPD0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM0.txt",ipd_output = "IPD0.txt")

###整理数据
ipd_files1 <- list("IPD1.txt")
ipd_files0 <- list("IPD0.txt")
ipd_files <- list("IPD0.txt","IPD1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("table1.txt")
delta_time<-temp_data$month[2]-temp_data$month[1]

###拟合曲线对比
fit <- survfit(Surv(time,event==1)~arm,data=data)
fit
ggsurvplot(fit,data=data,pval = TRUE,risk.table = TRUE,
           palette = c("#D95F02","steelblue"),
           legend.tittle="Intervention",
           legend.labs=c("Control","Intervention"),
           risk.table.height=0.2,
           xlab=c("Time(Months))"),
           break.x.by=delta_time,
           break.y.by=0.2,
           xlim=c(0,max_xlim),
           ylab=c("OS"),
           risk.table.col="strata",
           tables.theme=theme_cleantable(),
           ggtheme=theme_classic())

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/SUBGROUP/tps smaller than 1%/pfs")
write.table(intervation1,row.names=FALSE,"IPD_r304_tpss1_pfs1.txt")
write.table(intervation0,row.names=FALSE,"IPD_r304_tpss1_pfs0.txt")