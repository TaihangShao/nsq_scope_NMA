#library
library(survHE)
library(survival)
library(survminer)

###整理文件

##-------------------------------------------------------#    Camel    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/camel")
##OS
surv_inp <- "I_os_surv.txt"
nrisk_inp <- "I_os_table.txt"
km_out <- "KM_camel_os1.txt"
ipd_out <- "IPD_camel_os1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_camel_os1.txt",ipd_output = "IPD_camel_os1.txt")
surv_inp <- "C_os_surv.txt"
nrisk_inp <- "C_os_table.txt"
km_out <- "KM_camel_os0.txt"
ipd_out <- "IPD_camel_os0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_camel_os0.txt",ipd_output = "IPD_camel_os0.txt")

###整理数据
ipd_files1 <- list("IPD_camel_os1.txt")
ipd_files0 <- list("IPD_camel_os0.txt")
ipd_files <- list("IPD_camel_os0.txt","IPD_camel_os1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("I_os_table.txt")
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

##PFS
surv_inp <- "I_pfs_surv.txt"
nrisk_inp <- "I_pfs_table.txt"
km_out <- "KM_camel_pfs1.txt"
ipd_out <- "IPD_camel_pfs1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_camel_pfs1.txt",ipd_output = "IPD_camel_pfs1.txt")
surv_inp <- "C_pfs_surv.txt"
nrisk_inp <- "C_pfs_table.txt"
km_out <- "KM_camel_pfs0.txt"
ipd_out <- "IPD_camel_pfs0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_camel_pfs0.txt",ipd_output = "IPD_camel_pfs0.txt")

###整理数据
ipd_files1 <- list("IPD_camel_pfs1.txt")
ipd_files0 <- list("IPD_camel_pfs0.txt")
ipd_files <- list("IPD_camel_pfs0.txt","IPD_camel_pfs1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
max_xlim<-as.integer(max(data$time))+1

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

##-------------------------------------------------------#    Checkmate 9LA    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/checkmate9la")
##OS
surv_inp <- "I_os_surv.txt"
nrisk_inp <- "I_os_table.txt"
km_out <- "KM_cm9la_os1.txt"
ipd_out <- "IPD_cm9la_os1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_cm9la_os1.txt",ipd_output = "IPD_cm9la_os1.txt")
surv_inp <- "C_os_surv.txt"
nrisk_inp <- "C_os_table.txt"
km_out <- "KM_cm9la_os0.txt"
ipd_out <- "IPD_cm9la_os0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_cm9la_os0.txt",ipd_output = "IPD_cm9la_os0.txt")

###整理数据
ipd_files1 <- list("IPD_cm9la_os1.txt")
ipd_files0 <- list("IPD_cm9la_os0.txt")
ipd_files <- list("IPD_cm9la_os0.txt","IPD_cm9la_os1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("I_os_table.txt")
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

##PFS
surv_inp <- "I_pfs_surv.txt"
nrisk_inp <- "I_pfs_table.txt"
km_out <- "KM_cm9la_pfs1.txt"
ipd_out <- "IPD_cm9la_pfs1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_cm9la_pfs1.txt",ipd_output = "IPD_cm9la_pfs1.txt")
surv_inp <- "C_pfs_surv.txt"
nrisk_inp <- "C_pfs_table.txt"
km_out <- "KM_cm9la_pfs0.txt"
ipd_out <- "IPD_cm9la_pfs0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_cm9la_pfs0.txt",ipd_output = "IPD_cm9la_pfs0.txt")

###整理数据
ipd_files1 <- list("IPD_cm9la_pfs1.txt")
ipd_files0 <- list("IPD_cm9la_pfs0.txt")
ipd_files <- list("IPD_cm9la_pfs0.txt","IPD_cm9la_pfs1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
max_xlim<-as.integer(max(data$time))+1

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


##-------------------------------------------------------#    Checkmate 227    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/checkmate227")
##OS
surv_inp <- "I_os_surv.txt"
nrisk_inp <- "I_os_table.txt"
km_out <- "KM_cm227_os1.txt"
ipd_out <- "IPD_cm227_os1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_cm227_os1.txt",ipd_output = "IPD_cm227_os1.txt")
surv_inp <- "C_os_surv.txt"
nrisk_inp <- "C_os_table.txt"
km_out <- "KM_cm227_os0.txt"
ipd_out <- "IPD_cm227_os0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_cm227_os0.txt",ipd_output = "IPD_cm227_os0.txt")

###整理数据
ipd_files1 <- list("IPD_cm227_os1.txt")
ipd_files0 <- list("IPD_cm227_os0.txt")
ipd_files <- list("IPD_cm227_os0.txt","IPD_cm227_os1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("I_os_table.txt")
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


##-------------------------------------------------------#    Geomstone 302    #-------------------------------------------------------------##

##-------------------------------------------------------#    Impower 132    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/impower132")
##OS
surv_inp <- "I_os_surv.txt"
nrisk_inp <- "I_os_table.txt"
km_out <- "KM_ip132_os1.txt"
ipd_out <- "IPD_ip132_os1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ip132_os1.txt",ipd_output = "IPD_ip132_os1.txt")
surv_inp <- "C_os_surv.txt"
nrisk_inp <- "C_os_table.txt"
km_out <- "KM_ip132_os0.txt"
ipd_out <- "IPD_ip132_os0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ip132_os0.txt",ipd_output = "IPD_ip132_os0.txt")

###整理数据
ipd_files1 <- list("IPD_ip132_os1.txt")
ipd_files0 <- list("IPD_ip132_os0.txt")
ipd_files <- list("IPD_ip132_os0.txt","IPD_ip132_os1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("I_os_table.txt")
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

##PFS
surv_inp <- "I_pfs_surv.txt"
nrisk_inp <- "I_pfs_table.txt"
km_out <- "KM_ip132_pfs1.txt"
ipd_out <- "IPD_ip132_pfs1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ip132_pfs1.txt",ipd_output = "IPD_ip132_pfs1.txt")
surv_inp <- "C_pfs_surv.txt"
nrisk_inp <- "C_pfs_table.txt"
km_out <- "KM_ip132_pfs0.txt"
ipd_out <- "IPD_ip132_pfs0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ip132_pfs0.txt",ipd_output = "IPD_ip132_pfs0.txt")

###整理数据
ipd_files1 <- list("IPD_ip132_pfs1.txt")
ipd_files0 <- list("IPD_ip132_pfs0.txt")
ipd_files <- list("IPD_ip132_pfs0.txt","IPD_ip132_pfs1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
max_xlim<-as.integer(max(data$time))+1

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

##-------------------------------------------------------#    Impower 150    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/impower150")
##OS1
surv_inp <- "I_os_surv.txt"
nrisk_inp <- "I_os_table.txt"
km_out <- "KM_ip150_os1.txt"
ipd_out <- "IPD_ip150_os1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ip150_os1.txt",ipd_output = "IPD_ip150_os1.txt")
surv_inp <- "C_os_surv.txt"
nrisk_inp <- "C_os_table.txt"
km_out <- "KM_ip150_os0.txt"
ipd_out <- "IPD_ip150_os0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ip150_os0.txt",ipd_output = "IPD_ip150_os0.txt")

###整理数据
ipd_files1 <- list("IPD_ip150_os1.txt")
ipd_files0 <- list("IPD_ip150_os0.txt")
ipd_files <- list("IPD_ip150_os0.txt","IPD_ip150_os1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("I_os_table.txt")
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
surv_inp <- "I_os_surv2.txt"
nrisk_inp <- "I_os_table2.txt"
km_out <- "KM_ip150_os2.txt"
ipd_out <- "IPD_ip150_os2.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ip150_os2.txt",ipd_output = "IPD_ip150_os2.txt")
surv_inp <- "C_os_surv.txt"
nrisk_inp <- "C_os_table.txt"
km_out <- "KM_ip150_os0.txt"
ipd_out <- "IPD_ip150_os0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ip150_os0.txt",ipd_output = "IPD_ip150_os0.txt")

###整理数据
ipd_files1 <- list("IPD_ip150_os2.txt")
ipd_files0 <- list("IPD_ip150_os0.txt")
ipd_files <- list("IPD_ip150_os0.txt","IPD_ip150_os2.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("I_os_table.txt")
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
##PFS1
surv_inp <- "I_pfs_surv.txt"
nrisk_inp <- "I_pfs_table.txt"
km_out <- "KM_ip150_pfs1.txt"
ipd_out <- "IPD_ip150_pfs1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ip150_pfs1.txt",ipd_output = "IPD_ip150_pfs1.txt")
surv_inp <- "C_pfs_surv.txt"
nrisk_inp <- "C_pfs_table.txt"
km_out <- "KM_ip150_pfs0.txt"
ipd_out <- "IPD_ip150_pfs0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ip150_pfs0.txt",ipd_output = "IPD_ip150_pfs0.txt")

###整理数据
ipd_files1 <- list("IPD_ip150_pfs1.txt")
ipd_files0 <- list("IPD_ip150_pfs0.txt")
ipd_files <- list("IPD_ip150_pfs0.txt","IPD_ip150_pfs1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))

temp_data<-read.delim("I_pfs_table.txt")
max_xlim<-as.integer(max(data$time))+1
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
##PFS2
surv_inp <- "I_pfs_surv2.txt"
nrisk_inp <- "I_pfs_table2.txt"
km_out <- "KM_ip150_pfs2.txt"
ipd_out <- "IPD_ip150_pfs2.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ip150_pfs2.txt",ipd_output = "IPD_ip150_pfs2.txt")
surv_inp <- "C_pfs_surv.txt"
nrisk_inp <- "C_pfs_table.txt"
km_out <- "KM_ip150_pfs0.txt"
ipd_out <- "IPD_ip150_pfs0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ip150_pfs0.txt",ipd_output = "IPD_ip150_pfs0.txt")

###整理数据
ipd_files1 <- list("IPD_ip150_pfs2.txt")
ipd_files0 <- list("IPD_ip150_pfs0.txt")
ipd_files <- list("IPD_ip150_pfs0.txt","IPD_ip150_pfs2.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
max_xlim<-as.integer(max(data$time))+1

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

##-------------------------------------------------------#    Keynote 189    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/kenote189")
##OS
surv_inp <- "I_os_surv.txt"
nrisk_inp <- "I_os_table.txt"
km_out <- "KM_kn189_os1.txt"
ipd_out <- "IPD_kn189_os1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_kn189_os1.txt",ipd_output = "IPD_kn189_os1.txt")
surv_inp <- "C_os_surv.txt"
nrisk_inp <- "C_os_table.txt"
km_out <- "KM_kn189_os0.txt"
ipd_out <- "IPD_kn189_os0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_kn189_os0.txt",ipd_output = "IPD_kn189_os0.txt")

###整理数据
ipd_files1 <- list("IPD_kn189_os1.txt")
ipd_files0 <- list("IPD_kn189_os0.txt")
ipd_files <- list("IPD_kn189_os0.txt","IPD_kn189_os1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("I_os_table.txt")
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

##PFS
surv_inp <- "I_pfs_surv.txt"
nrisk_inp <- "I_pfs_table.txt"
km_out <- "KM_kn189_pfs1.txt"
ipd_out <- "IPD_kn189_pfs1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_kn189_pfs1.txt",ipd_output = "IPD_kn189_pfs1.txt")
surv_inp <- "C_pfs_surv.txt"
nrisk_inp <- "C_pfs_table.txt"
km_out <- "KM_kn189_pfs0.txt"
ipd_out <- "IPD_kn189_pfs0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_kn189_pfs0.txt",ipd_output = "IPD_kn189_pfs0.txt")

###整理数据
ipd_files1 <- list("IPD_kn189_pfs1.txt")
ipd_files0 <- list("IPD_kn189_pfs0.txt")
ipd_files <- list("IPD_kn189_pfs0.txt","IPD_kn189_pfs1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("I_pfs_table.txt")
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

##-------------------------------------------------------#    ONO    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/ono453852")
##OS
surv_inp <- "I_os_surv.txt"
nrisk_inp <- "I_os_table.txt"
km_out <- "KM_ono_os1.txt"
ipd_out <- "IPD_ono_os1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ono_os1.txt",ipd_output = "IPD_ono_os1.txt")
surv_inp <- "C_os_surv.txt"
nrisk_inp <- "C_os_table.txt"
km_out <- "KM_ono_os0.txt"
ipd_out <- "IPD_ono_os0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ono_os0.txt",ipd_output = "IPD_ono_os0.txt")

###整理数据
ipd_files1 <- list("IPD_ono_os1.txt")
ipd_files0 <- list("IPD_ono_os0.txt")
ipd_files <- list("IPD_ono_os0.txt","IPD_ono_os1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("I_os_table.txt")
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

##PFS
surv_inp <- "I_pfs_surv.txt"
nrisk_inp <- "I_pfs_table.txt"
km_out <- "KM_ono_pfs1.txt"
ipd_out <- "IPD_ono_pfs1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ono_pfs1.txt",ipd_output = "IPD_ono_pfs1.txt")
surv_inp <- "C_pfs_surv.txt"
nrisk_inp <- "C_pfs_table.txt"
km_out <- "KM_ono_pfs0.txt"
ipd_out <- "IPD_ono_pfs0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ono_pfs0.txt",ipd_output = "IPD_ono_pfs0.txt")

###整理数据
ipd_files1 <- list("IPD_ono_pfs1.txt")
ipd_files0 <- list("IPD_ono_pfs0.txt")
ipd_files <- list("IPD_ono_pfs0.txt","IPD_ono_pfs1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("I_pfs_table.txt")
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

##-------------------------------------------------------#    Orient 11    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/orient11")
##OS
surv_inp <- "I_os_surv.txt"
nrisk_inp <- "I_os_table.txt"
km_out <- "KM_o11_os1.txt"
ipd_out <- "IPD_o11_os1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_o11_os1.txt",ipd_output = "IPD_o11_os1.txt")
surv_inp <- "C_os_surv.txt"
nrisk_inp <- "C_os_table.txt"
km_out <- "KM_o11_os0.txt"
ipd_out <- "IPD_o11_os0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_o11_os0.txt",ipd_output = "IPD_o11_os0.txt")

###整理数据
ipd_files1 <- list("IPD_o11_os1.txt")
ipd_files0 <- list("IPD_o11_os0.txt")
ipd_files <- list("IPD_o11_os0.txt","IPD_o11_os1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("I_os_table.txt")
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

##PFS
surv_inp <- "I_pfs_surv.txt"
nrisk_inp <- "I_pfs_table.txt"
km_out <- "KM_o11_pfs1.txt"
ipd_out <- "IPD_o11_pfs1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_o11_pfs1.txt",ipd_output = "IPD_o11_pfs1.txt")
surv_inp <- "C_pfs_surv.txt"
nrisk_inp <- "C_pfs_table.txt"
km_out <- "KM_o11_pfs0.txt"
ipd_out <- "IPD_o11_pfs0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_o11_pfs0.txt",ipd_output = "IPD_o11_pfs0.txt")

###整理数据
ipd_files1 <- list("IPD_o11_pfs1.txt")
ipd_files0 <- list("IPD_o11_pfs0.txt")
ipd_files <- list("IPD_o11_pfs0.txt","IPD_o11_pfs1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("I_pfs_table.txt")
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

##-------------------------------------------------------#    Rationale 304    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/rationalt304")

##PFS
surv_inp <- "I_pfs_surv.txt"
nrisk_inp <- "I_pfs_table.txt"
km_out <- "KM_r304_pfs1.txt"
ipd_out <- "IPD_r304_pfs1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_r304_pfs1.txt",ipd_output = "IPD_r304_pfs1.txt")
surv_inp <- "C_pfs_surv.txt"
nrisk_inp <- "C_pfs_table.txt"
km_out <- "KM_r304_pfs0.txt"
ipd_out <- "IPD_r304_pfs0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_r304_pfs0.txt",ipd_output = "IPD_r304_pfs0.txt")

###整理数据
ipd_files1 <- list("IPD_r304_pfs1.txt")
ipd_files0 <- list("IPD_r304_pfs0.txt")
ipd_files <- list("IPD_r304_pfs0.txt","IPD_r304_pfs1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("I_pfs_table.txt")
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

##-------------------------------------------------------#    Impower 130    #-------------------------------------------------------------##
rm(list=ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/KM_data/impower130")
##OS
surv_inp <- "I_os_surv.txt"
nrisk_inp <- "I_os_table.txt"
km_out <- "KM_ip130_os1.txt"
ipd_out <- "IPD_ip130_os1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ip130_os1.txt",ipd_output = "IPD_ip130_os1.txt")
surv_inp <- "C_os_surv.txt"
nrisk_inp <- "C_os_table.txt"
km_out <- "KM_ip130_os0.txt"
ipd_out <- "IPD_ip130_os0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ip130_os0.txt",ipd_output = "IPD_ip130_os0.txt")

###整理数据
ipd_files1 <- list("IPD_ip130_os1.txt")
ipd_files0 <- list("IPD_ip130_os0.txt")
ipd_files <- list("IPD_ip130_os0.txt","IPD_ip130_os1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
#fig
max_xlim<-as.integer(max(data$time))+1
temp_data<-read.delim("I_os_table.txt")
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

##PFS
surv_inp <- "I_pfs_surv.txt"
nrisk_inp <- "I_pfs_table.txt"
km_out <- "KM_ip130_pfs1.txt"
ipd_out <- "IPD_ip130_pfs1.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ip130_pfs1.txt",ipd_output = "IPD_ip130_pfs1.txt")
surv_inp <- "C_pfs_surv.txt"
nrisk_inp <- "C_pfs_table.txt"
km_out <- "KM_ip130_pfs0.txt"
ipd_out <- "IPD_ip130_pfs0.txt"
digitise(surv_inp,nrisk_inp,km_output = "KM_ip130_pfs0.txt",ipd_output = "IPD_ip130_pfs0.txt")

###整理数据
ipd_files1 <- list("IPD_ip130_pfs1.txt")
ipd_files0 <- list("IPD_ip130_pfs0.txt")
ipd_files <- list("IPD_ip130_pfs0.txt","IPD_ip130_pfs1.txt")
#实验组个体数据
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#对照组个体数据
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#合并后两个个体数据
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
max_xlim<-as.integer(max(data$time))+1

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
