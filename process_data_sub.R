#------------------------------------------------------------------------------------------------#
#                                    Process TPS >=50 OS data                                    #
#------------------------------------------------------------------------------------------------#
rm(list = ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/IPD_data/Subgroup/tps o 50/os")

#Terminal：DIR *.* /B>Filename-list.txt
library("stringr")
library(survHE)
library(survival)
library(survminer)
library("dplyr")
library("tidyr")

filename<- read.delim("Filename-list.txt")
filename$trt<-c("che","niv+ipi","che","ACP","BCP","ABCP","ACP",
                "che","pem+che")
long<-as.integer(length(filename[[1]])) 


IPDfilename<-data.frame(matrix(nrow=long,ncol=1))

for (i in 1:long) {
  temp<-filename[i,1]
  temp<-str_sub(temp,-nchar(temp),-5)
  IPDfilename[i,1]<-temp
  data_temp<-read.delim(filename[i,1])
  data_temp<-separate(data_temp,c("time.event.arm"),c("time","event","arm"),sep=" ",remove=TRUE)
  data_temp$time<-as.numeric(data_temp$time)
  data_temp$event<-as.numeric(data_temp$event)
  data_temp$arm<-as.numeric(data_temp$arm)
  data_temp$patid<-seq(1,length(data_temp$event))
  data_temp$trt<-filename[i,2]
  # assign(IPDfilename[i,1],data_temp)
  write.table(data_temp,filename[i,1],row.names = FALSE)
}

# sb<-separate(sb,c("time.event.arm"),c("time","event","arm"),sep=" ",remove=TRUE)


ipd_files <- list(filename[1,1],filename[2,1])
Checkmate227 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Checkmate227$studyCode<-1
Checkmate227$study<-"Checkmate227"

ipd_files <- list(filename[3,1],filename[4,1])
Impower132 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower132$studyCode<-2
Impower132$study<-"Impower132"

ipd_files <- list(filename[5,1],filename[6,1],filename[7,1])
Impower150 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower150$studyCode<-3
Impower150$study<-"Impower150"

ipd_files <- list(filename[8,1],filename[9,1])
Keynote189 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Keynote189$studyCode<-4
Keynote189$study<-"Keynote189"


combined_IPD<-rbind(Checkmate227,Impower132,Impower150,Keynote189)
combined_IPD$i<-seq(1,length(combined_IPD$event))
combined_IPD <- combined_IPD %>% dplyr::select('i', 'patid' , everything())

#txCode

#che-1
#niv+ipi-2
#ACP-3
#BCP-4
#ABCP-5
#pem+che-6


combined_IPD$txCode[combined_IPD$trt=="che"]<-1
combined_IPD$txCode[combined_IPD$trt=="niv+ipi"]<-2
combined_IPD$txCode[combined_IPD$trt=="ACP"]<-3
combined_IPD$txCode[combined_IPD$trt=="BCP"]<-4
combined_IPD$txCode[combined_IPD$trt=="ABCP"]<-5
combined_IPD$txCode[combined_IPD$trt=="pem+che"]<-6


setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/IPD_data/Data_for_RP")
write.csv(combined_IPD,"RP_tpso50_OS.csv",row.names = FALSE)


#------------------------------------------------------------------------------------------------#
#                                    Process PFS data                                             #
#------------------------------------------------------------------------------------------------#
rm(list = ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/IPD_data/Subgroup/tps o 50/pfs")

#New Terminal：DIR *.* /B>Filename-list.txt
library("stringr")
library(survHE)
library(survival)
library(survminer)
library("dplyr")

filename<- read.delim("Filename-list.txt")
filename$trt<-c("che","ACP",
                "che","pem+che","che","sin+che","che","tis+che")
long<-as.integer(length(filename[[1]])) 


IPDfilename<-data.frame(matrix(nrow=long,ncol=1))

for (i in 1:long) {
  temp<-filename[i,1]
  temp<-str_sub(temp,-nchar(temp),-5)
  IPDfilename[i,1]<-temp
  data_temp<-read.delim(filename[i,1])
  data_temp<-separate(data_temp,c("time.event.arm"),c("time","event","arm"),sep=" ",remove=TRUE)
  data_temp$time<-as.numeric(data_temp$time)
  data_temp$event<-as.numeric(data_temp$event)
  data_temp$arm<-as.numeric(data_temp$arm)
  data_temp$patid<-seq(1,length(data_temp$event))
  data_temp$trt<-filename[i,2]
  # assign(IPDfilename[i,1],data_temp)
  write.table(data_temp,filename[i,1],row.names = FALSE)
}



ipd_files <- list(filename[1,1],filename[2,1])
Impower132 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower132$studyCode<-1
Impower132$study<-"Impower132"


ipd_files <- list(filename[3,1],filename[4,1])
Keynote189 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Keynote189$studyCode<-2
Keynote189$study<-"Keynote189"

ipd_files <- list(filename[5,1],filename[6,1])
Orient11 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Orient11$studyCode<-3
Orient11$study<-"Orient11"

ipd_files <- list(filename[7,1],filename[8,1])
Rationale304 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Rationale304$studyCode<-4
Rationale304$study<-"Rationale304"

combined_IPD<-rbind(Impower132,Keynote189,Orient11,Rationale304)
combined_IPD$i<-seq(1,length(combined_IPD$event))
combined_IPD <- combined_IPD %>% dplyr::select('i', 'patid' , everything())

#txCode

#che-1
#ACP-2
#pem+che-3
#sin+che-4
#tis+che-5

combined_IPD$txCode[combined_IPD$trt=="che"]<-1
combined_IPD$txCode[combined_IPD$trt=="ACP"]<-2
combined_IPD$txCode[combined_IPD$trt=="pem+che"]<-3
combined_IPD$txCode[combined_IPD$trt=="sin+che"]<-4
combined_IPD$txCode[combined_IPD$trt=="tis+che"]<-5


setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/IPD_data/Data_for_RP")
write.csv(combined_IPD,"RP_tpso50_PFS.csv",row.names = FALSE)

#------------------------------------------------------------------------------------------------#
#                                    Process TPS <= 1 OS data                                    #
#------------------------------------------------------------------------------------------------#
rm(list = ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/IPD_data/Subgroup/tps s 1/os")

#Terminal：DIR *.* /B>Filename-list.txt
library("stringr")
library(survHE)
library(survival)
library(survminer)
library("dplyr")
library("tidyr")

filename<- read.delim("Filename-list.txt")
filename$trt<-c("che","niv+ipi+che","che","ACP","BCP","ABCP","ACP",
                "che","pem+che")
long<-as.integer(length(filename[[1]])) 


IPDfilename<-data.frame(matrix(nrow=long,ncol=1))

for (i in 1:long) {
  temp<-filename[i,1]
  temp<-str_sub(temp,-nchar(temp),-5)
  IPDfilename[i,1]<-temp
  data_temp<-read.delim(filename[i,1])
  data_temp<-separate(data_temp,c("time.event.arm"),c("time","event","arm"),sep=" ",remove=TRUE)
  data_temp$time<-as.numeric(data_temp$time)
  data_temp$event<-as.numeric(data_temp$event)
  data_temp$arm<-as.numeric(data_temp$arm)
  data_temp$patid<-seq(1,length(data_temp$event))
  data_temp$trt<-filename[i,2]
  # assign(IPDfilename[i,1],data_temp)
  write.table(data_temp,filename[i,1],row.names = FALSE)
}

# sb<-separate(sb,c("time.event.arm"),c("time","event","arm"),sep=" ",remove=TRUE)

ipd_files <- list(filename[1,1],filename[2,1])
Checkmate9LA <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Checkmate9LA$studyCode<-1
Checkmate9LA$study<-"Checkmate9LA"

ipd_files <- list(filename[3,1],filename[4,1])
Impower132 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower132$studyCode<-2
Impower132$study<-"Impower132"

ipd_files <- list(filename[5,1],filename[6,1],filename[7,1])
Impower150 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower150$studyCode<-3
Impower150$study<-"Impower150"

ipd_files <- list(filename[8,1],filename[9,1])
Keynote189 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Keynote189$studyCode<-6
Keynote189$study<-"Keynote189"

combined_IPD<-rbind(Checkmate9LA,Impower132,Impower150,Keynote189)
combined_IPD$i<-seq(1,length(combined_IPD$event))
combined_IPD <- combined_IPD %>% dplyr::select('i', 'patid' , everything())

#txCode

#che-1
#niv+ipi+che-2
#ACP-3
#BCP-4
#ABCP-5
#pem+che-6


combined_IPD$txCode[combined_IPD$trt=="che"]<-1
combined_IPD$txCode[combined_IPD$trt=="niv+ipi+che"]<-2
combined_IPD$txCode[combined_IPD$trt=="ACP"]<-3
combined_IPD$txCode[combined_IPD$trt=="BCP"]<-4
combined_IPD$txCode[combined_IPD$trt=="ABCP"]<-5
combined_IPD$txCode[combined_IPD$trt=="pem+che"]<-6

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/IPD_data/Data_for_RP")
write.csv(combined_IPD,"RP_tpss1_OS.csv",row.names = FALSE)


#------------------------------------------------------------------------------------------------#
#                                    Process PFS data                                             #
#------------------------------------------------------------------------------------------------#
rm(list = ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/IPD_data/Subgroup/tps s 1/pfs")

#New Terminal：DIR *.* /B>Filename-list.txt
library("stringr")
library(survHE)
library(survival)
library(survminer)
library("dplyr")

filename<- read.delim("Filename-list.txt")
filename$trt<-c("che","niv+ipi+che","che","ACP",
                "che","pem+che","che","sin+che","che","tis+che")
long<-as.integer(length(filename[[1]])) 


IPDfilename<-data.frame(matrix(nrow=long,ncol=1))

for (i in 1:long) {
  temp<-filename[i,1]
  temp<-str_sub(temp,-nchar(temp),-5)
  IPDfilename[i,1]<-temp
  data_temp<-read.delim(filename[i,1])
  data_temp<-separate(data_temp,c("time.event.arm"),c("time","event","arm"),sep=" ",remove=TRUE)
  data_temp$time<-as.numeric(data_temp$time)
  data_temp$event<-as.numeric(data_temp$event)
  data_temp$arm<-as.numeric(data_temp$arm)
  data_temp$patid<-seq(1,length(data_temp$event))
  data_temp$trt<-filename[i,2]
  # assign(IPDfilename[i,1],data_temp)
  write.table(data_temp,filename[i,1],row.names = FALSE)
}

ipd_files <- list(filename[1,1],filename[2,1])
Checkmate9LA <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Checkmate9LA$studyCode<-1
Checkmate9LA$study<-"Checkmate9LA"

ipd_files <- list(filename[3,1],filename[4,1])
Impower132 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower132$studyCode<-2
Impower132$study<-"Impower132"

ipd_files <- list(filename[5,1],filename[6,1])
Keynote189 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Keynote189$studyCode<-3
Keynote189$study<-"Keynote189"

ipd_files <- list(filename[7,1],filename[8,1])
Orient11 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Orient11$studyCode<-4
Orient11$study<-"Orient11"

ipd_files <- list(filename[9,1],filename[10,1])
Rationale304 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Rationale304$studyCode<-5
Rationale304$study<-"Rationale304"

combined_IPD<-rbind(Checkmate9LA,Impower132,Keynote189,Orient11,Rationale304)
combined_IPD$i<-seq(1,length(combined_IPD$event))
combined_IPD <- combined_IPD %>% dplyr::select('i', 'patid' , everything())

#txCode

#che-1
#niv+ipi+che-2
#ACP-3
#pem+che-4
#sin+che-5
#tis+che-6

combined_IPD$txCode[combined_IPD$trt=="che"]<-1
combined_IPD$txCode[combined_IPD$trt=="niv+ipi+che"]<-2
combined_IPD$txCode[combined_IPD$trt=="ACP"]<-3
combined_IPD$txCode[combined_IPD$trt=="pem+che"]<-4
combined_IPD$txCode[combined_IPD$trt=="sin+che"]<-5
combined_IPD$txCode[combined_IPD$trt=="tis+che"]<-6


setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/IPD_data/Data_for_RP")
write.csv(combined_IPD,"RP_tpss1_PFS.csv",row.names = FALSE)

