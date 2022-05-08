#------------------------------------------------------------------------------------------------#
#                                    Process OS data                                             #
#------------------------------------------------------------------------------------------------#
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/IPD_data/g_os")

#Terminal：DIR *.* /B>Filename-list.txt
library("stringr")
library(survHE)
library(survival)
library(survminer)
library("dplyr")

filename<- read.delim("Filename-list.txt")
filename$trt<-c("che","niv+ipi","che","niv+ipi+che","che","ate+che","che","ate+che","bev+che","ate+bev+che","ate+che",
                "che","pem+che")
long<-as.integer(length(filename[[1]])) 


IPDfilename<-data.frame(matrix(nrow=long,ncol=1))

for (i in 1:long) {
  temp<-filename[i,1]
  temp<-str_sub(temp,-nchar(temp),-5)
  IPDfilename[i,1]<-temp
  data_temp<-read.delim(filename[i,1])
  data_temp$patid<-seq(1,length(data_temp$event))
  data_temp$trt<-filename[i,2]
  # assign(IPDfilename[i,1],data_temp)
  write.table(data_temp,filename[i,1],row.names = FALSE)
}



ipd_files <- list(filename[1,1],filename[2,1])
Checkmate227 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Checkmate227$studyCode<-1
Checkmate227$study<-"Checkmate227"

ipd_files <- list(filename[3,1],filename[4,1])
Checkmate9LA <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Checkmate9LA$studyCode<-2
Checkmate9LA$study<-"Checkmate9LA"

ipd_files <- list(filename[5,1],filename[6,1])
Impower130 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower130$studyCode<-3
Impower130$study<-"Impower130"


ipd_files <- list(filename[7,1],filename[8,1])
Impower132 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower132$studyCode<-4
Impower132$study<-"Impower132"

ipd_files <- list(filename[9,1],filename[10,1],filename[11,1])
Impower150 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower150$studyCode<-5
Impower150$study<-"Impower150"

ipd_files <- list(filename[12,1],filename[13,1])
Keynote189 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Keynote189$studyCode<-6
Keynote189$study<-"Keynote189"



combined_IPD<-rbind(Checkmate227,Checkmate9LA,Impower130,Impower132,Impower150,Keynote189)
combined_IPD$i<-seq(1,length(combined_IPD$event))
combined_IPD <- combined_IPD %>% dplyr::select('i', 'patid' , everything())



#txCode

#che-1
#cam+che-2
#niv+ipi-3
#niv+ipi+che-4
#A+che-5
#B+che-6
#AB+che-7
#pem+che-8
#sin+che-9
#BCP+niv-10

combined_IPD$txCode[combined_IPD$trt=="che"]<-1

combined_IPD$txCode[combined_IPD$trt=="niv+ipi"]<-2
combined_IPD$txCode[combined_IPD$trt=="niv+ipi+che"]<-3
combined_IPD$txCode[combined_IPD$trt=="ate+che"]<-4
combined_IPD$txCode[combined_IPD$trt=="bev+che"]<-5
combined_IPD$txCode[combined_IPD$trt=="ate+bev+che"]<-6
combined_IPD$txCode[combined_IPD$trt=="pem+che"]<-7


setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/IPD_data/Data_for_RP")
write.csv(combined_IPD,"RP_OS_globalsub.csv",row.names = FALSE)


#------------------------------------------------------------------------------------------------#
#                                    Process PFS data                                             #
#------------------------------------------------------------------------------------------------#
rm(list = ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/IPD_data/g_pfs")

#New Terminal：DIR *.* /B>Filename-list.txt
library("stringr")
library(survHE)
library(survival)
library(survminer)
library("dplyr")

filename<- read.delim("Filename-list.txt")
filename$trt<-c("che","niv+ipi+che","che","ate+che","che","ate+che","bev+che","ate+bev+che","ate+che",
                "che","pem+che")
long<-as.integer(length(filename[[1]])) 


IPDfilename<-data.frame(matrix(nrow=long,ncol=1))

for (i in 1:long) {
  temp<-filename[i,1]
  temp<-str_sub(temp,-nchar(temp),-5)
  IPDfilename[i,1]<-temp
  data_temp<-read.delim(filename[i,1])
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
Impower130 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower130$studyCode<-2
Impower130$study<-"Impower130"

ipd_files <- list(filename[5,1],filename[6,1])
Impower132 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower132$studyCode<-3
Impower132$study<-"Impower132"

ipd_files <- list(filename[7,1],filename[8,1],filename[9,1])
Impower150 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower150$studyCode<-4
Impower150$study<-"Impower150"

ipd_files <- list(filename[10,1],filename[11,1])
Keynote189 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Keynote189$studyCode<-5
Keynote189$study<-"Keynote189"


combined_IPD<-rbind(Checkmate9LA,Impower130,Impower132,Impower150,Keynote189)
combined_IPD$i<-seq(1,length(combined_IPD$event))
combined_IPD <- combined_IPD %>% dplyr::select('i', 'patid' , everything())

#txCode

#che-1
#cam+che-2
#niv+ipi+che-3
#sug+che-4
#ACP-5
#BCP-6
#ABCP-7
#pem+che-8
#sin+che-9
#BCP+niv-10
#tis+che-11

combined_IPD$txCode[combined_IPD$trt=="che"]<-1
combined_IPD$txCode[combined_IPD$trt=="niv+ipi+che"]<-2
combined_IPD$txCode[combined_IPD$trt=="ate+che"]<-3
combined_IPD$txCode[combined_IPD$trt=="bev+che"]<-4
combined_IPD$txCode[combined_IPD$trt=="ate+bev+che"]<-5
combined_IPD$txCode[combined_IPD$trt=="pem+che"]<-6


setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/IPD_data/Data_for_RP")
write.csv(combined_IPD,"RP_PFS_globalsub.csv",row.names = FALSE)

