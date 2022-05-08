#------------------------------------------------------------------------------------------------#
#                                    Process OS data                                             #
#------------------------------------------------------------------------------------------------#
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/IPD_data/OS")

#Terminal：DIR *.* /B>Filename-list.txt
library("stringr")
library(survHE)
library(survival)
library(survminer)
library("dplyr")

filename<- read.delim("Filename-list.txt")
filename$trt<-c("che","cam+che","che","niv+ipi","che","niv+ipi+che","che","ate+che","che","ate+che","bev+che","ate+bev+che","ate+che",
                "che","pem+che","che","sin+che","bev+che","niv+bev+che")
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
Camel <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Camel$studyCode<-1
Camel$study<-"CameL"

ipd_files <- list(filename[3,1],filename[4,1])
Checkmate227 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Checkmate227$studyCode<-2
Checkmate227$study<-"Checkmate227"

ipd_files <- list(filename[5,1],filename[6,1])
Checkmate9LA <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Checkmate9LA$studyCode<-3
Checkmate9LA$study<-"Checkmate9LA"

ipd_files <- list(filename[7,1],filename[8,1])
Impower130 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower130$studyCode<-4
Impower130$study<-"Impower130"


ipd_files <- list(filename[9,1],filename[10,1])
Impower132 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower132$studyCode<-5
Impower132$study<-"Impower132"

ipd_files <- list(filename[11,1],filename[12,1],filename[13,1])
Impower150 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower150$studyCode<-6
Impower150$study<-"Impower150"

ipd_files <- list(filename[14,1],filename[15,1])
Keynote189 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Keynote189$studyCode<-7
Keynote189$study<-"Keynote189"

ipd_files <- list(filename[16,1],filename[17,1])
Orient11 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Orient11$studyCode<-8
Orient11$study<-"Orient11"

ipd_files <- list(filename[18,1],filename[19,1])
TASUKI52 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
TASUKI52$studyCode<-9
TASUKI52$study<-"TASUKI52"


combined_IPD<-rbind(Camel,Checkmate227,Checkmate9LA,Impower130,Impower132,Impower150,Keynote189,Orient11,TASUKI52)
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
combined_IPD$txCode[combined_IPD$trt=="cam+che"]<-2
combined_IPD$txCode[combined_IPD$trt=="niv+ipi"]<-3
combined_IPD$txCode[combined_IPD$trt=="niv+ipi+che"]<-4
combined_IPD$txCode[combined_IPD$trt=="ate+che"]<-5
combined_IPD$txCode[combined_IPD$trt=="bev+che"]<-6
combined_IPD$txCode[combined_IPD$trt=="ate+bev+che"]<-7
combined_IPD$txCode[combined_IPD$trt=="pem+che"]<-8
combined_IPD$txCode[combined_IPD$trt=="sin+che"]<-9
combined_IPD$txCode[combined_IPD$trt=="niv+bev+che"]<-10

setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/IPD_data/Data_for_RP")
write.csv(combined_IPD,"RP_OS.csv",row.names = FALSE)


#------------------------------------------------------------------------------------------------#
#                                    Process PFS data                                             #
#------------------------------------------------------------------------------------------------#
rm(list = ls())
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/IPD_data/PFS")

#New Terminal：DIR *.* /B>Filename-list.txt
library("stringr")
library(survHE)
library(survival)
library(survminer)
library("dplyr")

filename<- read.delim("Filename-list.txt")
filename$trt<-c("che","cam+che","che","niv+ipi+che","che","sug+che","che","ate+che","che","ate+che","bev+che","ate+bev+che","ate+che",
                "che","pem+che","che","sin+che","bev+che","niv+bev+che","che","tis+che")
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
Camel <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Camel$studyCode<-1
Camel$study<-"CameL"

ipd_files <- list(filename[3,1],filename[4,1])
Checkmate9LA <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Checkmate9LA$studyCode<-2
Checkmate9LA$study<-"Checkmate9LA"

ipd_files <- list(filename[5,1],filename[6,1])
Gemstone302 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Gemstone302$studyCode<-3
Gemstone302$study<-"Gemstone302"

ipd_files <- list(filename[7,1],filename[8,1])
Impower130 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower130$studyCode<-4
Impower130$study<-"Impower130"

ipd_files <- list(filename[9,1],filename[10,1])
Impower132 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower132$studyCode<-5
Impower132$study<-"Impower132"

ipd_files <- list(filename[11,1],filename[12,1],filename[13,1])
Impower150 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Impower150$studyCode<-6
Impower150$study<-"Impower150"

ipd_files <- list(filename[14,1],filename[15,1])
Keynote189 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Keynote189$studyCode<-7
Keynote189$study<-"Keynote189"

ipd_files <- list(filename[16,1],filename[17,1])
Orient11 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Orient11$studyCode<-8
Orient11$study<-"Orient11"

ipd_files <- list(filename[18,1],filename[19,1])
TASUKI52 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
TASUKI52$studyCode<-9
TASUKI52$study<-"TASUKI52"

ipd_files <- list(filename[20,1],filename[21,1])
Rationale304 <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm","patid","trt"))
Rationale304$studyCode<-10
Rationale304$study<-"Rationale304"

combined_IPD<-rbind(Camel,Checkmate9LA,Gemstone302,Impower130,Impower132,Impower150,Keynote189,Orient11,TASUKI52,Rationale304)
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
combined_IPD$txCode[combined_IPD$trt=="cam+che"]<-2
combined_IPD$txCode[combined_IPD$trt=="niv+ipi+che"]<-3
combined_IPD$txCode[combined_IPD$trt=="sug+che"]<-4
combined_IPD$txCode[combined_IPD$trt=="ate+che"]<-5
combined_IPD$txCode[combined_IPD$trt=="bev+che"]<-6
combined_IPD$txCode[combined_IPD$trt=="ate+bev+che"]<-7
combined_IPD$txCode[combined_IPD$trt=="pem+che"]<-8
combined_IPD$txCode[combined_IPD$trt=="sin+che"]<-9
combined_IPD$txCode[combined_IPD$trt=="niv+bev+che"]<-10
combined_IPD$txCode[combined_IPD$trt=="tis+che"]<-11


setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/IPD_data/Data_for_RP")
write.csv(combined_IPD,"RP_PFS.csv",row.names = FALSE)

