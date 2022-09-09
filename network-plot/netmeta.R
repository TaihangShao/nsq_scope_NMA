library(netmeta)
HR<-read.csv("HR.csv",head=TRUE)
HR$logHR<-log(HR$hr)
HR$selogHR<-(log(HR$upper)-log(HR$lower))/(2*1.96)

# netmeta<-netmeta(logHR,selogHR,treat1,treat2,data=HR,comb.random=TRUE,sm="HR",studlab=Study,reference.group="bev+che")
# netgraph(netmeta)
# 
# forest(netmeta)

netmeta<-netmeta(logHR,selogHR,treat1,treat2,data=HR,comb.random=TRUE,sm="HR",studlab=Study,reference.group="ate+bev+che")
forest(netmeta)