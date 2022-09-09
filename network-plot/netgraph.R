# Create a network plot

# Load library
library(netmeta)

# Start with empty environment
rm(list=ls())

# Set the workign directory
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/network-plot")



par(mfrow=c(3,3))


# OS
data <- read.csv("data_os.csv")
data$lhr<-log(data$hr)
data$se<-(log(data$upper)-log(data$lower))/(2*1.96)
data<-data[1:11,]

# Create a netmeta object
a <- netmeta(data$lhr, data$se, treat1=data$t1, treat2=data$t2, studlab=data$Study, reference=1)
# Treatment labels
lab <- c("che", "cam+che", "niv+ipi", "niv+ipi+che", "ate+che", "bev+che", "ate+bev+che", "pem+che", "sin+che", "niv+bev+che")
netgraph(a, labels=lab, offset=0.02, plastic=F, col="skyblue", multiarm=T, col.multiarm="pink", points=T,
               col.points="purple", number.of.studies = T, cex=2,
         cex.number.of.studies =1.5,col.number.of.studies = "black",bg.number.of.studies = "white",
               cex.points=2*c(7,1,1,1,4,3,2,1,1,1))
title("OS",adj=0.5,cex.main=2.5)

# PFS
data <- read.csv("data_pfs.csv")
data$lhr<-log(data$hr)
data$se<-(log(data$upper)-log(data$lower))/(2*1.96)
data<-data[1:12,]


# Create a netmeta object
a <- netmeta(data$lhr, data$se, treat1=data$t1, treat2=data$t2, studlab=data$Study, reference=1)
# Treatment labels
lab <- c("che", "cam+che", "niv+ipi+che", "sug+che","ate+che", "bev+che", "ate+bev+che", "pem+che", "sin+che", "niv+bev+che","tis+che")
netgraph(a, labels=lab, offset=0.02, plastic=F, col="skyblue", multiarm=T, col.multiarm="pink", points=T,
         col.points="purple", number.of.studies = T, cex=2,
         cex.number.of.studies =1.5,col.number.of.studies = "black",bg.number.of.studies = "white",
         
         cex.points=2*c(8,1,1,1,4,3,2,1,1,1,1))
title("PFS",adj=0.5,cex.main=2.5)


# Safety
data <- read.csv("data_safety.csv")
data$lhr<-log(data$hr)
data$se<-(log(data$upper)-log(data$lower))/(2*1.96)


# Create a netmeta object
a <- netmeta(data$lhr, data$se, treat1=data$t1, treat2=data$t2, studlab=data$Study, reference=1,tol.multiarm = 4)
# Treatment labels
lab <- c("che", "cam+che", "ate+che", "bev+che", "ate+bev+che", "pem+che", "sin+che", "niv+bev+che","tis+che")
netgraph(a, labels=lab, offset=0.02, plastic=F, col="skyblue", multiarm=T, col.multiarm="pink", points=T,
         col.points="purple", number.of.studies = T, cex=2,
         cex.number.of.studies =1.5,col.number.of.studies = "black",bg.number.of.studies = "white",
         
         cex.points=2*c(6,1,4,3,2,1,1,1,1))
title("Safety",adj=0.5,cex.main=2.5)


# OS tps o50
data <- read.csv("data_ostpso50.csv")
data$lhr<-log(data$hr)
data$se<-(log(data$upper)-log(data$lower))/(2*1.96)
data<-data[1:6,]


# Create a netmeta object
a <- netmeta(data$lhr, data$se, treat1=data$t1, treat2=data$t2, studlab=data$Study, reference=1)
# Treatment labels
lab <- c("che","niv+ipi", "ate+che", "bev+che", "ate+bev+che", "pem+che")
netgraph(a, labels=lab, offset=0.02, plastic=F, col="skyblue", multiarm=T, col.multiarm="pink", points=T,
         col.points="purple", number.of.studies = T, cex=2,
         cex.number.of.studies =1.5,col.number.of.studies = "black",bg.number.of.studies = "white",
         
         cex.points=2*c(3,1,3,2,2,1))
title("OS with PD-L1 expression ≥ 50%",adj=0.5,cex.main=2.5)


# pfs tps o50
data <- read.csv("data_pfstpso50.csv")
data$lhr<-log(data$hr)
data$se<-(log(data$upper)-log(data$lower))/(2*1.96)
data<-data[1:4,]

# Create a netmeta object
a <- netmeta(data$lhr, data$se, treat1=data$t1, treat2=data$t2, studlab=data$Study, reference=1)
# Treatment labels
lab <- c("che", "ate+che", "pem+che", "sin+che", "tis+che")
netgraph(a, labels=lab, offset=0.02, plastic=F, col="skyblue", multiarm=T, col.multiarm="pink", points=T,
         col.points="purple", number.of.studies = T, cex=2,
         cex.number.of.studies =1.5,col.number.of.studies = "black",bg.number.of.studies = "white",
         
         cex.points=2*c(4,1,1,1,1))
title("PFS with PD-L1 expression ≥ 50%",adj=0.5,cex.main=2.5)


# OS tpss1
data <- read.csv("data_ostpss1.csv")
data$lhr<-log(data$hr)
data$se<-(log(data$upper)-log(data$lower))/(2*1.96)
data<-data[1:6,]


# Create a netmeta object
a <- netmeta(data$lhr, data$se, treat1=data$t1, treat2=data$t2, studlab=data$Study, reference=1,tol.multiarm = 5)
# Treatment labels
lab <- c("che", "niv+ipi+che", "ate+che", "bev+che", "ate+bev+che", "pem+che")
netgraph(a, labels=lab, offset=0.02, plastic=F, col="skyblue", multiarm=T, col.multiarm="pink", points=T,
         col.points="purple", number.of.studies = T, cex=2,
         cex.number.of.studies =1.5,col.number.of.studies = "black",bg.number.of.studies = "white",
         
         cex.points=2*c(3,1,3,2,2,1))
title("OS with PD-L1 expression < 1%",adj=0.5,cex.main=2.5)


# pfs tpss1
data <- read.csv("data_pfstpss1.csv")
data$lhr<-log(data$hr)
data$se<-(log(data$upper)-log(data$lower))/(2*1.96)
data<-data[1:5,]

# Create a netmeta object
a <- netmeta(data$lhr, data$se, treat1=data$t1, treat2=data$t2, studlab=data$Study, reference=1)
# Treatment labels
lab <- c("che", "niv+ipi+che", "ate+che", "pem+che", "sin+che", "tis+che")
netgraph(a, labels=lab, offset=0.02, plastic=F, col="skyblue", multiarm=T, col.multiarm="pink", points=T,
         col.points="purple", number.of.studies = T, cex=2,
         cex.number.of.studies =1.5,col.number.of.studies = "black",bg.number.of.studies = "white",
         
         cex.points=2*c(5,1,1,1,1,1))
title("PFS with PD-L1 expression < 1%",adj=0.5,cex.main=2.5)


# OS nccn
data <- read.csv("data_osgs.csv")
data$lhr<-log(data$hr)
data$se<-(log(data$upper)-log(data$lower))/(2*1.96)
data<-data[1:8,]

# Create a netmeta object
a <- netmeta(data$lhr, data$se, treat1=data$t1, treat2=data$t2, studlab=data$Study, reference=1)
# Treatment labels
lab <- c("che", "niv+ipi", "niv+ipi+che", "ate+che", "bev+che", "ate+bev+che", "pem+che")
netgraph(a, labels=lab, offset=0.02, plastic=F, col="skyblue", multiarm=T, col.multiarm="pink", points=T,
         col.points="purple", number.of.studies = T, cex=2,
         cex.number.of.studies =1.5,col.number.of.studies = "black",bg.number.of.studies = "white",
         
         cex.points=2*c(5,1,1,4,3,2,1))
title("OS with treatment recommended by NCCN",adj=0.5,cex.main=2.5)


# pfs nccn
data <- read.csv("data_pfsgs.csv")
data$lhr<-log(data$hr)
data$se<-(log(data$upper)-log(data$lower))/(2*1.96)
data<-data[1:7,]

# Create a netmeta object
a <- netmeta(data$lhr, data$se, treat1=data$t1, treat2=data$t2, studlab=data$Study, reference=1)
# Treatment labels
lab <- c("che", "niv+ipi+che", "ate+che", "bev+che", "ate+bev+che", "pem+che")
netgraph(a, labels=lab, offset=0.02, plastic=F, col="skyblue", multiarm=T, col.multiarm="pink", points=T,
         col.points="purple", number.of.studies = T, cex=2,
         cex.number.of.studies =1.5,col.number.of.studies = "black",bg.number.of.studies = "white",
         
         cex.points=2*c(4,1,4,2,2,1))
title("PFS with treatment recommended by NCCN",adj=0.5,cex.main=2.5)


