# Restricted mean survival time at 18 months with confidence intervals

# Load libraries
library(survRM2) 
library(foreach)
# Load library
library(R2WinBUGS)


# Start with empty environment
rm(list=ls())

# Set the working directory & load data
path<-"C:/Users/ECHO/Desktop/nsq-cea-scope/RMST/pfs"
setwd("C:/Users/ECHO/Desktop/nsq-cea-scope/RMST/pfs")
data <- read.csv("RP_PFS.csv")

# Create an empty data frame to store results
rmst_18 <- data.frame(rmst0=NA, rmst0_se=NA, rmst0_lci=NA, rmst0_uci=NA,
                      rmst1=NA, rmst1_se=NA, rmst1_lci=NA, rmst1_uci=NA,
                      rmstD=NA, rmstD_lci=NA, rmstD_uci=NA)

# Two-arm studies with 1 as the reference category
id1 <- c(1,2,3,4,5,7,8,10)

# Loop over studies
for(i in id1) {
  temp1 <- data[data$studyCode==i,]
  
  # Re-code treatments as 0 and 1
  temp1$newTx[temp1$txCode==1] <- 0
  temp1$newTx[temp1$txCode!=1] <- 1
  
  # Calculate rmst
  a <- rmst2(time=temp1$time, status=temp1$event, arm=temp1$newTx, tau=12)
  print(a)
  
  adf <- data.frame(rmst0=a$RMST.arm0$result[1,1], rmst0_se=a$RMST.arm0$result[1,2], 
                    rmst0_lci=a$RMST.arm0$result[1,3], rmst0_uci=a$RMST.arm0$result[1,4],
                    rmst1=a$RMST.arm1$result[1,1], rmst1_se=a$RMST.arm1$result[1,2],
                    rmst1_lci=a$RMST.arm1$result[1,3], rmst1_uci=a$RMST.arm1$result[1,4],
                    rmstD=a$unadjusted.result[1,1], rmstD_lci=a$unadjusted.result[1,2],
                    rmstD_uci=a$unadjusted.result[1,3])
  
  rmst_18 <- rbind(rmst_18, adf)
}
rmst_18 <- rmst_18[-1,]


# Two arm trials with 6 as the reference category
id2 <- 9

for(i in id2) {
  temp1 <- data[data$studyCode==i,]
  
  # Re-code treatments as 0 and 1
  temp1$newTx[temp1$txCode==6] <- 0
  temp1$newTx[temp1$txCode!=6] <- 1
  
  # Calculate rmst
  a <- rmst2(time=temp1$time, status=temp1$event, arm=temp1$newTx, tau=12)
  print(a)
  
  adf <- data.frame(rmst0=a$RMST.arm0$result[1,1], rmst0_se=a$RMST.arm0$result[1,2], 
                    rmst0_lci=a$RMST.arm0$result[1,3], rmst0_uci=a$RMST.arm0$result[1,4],
                    rmst1=a$RMST.arm1$result[1,1], rmst1_se=a$RMST.arm1$result[1,2],
                    rmst1_lci=a$RMST.arm1$result[1,3], rmst1_uci=a$RMST.arm1$result[1,4],
                    rmstD=a$unadjusted.result[1,1], rmstD_lci=a$unadjusted.result[1,2],
                    rmstD_uci=a$unadjusted.result[1,3])
  
  rmst_18 <- rbind(rmst_18, adf)
}


# Three arm trial with 6 as the reference category
id3 <- 6

for(i in id3) {
  for(j in c(5,7)) {
    temp1 <- data[data$studyCode==i,]
    temp1 <- temp1[temp1$txCode==6 | temp1$txCode==j,]
    
    # Re-code treatments as 0 and 1
    temp1$newTx[temp1$txCode==6] <- 0
    temp1$newTx[temp1$txCode!=6] <- 1
    
    # Calculate rmst
    a <- rmst2(time=temp1$time, status=temp1$event, arm=temp1$newTx, tau=12)
    print(a)
    
    adf <- data.frame(rmst0=a$RMST.arm0$result[1,1], rmst0_se=a$RMST.arm0$result[1,2], 
                      rmst0_lci=a$RMST.arm0$result[1,3], rmst0_uci=a$RMST.arm0$result[1,4],
                      rmst1=a$RMST.arm1$result[1,1], rmst1_se=a$RMST.arm1$result[1,2],
                      rmst1_lci=a$RMST.arm1$result[1,3], rmst1_uci=a$RMST.arm1$result[1,4],
                      rmstD=a$unadjusted.result[1,1], rmstD_lci=a$unadjusted.result[1,2],
                      rmstD_uci=a$unadjusted.result[1,3])
    
    rmst_18 <- rbind(rmst_18, adf)
  }
}

# Look at 5 v 7 in three arm trial

temp1 <- data[data$studyCode==6,]

# Re-code treatments as 0 and 1
temp1$newTx[temp1$txCode==5] <- 0
temp1$newTx[temp1$txCode!=5] <- 1

# Calculate rmst
a <- rmst2(time=temp1$time, status=temp1$event, arm=temp1$newTx, tau=12)
print(a)

adf <- data.frame(rmst0=a$RMST.arm0$result[1,1], rmst0_se=a$RMST.arm0$result[1,2], 
                  rmst0_lci=a$RMST.arm0$result[1,3], rmst0_uci=a$RMST.arm0$result[1,4],
                  rmst1=a$RMST.arm1$result[1,1], rmst1_se=a$RMST.arm1$result[1,2],
                  rmst1_lci=a$RMST.arm1$result[1,3], rmst1_uci=a$RMST.arm1$result[1,4],
                  rmstD=a$unadjusted.result[1,1], rmstD_lci=a$unadjusted.result[1,2],
                  rmstD_uci=a$unadjusted.result[1,3])

rmst_18 <- rbind(rmst_18, adf)



# Create data frame with study details
studies <- data.frame(studyCode=c(1,2,3,4,5,7,8,10,9,6,6,6),
                      study=c("CameL", "CheckMate9LA","Gemstone302", "Impower130", "Impower132",
                              "Keynote189", "Orient11","Rationale304","Tasuki52", "Impower150","Impower150","Impower150"),
                      t1=c(1,1,1,1,1,1,1,1,6,6,6,5),
                      t2=c(2,3,4,5,5,8,9,11,10,5,7,7))



rmst_18 <- cbind(studies, rmst_18)

rmst_18

# Store results as a csv file

write.csv(rmst_18, "rmst_12_pfs.csv", row.names=F)


# Fit NMA of RMST at 18 months in WinBUGS 

###############################################################



# Start with empty environment
rm(list=ls())

# Set working directory


# load RMST at 18 months data
rmst_18 <- read.csv("rmst_12_pfs.csv")
rmst_18<-rmst_18[1:length(rmst_18$studyCode)-1,]

# Now create a version which has one row per study
# At the moment the three-arm trial has two rows of data and need to change this to one row
rmst_18_wide <- data.frame(rmst_18[, 1:6],rmst_18[,9:10])
rmst_18_wide$t3 <- NA
rmst_18_wide$rmst2 <- NA
rmst_18_wide$rmst2_se <- NA
len<-length(rmst_18$studyCode)

rmst_18_wide$t3[len-1] <- rmst_18_wide$t2[len]
rmst_18_wide$rmst2[len-1] <- rmst_18$rmst1[len]
rmst_18_wide$rmst2_se[len-1] <- rmst_18$rmst1_se[len]
rmst_18_wide <- rmst_18_wide[1:(len-1),]

data <- rmst_18_wide

# Order by studycode
data <- data[order(data$studyCode),]

# Set the location for WinBUGS
bugs.directory <- "C:/Users/ECHO/Desktop/NMA study/parameter survival model/WinBUGS14"

# WinBUGS burn-in & simulation size
num.sims <- 10000
burn.in <- 10000


#-----------------------------------------------------------------------------
# Data
#-----------------------------------------------------------------------------

# No. of studies
ns <- length(unique(data$studyCode))

# No. of treatments
nt <- max(data$t2)

# No. of arms in each trial
data$na <- 2
data$na[data$studyCode==6] <- 3

y <- array(c(data$rmst0, data$rmst1, data$rmst2), dim=c(ns,3))
se <- array(c(data$rmst0_se, data$rmst1_se, data$rmst2_se), dim=c(ns,3))
t <- array(c(data$t1, data$t2, data$t3), dim=c(ns,3))

bugs_data <- list(ns=ns, nt=nt, t=t, y=y, se=se, na=data$na)

#-----------------------------------------------------------------------------
# Initial values
#-----------------------------------------------------------------------------

d1 <- c(NA, rep(0,10))
d2 <- c(NA, rep(0.1, 10))
d3 <- c(NA, rep(-0.1, 10))

mu1 <- c(rep(0.1, ns))
mu2 <- c(rep(0.3, ns))
mu3 <- c(rep(0.5, ns))

fe_inits <- list(list(mu=mu1, d=d1), 
                 list(mu=mu2, d=d2),
                 list(mu=mu3, d=d3))

#-----------------------------------------------------------------------------
# Fit FE model in WinBUGS
#-----------------------------------------------------------------------------

bugs.fe <- bugs(data=bugs_data, inits=fe_inits, 
                parameters.to.save=c("d", "best", "prob", "rk"), 
                model.file="FE_model.txt", clearWD=F, 
                summary.only=FALSE, n.iter=(num.sims+burn.in), 
                n.sims=num.sims, n.burnin=burn.in, n.chains=3, 
                bugs.seed=385916, bugs.directory=bugs.directory, 
                debug=F, working.directory=path)

fe_results <- bugs.fe$summary

# Save results in csv file
write.csv(fe_results,file="fe_results_pfs.csv")

#### Create forest plot of RMST results

# Load library
library(metafor)

# Start with empty environment

# Set working directory and load data
data <- read.csv("fe_results_pfs.csv") 
data <- data[1:10,]

data$trt <- c("cam+che", "niv+ipi+che","sug+che" ,"ate+che", "bev+che", "ate+bev+che", "pem+che", "sin+che", "niv+bev+che","tis+che")

forest_data<-data.frame(data$trt,data$X2.5.,data$mean,data$X97.5.)
forest_data$RMST<-paste(round(data$mean,2),"[",round(data$X2.5.,2),",",round(data$X97.5.,2),"]",seq="")
colnames(forest_data)<-c("trt","mean","lower","upper","RMST")
# forest(x=data$mean, ci.lb=data$X2.5, ci.ub=data$X97.5,
#        slab = data$trt,
#        refline=0, top=1, xlab="RMST (months)", cex=0.8)
# 
# dev.copy(pdf, "nma_forest_plot.pdf")
# dev.off()



# Plot of probabilities for each treatment obtaining each rank

# Load libraries
library(reshape2)
library(ggplot2)

# Start with empty environment

# Set workign directory

# Load results
data <- read.csv("fe_results_pfs.csv")

# Keep rows for ranking only
data <- data[22:142,]

# Variable for rank
data$rank_code <- c(rep(1, 11), rep(2, 11), rep(3, 11), rep(4, 11), rep(5, 11),
                    rep(6, 11), rep(7, 11), rep(8, 11), rep(9, 11),rep(10,11) ,rep(11, 11))

# Restrict probability to 2 decimal places
data$prob <- round(data$mean, 2)

# Add a treatment label
data$Treatment <- rep(c("che","cam+che", "niv+ipi+che","sug+che" ,"ate+che", "bev+che", "ate+bev+che", "pem+che", "sin+che", "niv+bev+che","tis+che"),11)

# data$Treatment <- c(rep("che", 10), rep("cam+che", 10), rep("ipi+niv", 10), rep("niv+ipi+che", 10), rep("ate+che", 10),
#                     rep("bev+che", 10), rep("ate+bev+che", 10), rep("pem+che", 10), rep("sin+che", 10), rep("niv+bev+che", 10))

# Rename mean column
names(data)[names(data)=="mean"] <- "Probability"

q <- ggplot(data, aes(x=rank_code, y=Treatment)) +
  geom_point(aes(size=Probability), shape=21, colour="skyblue", fill="skyblue") +
  theme(panel.background=element_blank(), panel.border=element_rect(colour="black", fill=NA, size=1),
        legend.position="bottom") +
  scale_size_area(max_size=11) +
  scale_x_continuous(name="Rank", limits=c(1, 11), breaks=seq(1,11,1)) +
  scale_y_discrete(name="Treatment") +  
  geom_text(aes(label=prob))
q

# save plot
dev.copy(png, "rank_plot_pfs.png")
dev.off()

write.csv(forest_data,"forest_data_pfs.csv",row.names =FALSE)
