# Royston-Parmar model in R - fixed effect

# Empty environment
rm(list = ls()) 

# Load libraries
library(survival)
library(foreach)
library(R2WinBUGS)
library(here)

# Working diectory
path <- "C:/Users/ECHO/Desktop/nsq-cea-scope/RP-model/S_tpso50_pfs"
setwd(path)

# Load IPD data
data <- read.csv("RP_tpso50_PFS.csv")

# Set the location for WinBUGS
bugs.directory <- "C:/Users/ECHO/Desktop/NMA study/parameter survival model/WinBUGS14"

# WinBUGS burn-in & simulation size
num.sims <- 10000
burn.in <- 10000

# Number of studies
num_studies <- length(unique(data$studyCode))

# Sort data into trialid order
data <- data[order(data$studyCode),]

# Number of patients in each study
# Note: if data is re-ordered will now to re-order this vector
num_patients <- c(45,202,168,110)####

# Number of treatments
nt <- length(unique(data$txCode))

# Create treatment indicator variables - caution needed to ensure consistency equations hold
data$trt2 <- 0
data$trt2[data$txCode==2] <- 1

data$trt3 <- 0
data$trt3[data$txCode==3] <- 1

data$trt4 <- 0
data$trt4[data$txCode==4] <- 1

data$trt5 <- 0
data$trt5[data$txCode==5] <- 1

# Add  trialid variable - easier than changing trialid to studyCode throughout this file!
data$trialid <- data$studyCode

# Put eventtime onto the ln scale
data$eventtime <- data$time
data$lnt <- log(data$eventtime)

# Create trt*lnt variables
data$trt2lnt <- data$trt2*data$lnt
data$trt3lnt <- data$trt3*data$lnt
data$trt4lnt <- data$trt4*data$lnt
data$trt5lnt <- data$trt5*data$lnt


# Total number of patients
pts <- nrow(data)

# Set location of knots for each trial - 33rd and 67th percentiles of uncensored survival times

knots <- data.frame(trialid=NA, k1=NA, k2=NA, k3=NA, k4=NA)
foreach(i=1:num_studies) %do% {
  k <- quantile(data$lnt[data$event==1 & data$trialid==i], c(0, 0.33, 0.67, 1))
  knots <- rbind(knots, data.frame(trialid=i, k1=k[1], k2=k[2], k3=k[3], k4=k[4]))
}
# Remove the empty top row
knots <- knots[-1,]

# Add knot values to data
foreach(i=1:num_studies) %do% {
  data$k1[data$trialid==i] <- knots$k1[i]
  data$k2[data$trialid==i] <- knots$k2[i]
  data$k3[data$trialid==i] <- knots$k3[i]
  data$k4[data$trialid==i] <- knots$k4[i]
}  

# Orthogonalisation
data$t1 <- data$lnt-data$k2
data$t2 <- data$lnt-data$k1
data$t3 <- data$lnt-data$k4
data$t4 <- data$lnt-data$k3

foreach(i=1:pts) %do% {
  data$a[i] <- max(c(data$t1[i], 0))
  data$b[i] <- max(c(data$t2[i], 0))
  data$c[i] <- max(c(data$t3[i], 0))
  data$e[i] <- max(c(data$t4[i], 0))
  
  data$phi1[i] <- (data$k4[i]-data$k2[i])/(data$k4[i]-data$k1[i])
  data$phi2[i] <- (data$k4[i]-data$k3[i])/(data$k4[i]-data$k1[i])
  
  data$v0[i] <- data$lnt[i]
  data$v1[i] <- data$a[i]^3 - (data$phi1[i]*(data$b[i]^3)) - ((1-data$phi1[i])*(data$c[i]^3))
  data$v2[i] <- data$e[i]^3 - (data$phi2[i]*(data$b[i]^3)) - ((1-data$phi2[i])*(data$c[i]^3))
}
##--------------##
df1 <- data.frame(trialid=NA, mean=NA, sd=NA)
foreach(i=1:num_studies) %do% {
  mv0 <- mean(data$v0[data$trialid==i])
  sdv0 <- sd(data$v0[data$trialid==i])
  df1 <- rbind(df1, data.frame(trialid=i, mean=mv0, sd=sdv0))
}
df1 <- df1[-1,]

# Add values back to data
foreach(i=1:num_studies) %do% {
  data$mv0[data$trialid==i] <- df1$mean[i]
  data$sdv0[data$trialid==i] <- df1$sd[i]
} 

foreach(i=1:pts) %do% {
  data$u0[i] <- (data$v0[i]-data$mv0[i])/data$sdv0[i]
  data$f[i] <- data$v1[i]*data$u0[i]
  data$g[i] <- data$u0[i]*data$u0[i]
}

df2 <- data.frame(trialid=NA, v1u0=NA, u0u0=NA)
foreach(i=1:num_studies) %do% {
  v1u0 <- sum(data$f[data$trialid==i])
  u0u0 <- sum(data$g[data$trialid==i])
  df2 <- rbind(df2, data.frame(trialid=i, v1u0=v1u0, u0u0=u0u0))
}
df2 <- df2[-1,]

# Add values back to data
foreach(i=1:num_studies) %do% {
  data$v1u0[data$trialid==i] <- df2$v1u0[i]
  data$u0u0[data$trialid==i] <- df2$u0u0[i]
}

foreach(i=1:pts) %do% {
  data$u1[i] <- data$v1[i] - ((data$v1u0[i]/data$u0u0[i])*data$u0[i])
}
##--------------##
df3 <- data.frame(trialid=NA, mean=NA, sd=NA)
foreach(i=1:num_studies) %do% {
  mu1 <- mean(data$u1[data$trialid==i])
  sdu1 <- sd(data$u1[data$trialid==i])
  df3 <- rbind(df3, data.frame(trialid=i, mean=mu1, sd=sdu1))
}
df3 <- df3[-1,]

# Add values back to data
foreach(i=1:num_studies) %do% {
  data$mu1[data$trialid==i] <- df3$mean[i]
  data$sdu1[data$trialid==i] <- df3$sd[i]
} 

foreach(i=1:pts) %do% {
  data$u1n[i] <- (data$u1[i]-data$mu1[i])/data$sdu1[i]
  data$h[i] <- data$v2[i]*data$u0[i]
  data$aa[i] <- data$v2[i]*data$u1n[i]
  data$j[i] <- data$u1n[i]*data$u1n[i]
}

df4 <- data.frame(trialid=NA, v2u0=NA, v2u1n=NA, u1nu1n=NA)
foreach(i=1:num_studies) %do% {
  v2u0 <- sum(data$h[data$trialid==i])
  v2u1n <- sum(data$aa[data$trialid==i])
  u1nu1n <- sum(data$j[data$trialid==i])
  df4 <- rbind(df4, data.frame(trialid=i, v2u0=v2u0, v2u1n=v2u1n, u1nu1n=u1nu1n))
}
df4 <- df4[-1,]

# Add values back to data
foreach(i=1:num_studies) %do% {
  data$v2u0[data$trialid==i] <- df4$v2u0[i]
  data$v2u1n[data$trialid==i] <- df4$v2u1n[i]
  data$u1nu1n[data$trialid==i] <- df4$u1nu1n[i]
}

foreach(i=1:pts) %do% {
  data$u2[i] <- data$v2[i] - ((data$v2u0[i]/data$u0u0[i])*data$u0[i]) - ((data$v2u1n[i]/data$u1nu1n[i])*data$u1n[i])
}
##--------------##

df5 <- data.frame(trialid=NA, mean=NA, sd=NA)
foreach(i=1:num_studies) %do% {
  mu2 <- mean(data$u2[data$trialid==i])
  sdu2 <- sd(data$u2[data$trialid==i])
  df5 <- rbind(df5, data.frame(trialid=i, mean=mu2, sd=sdu2))
}
df5 <- df5[-1,]

# Add values back to data
foreach(i=1:num_studies) %do% {
  data$mu2[data$trialid==i] <- df5$mean[i]
  data$sdu2[data$trialid==i] <- df5$sd[i]
} 

foreach(i=1:pts) %do% {
  data$u2n[i] <- (data$u2[i]-data$mu2[i])/data$sdu2[i]
  data$du0[i] <- 1/data$sdv0[i]
  data$k[i] <- (3*(data$a[i]^2)) -((3*data$phi1[i])*(data$b[i]^2))- (3*(1-data$phi1[i])*(data$c[i]^2))
  data$l[i] <- data$sdu1[i]
  data$du1n[i] <- (1/data$l[i])*data$k[i] - (((data$v1u0[i]/data$u0u0[i])/data$l[i])*data$du0[i])
  data$m[i] <- (3*(data$e[i]^2)) - ((3*data$phi2[i])*(data$b[i]^2))- (3*(1-data$phi2[i])*(data$c[i]^2))
  data$n[i] <- data$sdu2[i]
  data$du2n[i] <- ((1/data$n[i])*data$m[i]) - (((data$v2u0[i]/data$u0u0[i])/data$n[i])*data$du0[i]) - (((data$v2u1n[i]/data$u1nu1n[[i]])/data$n[i])*data$du1n[i])
}

offset <- 0
foreach(i=1:num_studies) %do% {
  offset[i+1] <- offset[i] + num_patients[i]
}


# Prepare to fit model in WinBUGS

bugs_data <- list(d=data$event, trt2=data$trt2, trt3=data$trt3, trt4=data$trt4, trt5=data$trt5,
                  trt2lnt=data$trt2lnt, trt3lnt=data$trt3lnt, trt4lnt=data$trt4lnt, trt5lnt=data$trt5lnt, 
                  u0=data$u0, u1n=data$u1n, u2n=data$u2n, du0=data$du0,
                  du1n=data$du1n, du2n=data$du2n, Ntrials=num_studies, offset=offset,
                  k0=knots$k1[2], k1=knots$k2[2], k2=knots$k3[2], k3=knots$k4[2],
                  meanv0=df1$mean[2], sdv0=df1$sd[2], v1w0=df2$v1u0[2], w0w0=df2$u0u0[2],
                  meanw1=df3$mean[2], sdw1=df3$sd[2], v2w0=df4$v2u0[2], v2w1n=df4$v2u1n[2],
                  w1nw1n=df4$u1nu1n[2], meanw2=df5$mean[2], sdw2=df5$sd[2],
                  nt=nt)

# Create initial values for model
beta1 <- c(rep(0.1, 8))
beta2 <- c(rep(0.2, 8))
beta3 <- c(rep(0.3, 8))

gamma1 <- array(rep(c(0.2), 4*num_studies), dim=c(4,num_studies))
gamma2 <- array(rep(c(0.4), 4*num_studies), dim=c(4,num_studies))
gamma3 <- array(rep(c(0.1), 4*num_studies), dim=c(4,num_studies))

inits <- list(list(beta=beta1, gamma=gamma1), 
              list(beta=beta2, gamma=gamma2),
              list(beta=beta3, gamma=gamma3))

# Fit model in WinBUGS
bugs.object<-bugs(data=bugs_data, inits=inits, 
                  parameters.to.save=c("beta", "surv", "rk", "gamma"), 
                  model.file="model_pfs.txt", clearWD=F, 
                  summary.only=FALSE, n.iter=(num.sims+burn.in), n.sims=num.sims,
                  n.burnin=burn.in, n.chains=3, 
                  bugs.seed=3, bugs.directory=bugs.directory, 
                  debug=F, working.directory=path)

results <- bugs.object$summary

# Save results as a csv file
write.csv(results, "results_tpso50_pfs.csv")