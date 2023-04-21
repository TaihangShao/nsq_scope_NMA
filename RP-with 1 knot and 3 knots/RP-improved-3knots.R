# Royston-Parmar model in R - fixed effect

# Empty environment
rm(list = ls()) 

# Load libraries
library(survival)
library(foreach)
library(R2WinBUGS)
library(here)

# Working diectory
path <- "C:/Users/Administrator/Desktop/RP-nma-IMPROVE"
setwd(path)

# Load IPD data
data <- read.csv("melanoma_ipd.csv")

# Set the location for WinBUGS
bugs.directory <- "D:/WinBUGS14"

# WinBUGS burn-in & simulation size
num.sims <- 200
burn.in <- 200

# Number of studies
num_studies <- length(unique(data$studyCode))

# Sort data into trialid order
data <- data[order(data$studyCode),]

# Number of patients in each study
# Note: if data is re-ordered will now to re-order this vector
num_patients <- c(250, 675, 418, 945, 142, 495, 422, 704, 245, 834, 655, 502, 91)####

# Number of treatments
nt <- length(unique(data$txCode))

# Create treatment indicator variables - caution needed to ensure consistency equations hold
data$trt2 <- 0
data$trt2[data$txCode==2] <- 1

data$trt3 <- 0
data$trt3[data$txCode==3] <- 1

data$trt7 <- 0
data$trt7[data$txCode==7] <- 1

data$trt10 <- 0
data$trt10[data$txCode==10] <- 1

data$trt11 <- 0
data$trt11[data$txCode==11] <- 1

data$trt12 <- 0
data$trt12[data$txCode==12] <- 1

data$trt13 <- 0
data$trt13[data$txCode==13] <- 1

data$trt4 <- 0
data$trt4[data$txCode==4 & data$studyCode==1] <- 1
data$trt4[data$txCode==4 & data$studyCode==7] <- -1

data$trt5 <- 0
data$trt5[data$txCode==5 & data$studyCode==7] <- 1
data$trt5[data$txCode==5 & data$studyCode==8] <- -1

data$trt6 <- 0
data$trt6[data$txCode==6 & (data$studyCode==2 | data$studyCode==8)] <- 1

data$trt8 <- 0
data$trt8[data$txCode==9 & data$studyCode==4] <- 1

data$trt9 <- 0
data$trt9[data$txCode==9 & data$studyCode==3] <- 1

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
data$trt6lnt <- data$trt6*data$lnt
data$trt7lnt <- data$trt7*data$lnt
data$trt8lnt <- data$trt8*data$lnt
data$trt9lnt <- data$trt9*data$lnt
data$trt10lnt <- data$trt10*data$lnt
data$trt11lnt <- data$trt11*data$lnt
data$trt12lnt <- data$trt12*data$lnt
data$trt13lnt <- data$trt13*data$lnt

# Total number of patients
pts <- nrow(data)

# Set location of knots for each trial - 25th and 50th and 75th percentiles of uncensored survival times

knots <- data.frame(trialid=NA, k1=NA, k2=NA, k3=NA, k4=NA,k5=NA)
foreach(i=1:num_studies) %do% {
  k <- quantile(data$lnt[data$event==1 & data$trialid==i], c(0, 0.25, 0.5,0.75, 1))
  knots <- rbind(knots, data.frame(trialid=i, k1=k[1], k2=k[2], k3=k[3], k4=k[4],k5=k[5]))
}
# Remove the empty top row
knots <- knots[-1,]

# Add knot values to data
foreach(i=1:num_studies) %do% {
  data$k1[data$trialid==i] <- knots$k1[i]
  data$k2[data$trialid==i] <- knots$k2[i]
  data$k3[data$trialid==i] <- knots$k3[i]
  data$k4[data$trialid==i] <- knots$k4[i]
  data$k5[data$trialid==i] <- knots$k5[i]
}  

# Orthogonalisation
data$t1 <- data$lnt-data$k2
data$t2 <- data$lnt-data$k1
data$t3 <- data$lnt-data$k5
data$t4 <- data$lnt-data$k3
data$t5 <- data$lnt-data$k4

foreach(i=1:pts) %do% {
  data$a[i] <- max(c(data$t1[i], 0))
  data$b[i] <- max(c(data$t2[i], 0))
  data$c[i] <- max(c(data$t3[i], 0))
  data$z[i] <- max(c(data$t4[i], 0))
  data$e[i] <- max(c(data$t5[i], 0))
  
  data$phi1[i] <- (data$k5[i]-data$k2[i])/(data$k5[i]-data$k1[i])
  data$phi2[i] <- (data$k5[i]-data$k3[i])/(data$k5[i]-data$k1[i])
  data$phi3[i] <- (data$k5[i]-data$k4[i])/(data$k5[i]-data$k1[i])
  
  data$v0[i] <- data$lnt[i]
  data$v1[i] <- data$a[i]^3 - (data$phi1[i]*(data$b[i]^3)) - ((1-data$phi1[i])*(data$c[i]^3))
  data$v2[i] <- data$z[i]^3 - (data$phi2[i]*(data$b[i]^3)) - ((1-data$phi2[i])*(data$c[i]^3))
  data$v3[i] <- data$e[i]^3 - (data$phi3[i]*(data$b[i]^3)) - ((1-data$phi3[i])*(data$c[i]^3))
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
  data$h2[i] <- data$v3[i]*data$u0[i]
  data$aa2[i] <- data$v3[i]*data$u1n[i]
  data$j2[i] <- data$v3[i]*data$u2n[i]
  data$j3[i] <- data$u2n[i]*data$u2n[i]
}

df6 <- data.frame(trialid=NA, v3u0=NA, v3u1n=NA, v3u2n=NA, u2nu2n=NA)
foreach(i=1:num_studies) %do% {
  v3u0 <- sum(data$h2[data$trialid==i])
  v3u1n <- sum(data$aa2[data$trialid==i])
  v3u2n <- sum(data$j2[data$trialid==i])
  u2nu2n <- sum(data$j3[data$trialid==i])
  df6 <- rbind(df6, data.frame(trialid=i, v3u0=v3u0, v3u1n=v3u1n, v3u2n=v3u2n, u2nu2n=u2nu2n))
}
df6 <- df6[-1,]

# Add values back to data
foreach(i=1:num_studies) %do% {
  data$v3u0[data$trialid==i] <- df6$v3u0[i]
  data$v3u1n[data$trialid==i] <- df6$v3u1n[i]
  data$v3u2n[data$trialid==i] <- df6$v3u2n[i]
  data$u2nu2n[data$trialid==i] <- df6$u2nu2n[i]
}

foreach(i=1:pts) %do% {
  data$u3[i] <- data$v3[i] - ((data$v3u0[i]/data$u0u0[i])*data$u0[i]) - ((data$v3u1n[i]/data$u1nu1n[i])*data$u1n[i]) - ((data$v3u2n[i]/data$u2nu2n[i])*data$u2n[i])
}

##--------------##

df7 <- data.frame(trialid=NA, mean=NA, sd=NA)
foreach(i=1:num_studies) %do% {
  mu3 <- mean(data$u3[data$trialid==i])
  sdu3 <- sd(data$u3[data$trialid==i])
  df7 <- rbind(df7, data.frame(trialid=i, mean=mu3, sd=sdu3))
}
df7 <- df7[-1,]

# Add values back to data
foreach(i=1:num_studies) %do% {
  data$mu3[data$trialid==i] <- df7$mean[i]
  data$sdu3[data$trialid==i] <- df7$sd[i]
} 

foreach(i=1:pts) %do% {
  data$u3n[i] <- (data$u3[i]-data$mu3[i])/data$sdu3[i]
  
  data$du0[i] <- 1/data$sdv0[i]
  
  data$k[i] <- (3*(data$a[i]^2)) -((3*data$phi1[i])*(data$b[i]^2))- (3*(1-data$phi1[i])*(data$c[i]^2))
  data$l[i] <- data$sdu1[i]
  data$du1n[i] <- (1/data$l[i])*data$k[i] - (((data$v1u0[i]/data$u0u0[i])/data$l[i])*data$du0[i])
  
  data$m[i] <- (3*(data$z[i]^2)) - ((3*data$phi2[i])*(data$b[i]^2))- (3*(1-data$phi2[i])*(data$c[i]^2))
  data$n[i] <- data$sdu2[i]
  data$du2n[i] <- ((1/data$n[i])*data$m[i]) - (((data$v2u0[i]/data$u0u0[i])/data$n[i])*data$du0[i]) - (((data$v2u1n[i]/data$u1nu1n[[i]])/data$n[i])*data$du1n[i])
  
  data$m1[i] <- (3*(data$e[i]^2)) - ((3*data$phi3[i])*(data$b[i]^2))- (3*(1-data$phi3[i])*(data$c[i]^2))
  data$n1[i] <- data$sdu3[i]
  data$du3n[i] <- ((1/data$n1[i])*data$m1[i]) - (((data$v3u0[i]/data$u0u0[i])/data$n1[i])*data$du0[i]) - (((data$v3u1n[i]/data$u1nu1n[[i]])/data$n1[i])*data$du1n[i])
                                                                                                        - (((data$v3u2n[i]/data$u2nu2n[[i]])/data$n1[i])*data$du2n[i])
}

offset <- 0
foreach(i=1:num_studies) %do% {
  offset[i+1] <- offset[i] + num_patients[i]
}


# Prepare to fit model in WinBUGS

bugs_data <- list(d=data$event, trt2=data$trt2, trt3=data$trt3, trt4=data$trt4, trt5=data$trt5, trt6=data$trt6, 
                  trt7=data$trt7, trt8=data$trt8, trt9=data$trt9, trt10=data$trt10, trt11=data$trt11, 
                  trt12=data$trt12, trt13=data$trt13,
                  trt2lnt=data$trt2lnt, trt3lnt=data$trt3lnt, trt4lnt=data$trt4lnt, trt5lnt=data$trt5lnt, 
                  trt6lnt=data$trt6lnt, trt7lnt=data$trt7lnt, trt8lnt=data$trt8lnt, trt9lnt=data$trt9lnt, 
                  trt10lnt=data$trt10lnt, trt11lnt=data$trt11lnt,
                  trt12lnt=data$trt12lnt, trt13lnt=data$trt13lnt,
                  u0=data$u0, u1n=data$u1n, u2n=data$u2n, u3n=data$u3n, 
                  du0=data$du0,du1n=data$du1n, du2n=data$du2n, du3n=data$du3n,
                  Ntrials=num_studies, offset=offset,
                  k0=knots$k1[3], k1=knots$k2[3], k2=knots$k3[3], k3=knots$k4[3], k4=knots$k5[3],
                  meanv0=df1$mean[3], sdv0=df1$sd[3], 
                  v1w0=df2$v1u0[3], w0w0=df2$u0u0[3],
                  meanw1=df3$mean[3], sdw1=df3$sd[3], 
                  v2w0=df4$v2u0[3], v2w1n=df4$v2u1n[3], w1nw1n=df4$u1nu1n[3], 
                  meanw2=df5$mean[3], sdw2=df5$sd[3],
                  v3w0=df6$v3u0[3], v3w1n=df6$v3u1n[3],v3w2n=df6$v3u2n[3], w2nw2n=df6$u2nu2n[3],
                  meanw3=df7$mean[3], sdw3=df7$sd[3],
                  nt=nt)

# Create initial values for model
beta1 <- c(rep(0.1, 24))
beta2 <- c(rep(0.2, 24))
beta3 <- c(rep(0.3, 24))
# beta4 <- c(rep(0.4, 24))


gamma1 <- array(rep(c(0.02), 5*num_studies), dim=c(5,num_studies))
gamma2 <- array(rep(c(0.04), 5*num_studies), dim=c(5,num_studies))
gamma3 <- array(rep(c(0.01), 5*num_studies), dim=c(5,num_studies))
# gamma4 <- array(rep(c(0.3), 5*num_studies), dim=c(5,num_studies))


inits <- list(list(beta=beta1, gamma=gamma1), 
              list(beta=beta2, gamma=gamma2),
              list(beta=beta3, gamma=gamma3)
              # ,list(beta=beta4, gamma=gamma4)
              )

# Fit model in WinBUGS
bugs.object<-bugs(data=bugs_data, inits=inits, 
                  parameters.to.save=c("beta", "surv", "rk60", "gamma"), 
                  model.file="model3.txt", clearWD=F, 
                  summary.only=FALSE, n.iter=(num.sims+burn.in), n.sims=num.sims,
                  n.burnin=burn.in, n.chains=3, 
                  bugs.seed=3, bugs.directory=bugs.directory, 
                  debug=F, working.directory=path)

results <- bugs.object$summary

# Save results as a csv file
write.csv(results, "results-3.csv")