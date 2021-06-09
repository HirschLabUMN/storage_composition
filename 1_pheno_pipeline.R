## Script written by Jonathan Renk
## 17 May 2021

## Clearing the global environment
rm(list=ls(all=TRUE))

## Setting up the working directory
getwd()
setwd("/Users/jonathanrenk/Desktop/Storage Work/")

## Loading packages
library("car")
library("lme4")
library("dfoptim")
library("lmerTest")
library("Hmisc")

## Loading in the data
data <- read.csv("data/master_data_final.csv")

str(data)
summary(data)

## Setting variables as factors for hybrid, sample, year, hotplate, rep, temp, and storage
data[,1] <- as.factor(data[,1])
data[,2] <- as.factor(data[,2])
data[,3] <- as.factor(data[,3])
data[,4] <- as.factor(data[,4])
data[,5] <- as.factor(data[,5])
data[,6] <- as.factor(data[,6])
data[,7] <- as.factor(data[,7])

str(data)

## Subsetting the data by sample type
krn <- data.frame(data [ which(data$Sample == 'KRN'),])
wk <- data.frame(data [ which(data$Sample == 'WK'),])

## Subsetting the year with washed kernels
wk_yr1 <- data.frame(wk [ which(wk$Year == '1'),])
wk_yr2 <- data.frame(wk [ which(wk$Year == '2'),])

## Subsetting for hybrid within year
p0707_wk_yr1 <- data.frame(wk_yr1 [ which(wk_yr1$Hybrid == 'P0707'),])
p0707_wk_yr2 <- data.frame(wk_yr2 [ which(wk_yr2$Hybrid == 'P0707'),])
p1306w_wk_yr1 <- data.frame(wk_yr1 [ which(wk_yr1$Hybrid == 'P1306W'),])
p1306w_wk_yr2 <- data.frame(wk_yr2 [ which(wk_yr2$Hybrid == 'P1306W'),])

## Subsetting the year with kernels
krn_yr1 <- data.frame(krn [ which(krn$Year == '1'),])
krn_yr2 <- data.frame(krn [ which(krn$Year == '2'),])

## Subsetting for hybrid within year
p0707_krn_yr1 <- data.frame(krn_yr1 [ which(krn_yr1$Hybrid == 'P0707'),])
p0707_krn_yr2 <- data.frame(krn_yr2 [ which(krn_yr2$Hybrid == 'P0707'),])
p1306w_krn_yr1 <- data.frame(krn_yr1 [ which(krn_yr1$Hybrid == 'P1306W'),])
p1306w_krn_yr2 <- data.frame(krn_yr2 [ which(krn_yr2$Hybrid == 'P1306W'),])

####################################################################################################################################
### P0707 WK Year 1 ###
traits <- colnames(p0707_wk_yr1)[8:ncol(p0707_wk_yr1)]

for(trait in traits){
  pdf(paste0("plots_", trait, ".pdf"), width = 15, height = 5)
  par(mfrow=c(1,3))
  
  # Histogram of raw data
  hist(p0707_wk_yr1[,trait],
       main=paste0("Histogram of raw\n", trait, " values"),
       xlab=trait,
       col="cadetblue")
  
  # Stripchart of raw data across storage
  stripchart(p0707_wk_yr1[,trait] ~p0707_wk_yr1$Storage,
             main=paste0("Stripchart of raw\n", trait, " values"),
             xlab="Storage",
             ylab=trait,
             vertical=TRUE, 
             method="jitter",
             bg="cadetblue",
             pch=21)
  
  # Stripchart of raw data across temp
  stripchart(p0707_wk_yr1[,trait] ~p0707_wk_yr1$Temp,
             main=paste0("Stripchart of raw\n", trait, " values"),
             xlab="Temp",
             ylab=trait,
             vertical=TRUE, 
             method="jitter",
             bg="cadetblue",
             pch=21)
  
  dev.off()
  
  if(paste0("stats_", trait, ".txt") %in% list.files()){
    system(paste0("rm stats_", trait, ".txt"))}
  # Summary statistics of the trait
  summary <- summary(p0707_wk_yr1[,trait], )
  out <- capture.output(summary)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Test for normality
  normality <- shapiro.test(p0707_wk_yr1[,trait])
  out <- capture.output(normality)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Run an ANOVA (switched : in Env:Rep and Rep:Block for nesting)
  model <- lm(get(trait) ~ Temp + Storage + Temp:Storage + Hotplate/Rep, data=p0707_wk_yr1)
  anova <- anova(model)
  out <- capture.output(anova)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Run a random effects model
  model.1 <- lmer(get(trait) ~ Temp + Storage + Temp:Storage + (1|Hotplate/Rep), data = p0707_wk_yr1, REML = TRUE)
  # Decreasing stopping tolerances
  strict_tol <- lmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8))
  if (all(model.1@optinfo$optimizer=="nloptwrap")) {
    model <- update(model.1, control=strict_tol)
  }
  
  # Summary of random effects
  summary <- summary(model, correlation=FALSE)
  out <- capture.output(summary)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  # ANOVA of mixed linear model
  anova <- anova(model)
  out <- capture.output(anova)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  # Write out residuals from ANOVA
  write.table(resid(model), paste0("resids_", trait, ".csv"), col.names=F, row.names=F, sep=",")
  
  pdf(paste0("assumptions_", trait, ".pdf"), width = 15, height = 5)
  par(mfrow=c(1,3))
  
  # Model Fit with REML
  plot(fitted(model), residuals(model), pch=19, col="dark blue", ylab="Residuals", xlab="Predicted")
  abline(h=0,col="red", lwd=1, lty=1)
  # histogram of residuals
  hist(residuals(model),main="Histogram of residuals",freq=F, xlab="Residuals", ylab= "Freq", col="palegreen", col.main="darkblue")
  x=seq(-5e-15,9e-15,5e-15)
  curve(dnorm(x,mean(residuals(model)),sd(residuals(model))),add=T,lwd=2, col="red", lty=1)
  # qq plot
  qqPlot(residuals(model), pch=19, col="dark blue", col.lines="red", xlab="Pred quantiles", ylab="Obs quantiles") 
  
  dev.off()
  
}

### P0707 WK Year 2 ###
traits <- colnames(p0707_wk_yr2)[8:ncol(p0707_wk_yr2)]

for(trait in traits){
  pdf(paste0("plots_", trait, ".pdf"), width = 15, height = 5)
  par(mfrow=c(1,3))
  
  # Histogram of raw data
  hist(p0707_wk_yr2[,trait],
       main=paste0("Histogram of raw\n", trait, " values"),
       xlab=trait,
       col="cadetblue")
  
  # Stripchart of raw data across storage
  stripchart(p0707_wk_yr2[,trait] ~p0707_wk_yr2$Storage,
             main=paste0("Stripchart of raw\n", trait, " values"),
             xlab="Storage",
             ylab=trait,
             vertical=TRUE, 
             method="jitter",
             bg="cadetblue",
             pch=21)
  
  # Stripchart of raw data across temp
  stripchart(p0707_wk_yr2[,trait] ~p0707_wk_yr2$Temp,
             main=paste0("Stripchart of raw\n", trait, " values"),
             xlab="Temp",
             ylab=trait,
             vertical=TRUE, 
             method="jitter",
             bg="cadetblue",
             pch=21)
  
  dev.off()
  
  if(paste0("stats_", trait, ".txt") %in% list.files()){
    system(paste0("rm stats_", trait, ".txt"))}
  # Summary statistics of the trait
  summary <- summary(p0707_wk_yr2[,trait], )
  out <- capture.output(summary)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Test for normality
  normality <- shapiro.test(p0707_wk_yr2[,trait])
  out <- capture.output(normality)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Run an ANOVA (switched : in Env:Rep and Rep:Block for nesting)
  model <- lm(get(trait) ~ Temp + Storage + Temp:Storage + Hotplate/Rep, data=p0707_wk_yr2)
  anova <- anova(model)
  out <- capture.output(anova)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Run a random effects model
  model.1 <- lmer(get(trait) ~ Temp + Storage + Temp:Storage + (1|Hotplate/Rep), data = p0707_wk_yr2, REML = TRUE)
  # Decreasing stopping tolerances
  strict_tol <- lmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8))
  if (all(model.1@optinfo$optimizer=="nloptwrap")) {
    model <- update(model.1, control=strict_tol)
  }
  
  # Summary of random effects
  summary <- summary(model, correlation=FALSE)
  out <- capture.output(summary)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  # ANOVA of mixed linear model
  anova <- anova(model)
  out <- capture.output(anova)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  # Write out residuals from ANOVA
  write.table(resid(model), paste0("resids_", trait, ".csv"), col.names=F, row.names=F, sep=",")
  
  pdf(paste0("assumptions_", trait, ".pdf"), width = 15, height = 5)
  par(mfrow=c(1,3))
  
  # Model Fit with REML
  plot(fitted(model), residuals(model), pch=19, col="dark blue", ylab="Residuals", xlab="Predicted")
  abline(h=0,col="red", lwd=1, lty=1)
  # histogram of residuals
  hist(residuals(model),main="Histogram of residuals",freq=F, xlab="Residuals", ylab= "Freq", col="palegreen", col.main="darkblue")
  x=seq(-5e-15,9e-15,5e-15)
  curve(dnorm(x,mean(residuals(model)),sd(residuals(model))),add=T,lwd=2, col="red", lty=1)
  # qq plot
  qqPlot(residuals(model), pch=19, col="dark blue", col.lines="red", xlab="Pred quantiles", ylab="Obs quantiles") 
  
  dev.off()
  
}

### P1306W WK Year 1 ###
traits <- colnames(p1306w_wk_yr1)[8:ncol(p1306w_wk_yr1)]

for(trait in traits){
  pdf(paste0("plots_", trait, ".pdf"), width = 15, height = 5)
  par(mfrow=c(1,3))
  
  # Histogram of raw data
  hist(p1306w_wk_yr1[,trait],
       main=paste0("Histogram of raw\n", trait, " values"),
       xlab=trait,
       col="cadetblue")
  
  # Stripchart of raw data across storage
  stripchart(p1306w_wk_yr1[,trait] ~p1306w_wk_yr1$Storage,
             main=paste0("Stripchart of raw\n", trait, " values"),
             xlab="Storage",
             ylab=trait,
             vertical=TRUE, 
             method="jitter",
             bg="cadetblue",
             pch=21)
  
  # Stripchart of raw data across temp
  stripchart(p1306w_wk_yr1[,trait] ~p1306w_wk_yr1$Temp,
             main=paste0("Stripchart of raw\n", trait, " values"),
             xlab="Temp",
             ylab=trait,
             vertical=TRUE, 
             method="jitter",
             bg="cadetblue",
             pch=21)
  
  dev.off()
  
  if(paste0("stats_", trait, ".txt") %in% list.files()){
    system(paste0("rm stats_", trait, ".txt"))}
  # Summary statistics of the trait
  summary <- summary(p1306w_wk_yr1[,trait], )
  out <- capture.output(summary)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Test for normality
  normality <- shapiro.test(p1306w_wk_yr1[,trait])
  out <- capture.output(normality)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Run an ANOVA (switched : in Env:Rep and Rep:Block for nesting)
  model <- lm(get(trait) ~ Temp + Storage + Temp:Storage + Hotplate/Rep, data=p1306w_wk_yr1)
  anova <- anova(model)
  out <- capture.output(anova)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Run a random effects model
  model.1 <- lmer(get(trait) ~ Temp + Storage + Temp:Storage + (1|Hotplate/Rep), data = p1306w_wk_yr1, REML = TRUE)
  # Decreasing stopping tolerances
  strict_tol <- lmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8))
  if (all(model.1@optinfo$optimizer=="nloptwrap")) {
    model <- update(model.1, control=strict_tol)
  }
  
  # Summary of random effects
  summary <- summary(model, correlation=FALSE)
  out <- capture.output(summary)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  # ANOVA of mixed linear model
  anova <- anova(model)
  out <- capture.output(anova)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  # Write out residuals from ANOVA
  write.table(resid(model), paste0("resids_", trait, ".csv"), col.names=F, row.names=F, sep=",")
  
  pdf(paste0("assumptions_", trait, ".pdf"), width = 15, height = 5)
  par(mfrow=c(1,3))
  
  # Model Fit with REML
  plot(fitted(model), residuals(model), pch=19, col="dark blue", ylab="Residuals", xlab="Predicted")
  abline(h=0,col="red", lwd=1, lty=1)
  # histogram of residuals
  hist(residuals(model),main="Histogram of residuals",freq=F, xlab="Residuals", ylab= "Freq", col="palegreen", col.main="darkblue")
  x=seq(-5e-15,9e-15,5e-15)
  curve(dnorm(x,mean(residuals(model)),sd(residuals(model))),add=T,lwd=2, col="red", lty=1)
  # qq plot
  qqPlot(residuals(model), pch=19, col="dark blue", col.lines="red", xlab="Pred quantiles", ylab="Obs quantiles") 
  
  dev.off()
  
}

### P1306W WK Year 2 ###
traits <- colnames(p1306w_wk_yr2)[8:ncol(p1306w_wk_yr2)]

for(trait in traits){
  pdf(paste0("plots_", trait, ".pdf"), width = 15, height = 5)
  par(mfrow=c(1,3))
  
  # Histogram of raw data
  hist(p1306w_wk_yr2[,trait],
       main=paste0("Histogram of raw\n", trait, " values"),
       xlab=trait,
       col="cadetblue")
  
  # Stripchart of raw data across storage
  stripchart(p1306w_wk_yr2[,trait] ~p1306w_wk_yr2$Storage,
             main=paste0("Stripchart of raw\n", trait, " values"),
             xlab="Storage",
             ylab=trait,
             vertical=TRUE, 
             method="jitter",
             bg="cadetblue",
             pch=21)
  
  # Stripchart of raw data across temp
  stripchart(p1306w_wk_yr2[,trait] ~p1306w_wk_yr2$Temp,
             main=paste0("Stripchart of raw\n", trait, " values"),
             xlab="Temp",
             ylab=trait,
             vertical=TRUE, 
             method="jitter",
             bg="cadetblue",
             pch=21)
  
  dev.off()
  
  if(paste0("stats_", trait, ".txt") %in% list.files()){
    system(paste0("rm stats_", trait, ".txt"))}
  # Summary statistics of the trait
  summary <- summary(p1306w_wk_yr2[,trait], )
  out <- capture.output(summary)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Test for normality
  normality <- shapiro.test(p1306w_wk_yr2[,trait])
  out <- capture.output(normality)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Run an ANOVA (switched : in Env:Rep and Rep:Block for nesting)
  model <- lm(get(trait) ~ Temp + Storage + Temp:Storage + Hotplate/Rep, data=p1306w_wk_yr2)
  anova <- anova(model)
  out <- capture.output(anova)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Run a random effects model
  model.1 <- lmer(get(trait) ~ Temp + Storage + Temp:Storage + (1|Hotplate/Rep), data = p1306w_wk_yr2, REML = TRUE)
  # Decreasing stopping tolerances
  strict_tol <- lmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8))
  if (all(model.1@optinfo$optimizer=="nloptwrap")) {
    model <- update(model.1, control=strict_tol)
  }
  
  # Summary of random effects
  summary <- summary(model, correlation=FALSE)
  out <- capture.output(summary)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  # ANOVA of mixed linear model
  anova <- anova(model)
  out <- capture.output(anova)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  # Write out residuals from ANOVA
  write.table(resid(model), paste0("resids_", trait, ".csv"), col.names=F, row.names=F, sep=",")
  
  pdf(paste0("assumptions_", trait, ".pdf"), width = 15, height = 5)
  par(mfrow=c(1,3))
  
  # Model Fit with REML
  plot(fitted(model), residuals(model), pch=19, col="dark blue", ylab="Residuals", xlab="Predicted")
  abline(h=0,col="red", lwd=1, lty=1)
  # histogram of residuals
  hist(residuals(model),main="Histogram of residuals",freq=F, xlab="Residuals", ylab= "Freq", col="palegreen", col.main="darkblue")
  x=seq(-5e-15,9e-15,5e-15)
  curve(dnorm(x,mean(residuals(model)),sd(residuals(model))),add=T,lwd=2, col="red", lty=1)
  # qq plot
  qqPlot(residuals(model), pch=19, col="dark blue", col.lines="red", xlab="Pred quantiles", ylab="Obs quantiles") 
  
  dev.off()
  
}

#####################################################################################################################################
#####################################################################################################################################

### P0707 KRN Year 1 ###
traits <- colnames(p0707_krn_yr1)[10:ncol(p0707_krn_yr1)]

for(trait in traits){
  pdf(paste0("plots_", trait, ".pdf"), width = 15, height = 5)
  par(mfrow=c(1,3))
  
  # Histogram of raw data
  hist(p0707_krn_yr1[,trait],
       main=paste0("Histogram of raw\n", trait, " values"),
       xlab=trait,
       col="cadetblue")
  
  # Stripchart of raw data across storage
  stripchart(p0707_krn_yr1[,trait] ~p0707_krn_yr1$Storage,
             main=paste0("Stripchart of raw\n", trait, " values"),
             xlab="Storage",
             ylab=trait,
             vertical=TRUE, 
             method="jitter",
             bg="cadetblue",
             pch=21)
  
  # Stripchart of raw data across temp
  stripchart(p0707_krn_yr1[,trait] ~p0707_krn_yr1$Temp,
             main=paste0("Stripchart of raw\n", trait, " values"),
             xlab="Temp",
             ylab=trait,
             vertical=TRUE, 
             method="jitter",
             bg="cadetblue",
             pch=21)
  
  dev.off()
  
  if(paste0("stats_", trait, ".txt") %in% list.files()){
    system(paste0("rm stats_", trait, ".txt"))}
  # Summary statistics of the trait
  summary <- summary(p0707_krn_yr1[,trait], )
  out <- capture.output(summary)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Test for normality
  normality <- shapiro.test(p0707_krn_yr1[,trait])
  out <- capture.output(normality)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Run an ANOVA (switched : in Env:Rep and Rep:Block for nesting)
  model <- lm(get(trait) ~ Temp + Storage + Temp:Storage + Rep, data=p0707_krn_yr1)
  anova <- anova(model)
  out <- capture.output(anova)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Run a random effects model
  model.1 <- lmer(get(trait) ~ Temp + Storage + Temp:Storage + (1|Rep), data = p0707_krn_yr1, REML = TRUE)
  # Decreasing stopping tolerances
  strict_tol <- lmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8))
  if (all(model.1@optinfo$optimizer=="nloptwrap")) {
    model <- update(model.1, control=strict_tol)
  }
  
  # Summary of random effects
  summary <- summary(model, correlation=FALSE)
  out <- capture.output(summary)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  # ANOVA of mixed linear model
  anova <- anova(model)
  out <- capture.output(anova)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  # Write out residuals from ANOVA
  write.table(resid(model), paste0("resids_", trait, ".csv"), col.names=F, row.names=F, sep=",")
  
  pdf(paste0("assumptions_", trait, ".pdf"), width = 15, height = 5)
  par(mfrow=c(1,3))
  
  # Model Fit with REML
  plot(fitted(model), residuals(model), pch=19, col="dark blue", ylab="Residuals", xlab="Predicted")
  abline(h=0,col="red", lwd=1, lty=1)
  # histogram of residuals
  hist(residuals(model),main="Histogram of residuals",freq=F, xlab="Residuals", ylab= "Freq", col="palegreen", col.main="darkblue")
  x=seq(-5e-15,9e-15,5e-15)
  curve(dnorm(x,mean(residuals(model)),sd(residuals(model))),add=T,lwd=2, col="red", lty=1)
  # qq plot
  qqPlot(residuals(model), pch=19, col="dark blue", col.lines="red", xlab="Pred quantiles", ylab="Obs quantiles") 
  
  dev.off()
  
}

### P0707 KRN Year 2 ###
traits <- colnames(p0707_krn_yr2)[10:ncol(p0707_krn_yr2)]

for(trait in traits){
  pdf(paste0("plots_", trait, ".pdf"), width = 15, height = 5)
  par(mfrow=c(1,3))
  
  # Histogram of raw data
  hist(p0707_krn_yr2[,trait],
       main=paste0("Histogram of raw\n", trait, " values"),
       xlab=trait,
       col="cadetblue")
  
  # Stripchart of raw data across storage
  stripchart(p0707_krn_yr2[,trait] ~p0707_krn_yr2$Storage,
             main=paste0("Stripchart of raw\n", trait, " values"),
             xlab="Storage",
             ylab=trait,
             vertical=TRUE, 
             method="jitter",
             bg="cadetblue",
             pch=21)
  
  # Stripchart of raw data across temp
  stripchart(p0707_krn_yr2[,trait] ~p0707_krn_yr2$Temp,
             main=paste0("Stripchart of raw\n", trait, " values"),
             xlab="Temp",
             ylab=trait,
             vertical=TRUE, 
             method="jitter",
             bg="cadetblue",
             pch=21)
  
  dev.off()
  
  if(paste0("stats_", trait, ".txt") %in% list.files()){
    system(paste0("rm stats_", trait, ".txt"))}
  # Summary statistics of the trait
  summary <- summary(p0707_krn_yr2[,trait], )
  out <- capture.output(summary)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Test for normality
  normality <- shapiro.test(p0707_krn_yr2[,trait])
  out <- capture.output(normality)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Run an ANOVA (switched : in Env:Rep and Rep:Block for nesting)
  model <- lm(get(trait) ~ Temp + Storage + Temp:Storage + Rep, data=p0707_krn_yr2)
  anova <- anova(model)
  out <- capture.output(anova)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Run a random effects model
  model.1 <- lmer(get(trait) ~ Temp + Storage + Temp:Storage + (1|Rep), data = p0707_krn_yr2, REML = TRUE)
  # Decreasing stopping tolerances
  strict_tol <- lmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8))
  if (all(model.1@optinfo$optimizer=="nloptwrap")) {
    model <- update(model.1, control=strict_tol)
  }
  
  # Summary of random effects
  summary <- summary(model, correlation=FALSE)
  out <- capture.output(summary)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  # ANOVA of mixed linear model
  anova <- anova(model)
  out <- capture.output(anova)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  # Write out residuals from ANOVA
  write.table(resid(model), paste0("resids_", trait, ".csv"), col.names=F, row.names=F, sep=",")
  
  pdf(paste0("assumptions_", trait, ".pdf"), width = 15, height = 5)
  par(mfrow=c(1,3))
  
  # Model Fit with REML
  plot(fitted(model), residuals(model), pch=19, col="dark blue", ylab="Residuals", xlab="Predicted")
  abline(h=0,col="red", lwd=1, lty=1)
  # histogram of residuals
  hist(residuals(model),main="Histogram of residuals",freq=F, xlab="Residuals", ylab= "Freq", col="palegreen", col.main="darkblue")
  x=seq(-5e-15,9e-15,5e-15)
  curve(dnorm(x,mean(residuals(model)),sd(residuals(model))),add=T,lwd=2, col="red", lty=1)
  # qq plot
  qqPlot(residuals(model), pch=19, col="dark blue", col.lines="red", xlab="Pred quantiles", ylab="Obs quantiles") 
  
  dev.off()
  
}

### P1306W KRN Year 1 ###
traits <- colnames(p1306w_krn_yr1)[10:ncol(p1306w_krn_yr1)]

for(trait in traits){
  pdf(paste0("plots_", trait, ".pdf"), width = 15, height = 5)
  par(mfrow=c(1,3))
  
  # Histogram of raw data
  hist(p1306w_krn_yr1[,trait],
       main=paste0("Histogram of raw\n", trait, " values"),
       xlab=trait,
       col="cadetblue")
  
  # Stripchart of raw data across storage
  stripchart(p1306w_krn_yr1[,trait] ~p1306w_krn_yr1$Storage,
             main=paste0("Stripchart of raw\n", trait, " values"),
             xlab="Storage",
             ylab=trait,
             vertical=TRUE, 
             method="jitter",
             bg="cadetblue",
             pch=21)
  
  # Stripchart of raw data across temp
  stripchart(p1306w_krn_yr1[,trait] ~p1306w_krn_yr1$Temp,
             main=paste0("Stripchart of raw\n", trait, " values"),
             xlab="Temp",
             ylab=trait,
             vertical=TRUE, 
             method="jitter",
             bg="cadetblue",
             pch=21)
  
  dev.off()
  
  if(paste0("stats_", trait, ".txt") %in% list.files()){
    system(paste0("rm stats_", trait, ".txt"))}
  # Summary statistics of the trait
  summary <- summary(p1306w_krn_yr1[,trait], )
  out <- capture.output(summary)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Test for normality
  normality <- shapiro.test(p1306w_krn_yr1[,trait])
  out <- capture.output(normality)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Run an ANOVA (switched : in Env:Rep and Rep:Block for nesting)
  model <- lm(get(trait) ~ Temp + Storage + Temp:Storage + Rep, data=p1306w_krn_yr1)
  anova <- anova(model)
  out <- capture.output(anova)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Run a random effects model
  model.1 <- lmer(get(trait) ~ Temp + Storage + Temp:Storage + (1|Rep), data = p1306w_krn_yr1, REML = TRUE)
  # Decreasing stopping tolerances
  strict_tol <- lmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8))
  if (all(model.1@optinfo$optimizer=="nloptwrap")) {
    model <- update(model.1, control=strict_tol)
  }
  
  # Summary of random effects
  summary <- summary(model, correlation=FALSE)
  out <- capture.output(summary)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  # ANOVA of mixed linear model
  anova <- anova(model)
  out <- capture.output(anova)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  # Write out residuals from ANOVA
  write.table(resid(model), paste0("resids_", trait, ".csv"), col.names=F, row.names=F, sep=",")
  
  pdf(paste0("assumptions_", trait, ".pdf"), width = 15, height = 5)
  par(mfrow=c(1,3))
  
  # Model Fit with REML
  plot(fitted(model), residuals(model), pch=19, col="dark blue", ylab="Residuals", xlab="Predicted")
  abline(h=0,col="red", lwd=1, lty=1)
  # histogram of residuals
  hist(residuals(model),main="Histogram of residuals",freq=F, xlab="Residuals", ylab= "Freq", col="palegreen", col.main="darkblue")
  x=seq(-5e-15,9e-15,5e-15)
  curve(dnorm(x,mean(residuals(model)),sd(residuals(model))),add=T,lwd=2, col="red", lty=1)
  # qq plot
  qqPlot(residuals(model), pch=19, col="dark blue", col.lines="red", xlab="Pred quantiles", ylab="Obs quantiles") 
  
  dev.off()
  
}

### P1306W KRN Year 2 ###
traits <- colnames(p1306w_krn_yr2)[10:ncol(p1306w_krn_yr2)]

for(trait in traits){
  pdf(paste0("plots_", trait, ".pdf"), width = 15, height = 5)
  par(mfrow=c(1,3))
  
  # Histogram of raw data
  hist(p1306w_krn_yr2[,trait],
       main=paste0("Histogram of raw\n", trait, " values"),
       xlab=trait,
       col="cadetblue")
  
  # Stripchart of raw data across storage
  stripchart(p1306w_krn_yr2[,trait] ~p1306w_krn_yr2$Storage,
             main=paste0("Stripchart of raw\n", trait, " values"),
             xlab="Storage",
             ylab=trait,
             vertical=TRUE, 
             method="jitter",
             bg="cadetblue",
             pch=21)
  
  # Stripchart of raw data across temp
  stripchart(p1306w_krn_yr2[,trait] ~p1306w_krn_yr2$Temp,
             main=paste0("Stripchart of raw\n", trait, " values"),
             xlab="Temp",
             ylab=trait,
             vertical=TRUE, 
             method="jitter",
             bg="cadetblue",
             pch=21)
  
  dev.off()
  
  if(paste0("stats_", trait, ".txt") %in% list.files()){
    system(paste0("rm stats_", trait, ".txt"))}
  # Summary statistics of the trait
  summary <- summary(p1306w_krn_yr2[,trait], )
  out <- capture.output(summary)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Test for normality
  normality <- shapiro.test(p1306w_krn_yr2[,trait])
  out <- capture.output(normality)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Run an ANOVA (switched : in Env:Rep and Rep:Block for nesting)
  model <- lm(get(trait) ~ Temp + Storage + Temp:Storage + Rep, data=p1306w_krn_yr2)
  anova <- anova(model)
  out <- capture.output(anova)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  
  # Run a random effects model
  model.1 <- lmer(get(trait) ~ Temp + Storage + Temp:Storage + (1|Rep), data = p1306w_krn_yr2, REML = TRUE)
  # Decreasing stopping tolerances
  strict_tol <- lmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8))
  if (all(model.1@optinfo$optimizer=="nloptwrap")) {
    model <- update(model.1, control=strict_tol)
  }
  
  # Summary of random effects
  summary <- summary(model, correlation=FALSE)
  out <- capture.output(summary)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  # ANOVA of mixed linear model
  anova <- anova(model)
  out <- capture.output(anova)
  cat(out, file=paste0("stats_",trait,".txt"), sep="\n", append=TRUE)
  # Write out residuals from ANOVA
  write.table(resid(model), paste0("resids_", trait, ".csv"), col.names=F, row.names=F, sep=",")
  
  pdf(paste0("assumptions_", trait, ".pdf"), width = 15, height = 5)
  par(mfrow=c(1,3))
  
  # Model Fit with REML
  plot(fitted(model), residuals(model), pch=19, col="dark blue", ylab="Residuals", xlab="Predicted")
  abline(h=0,col="red", lwd=1, lty=1)
  # histogram of residuals
  hist(residuals(model),main="Histogram of residuals",freq=F, xlab="Residuals", ylab= "Freq", col="palegreen", col.main="darkblue")
  x=seq(-5e-15,9e-15,5e-15)
  curve(dnorm(x,mean(residuals(model)),sd(residuals(model))),add=T,lwd=2, col="red", lty=1)
  # qq plot
  qqPlot(residuals(model), pch=19, col="dark blue", col.lines="red", xlab="Pred quantiles", ylab="Obs quantiles") 
  
  dev.off()
  
}

#####################################################################################################################################

### Calculating Emmeans for Pairwise Difference Comparisons with Tukey's HSD ####

## Clearing the global environment
rm(list=ls(all=TRUE))

## Setting up the working directory
getwd()
setwd("/Users/jonathanrenk/Desktop/Storage Work/")

## Loading packages
library("car")
library("lme4")
library("dfoptim")
library("lmerTest")
library("Hmisc")
library("multcomp")
library("multcompView")
library("emmeans")


## Loading in the data
data <- read.csv("data/master_data_final.csv")

str(data)
summary(data)

## Setting variables as factors for hybrid, sample, year, hotplate, rep, temp, and storage
data[,1] <- as.factor(data[,1])
data[,2] <- as.factor(data[,2])
data[,3] <- as.factor(data[,3])
data[,4] <- as.factor(data[,4])
data[,5] <- as.factor(data[,5])
data[,6] <- as.factor(data[,6])
data[,7] <- as.factor(data[,7])

str(data)

## Subsetting the data by sample type
krn <- data.frame(data [ which(data$Sample == 'KRN'),])
wk <- data.frame(data [ which(data$Sample == 'WK'),])

## Subsetting the year with washed kernels
wk_yr1 <- data.frame(wk [ which(wk$Year == '1'),])
wk_yr2 <- data.frame(wk [ which(wk$Year == '2'),])

## Subsetting for hybrid within year
p0707_wk_yr1 <- data.frame(wk_yr1 [ which(wk_yr1$Hybrid == 'P0707'),])
p0707_wk_yr2 <- data.frame(wk_yr2 [ which(wk_yr2$Hybrid == 'P0707'),])
p1306w_wk_yr1 <- data.frame(wk_yr1 [ which(wk_yr1$Hybrid == 'P1306W'),])
p1306w_wk_yr2 <- data.frame(wk_yr2 [ which(wk_yr2$Hybrid == 'P1306W'),])

## Subsetting the year with kernels
krn_yr1 <- data.frame(krn [ which(krn$Year == '1'),])
krn_yr2 <- data.frame(krn [ which(krn$Year == '2'),])

## Subsetting for hybrid within year
p0707_krn_yr1 <- data.frame(krn_yr1 [ which(krn_yr1$Hybrid == 'P0707'),])
p0707_krn_yr2 <- data.frame(krn_yr2 [ which(krn_yr2$Hybrid == 'P0707'),])
p1306w_krn_yr1 <- data.frame(krn_yr1 [ which(krn_yr1$Hybrid == 'P1306W'),])
p1306w_krn_yr2 <- data.frame(krn_yr2 [ which(krn_yr2$Hybrid == 'P1306W'),])

#####################################################################################
#### emmeans portion #####
#####################################################################################
# switch for every trait and subsample of dataset #
model.1 <- lmer(Sucrose_g.100g_dwb ~ Temp + Storage + Temp:Storage + (1|Rep), data = p0707_krn_yr1, REML = TRUE)
summary(model.1, correlation=FALSE)
anova(model.1) # fixed effects
rand(model.1)

# Decreasing stopping tolerances
strict_tol <- lmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8))
if (all(model.1@optinfo$optimizer=="nloptwrap")) {
  model <- update(model.1, control=strict_tol)
}

# Emmeans
emm1 <- emmeans(model.1, ~ Temp:Storage)
cld(emm1)

new_data <- as.data.frame(cld(emm1))
write.table(new_data, file="P0707_KRNY1_Sucrose_emmeans.csv", col.names=T, row.names=F, sep=",")

