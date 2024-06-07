#Install packages 
install.packages("metafor")
library(metafor)
install.packages("clubSandwich")
library (clubSandwich)
install.packages("DescTools")
library(DescTools)

#Import "Intensity of Work_ITT_Concurrent.csv" dataset into R. Set "Headings" option to "yes" prior to importing. 

#Preparing the dataset for analysis
IntensityofWorkITT=Intensity.of.Work_ITT_Concurrent
IntensityofWorkITT<- IntensityofWorkITT[-c(19:25),]
IntensityofWorkITT$Variance<-IntensityofWorkITT$SE^2
options(scipen=999)

#------MAIN ANALYSIS/TEST FOR HETEROGENEITY-------------------
#Estimating the overall effect
overalleffect <- rma.mv(MarginalEffect, Variance, random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=
                          TRUE, data=IntensityofWorkITT)
overalleffect

#Computing I2 statistics 
n <- length(IntensityofWorkITT$Variance)
list.inverse.variances <- 1 / (IntensityofWorkITT$Variance)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (IntensityofWorkITT$Variance^2)
sum.inverse.variances.square <-
  sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances -
  sum.inverse.variances.square
estimated.sampling.variance <- numerator / denominator
I2_1 <- (estimated.sampling.variance) / (overalleffect$sigma2[1]
                                         + overalleffect$sigma2[2] + estimated.sampling.variance)
I2_2 <- (overalleffect$sigma2[1]) / (overalleffect$sigma2[1]
                                     + overalleffect$sigma2[2] + estimated.sampling.variance)
I2_3 <- (overalleffect$sigma2[2]) / (overalleffect$sigma2[1]
                                     + overalleffect$sigma2[2] + estimated.sampling.variance)
amountvariancelevel1 <- I2_1 * 100
amountvariancelevel2 <- I2_2 * 100
amountvariancelevel3 <- I2_3 * 100

amountvariancelevel1 
amountvariancelevel2 
amountvariancelevel3 

#-----OUTLIERS----------------------
res <- rma(MarginalEffect, Variance, data = IntensityofWorkITT)
rstudent(res)

#---WINSORIZED MODEL
windsorizedvector<-Winsorize(IntensityofWorkITT$MarginalEffect, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
overalleffectWind <- rma.mv(windsorizedvector, Variance, random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=
                              TRUE, data=IntensityofWorkITT)
overalleffectWind

#------------PUBLICATION BIAS-----------------

#PET (windsorized)
overalleffectPET <- rma.mv(windsorizedvector, Variance, 
                           mods = ~ SE,
                           random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), 
                           tdist= TRUE, data=IntensityofWorkITT)
overalleffectPET


#Egger's RVE
egger_multi <- rma.mv(windsorizedvector, Variance, random = ~ 1 | Study_ID/EffectSize_ID, mods = ~ SE, data = IntensityofWorkITT)
coef_test(egger_multi, vcov = "CR2")

#Funnel Plot
funnel(overalleffect,xlab = "Average Marginal Effect", yaxis = "seinv")

#Funnel Plot (Windsorized)
funnel(overalleffectWind,xlab = "Average Marginal Effect (Winsorized)", yaxis = "seinv")


