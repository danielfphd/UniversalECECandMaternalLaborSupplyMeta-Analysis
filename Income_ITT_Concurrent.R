#Install packages 
install.packages("metafor")
library(metafor)
install.packages("clubSandwich")
library (clubSandwich)

#Import "Income_ITT_Concurrent.csv" dataset into R. Set "Headings" option to "yes" prior to importing. 

#Preparing the dataset for analysis
IncomeITT=Income_ITT_Concurrent
options(scipen=999) 

#---------MAIN ANALYSIS/TEST FOR HETEROGENEITY-------------------
#Estimating the overall effect
overalleffect <- rma.mv(PartialCorrelation, Variance, random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=
                          TRUE, data=IncomeITT)
overalleffect

#Computing I2 statistics 
n <- length(IncomeITT$Variance)
list.inverse.variances <- 1 / (IncomeITT$Variance)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (IncomeITT$Variance^2)
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

#-----------OUTLIERS----------------------
res <- rma(PartialCorrelation, Variance, data = IncomeITT)
rstudent(res)

#-------------------------------------ASSESSING PUBLICATION BIAS----------
#FAT-PET
overalleffectPET <- rma.mv(PartialCorrelation, Variance, 
                           mods = ~ SEofPC,
                           random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), 
                           tdist= TRUE, data=IncomeITT)
overalleffectPET

#Egger's Regression Test
egger_multi <- rma.mv(PartialCorrelation, Variance, random = ~ 1 | Study_ID/EffectSize_ID, mods = ~ SEofPC, data = IncomeITT, verbose=TRUE, control=list(rel.tol=1e-8))
coef_test(egger_multi, vcov = "CR2")

#funnel plot
funnel(overalleffect,xlab = "Partial Correlation Coefficeint", yaxis = "seinv")
