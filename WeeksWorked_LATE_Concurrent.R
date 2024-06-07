install.packages("metafor")
library(metafor)
install.packages("clubSandwich")
library (clubSandwich)
install.packages("DescTools")
library(DescTools)

#Import "WeeksWorked_LATE_Concurrent.csv" dataset into R. Set "Headings" option to "yes" prior to importing. 

#Preparing the dataset for analysis
EmployContinuousLATE=WeeksWorked_LATE_Concurrent
EmployContinuousLATE$Variance<-EmployContinuousLATE$SE^2
options(scipen=999)

#----------------------------------------------MAIN ANALYSIS/TEST FOR HETEROGENEITY-------------------
#Estimating the overall effect
overalleffect <- rma.mv(OLS, Variance, random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=
                          TRUE, data=EmployContinuousLATE)
overalleffect

#Computing I2 statistics 
n <- length(EmployContinuousLATE$Variance)
list.inverse.variances <- 1 / (EmployContinuousLATE$Variance)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (EmployContinuousLATE$Variance^2)
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

#---------------------------------------------EXPLORING OUTLIERS----------------------
res <- rma(OLS, Variance, data = EmployContinuousLATE)
rstudent(res)

#--WINSORIZED MODEL
windsorizedvector<-Winsorize(EmployContinuousLATE$OLS, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
overalleffectWIND <- rma.mv(windsorizedvector, Variance, random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=
                          TRUE, data=EmployContinuousLATE)
overalleffectWIND

#I2 (winsorized)
n <- length(EmployContinuousLATE$Variance)
list.inverse.variances <- 1 / (EmployContinuousLATE$Variance)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (EmployContinuousLATE$Variance^2)
sum.inverse.variances.square <-
  sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances
denominator <- squared.sum.inverse.variances -
  sum.inverse.variances.square
estimated.sampling.variance <- numerator / denominator
I2_1 <- (estimated.sampling.variance) / (overalleffectWIND$sigma2[1]
                                         + overalleffectWIND$sigma2[2] + estimated.sampling.variance)
I2_2 <- (overalleffectWIND$sigma2[1]) / (overalleffectWIND$sigma2[1]
                                        + overalleffectWIND$sigma2[2] + estimated.sampling.variance)
I2_3 <- (overalleffectWIND$sigma2[2]) / (overalleffectWIND$sigma2[1]
                                        + overalleffect$sigma2[2] + estimated.sampling.variance)
amountvariancelevel1 <- I2_1 * 100
amountvariancelevel2 <- I2_2 * 100
amountvariancelevel3 <- I2_3 * 100

amountvariancelevel1 
amountvariancelevel2 
amountvariancelevel3

 
#-------------------------------------ASSESSING PUBLICATION BIAS----------
#PET (winsorized)
overalleffectPETWIND <- rma.mv(windsorizedvector, Variance, 
                           mods = ~ SE,
                           random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), 
                           tdist= TRUE, data=EmployContinuousLATE)
overalleffectPETWIND

#Eggers RVE (winsorized)
egger_multi <- rma.mv(windsorizedvector, Variance, random = ~ 1 | Study_ID/EffectSize_ID, mods = ~ SE, data = EmployContinuousLATE)
coef_test(egger_multi, vcov = "CR2")

#Funnel Plot
funnel(overalleffect,xlab = "Average Marginal Effect", yaxis = "seinv")

#Funnel Plot (winsorized) 
funnel(overalleffectWIND,xlab = "Average Marginal Effect (Winsorized)", yaxis = "seinv")

# --------------------------- UNIVARIATE MODERATOR ANALYSIS---------------------

#----------------------------------------------------MARITIAL STATUS--------------------
#Single mothers as reference group 

Single <- rma.mv(OLS, Variance, mods = ~ MaritalStatus_TwoParant, random = list(~ 1 | EffectSize_ID, ~
                                                                                        1 | Study_ID), tdist=TRUE, data=EmployContinuousLATE)
summary(Single, digits=3)


#Two Parent as reference group 

TwoParentMarried <- rma.mv(OLS, Variance, mods = ~ MaritalStatus_Single, random = list(~ 1 | EffectSize_ID, ~
                                                                                        1 | Study_ID), tdist=TRUE, data=EmployContinuousLATE)
summary(TwoParentMarried, digits=3)

#-----------------------------------YOUNGEST CHILD IN THE HOUSEHOLD---------------------

#Is youngest child as reference group 
IsYoungest <- rma.mv(OLS, Variance, mods = ~ NotYoungestChildHousehold, random = list(~ 1 | EffectSize_ID, ~
                                                                                          1 | Study_ID), tdist=TRUE, data=EmployContinuousLATE)
summary(IsYoungest, digits=3)

#Not youngest child as reference group 
NotYoungest <- rma.mv(OLS, Variance, mods = ~ ISYoungestChildHousehold, random = list(~ 1 | EffectSize_ID, ~
                                                                                          1 | Study_ID), tdist=TRUE, data=EmployContinuousLATE)
summary(NotYoungest, digits=3)

#-----------------------------------NUBMBER OF CHILDREN IN THE HOUSEHOLD 0 to <19---------------------

NumChildren0to19 <- rma.mv(OLS, Variance, mods = ~ Number.of.Other.Children.in.the.Household.Age.0.to..19_AVG0.9, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                  1 | Study_ID), tdist=TRUE, data=EmployContinuousLATE)
summary(NumChildren0to19, digits=3)

#--------------------------------------WEEK WORKED INDICES ------------
#General Weeks Worked as reference group 
GeneralWeeksWorked <- rma.mv(OLS, Variance, mods = ~ AnnualWeeksWorked_Above4Hours + AnnualWeeksWorked_Above30Hours, random = list(~
                                                                                                     1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployContinuousLATE)
summary(GeneralWeeksWorked, digits=3)

# Weeks Worked Aove 4 Hours as reference group 
WeeksWorkedAbove4Hours <- rma.mv(OLS, Variance, mods = ~ AnnualWeeksWorked_General + AnnualWeeksWorked_Above30Hours, random = list(~
                                                                                                                                     1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployContinuousLATE)
summary(WeeksWorkedAbove4Hours, digits=3)

# Weeks Worked Above 4 Hours as reference group 
WeeksWorkedAbove30Hours <- rma.mv(OLS, Variance, mods = ~ AnnualWeeksWorked_General + AnnualWeeksWorked_Above4Hours, random = list(~
                                                                                                                                     1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployContinuousLATE)
summary(WeeksWorkedAbove30Hours, digits=3)

#-------------------------------------------------------MULTIPLE MODERATOR ANALYSIS-------------------------------------------
multiplemoderator <- rma.mv(windsorizedvector, Variance, mods = ~ 
                             ISYoungestChildHousehold + 
                              MaritalStatus_TwoParant +
                              Number.of.Other.Children.in.the.Household.Age.0.to..19_AVG0.9+
                              Number.of.Other.Children.in.the.Household.Age.0.to..6_AVG1.5 +
                              AnnualWeeksWorked_Above4Hours + 
                              AnnualWeeksWorked_Above30Hours, 
                            random =list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), 
                            tdist=TRUE, data=EmployContinuousLATE)

summary(multiplemoderator, digits=3)




