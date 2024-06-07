#Install packages 
install.packages("metafor")
library(metafor)
install.packages("clubSandwich")
library (clubSandwich)

#Import "Intensity of Work_LATE_Concurrent.csv" dataset into R. Set "Headings" option to "yes" prior to importing. 

#Preparing the dataset for analysis
IntensityofWorkLATE=Intensity.of.Work_LATE_Concurrent
IntensityofWorkLATE$Variance<-IntensityofWorkLATE$SE^2
options(scipen=999)

#------------MAIN ANALYSIS/TEST FOR HETEROGENEITY-------------------
#Estimating the overall effect
overalleffect <- rma.mv(MarginalEffect, Variance, random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=
                          TRUE, data=IntensityofWorkLATE)
overalleffect

#Computing I2 statistics 
n <- length(IntensityofWorkLATE$Variance)
list.inverse.variances <- 1 / (IntensityofWorkLATE$Variance)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (IntensityofWorkLATE$Variance^2)
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

#-------- OUTLIERS----------------------
res <- rma(MarginalEffect, Variance, data = IntensityofWorkLATE)
rstudent(res)

#------------PUBLICATION BIAS-----------------

#PET
overalleffectPET <- rma.mv(MarginalEffect, Variance, 
                           mods = ~ SE,
                           random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), 
                           tdist= TRUE, data=IntensityofWorkLATE)
overalleffectPET


#Egger's RVE
egger_multi <- rma.mv(MarginalEffect, Variance, random = ~ 1 | Study_ID/EffectSize_ID, mods = ~ SE, data = IntensityofWorkLATE)
coef_test(egger_multi, vcov = "CR2")

#Funnel Plot
funnel(overalleffect,xlab = "Average Marginal Effect", yaxis = "seinv")

#-------UNIVARIATE MODERATOR ANALYSIS---------------------

#----------------MARITIAL STATUS--------------------

#Sinlge parents as reference group 

Single <- rma.mv(MarginalEffect, Variance, mods = ~ MaritalStatus_TwoParant, random = list(~ 1 | EffectSize_ID, ~
                                                                                             1 | Study_ID), tdist=TRUE, data=IntensityofWorkLATE)
summary(Single, digits=3)


#Majority Married as reference group 

TwoParentMarried <- rma.mv(MarginalEffect, Variance, mods = ~ MaritalStatus_Single, random = list(~ 1 | EffectSize_ID, ~
                                                                                                    1 | Study_ID), tdist=TRUE, data=IntensityofWorkLATE)
summary(TwoParentMarried, digits=3)

#-----------------------------------YOUNGEST CHILD IN THE HOUSEHOLD---------------------
#Not youngest child as reference group 
NotYoungest <- rma.mv(MarginalEffect, Variance, mods = ~ ISYoungestChildHousehold, random = list(~ 1 | EffectSize_ID, ~
                                                                                                   1 | Study_ID), tdist=TRUE, data=IntensityofWorkLATE)
summary(NotYoungest, digits=3)


#Is youngest child as reference group 
IsYoungest <- rma.mv(MarginalEffect, Variance, mods = ~ NotYoungestChildHousehold, random = list(~ 1 | EffectSize_ID, ~
                                                                                                   1 | Study_ID), tdist=TRUE, data=IntensityofWorkLATE)
summary(IsYoungest, digits=3)

#--------------------------------------------------INDICE OF INTENSITY FO WORK (3 LEVELS)-----------------------

#PT Employment as reference group 
PTEmploy <- rma.mv(MarginalEffect, Variance, mods = ~ FT_Author.Defined + MarginalEmploy_Author.Defined, random = list(~
                                                                                        1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=IntensityofWorkLATE)
summary(PTEmploy, digits=3)

#Full-time employment as reference group 
FTEmploy <- rma.mv(MarginalEffect, Variance, mods = ~ PT_Author.Defined + MarginalEmploy_Author.Defined, random = list(~
                                                                                            1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=IntensityofWorkLATE)
summary(FTEmploy, digits=3)


#Marginal Employment as reference group 
MarginalEmploy<- rma.mv(MarginalEffect, Variance, mods = ~ FT_Author.Defined + PT_Author.Defined, random = list(~
                                                                                                                                         1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=IntensityofWorkLATE)
summary(MarginalEmploy, digits=3)


#--------------Multiple moderator model
overalleffectMultiple <- rma.mv(MarginalEffect, Variance, mods = ~ MaritalStatus_TwoParant + NotYoungestChildHousehold + FT_Author.Defined + PT_Author.Defined, random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=
                          TRUE, data=IntensityofWorkLATE)
overalleffectMultiple

