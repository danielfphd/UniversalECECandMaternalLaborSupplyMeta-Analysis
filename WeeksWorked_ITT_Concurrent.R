#Install packages 
install.packages("metafor")
library(metafor)
install.packages("clubSandwich")
library (clubSandwich)

#Import "WeeksWorked_ITT_Concurrent" dataset into R. Set "Headings" option to "yes" prior to importing. 

#Preparing the dataset for analysis
EmployContinuousITT=WeeksWorked_ITT_Concurrent
EmployContinuousITT <- EmployContinuousITT[-c(6:6),]
EmployContinuousITT$Variance<-EmployContinuousITT$SE^2
options(scipen=999)

#----------MAIN ANALYSIS/TEST FOR HETERGENEITY-------------------
#Estimating the overall effect
overalleffect <- rma.mv(OLS, Variance, random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=
                          TRUE, data=EmployContinuousITT)
overalleffect

#Computing I2 statistics 
n <- length(EmployContinuousITT$Variance)
list.inverse.variances <- 1 / (EmployContinuousITT$Variance)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (EmployContinuousITT$Variance^2)
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


#-----EXPLORING OUTLIERS----------------------

res <- rma(OLS, Variance, data = EmployContinuousITT)
rstudent(res)

#-------------------------------------ASSESSING PUBLICATION BIAS----------
#PET 
overalleffectPET <- rma.mv(OLS, Variance, 
                           mods = ~ SE,
                           random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), 
                           tdist= TRUE, data=EmployContinuousITT)
overalleffectPET


#Egger's RVE
egger_multi <- rma.mv(OLS, Variance, random = ~ 1 | Study_ID/EffectSize_ID, mods = ~ SE, data = EmployContinuousITT)
coef_test(egger_multi, vcov = "CR2")

#Funnel Plot
funnel(overalleffect,xlab = "Average Marginal Effect", yaxis = "seinv")

#   ---------------------------UNIVARIATE MODERATOR ANALYSIS---------------------


#                                           --------STUDY YEAR (CONTINUOUS)--------
studyyear <- rma.mv(PartialCorrelation, Variance, mods = ~ Study.Year_AVG2019, random = list(~ 1 | EffectSize_ID, ~
                                                                                               1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(studyyear, digits=3)

                                                        # ----PUBLICATION TYPE----
# Working/discussion paper as reference group 
notpublished <- rma.mv(PartialCorrelation, Variance, mods = ~ TypeofPublication_Journal, random = list(~ 1 | EffectSize_ID, ~
                                                                                                         1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(notpublished, digits=3)

# Journal as reference group 

published <- rma.mv(PartialCorrelation, Variance, mods = ~ TypeofPublication_WorkingPaper, random = list(~ 1 | EffectSize_ID, ~
                                                                                                           1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(published, digits=3)

#-----------------------COUNTRY REGION (Moderator with 4 Levels) -----------
#North America as the reference group 
CountryRegionNorthAm <- rma.mv(PartialCorrelation, Variance, mods = ~ Country_EuropeCentralAsia, random = list(~
                                                                                                                                                           1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(CountryRegionNorthAm, digits=3)

#Europe/Central Asia as Reference Group

CountryRegionEuropeCenAsia <- rma.mv(PartialCorrelation, Variance, mods = ~ Country_NorthAmerica, random = list(~
                                                                                                                                                            1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(CountryRegionEuropeCenAsia, digits=3)

# --------------------------------------------------COUNTRY INCOME ----------------------------
#All studies report on high income countries 

#--------------------------------------------MATERNAL AGE (CONTINUOUS)------------------  
#only one study reports on maternal age 

#----------------------------------------------------MARITIAL STATUS--------------------
#Mixed Married as reference group 

MixedMarried <- rma.mv(PartialCorrelation, Variance, mods = ~ MaritalStatus_TwoParant, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(MixedMarried, digits=3)


#Two Parent as reference group 

TwoParentMarried <- rma.mv(PartialCorrelation, Variance, mods = ~ MaritalStatus_Mixed, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(TwoParentMarried, digits=3)

#-----------------------------------------------------IMMIGRANT---------------
#only one study reports on mother's immigration status

#-------------------------------------------URBAN VS. RURAL REGIONS-------------
#only one study report on urban/rural status

#---------------------------------MATERNAL EDUCATION AS AN ORDINAL VARIABLE-------------
#all studies report on mother's with mixed education levels (lower/higher than bachelor)

#---------------------------------MATERNAL EDUCATION IN YEARS- CONTINUOUS VARIABLE-------------
#no studies report on maternal education in years 

#-----------------------------NUMBER OF CHIDREN IN HH 0 to 6 CONTINUOUS VARIABLE---------
#Only 2 studies report on number of children 0 to 6 

#-----------------------------NUMBER OF CHIDREN IN HH 0 to 19 CONTINUOUS VARIABLE---------
#Only 2 studies report on number of children 0 to 6 

#-------------------------------COUNTRY LEVEL FEMALE LFP RATE (CONTINUOUS)-------------

FemaleLFPRate <- rma.mv(PartialCorrelation, Variance, mods = ~ FemaleLFPRate_AVG74, random = list(~ 1 | EffectSize_ID, ~
                                                                                                    1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(FemaleLFPRate, digits=3)

#-------------------------------COUNTRY LEVEL FEMALE PT Participation RATE (CONTINUOUS)-------------
#Data only available for 2 countries 

#-----------------------------------YOUNGEST CHILD IN THE HOUSEHOLD---------------------
#Not youngest child dropped because no studies report on it 

#Mixed youngest child as reference group 
MixedYoungest <- rma.mv(PartialCorrelation, Variance, mods = ~ ISYoungestChildHousehold, random = list(~ 1 | EffectSize_ID, ~
                                                                                                         1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(MixedYoungest, digits=3)

#Is youngest child as reference group 
IsYoungest <- rma.mv(PartialCorrelation, Variance, mods = ~ MixedYoungestChildHousehold, random = list(~ 1 | EffectSize_ID, ~
                                                                                                         1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(IsYoungest, digits=3)

#-----------------------------------PROGRAM DELIVERY LEVEL (3 LEVELS)---------------------

#National/Federal level as the reference group 
NationalFederal <- rma.mv(PartialCorrelation, Variance, mods = ~ DeliveryLevel_StateProvincia, random = list(~
                                                                                                                                         1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(NationalFederal, digits=3)

#State level as the reference group 
StateLevel <- rma.mv(PartialCorrelation, Variance, mods = ~ DeliveryLevel_NationalFederal, random = list(~
                                                                                                                                     1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(StateLevel, digits=3)

#--------------------------------------AUSPICE TYPE---------------------
# Majority private delivery dropped because no studies report on it

#Mixed delivery as reference group
MixedAuspice <- rma.mv(PartialCorrelation, Variance, mods = ~ ProgramDeliveryPublic, random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(MixedAuspice, digits=3)

#Public delivery as reference group
PublicAuspice <- rma.mv(PartialCorrelation, Variance, mods = ~ ProgramDeliveryMixed, random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(PublicAuspice, digits=3)


#--------------------------------------DAILY OPERATING HOURS---------------------
#Only one study reports on daily operating hours 

#--------------------------------------ECE GARUNTEED YEAR ROUND---------------------
#only 1 study report on whether ECE is offered year round 

#--------------------------------------COST OF ECE---------------------

#Sliding Scale as Reference Group 

SlidingScale <- rma.mv(PartialCorrelation, Variance, mods = ~ CostofECE_Free, random = list(~ 1 | EffectSize_ID, ~
                                                                                              1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary( SlidingScale, digits=3)

#Free programs as Reference Group 

FreeProgram <- rma.mv(PartialCorrelation, Variance, mods = ~ CostofECE_SlidingScale, random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary( FreeProgram, digits=3)

#---------------------------------------RIGHT TO ACCESS ECE (2 LEVELS)-----------------

#No legal provisions as reference group 
NoProvisions <- rma.mv(PartialCorrelation, Variance, mods = ~ AccesstoECE_LegalRight, random = list(~
                                                                                                                               1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(NoProvisions, digits=3)

#legal provision as reference group 
LegalProvision <- rma.mv(PartialCorrelation, Variance, mods = ~ AccesstoECE_NoLegalProvisions, random = list(~
                                                                                                                                        1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(LegalProvision, digits=3)

#---------------------------------------YEAR OF POLICY IMPLEMENTATION (CONTINUOUS)-----------------
#only two studies report on the year ofl policy implementation 

#---------------------------------------AGE OF ELIGIBILTY (3 LEVELS)-----------------

#Younger than 3 y/o as reference group 
ThreeandYounger <- rma.mv(PartialCorrelation, Variance, mods = ~ ChildAge_3andOlder + ChildAge_Mixed, random = list(~
                                                                                                                      1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(ThreeandYounger, digits=3)

#Older than 3 y/o as reference group 
ThreeandOlder <- rma.mv(PartialCorrelation, Variance, mods = ~ ChildAge_Youngerthan3 + ChildAge_Mixed, random = list(~
                                                                                                                       1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(ThreeandOlder, digits=3)

#Mixed 0 to 6 as reference as reference group 
Mixed0to6 <- rma.mv(PartialCorrelation, Variance, mods = ~ ChildAge_Youngerthan3 + ChildAge_3andOlder, random = list(~
                                                                                                                       1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(Mixed0to6, digits=3)

#-------------------------------------------STUDY DESIGN (2 LEVELS)----------------------------

#DiD as reference group 
DiD <- rma.mv(PartialCorrelation, Variance, mods = ~ StudyDesign_TripleDiffandDiff, random = list(~
                                                                                                                                                     1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(DiD, digits=3)

#Triple DiD as Reference group
TripleDiD <- rma.mv(PartialCorrelation, Variance, mods = ~ StudyDesign_DiffandDiff, random = list(~
                                                                                                                                                     1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(TripleDiD, digits=3)

#---------------------------------------------------------CROWDING OUT----------------------------
#ONLY 2 STUDIES REPORT ON CROWDING OUT 

#--------------------------------------------------------ROBUSTNESS CHECK -------------

#No robustness check as reference 
NoRobustness <- rma.mv(PartialCorrelation, Variance, mods = ~ RobustnessAnalysis, random = list(~ 1 | EffectSize_ID, ~
                                                                                                  1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(NoRobustness, digits=3)

#Has robustness check as reference 
Robustness <- rma.mv(PartialCorrelation, Variance, mods = ~ NORobustnessAnalysis, random = list(~ 1 | EffectSize_ID, ~
                                                                                                  1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(Robustness, digits=3)

#--------------------------------------------------------Censored Outcomes -------------

#Not censored as reference 
NotCensroed <- rma.mv(PartialCorrelation, Variance, mods = ~ Censored, random = list(~ 1 | EffectSize_ID, ~
                                                                                   1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(NoRobustness, digits=3)

#Has robustness check as reference 
Censored <- rma.mv(PartialCorrelation, Variance, mods = ~ NotCensored, random = list(~ 1 | EffectSize_ID, ~
                                                                                   1 | Study_ID), tdist=TRUE, data=EmployContinuousITT)
summary(Robustness, digits=3)



