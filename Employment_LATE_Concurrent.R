#Install packages 
install.packages("metafor")
library(metafor)
install.packages("clubSandwich")
library (clubSandwich)
install.packages("DescTools")
library(DescTools)

#Import "Employment_LATE_Concurrent.csv" dataset into R. Set "Headings" option to "yes" prior to importing. 

#Preparing the dataset for analysis
EmployBinaryLATE=Employment_LATE_Concurrent
EmployBinaryLATE <- EmployBinaryLATE[-c(21:32),]
EmployBinaryLATE$Variance<-EmployBinaryLATE$SE^2
options(scipen=999)

#-----------------------------MAIN ANALYSIS/TEST FOR HETEROGENEITY-------------------
#Estimating the overall effect
overalleffect <- rma.mv(MarginalEffect, Variance, random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=
                          TRUE, data=EmployBinaryLATE)
overalleffect

#Computing I2 statistics 
n <- length(EmployBinaryLATE$Variance)
list.inverse.variances <- 1 / (EmployBinaryLATE$Variance)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (EmployBinaryLATE$Variance^2)
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

#------ OUTLIERS----------------------
res <- rma(MarginalEffect, Variance, mods = cbind(NumberoChildren.inHH_0to.6_AVG1.5, NumberoChildren.inHH_0to.19_AVG0.6, NotYoungestChildHousehold, ECENOTGuaranteedYearRound, CostofECE_Free, StudyDesign_RD, ChildAge_Youngerthan3), data = EmployBinaryLATE)
rstudent(res)

#---WINSORIZED MODEL
windsorizedvector<-Winsorize(EmployBinaryLATE$MarginalEffect, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),
                             na.rm = FALSE, type = 7)
overalleffectWind <- rma.mv(windsorizedvector, Variance, random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=
                              TRUE, data=EmployBinaryLATE)
overalleffectWind

#calculating I^2 for winsorized model
n <- length(EmployBinaryLATE$Variance)
list.inverse.variances <- 1 / (EmployBinaryLATE$Variance)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (EmployBinaryLATE$Variance^2)
sum.inverse.variances.square <-
  sum(list.inverse.variances.square)
numerator <- (n - 1) * sum.inverse.variances

denominator <- squared.sum.inverse.variances -
  sum.inverse.variances.square
estimated.sampling.variance <- numerator / denominator
I2_1 <- (estimated.sampling.variance) / (overalleffectWind$sigma2[1]
                                         + overalleffectWind$sigma2[2] + estimated.sampling.variance)
I2_2 <- (overalleffectWind$sigma2[1]) / (overalleffectWind$sigma2[1]
                                         + overalleffectWind$sigma2[2] + estimated.sampling.variance)
I2_3 <- (overalleffectWind$sigma2[2]) / (overalleffectWind$sigma2[1]
                                         + overalleffectWind$sigma2[2] + estimated.sampling.variance)
amountvariancelevel1 <- I2_1 * 100
amountvariancelevel2 <- I2_2 * 100
amountvariancelevel3 <- I2_3 * 100

amountvariancelevel1 
amountvariancelevel2 
amountvariancelevel3 

#-----------PUBLICATION BIAS ----------

#FAT-PET (Winsorized)
overalleffectWindPET <- rma.mv(windsorizedvector, Variance, mods = ~SE, random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=
                                 TRUE, data=EmployBinaryLATE)
overalleffectWindPET

#Egger's REV (Winsorized)
egger_multi <- rma.mv(windsorizedvector, Variance, random = ~ 1 | Study_ID/EffectSize_ID, mods = ~ SE, data = EmployBinaryLATE)
coef_test(egger_multi, vcov = "CR2")

#Funnel Plot (Winsorized)
funnel(overalleffectWind,xlab = "Average Marginal Effect (Winsorized)", yaxis = "seinv")


#---------------------------UNIVARIATE MODERATOR ANALYSIS---------------------

#----------Study Year (CONTINUOUS)------------------         

StudyYear <- rma.mv(MarginalEffect, Variance, mods = ~ Study.Year_AVG2104, random = list(~ 1 | EffectSize_ID, ~
                                                                                            1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(StudyYear, digits=3)

                    # ----PUBLICATION TYPE----
# Working/discussion paper as reference group 
notpublished <- rma.mv(MarginalEffect, Variance, mods = ~ TypeofPublication_Journal, random = list(~ 1 | EffectSize_ID, ~
                                                                                                         1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(notpublished, digits=3)

# Journal as reference group 

published <- rma.mv(MarginalEffect, Variance, mods = ~ TypeofPublication_WorkingPaper, random = list(~ 1 | EffectSize_ID, ~
                                                                                                           1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(published, digits=3)

                  #-----------------------COUNTRY REGION (Moderator with 3 Levels) -----------
#North America as the reference group 
CountryRegionNorthAm <- rma.mv(MarginalEffect, Variance, mods = ~ Country_EuropeCentralAsia + Country_LatinAmerica, random = list(~
                                                                                                                                        1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(CountryRegionNorthAm, digits=3)

#Europe/Central Asia as Reference Group

CountryRegionEuropeCenAsia <- rma.mv(MarginalEffect, Variance, mods = ~ Country_NorthAmerica + Country_LatinAmerica, random = list(~
                                                                                                                                         1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(CountryRegionEuropeCenAsia, digits=3)

#Latin American As Reference Group
CountryRegionLatinAm <- rma.mv(MarginalEffect, Variance, mods = ~ Country_NorthAmerica + Country_EuropeCentralAsia, random = list(~
                                                                                                                                        1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(CountryRegionLatinAm, digits=3)

# --------------------------------------------------COUNTRY INCOME ----------------------------

#Upper Middle Income Countries as Reference Group

UpperMiddleIncome <- rma.mv(MarginalEffect, Variance, mods = ~ CountryIncome_High, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(UpperMiddleIncome, digits=3)

#High Income Countries as Reference Group

HighIncome <- rma.mv(MarginalEffect, Variance, mods = ~ CountryIncome_UpperMiddle, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(UpperMiddleIncome, digits=3)

#--------------------------------------------MATERNAL AGE (CONTINUOUS)------------------         

MaternalAge <- rma.mv(MarginalEffect, Variance, mods = ~ MaternalAge_AVG32, random = list(~ 1 | EffectSize_ID, ~
                                                                                                 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(MaternalAge, digits=3)

#----------------------------------------------------MARITIAL STATUS--------------------

#MSinlge parents as reference group 

Single <- rma.mv(MarginalEffect, Variance, mods = ~ MaritalStatus_TwoParant, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(Single, digits=3)


#Majority Married as reference group 

TwoParentMarried <- rma.mv(MarginalEffect, Variance, mods = ~ MaritalStatus_Single, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(TwoParentMarried, digits=3)

    #-----------------------------------------------------IMMIGRANT---------------
#Majority Non-immigrants as reference category
NonImmgrnt <- rma.mv(MarginalEffect, Variance, mods = ~ MixedImmigrant, random = list(~ 1 | EffectSize_ID, ~
                                                                                            1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(NonImmgrnt, digits=3)

#Majority Mixed immigrants as reference category
MixedImmgrnt <- rma.mv(MarginalEffect, Variance, mods = ~ NonImmigrant, random = list(~ 1 | EffectSize_ID, ~
                                                                                            1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(MixedImmgrnt, digits=3)

#-------------------------------------------URBAN VS. RURAL REGIONS-------------
#only one study reported on urban residence 

#---------------------------------MATERNAL EDUCATION AS AN ORDINAL VARIABLE-------------

#Mixed Bachelors as reference category
MixedBachelors <- rma.mv(MarginalEffect, Variance, mods = ~ MaternalEducation_LessThanBachelors, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                     1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(MixedBachelors, digits=3)

#Less than Bachelors as reference category
LessThanBachelors <- rma.mv(MarginalEffect, Variance, mods = ~ MaternalEducation_MixedBachelors, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                     1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(LessThanBachelors, digits=3)

#---------------------------------MATERNAL EDUCATION IN YEARS- CONTINUOUS VARIABLE-------------

MaternalEducationYEARS <- rma.mv(MarginalEffect, Variance, mods = ~ MaternalEducationYears_AVG11, random = list(~ 1 | EffectSize_ID, ~
                                                                                                1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(MaternalEducationYEARS, digits=3)

#-----------------------------NUMBER OF CHIDREN IN HH 0 to 6 CONTINUOUS VARIABLE---------

NumChildren0to6 <- rma.mv(MarginalEffect, Variance, mods = ~ NumberoChildren.inHH_0to.6_AVG1.5, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                               1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(NumChildren0to6, digits=3)

#-----------------------------NUMBER OF CHIDREN IN HH 0 to 19 CONTINUOUS VARIABLE---------

NumChildren0to19 <- rma.mv(MarginalEffect, Variance, mods = ~ NumberoChildren.inHH_0to.19_AVG0.6, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                                 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(NumChildren0to19, digits=3)


#-------------------------------COUNTRY LEVEL FEMALE LFP RATE (categorical) NEW -------------

#<60 FLFP Rate 
lowerthan60 <- rma.mv(MarginalEffect, Variance, mods = ~ FemaleLFPRate_60..65 + FemaleLFPRate_65..70 + FemaleLFPRate_70., random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                          1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(lowerthan60, digits=3)

#60 to<65 FLFP Rate 
from60to65 <- rma.mv(MarginalEffect, Variance, mods = ~ FemaleLFPRate_.60 + FemaleLFPRate_65..70 + FemaleLFPRate_70., random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                      1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(from60to65, digits=3)

#66 to<70 FLFP Rate 
from65to70 <- rma.mv(MarginalEffect, Variance, mods = ~ FemaleLFPRate_.60 + FemaleLFPRate_60..65 + FemaleLFPRate_70., random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                      1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(from65to70, digits=3)

#70+ FLFP Rate 
from70 <- rma.mv(MarginalEffect, Variance, mods = ~ FemaleLFPRate_.60 + FemaleLFPRate_60..65 + FemaleLFPRate_65..70, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                     1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(from70, digits=3)

#-------------------------------COUNTRY LEVEL FEMALE PT Participation RATE (CONTINUOUS) NEW-------------

PartTimeRate <- rma.mv(MarginalEffect, Variance, mods = ~ PartTimeRate_Av22, random = list(~ 1 | EffectSize_ID, ~
                                                                                                1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(PartTimeRate, digits=3)


#-----------------------------------YOUNGEST CHILD IN THE HOUSEHOLD---------------------
#Not youngest child as reference group 
NotYoungest <- rma.mv(MarginalEffect, Variance, mods = ~ ISYoungestChildHousehold, random = list(~ 1 | EffectSize_ID, ~
                                                                                                         1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(NotYoungest, digits=3)


#Is youngest child as reference group 
IsYoungest <- rma.mv(MarginalEffect, Variance, mods = ~ NotYoungestChildHousehold, random = list(~ 1 | EffectSize_ID, ~
                                                                                                         1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(IsYoungest, digits=3)

#-----------------------------------PROGRAM DELIVERY LEVEL (2 LEVELS)---------------------

#State level as the reference group 
StateLevel <- rma.mv(MarginalEffect, Variance, mods = ~ DeliveryLevel_NationalFederal, random = list(~
                                                                                                           1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(StateLevel, digits=3)

#National/Federal level as the reference group 
NationalFederal <- rma.mv(MarginalEffect, Variance, mods = ~ DeliveryLevel_StateProvincia, random = list(~
                                                                                                                                         1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(NationalFederal, digits=3)

#--------------------------------------AUSPICE TYPE---------------------
# Majority private delivery dropped because no studies report on it

#Mixed delivery as reference group
MixedAuspice <- rma.mv(MarginalEffect, Variance, mods = ~ ProgramDeliveryPublic, random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(MixedAuspice, digits=3)

#Public delivery as reference group
PublicAuspice <- rma.mv(MarginalEffect, Variance, mods = ~ ProgramDeliveryMixed, random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(PublicAuspice, digits=3)


#--------------------------------------DAILY OPERATING HOURS (3 LEVELS)---------------------
#dropping All day because no studies reported on it. 

#Half day as reference group 
HalfDay <- rma.mv(MarginalEffect, Variance, mods = ~ DailyOperatingHours_FullDay + DailyOperatingHours_Mixed, random = list(~
                                                                                                                                 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(HalfDay, digits=3)

#Full day as reference group 
FullDay <- rma.mv(MarginalEffect, Variance, mods = ~ DailyOperatingHours_Mixed + DailyOperatingHours_HalfDay, random = list(~
                                                                                                                                 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(FullDay, digits=3)

#Mixed Hours as Reference Group

MixedHours <- rma.mv(MarginalEffect, Variance, mods = ~ DailyOperatingHours_HalfDay + DailyOperatingHours_FullDay, random = list(~
                                                                                                                                      1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(MixedHours, digits=3)

#--------------------------------------ECE GARUNTEED YEAR ROUND---------------------

# Not guaranteed year round as reference group 
NotYearRound <- rma.mv(MarginalEffect, Variance, mods = ~ ECEGuaranteedYearRound, random = list(~ 1 | EffectSize_ID, ~
                                                                                                      1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(NotYearRound, digits=3)

# Guaranteed year round as reference group 
YearRound <- rma.mv(MarginalEffect, Variance, mods = ~ ECENOTGuaranteedYearRound, random = list(~ 1 | EffectSize_ID, ~
                                                                                                      1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(YearRound, digits=3)

#--------------------------------------COST OF ECE---------------------

#Sliding Scale as Reference Group 

SlidingScale <- rma.mv(MarginalEffect, Variance, mods = ~ CostofECE_Free, random = list(~ 1 | EffectSize_ID, ~
                                                                                              1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary( SlidingScale, digits=3)

#Free programs as Reference Group 

FreeProgram <- rma.mv(MarginalEffect, Variance, mods = ~ CostofECE_SlidingScale, random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary( FreeProgram, digits=3)


#---------------------------------------RIGHT TO ACCESS ECE (2 LEVELS)-----------------

#legal provision as reference group 
LegalProvision <- rma.mv(MarginalEffect, Variance, mods = ~ AccesstoECE_Compulsory, random = list(~
                                                                                                                                        1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(LegalProvision, digits=3)

#Compulsory provision as reference group 
Compulsory <- rma.mv(MarginalEffect, Variance, mods = ~ AccesstoECE_LegalRight, random = list(~
                                                                                                                                    1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(Compulsory, digits=3)

#---------------------------------------YEAR OF POLICY IMPLEMENTATION (CONTINUOUS)-----------------

YearofPolicy<- rma.mv(MarginalEffect, Variance, mods = ~ YearofPolicyImplementation_AVG1999, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                    1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(YearofPolicy, digits=3)

#---------------------------------------AGE OF ELIGIBILTY (3 LEVELS)-----------------
# no studies reported on mixed age groups 

#Younger than 3 y/o as reference group 
ThreeandYounger <- rma.mv(MarginalEffect, Variance, mods = ~ ChildAge_3andOlder, random = list(~
                                                                                                                      1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(ThreeandYounger, digits=3)

#Older than 3 y/o as reference group 
ThreeandOlder <- rma.mv(MarginalEffect, Variance, mods = ~ ChildAge_Youngerthan3, random = list(~
                                                                                                                       1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(ThreeandOlder, digits=3)

#-------------------------------------------STUDY DESIGN (4 LEVELS)----------------------------
#IV studies dropped because no studies utlize this design 

#RD as Reference group
RD <- rma.mv(MarginalEffect, Variance, mods = ~ StudyDesign_IV, random = list(~
                                                                                                                                                             1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(RD, digits=4)

#Instrumental variables as reference group 
IV <- rma.mv(MarginalEffect, Variance, mods = ~ StudyDesign_RD, random = list(~
                                                                                                                                                   1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(IV, digits=3)

#---------------------------------------------------------CROWDING OUT----------------------------

#No crowding out as reference 
NoCrowdingOut <- rma.mv(MarginalEffect, Variance, mods = ~ CrowdingOutEffect, random = list(~ 1 | EffectSize_ID, ~
                                                                                                  1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary( NoCrowdingOut, digits=3)

#No crowding out as reference 
CrowdingOut <- rma.mv(MarginalEffect, Variance, mods = ~ NOCrowdingOutEffect, random = list(~ 1 | EffectSize_ID, ~
                                                                                                  1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary( CrowdingOut, digits=3)


#--------------------------------------------------------ROBUSTNESS CHECK -------------
#All included studies carried out robustness analyses. 

#--------------------------------------------------INDICE OF EMPLOYMENT (5 LEVELS)-----------------------
#WHY DOES THE P_VALUE OF THE OMNIBUS TEST CHANGE ACROSS ANALYSES???

#General Employment as reference group 
GeneralEmploy <- rma.mv(MarginalEffect, Variance, mods = ~ Outcome_LFP, random = list(~
                                                                                                                                                           1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(GeneralEmploy, digits=3)

#LFP as reference group 
LFP <- rma.mv(MarginalEffect, Variance, mods = ~ Outcome_GeneralEmployment, random = list(~
                                                                                                                                                               1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(LFP, digits=3)


#Looking for Work as reference group 
LookingforWork<- rma.mv(MarginalEffect, Variance, mods = ~ Outcome_GeneralEmployment + Outcome_LFP + Outcome_SelfEmploy, random = list(~
                                                                                                                                                              1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(LookingforWork, digits=3)

#Self Employed as reference group 
SelfEmployed<- rma.mv(MarginalEffect, Variance, mods = ~ Outcome_GeneralEmployment + Outcome_LFP + Outcome_LookingforWork, random = list(~
                                                                                                                                                                                   1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryLATE)
summary(SelfEmployed, digits=3)

#-----------MULTIPLE MODERATOR MODEL-------------------
multiplemoderator <- rma.mv(MarginalEffect, Variance, mods = ~ 
                              ChildAge_Youngerthan3+
                              NotYoungestChildHousehold + 
                              MaternalEducationYears_AVG11 +
                              CostofECE_Free + 
                              ECENOTGuaranteedYearRound+
                              DeliveryLevel_StateProvincia +
                              Country_EuropeCentralAsia +
                              Country_LatinAmerica +
                              NumberoChildren.inHH_0to.6_AVG1.5 + 
                              NumberoChildren.inHH_0to.19_AVG0.6 + 
                              Study.Year_AVG2104 +
                              PartTimeRate_Av22+
                              StudyDesign_RD, 
                            random =list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), 
                            tdist=TRUE, data=EmployBinaryLATE)

summary(multiplemoderator, digits=3)


