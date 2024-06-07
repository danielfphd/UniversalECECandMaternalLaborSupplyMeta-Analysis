#Install packages 
install.packages("metafor")
library(metafor)
install.packages("clubSandwich")
library (clubSandwich)

#Import "HoursWorked_LATE_Concurrent.csv" dataset into R. Set "Headings" option to "yes" prior to importing. 

#Preparing the dataset for analysis
HoursWorkedLATE=HoursWorked_LATE_Concurrent
HoursWorkedLATE <-HoursWorkedLATE[-c(12:14),]
HoursWorkedLATE$Variance<-HoursWorkedLATE$SE^2
options(scipen=999)

#----------------------------------------------MAIN ANALYSIS/TEST FOR HETEROGENEITY-------------------
#Estimating the overall effect
overalleffect <- rma.mv(X2SLS, Variance, random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=
                          TRUE, data=HoursWorkedLATE)
overalleffect

#Computing I2 statistics 
n <- length(HoursWorkedLATE$Variance)
list.inverse.variances <- 1 / (HoursWorkedLATE$Variance)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (HoursWorkedLATE$Variance^2)
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

#----------------------------PUBLICATION BIAS---------------------
#PET 
overalleffectPET <- rma.mv(X2SLS, Variance, 
                           mods = ~ SE,
                           random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), 
                           tdist= TRUE, data=HoursWorkedLATE)
overalleffectPET

#Egger's RVE
egger_multi <- rma.mv(X2SLS, Variance, random = ~ 1 | Study_ID/EffectSize_ID, mods = ~ SE, data = HoursWorkedLATE, control=list(rel.tol=1e-8))
coef_test(egger_multi, vcov = "CR2")

#Funnel plot 
funnel(overalleffect,xlab = "Average Marginal Effect", yaxis = "seinv")

#------ OUTLIERS----------------------
res <- rma(X2SLS, Variance, mods = cbind(Study.Year_AVG2014, Country_NorthAmerica, Country_LatinAmerica, Number.of.Other.Children.in.the.Household.Age.0.to..19_AVG0.6, DeliveryLevel_StateProvincia, CostofECE_SlidingScale, FemaleLFPRate_60..65, PartTimeRate_REVISED), data = HoursWorkedLATE)
rstudent(res)

 #---------------------------UNIVARIAE MODERATOR ANALYSIS---------------------

 #--------STUDY YEAR (CONTINUOUS)--------
studyyear <- rma.mv(X2SLS, Variance, mods = ~ Study.Year_AVG2014, random = list(~ 1 | EffectSize_ID, ~
                                                                                               1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(studyyear, digits=3)

                                          # ----PUBLICATION TYPE----
#all journal articles 
                #-----------------------COUNTRY REGION (Moderator with 3 Levels) -----------
#North America as the reference group 
CountryRegionNorthAm <- rma.mv(X2SLS, Variance, mods = ~ Country_EuropeCentralAsia + Country_LatinAmerica, random = list(~
                                                                                                                                                           1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(CountryRegionNorthAm, digits=3)

#Europe/Central Asia as Reference Group

CountryRegionEuropeCenAsia <- rma.mv(X2SLS, Variance, mods = ~ Country_NorthAmerica + Country_LatinAmerica, random = list(~
                                                                                                                                                            1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(CountryRegionEuropeCenAsia, digits=3)

#Latin American As Reference Group
CountryRegionLatinAm <- rma.mv(X2SLS, Variance, mods = ~ Country_NorthAmerica + Country_EuropeCentralAsia, random = list(~
                                                                                                                                                           1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(CountryRegionLatinAm, digits=3)

# --------------------------------------------------COUNTRY INCOME ----------------------------

#Upper Middle Income Countries as Reference Group

UpperMiddleIncome <- rma.mv(X2SLS, Variance, mods = ~ CountryIncome_High, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(UpperMiddleIncome, digits=3)

#High Income Countries as Reference Group

HighIncome <- rma.mv(X2SLS, Variance, mods = ~ CountryIncome_UpperMiddle, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(UpperMiddleIncome, digits=3)

#--------------------------------------------MATERNAL AGE (CONTINUOUS)------------------         

MaternalAge <- rma.mv(X2SLS, Variance, mods = ~ Maternal.Age_AVG32, random = list(~ 1 | EffectSize_ID, ~
                                                                                                 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(MaternalAge, digits=3)

#----------------------------------------------------MARITIAL STATUS--------------------

#Mixed Married as reference group 

MixedMarried <- rma.mv(X2SLS, Variance, mods = ~ MaritalStatus_TwoParant, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(MixedMarried, digits=3)


#Mixed Married as reference group 

TwoParentMarried <- rma.mv(X2SLS, Variance, mods = ~ MaritalStatus_Single, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(TwoParentMarried, digits=3)

#-----------------------------------------------------IMMIGRANT---------------
#Only one study report on the proportion of immigrant mothers sampled 

#-------------------------------------------URBAN VS. RURAL REGIONS-------------
#Only one study report on the proportion of mothers residing in an urban vs. rural regions  

#---------------------------------MATERNAL EDUCATION AS AN ORDINAL VARIABLE-------------
# Cannot examine as a 3 level moderator, because no studies report on a sample of mothers, the majority of which have a bachelors degree

#Mixed Bachelors as reference category
MixedBachelors <- rma.mv(X2SLS, Variance, mods = ~ MaternalEducation_LessThanBachelors, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                     1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(MixedBachelors, digits=3)

#Less than Bachelors as reference category
LessThanBachelors <- rma.mv(X2SLS, Variance, mods = ~ MaternalEducation_MixedBachelors, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                     1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(LessThanBachelors, digits=3)


#---------------------------------MATERNAL EDUCATION IN YEARS- CONTINUOUS VARIABLE-------------
MaternalEdYears <- rma.mv(X2SLS, Variance, mods = ~ MaternalEducation_YearsAVG10, random = list(~ 1 | EffectSize_ID, ~
                                                                                                 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(MaternalEdYears, digits=3)

#-----------------------------NUMBER OF CHIDREN IN HH 0 to 6 CONTINUOUS VARIABLE---------

NumChildren0to6 <- rma.mv(X2SLS, Variance, mods = ~ Number.of.Other.Children.in.the.Household.Age.0.to..6_AVG1.5, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                               1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(NumChildren0to6, digits=3)

#-----------------------------NUMBER OF CHIDREN IN HH 0 to 19 CONTINUOUS VARIABLE---------

NumChildren0to19 <- rma.mv(X2SLS, Variance, mods = ~ Number.of.Other.Children.in.the.Household.Age.0.to..19_AVG0.6, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                                 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(NumChildren0to19, digits=3)


#-------------------------------COUNTRY LEVEL FEMALE LFP RATE (categorical) NEW -------------

# NO lower than <60 FLFP Rate 

#60 to<65 FLFP Rate 
from60to65 <- rma.mv(X2SLS, Variance, mods = ~ FemaleLFPRate_70., random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                      1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(from60to65, digits=3)

#No 65 to<70 FLFP Rate 

#70+ FLFP Rate 
from70 <- rma.mv(X2SLS, Variance, mods = ~ FemaleLFPRate_60..65, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                     1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(from70, digits=3)

#-------------------------------COUNTRY LEVEL FEMALE PT Participation RATE (CONTINUOUS) NEW-------------

PartTimeRate <- rma.mv(X2SLS, Variance, mods = ~ PartTimeRate_AV21, random = list(~ 1 | EffectSize_ID, ~
                                                                                                1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(PartTimeRate, digits=3)


#-----------------------------------YOUNGEST CHILD IN THE HOUSEHOLD---------------------

#Not youngest child as reference group 

NotYoungest <- rma.mv(X2SLS, Variance, mods = ~ ISYoungestChildHousehold, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                        1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(NotYoungest, digits=3)

#Is youngest child as reference group 
IsYoungest <- rma.mv(X2SLS, Variance, mods = ~ NotYoungestChildHousehold, random = list(~ 1 | EffectSize_ID, ~
                                                                                                         1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(IsYoungest, digits=3)

#-----------------------------------PROGRAM DELIVERY LEVEL (3 LEVELS)---------------------

#National/Federal level as the reference group 
NationalFederal <- rma.mv(X2SLS, Variance, mods = ~ DeliveryLevel_StateProvincia, random = list(~
                                                                                                                                         1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(NationalFederal, digits=3)

#State level as the reference group 
StateLevel <- rma.mv(X2SLS, Variance, mods = ~ DeliveryLevel_NationalFederal, random = list(~
                                                                                                                                     1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(StateLevel, digits=3)

#--------------------------------------AUSPICE TYPE---------------------
# Majority private delivery dropped because no studies report on it

#Mixed delivery as reference group
MixedAuspice <- rma.mv(X2SLS, Variance, mods = ~ ProgramDeliveryPublic, random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(MixedAuspice, digits=3)

#Public delivery as reference group
PublicAuspice <- rma.mv(X2SLS, Variance, mods = ~ ProgramDeliveryMixed, random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(PublicAuspice, digits=3)

#--------------------------------------DAILY OPERATING HOURS (2 LEVELS)---------------------
#dropping All day and mixed dday because no studies reported on it. 

#Half day as reference group 
HalfDay <- rma.mv(X2SLS, Variance, mods = ~ DailyOperatingHours_FullDay, random = list(~
                                                                                                                                 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(HalfDay, digits=3)

#Full day as Reference Group

Fullday <- rma.mv(X2SLS, Variance, mods = ~ DailyOperatingHours_HalfDay, random = list(~
                                                                                                                                      1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(Fullday, digits=3)

#--------------------------------------ECE GARUNTEED YEAR ROUND---------------------
#no programs guaranteed year round 

#--------------------------------------COST OF ECE---------------------

#Sliding Scale as Reference Group 

SlidingScale <- rma.mv(X2SLS, Variance, mods = ~ CostofECE_Free, random = list(~ 1 | EffectSize_ID, ~
                                                                                              1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary( SlidingScale, digits=3)

#Free programs as Reference Group 

FreeProgram <- rma.mv(X2SLS, Variance, mods = ~ CostofECE_SlidingScale, random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary( FreeProgram, digits=3)    

#---------------------------------------RIGHT TO ACCESS ECE (3 LEVELS)-----------------

#legal provision as reference group 
LegalProvision <- rma.mv(X2SLS, Variance, mods = ~ AccesstoECE_Compulsory, random = list(~
                                                                                                                                        1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(LegalProvision, digits=3)

#Compulsory provision as reference group 
Compulsory <- rma.mv(X2SLS, Variance, mods = ~ AccesstoECE_LegalRight, random = list(~
                                                                                                                                    1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(Compulsory, digits=3)

#---------------------------------------YEAR OF POLICY IMPLEMENTATION (CONTINUOUS)-----------------

YearofPolicy<- rma.mv(X2SLS, Variance, mods = ~ YearofPolicyImplementation_AVG1999, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                    1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(YearofPolicy, digits=3)

#-------------------------------------------STUDY DESIGN (2 LEVELS)----------------------------
#RD as Reference group
RD <- rma.mv(X2SLS, Variance, mods = ~ StudyDesign_IV, random = list(~
                                                                                    1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(RD, digits=4)

#Instrumental variables as reference group 
IV <- rma.mv(X2SLS, Variance, mods = ~ StudyDesign_RD, random = list(~
                                                                                    1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(IV, digits=3)

#---------------------------------------------------------CROWDING OUT----------------------------

#No crowding out as reference 
NoCrowdingOut <- rma.mv(X2SLS, Variance, mods = ~ CrowdingOutEffect, random = list(~ 1 | EffectSize_ID, ~
                                                                                                  1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary( NoCrowdingOut, digits=3)

#No crowding out as reference 
CrowdingOut <- rma.mv(X2SLS, Variance, mods = ~ NOCrowdingOutEffect, random = list(~ 1 | EffectSize_ID, ~
                                                                                                  1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary( CrowdingOut, digits=3)

#--------------------------------------------------------ROBUSTNESS CHECK -------------
#all included studies carried out sensitivity checks 

#--------------------------------------------------INDICE OF Hours Worked (3 LEVELS)-----------------------
#Weekly Hours wroked as reference group 
Weeklyhours <- rma.mv(X2SLS, Variance, mods = ~ UsualHoursWeekly, random = list(~
                                                                                                                                                           1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(Weeklyhours, digits=3)

#Usual Hours wroked as reference group 
Usualhours <- rma.mv(X2SLS, Variance, mods = ~ HoursWeekly, random = list(~
                                                                                                                                                               1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(Usualhours, digits=3)

#-------- MULTIPLE MODERATOR MODEL-------------------
multiplemoderator <- rma.mv(X2SLS, Variance, mods = ~ 
                              Study.Year_AVG2014 + 
                              Country_NorthAmerica + 
                              Country_LatinAmerica + 
                              Number.of.Other.Children.in.the.Household.Age.0.to..19_AVG0.6 + 
                              FemaleLFPRate_AVG74 + 
                              DeliveryLevel_StateProvincia + 
                              CostofECE_SlidingScale +
                              FemaleLFPRate_60..65 + 
                              PartTimeRate_AV21, random =
                              list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedLATE)
summary(multiplemoderator, digits=3)



