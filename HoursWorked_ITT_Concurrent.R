#Install packages 
install.packages("metafor")
library(metafor)
install.packages("clubSandwich")
library (clubSandwich)

#Import "HoursWorked_ITT_Concurrentt.csv" dataset into R. Set "Headings" option to "yes" prior to importing. 

#Preparing the dataset for analysis
HoursWorkedConITT = HoursWorked_ITT_Concurrent
HoursWorkedConITT <- HoursWorkedConITT[-c(13:43),]
HoursWorkedConITT$Variance <-HoursWorkedConITT$SE^2

#--------------------MAIN ANALYSIS/TEST FOR HETEROGENEITY-------------------
#Estimating the overall effect
overalleffect <- rma.mv(Marginal.Probabilty.Coefficient, Variance, random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=
                          TRUE, data=HoursWorkedConITT)
overalleffect

#Computing I2 statistics 
n <- length(HoursWorkedConITT$Variance)
list.inverse.variances <- 1 / (HoursWorkedConITT$Variance)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances)^2
list.inverse.variances.square <- 1 / (HoursWorkedConITT$Variance^2)
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
options(scipen=999)
amountvariancelevel1
amountvariancelevel2
amountvariancelevel3

#-----------------PUBLICATION BIAS-----------------------
#PET
overalleffectPET <- rma.mv(Marginal.Probabilty.Coefficient, Variance, 
                           mods = ~ SE,
                           random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), 
                           tdist= TRUE, data=HoursWorkedConITT)
overalleffectPET

#Eggers regression test REV for dependant estimates 
egger_multi <- rma.mv(Marginal.Probabilty.Coefficient, Variance, random = ~ 1 | Study_ID/EffectSize_ID, mods = ~ SE, data = HoursWorkedConITT)
coef_test(egger_multi, vcov = "CR2")

#Funnel plot
funnel(overalleffect,xlab = "Average Marginal Effect", yaxis = "seinv")

#---- OUTLIERS----------------------
res <- rma(Marginal.Probabilty.Coefficient, Variance, data = HoursWorkedConITT)
rstudent(res)

#-------------------------UNIVARIATE MODERATOR ANALYSIS---------------------

#------Study Year------------------         

StudyYear <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ StudyYear_AVRG2019, random = list(~ 1 | EffectSize_ID, ~
                                                                                           1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(StudyYear, digits=3)

# ----PUBLICATION TYPE----
# Working/discussion paper as reference group 
notpublished <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~Type.of.Publication_Journal_1.Yes, random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(notpublished, digits=3)

# Journal as reference group 

published <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~  HTypeofPublication_WorkingPaper_1.Yes, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(published, digits=3)

#-----------------------COUNTRY REGION (Moderator with 3 Levels) -----------
#North America as the reference group 
CountryRegionNorthAm <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~  Country_EuropeCentralAsia_1.yes + Country_EastAsia_1.yes, random = list(~
                                                                                                                                    1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(CountryRegionNorthAm, digits=3)

#Europe/Central Asia as Reference Group

CountryRegionEuropeCenAsia <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~  Country_NorthAmerica_1.yes + Country_EastAsia_1.yes, random = list(~
                                                                                                                                     1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(CountryRegionEuropeCenAsia, digits=3)

#Latin American As Reference Group
CountryEastAsia <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ Country_NorthAmerica_1.yes + Country_EuropeCentralAsia_1.yes, random = list(~
                                                                                                                                    1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(CountryEastAsia, digits=3)

# --------------------------------------------------COUNTRY INCOME ----------------------------
#all high income

#--------------------------------------------MATERNAL AGE (CONTINUOUS)------------------         

MaternalAge <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ Maternal.Age_AVRG32, random = list(~ 1 | EffectSize_ID, ~
                                                                                            1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(MaternalAge, digits=3)

#----------------------------------------------------MARITIAL STATUS--------------------

#Sinlge parents as reference group 

Single <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ MaritalStatus_TwoParant_1.yes, random = list(~ 1 | EffectSize_ID, ~
                                                                                             1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(Single, digits=3)


#Majority Married as reference group 

TwoParentMarried <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ MaritalStatus_Mixed_1.yes, random = list(~ 1 | EffectSize_ID, ~
                                                                                                    1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(TwoParentMarried, digits=3)

#-----------------------------------------------------IMMIGRANT---------------
#Majority Non-immigrants as reference category
NonImmgrnt <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ MixedImmigrant_1.yes, random = list(~ 1 | EffectSize_ID, ~
                                                                                        1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(NonImmgrnt, digits=3)

#Majority Mixed immigrants as reference category
MixedImmgrnt <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ NonImmigrant_1.yes, random = list(~ 1 | EffectSize_ID, ~
                                                                                        1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(MixedImmgrnt, digits=3)

#-------------------------------------------URBAN VS. RURAL REGIONS-------------
#only one study reported on urban residence 

#---------------------------------MATERNAL EDUCATION AS AN ORDINAL VARIABLE-------------

#Mixed Bachelors as reference category
MixedBachelors <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ MaternalEducation_LessThanBachelors_1.yes, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(MixedBachelors, digits=3)

#Less than Bachelors as reference category
LessThanBachelors <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ MaternalEducation_MixedBachelors_1.yes, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(LessThanBachelors, digits=3)

#---------------------------------MATERNAL EDUCATION IN YEARS-------------
#Not enough studies to carry out moderator analysis 
                   
#-----------------------------NUMBER OF CHIDREN IN HH 0 to 6 CONTINUOUS VARIABLE---------

NumChildren0to6 <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ Number.of.Other.Children.in.the.Household.Age.0.to..6_AVG.9, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(NumChildren0to6, digits=3)

#-----------------------------NUMBER OF CHIDREN IN HH 0 to 19 CONTINUOUS VARIABLE---------

NumChildren0to19 <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ Number.of.Other.Children.in.the.Household.Age.0.to..19_AVG2.0, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                  1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(NumChildren0to19, digits=3)

#-------------------------------COUNTRY LEVEL FEMALE LFP RATE -------------
#<60 FLFP Rate 
lowerthan60 <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ FemaleLFPRate_60..65 + FemaleLFPRate_65..70 + FemaleLFPRate_70., random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                          1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(lowerthan60, digits=3)

#60 to<65 FLFP Rate 
from60to65 <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ FemaleLFPRate_.60 + FemaleLFPRate_65..70 + FemaleLFPRate_70., random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                      1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(from60to65, digits=3)

#65 to<70 FLFP Rate 
from65to70 <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ FemaleLFPRate_.60 + FemaleLFPRate_60..65 + FemaleLFPRate_70., random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                      1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(from65to70, digits=3)

#70+ FLFP Rate 
from70 <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ FemaleLFPRate_.60 + FemaleLFPRate_60..65 + FemaleLFPRate_65..70, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                     1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(from70, digits=3)

#-------------------------------COUNTRY LEVEL FEMALE PT Participation RATE (CONTINUOUS) NEW-------------

PartTimeRate <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ PartTimeRate_AVG34, random = list(~ 1 | EffectSize_ID, ~
                                                                                                1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(PartTimeRate, digits=3)


#-----------------------------------YOUNGEST CHILD IN THE HOUSEHOLD---------------------
#Not youngest child as reference group 
NotYoungest <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ ISYoungestChildHousehold_1.yes, random = list(~ 1 | EffectSize_ID, ~
                                                                                                   1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(NotYoungest, digits=3)


#Is youngest child as reference group 
MixedYoungest <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ MixedYoungestChildHousehold_1.yes, random = list(~ 1 | EffectSize_ID, ~
                                                                                                   1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(MixedYoungest, digits=3)

#-----------------------------------PROGRAM DELIVERY LEVEL (2 LEVELS)---------------------

#State level as the reference group 
StateLevel <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ DeliveryLevel_NationalFederal_1.yes + DeliveryLevel_Municipal_1.yes, random = list(~
                                                                                                       1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(StateLevel, digits=3)

#National/Federal level as the reference group 
NationalFederal <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ DeliveryLevel_StateProvincial_1.yes + DeliveryLevel_Municipal_1.yes, random = list(~
                                                                                                           1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(NationalFederal, digits=3)

#municipal level as the reference group 
municipal <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ DeliveryLevel_StateProvincial_1.yes + DeliveryLevel_NationalFederal_1.yes, random = list(~
                                                                                                                                                                   1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(municipal, digits=3)

#--------------------------------------AUSPICE TYPE---------------------
# Majority private delivery dropped because no studies report on it

#Mixed delivery as reference group
MixedAuspice <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ ProgramDeliveryPublic_1.yes, random = list(~ 1 | EffectSize_ID, ~
                                                                                                 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(MixedAuspice, digits=3)

#Public delivery as reference group
PublicAuspice <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ ProgramDeliveryMixed_1.yes, random = list(~ 1 | EffectSize_ID, ~
                                                                                                 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(PublicAuspice, digits=3)

#--------------------------------------DAILY OPERATING HOURS (3 LEVELS)---------------------
#dropping All day because no studies reported on it. 

#Half day as reference group 
HalfDay <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ DailyOperatingHours_FullDay + DailyOperatingHours_AllDay, random = list(~
                                                                                                                              1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(HalfDay, digits=3)

#Full day as reference group 
FullDay <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ DailyOperatingHours_AllDay + DailyOperatingHours_HalfDay, random = list(~
                                                                                                                              1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(FullDay, digits=3)

#Mixed Hours as Reference Group

AllDayHours <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ DailyOperatingHours_HalfDay + DailyOperatingHours_FullDay, random = list(~
                                                                                                                                   1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(AllDayHours, digits=3)

#--------------------------------------ECE GARUNTEED YEAR ROUND---------------------

# Not guaranteed year round as reference group 
NotYearRound <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ ECEGuaranteedYearRound, random = list(~ 1 | EffectSize_ID, ~
                                                                                                  1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(NotYearRound, digits=3)

# Guaranteed year round as reference group 
YearRound <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ ECENOTGuaranteedYearRound, random = list(~ 1 | EffectSize_ID, ~
                                                                                                  1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(YearRound, digits=3)

#--------------------------------------COST OF ECE---------------------

#Sliding Scale as Reference Group 

SlidingScale <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ CostofECE_Free, random = list(~ 1 | EffectSize_ID, ~
                                                                                          1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary( SlidingScale, digits=3)

#Free programs as Reference Group 

FreeProgram <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ CostofECE_SlidingScale, random = list(~ 1 | EffectSize_ID, ~
                                                                                                 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary( FreeProgram, digits=3)

#---------------------------------------RIGHT TO ACCESS ECE (2 LEVELS)-----------------

#legal provision as reference group 
LegalProvision <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ AccesstoECE_NoLegalProvisions, random = list(~
                                                                                                    1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(LegalProvision, digits=3)

#Compulsory provision as reference group 
Nolegalprovision <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ AccesstoECE_LegalRight, random = list(~
                                                                                                1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(Nolegalprovision, digits=3)

#---------------------------------------YEAR OF POLICY IMPLEMENTATION (CONTINUOUS)-----------------

YearofPolicy<- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ Year.of.Policy.Implementation_AVRG2008, random = list(~ 1 | EffectSize_ID, ~
                                                                                                             1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(YearofPolicy, digits=3)

#---------------------------------------AGE OF ELIGIBILTY (3 LEVELS)-----------------
# no studies reported on mixed age groups 

#Younger than 3 y/o as reference group 
ThreeandYounger <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ ChildAge_3andOlder, random = list(~
                                                                                                 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(ThreeandYounger, digits=3)

#SixandYoungeras reference group 
SixandYounger <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ ChildAge_6andYounger, random = list(~
                                                                                                  1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(SixandYounger, digits=3)

#-------------------------------------------STUDY DESIGN (4 LEVELS)----------------------------
#IV studies dropped because no studies utlize this design 

#DiD as Reference group
DiD <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ StudyDesign_TripleDiffandDiff + StudyDesign_PanelFixedEffects, random = list(~
                                                                                1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(DiD, digits=4)

#TripleDiD as reference group 
TripleDiD <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ StudyDesign_DiffandDiff + StudyDesign_PanelFixedEffects, random = list(~
                                                                                1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(TripleDiD, digits=3)

#Instrumental variables as reference group 
PFE <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ StudyDesign_DiffandDiff + StudyDesign_TripleDiffandDiff, random = list(~
                                                                                                                                                                                  1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(PFE, digits=3)

#---------------------------------------------------------CROWDING OUT----------------------------

#No crowding out as reference 
NoCrowdingOut <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ CrowdingOutEffect, random = list(~ 1 | EffectSize_ID, ~
                                                                                              1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary( NoCrowdingOut, digits=3)

#No crowding out as reference 
CrowdingOut <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ NOCrowdingOutEffect, random = list(~ 1 | EffectSize_ID, ~
                                                                                              1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary( CrowdingOut, digits=3)


#--------------------------------------------------------ROBUSTNESS CHECK -------------
Robustness <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ NORobustnessAnalysis, random = list(~ 1 | EffectSize_ID, ~
                                                                                                               1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary( Robustness, digits=3)

#No robustness as reference 
NoRobustness <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ RobustnessAnalysis, random = list(~ 1 | EffectSize_ID, ~
                                                                                                               1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(NoRobustness, digits=3)

#---------------------------------------------CENSORED OUTCOME MEASURE-----------------------
#only two studies 

#--------------------------------------------------INDICE OF hours worked -----------------------
#General Employment as reference group 
HoursWorked <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ ContractualHoursWeekly + PreferredHoursWeekly, random = list(~
                                                                                        1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(HoursWorked, digits=3)

#Preferred as reference group 
Preferred <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ ContractualHoursWeekly + HoursWeekly, random = list(~
                                                                                            1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(Preferred, digits=3)

#contracted as reference group 
Contracted <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ PreferredHoursWeekly + HoursWeekly, random = list(~
                                                                                                                              1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(Contracted , digits=3)

#------Multiple moderator model
#with 70+ LFP as reference
multiplemoderator <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ DailyOperatingHours_AllDay + DailyOperatingHours_FullDay + AccesstoECE_LegalRight + FemaleLFPRate_.60 + FemaleLFPRate_60..65 + FemaleLFPRate_65..70, random =
                              list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(multiplemoderator, digits=3)

#with 65-70 LFP as reference
multiplemoderator <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ DailyOperatingHours_AllDay + DailyOperatingHours_FullDay + AccesstoECE_LegalRight + FemaleLFPRate_.60 + FemaleLFPRate_60..65 + FemaleLFPRate_70., random =
                              list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(multiplemoderator, digits=3)

#When full day is reference group
multiplemoderator <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ DailyOperatingHours_AllDay + DailyOperatingHours_HalfDay + AccesstoECE_LegalRight + FemaleLFPRate_.60 + FemaleLFPRate_60..65 + FemaleLFPRate_65..70, random =
                              list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(multiplemoderator, digits=3)

#When all day is reference group
multiplemoderator <- rma.mv(Marginal.Probabilty.Coefficient, Variance, mods = ~ DailyOperatingHours_FullDay + DailyOperatingHours_HalfDay + AccesstoECE_LegalRight + FemaleLFPRate_.60 + FemaleLFPRate_60..65 + FemaleLFPRate_65..70, random =
                              list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(multiplemoderator, digits=3)


