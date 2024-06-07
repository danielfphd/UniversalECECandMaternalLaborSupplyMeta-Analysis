#Install packages 
install.packages("metafor")
library(metafor)
install.packages("clubSandwich")
library (clubSandwich)

#Import "Employment_ITT_Concurrent.csv" dataset into R. Set "Headings" option to "yes" prior to importing. 

#Preparing the dataset for analysis
EmployBinaryITT=Employment_ITT_Concurrent
EmployBinaryITT <- EmployBinaryITT[-c(28:68),]
EmployBinaryITT$Variance<-EmployBinaryITT$SE^2
options(scipen=999)

#--------------------MAIN ANALYSIS/TEST FOR HETEROGENEITY-------------------
#Estimating the overall effect
overalleffect <- rma.mv(MarginalEffect, Variance, random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=
                          TRUE, data=EmployBinaryITT)
overalleffect

#Computing I2 statistics 
n <- length(EmployBinaryITT$Variance)
list.inverse.variances <- 1 / (EmployBinaryITT$Variance)
sum.inverse.variances <- sum(list.inverse.variances)
squared.sum.inverse.variances <- (sum.inverse.variances) ^ 2
list.inverse.variances.square <- 1 / (EmployBinaryITT$Variance^2)
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

#------------PUBLICATION BIAS-----------------
#PET 
overalleffectPET <- rma.mv(MarginalEffect, Variance, 
                           mods = ~ SE,
                           random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), 
                           tdist= TRUE, data=EmployBinaryITT)
overalleffectPET

#Egger's RVE
egger_multi <- rma.mv(MarginalEffect, Variance, random = ~ 1 | Study_ID/EffectSize_ID, mods = ~ SE, data = EmployBinaryITT)
coef_test(egger_multi, vcov = "CR2")

#Funnel Plot
funnel(overalleffect,xlab = "Average Marginal Effect", yaxis = "seinv")

#----------OUTLIERS------
res <- rma(MarginalEffect, Variance, mods = cbind(SelectiveReporting, ChildAge_3andOlder, ChildAge_Mixed, TypeofPublication_WorkingPaper), data = EmployBinaryITT)
rstudent(res)

#-------------------------UNIVARIATE MODERATOR ANALYSIS---------------------

#--------STUDY YEAR--------
studyyear <- rma.mv(MarginalEffect, Variance, mods = ~ Study.Year_AVG2017, random = list(~ 1 | EffectSize_ID, ~
                                                                1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(studyyear, digits=3)

#----PUBLICATION TYPE----
# Working/discussion paper as reference group 
notpublished <- rma.mv(MarginalEffect, Variance, mods = ~ TypeofPublication_Journal, random = list(~ 1 | EffectSize_ID, ~
                                                                1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(notpublished, digits=3)

# Journal as reference group 

published <- rma.mv(MarginalEffect, Variance, mods = ~ TypeofPublication_WorkingPaper, random = list(~ 1 | EffectSize_ID, ~
                                                               1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(published, digits=3)

#--- COUNTRY REGION-----------
#North America as the reference group 
CountryRegionNorthAm <- rma.mv(MarginalEffect, Variance, mods = ~ Country_EuropeCentralAsia + Country_LatinAmerica + Country_EastAsia, random = list(~
                                                                                    1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(CountryRegionNorthAm, digits=3)

#Europe/Central Asia as Reference Group

CountryRegionEuropeCenAsia <- rma.mv(MarginalEffect, Variance, mods = ~ Country_NorthAmerica + Country_LatinAmerica + Country_EastAsia, random = list(~
                                                                                                                                                           1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(CountryRegionEuropeCenAsia, digits=3)

#Latin American As Reference Group
CountryRegionLatinAm <- rma.mv(MarginalEffect, Variance, mods = ~ Country_NorthAmerica + Country_EuropeCentralAsia + Country_EastAsia, random = list(~
                                                                                                                                                           1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(CountryRegionLatinAm, digits=3)

#East Asia as Reference Group 
CountryRegionEastAsia <- rma.mv(MarginalEffect, Variance, mods = ~ Country_NorthAmerica + Country_EuropeCentralAsia + Country_LatinAmerica, random = list(~
                                                                                                                                                           1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(CountryRegionEastAsia, digits=3)

#------COUNTRY INCOME ------

#Upper Middle Income Countries as Reference Group

UpperMiddleIncome <- rma.mv(MarginalEffect, Variance, mods = ~ CountryIncome_High, random = list(~ 1 | EffectSize_ID, ~
                                                                                            1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(UpperMiddleIncome, digits=3)

#High Income Countries as Reference Group

HighIncome <- rma.mv(MarginalEffect, Variance, mods = ~ CountryIncome_UpperMiddle, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(UpperMiddleIncome, digits=3)

#----MATERNAL AGE------------------         

MaternalAge <- rma.mv(MarginalEffect, Variance, mods = ~ Maternal.Age_AVG32, random = list(~ 1 | EffectSize_ID, ~
                                                                                               1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(MaternalAge, digits=3)

#----MARITIAL STATUS--------------------
#Mixed Married as reference group 
MixedMarried <- rma.mv(MarginalEffect, Variance, mods = ~ MaritalStatus_TwoParant, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(MixedMarried, digits=3)

#Two Parent as reference group 

TwoParentMarried <- rma.mv(MarginalEffect, Variance, mods = ~ MaritalStatus_Mixed, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(TwoParentMarried, digits=3)

#---IMMIGRANT---------------
#Majority non-immigrants as reference category
NonImmgrnt <- rma.mv(MarginalEffect, Variance, mods = ~ MixedImmigrant, random = list(~ 1 | EffectSize_ID, ~
                                                                                                           1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(NonImmgrnt, digits=3)

#Majority Mixed immigrants as reference category
MixedImmgrnt <- rma.mv(MarginalEffect, Variance, mods = ~ NonImmigrant, random = list(~ 1 | EffectSize_ID, ~
                                                                                            1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(MixedImmgrnt, digits=3)


#----URBAN VS. RURAL REGIONS-------------
#Mixed as reference category
MajorityMixed <- rma.mv(MarginalEffect, Variance, mods = ~ MajorityUrban, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(MajorityMixed, digits=3)

#Majority urban as reference category
MajorityUrban <- rma.mv(MarginalEffect, Variance, mods = ~ MixedUrban, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(MajorityUrban, digits=3)


#----MATERNAL EDUCATION AS AN ORDINAL VARIABLE-------------
#Mixed Bachelors as reference category
MixedBachelors <- rma.mv(MarginalEffect, Variance, mods = ~ MaternalEducation_LessThanBachelors, random = list(~ 1 | EffectSize_ID, ~
                                                                                                      1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(MixedBachelors, digits=3)

#Less than Bachelors as reference category
LessThanBachelors <- rma.mv(MarginalEffect, Variance, mods = ~ MaternalEducation_MixedBachelors, random = list(~ 1 | EffectSize_ID, ~
                                                                                                   1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(LessThanBachelors, digits=3)

#-----MATERNAL EDUCATION IN YEARS--------------
#Not enough studies to carry out moderator analysis 

#----NUMBER OF CHIDREN IN HH 0 to 6---------
NumChildren0to6 <- rma.mv(MarginalEffect, Variance, mods = ~ Number.of.Other.Children.in.the.Household.Age.0.to..6_AVG0.6, random = list(~ 1 | EffectSize_ID, ~
                                                                                                 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(NumChildren0to6, digits=3)

#----NUMBER OF CHIDREN IN HH 0 to 19 CONTINUOUS VARIABLE---------
NumChildren0to19 <- rma.mv(MarginalEffect, Variance, mods = ~ Number.of.Other.Children.in.the.Household.Age.0.to..19_AVG1.8, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                               1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(NumChildren0to19, digits=3)

#----------------COUNTRY LEVEL FEMALE LFP RATE  -------------
#<60 FLFP Rate 
lowerthan60 <- rma.mv(MarginalEffect, Variance, mods = ~ FemaleLFPRate_60..65 + FemaleLFPRate_65..70 + FemaleLFPRate_70., random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(lowerthan60, digits=3)

#60 to<65 FLFP Rate 
from60to65 <- rma.mv(MarginalEffect, Variance, mods = ~ FemaleLFPRate_.60 + FemaleLFPRate_65..70 + FemaleLFPRate_70., random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                          1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(from60to65, digits=3)

#66 to<70 FLFP Rate 
from65to70 <- rma.mv(MarginalEffect, Variance, mods = ~ FemaleLFPRate_.60 + FemaleLFPRate_60..65 + FemaleLFPRate_70., random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                      1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(from65to70, digits=3)

#70+ FLFP Rate 
from70 <- rma.mv(MarginalEffect, Variance, mods = ~ FemaleLFPRate_.60 + FemaleLFPRate_60..65 + FemaleLFPRate_65..70, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                      1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(from70, digits=3)

#----COUNTRY LEVEL FEMALE PT Participation Rate-------------

PartTimeRate <- rma.mv(MarginalEffect, Variance, mods = ~ PartTimeRate_Av28, random = list(~ 1 | EffectSize_ID, ~
                                                                                              1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(PartTimeRate, digits=3)

#-----------------------------------YOUNGEST CHILD IN THE HOUSEHOLD---------------------
#Not youngest child dropped because no studies report on it 

#Mixed youngest child as reference group 
MixedYoungest <- rma.mv(MarginalEffect, Variance, mods = ~ ISYoungestChildHousehold, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                     1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(MixedYoungest, digits=3)

#Is youngest child as reference group 
IsYoungest <- rma.mv(MarginalEffect, Variance, mods = ~ MixedYoungestChildHousehold, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(IsYoungest, digits=3)

#-----------------------------------PROGRAM DELIVERY LEVEL (3 LEVELS)---------------------

#National/Federal level as the reference group 
NationalFederal <- rma.mv(MarginalEffect, Variance, mods = ~ DeliveryLevel_StateProvincia + DeliveryLevel_Municipal, random = list(~
                                                                                                                                                           1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(NationalFederal, digits=3)

#State level as the reference group 
StateLevel <- rma.mv(MarginalEffect, Variance, mods = ~ DeliveryLevel_NationalFederal + DeliveryLevel_Municipal, random = list(~
                                                                                                                                                            1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(StateLevel, digits=3)

#Municipal level as the reference group 
MunicipalLevel <- rma.mv(MarginalEffect, Variance, mods = ~ DeliveryLevel_NationalFederal + DeliveryLevel_StateProvincia, random = list(~
                                                                                                                                                        1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(MunicipalLevel, digits=3)

#--------------------------------------AUSPICE TYPE---------------------
# Majority private delivery dropped because no studies report on it

#Mixed delivery as reference group
MixedAuspice <- rma.mv(MarginalEffect, Variance, mods = ~ ProgramDeliveryPublic, random = list(~ 1 | EffectSize_ID, ~
                                                                                                         1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(MixedAuspice, digits=3)

#Public delivery as reference group
PublicAuspice <- rma.mv(MarginalEffect, Variance, mods = ~ ProgramDeliveryMixed, random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(PublicAuspice, digits=3)


#--------------------------------------DAILY OPERATING HOURS (4 LEVELS)---------------------
#FullDay day as reference group 
FullDay <- rma.mv(MarginalEffect, Variance, mods = ~ DailyOperatingHours_AllDay + DailyOperatingHours_Mixed + DailyOperatingHours_HalfDay, random = list(~
                                                                                                                             1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(FullDay, digits=3)

#Half day as reference group 
HalfDay <- rma.mv(MarginalEffect, Variance, mods = ~ DailyOperatingHours_AllDay + DailyOperatingHours_Mixed + DailyOperatingHours_FullDay, random = list(~
                                                                                                                                         1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(HalfDay, digits=3)

#Mixed Hours as Reference Group
MixedHours <- rma.mv(MarginalEffect, Variance, mods = ~ DailyOperatingHours_HalfDay + DailyOperatingHours_AllDay + DailyOperatingHours_FullDay, random = list(~
                                                                                                                                 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(MixedHours, digits=3)

#All Day as Reference Group
AllDay <- rma.mv(MarginalEffect, Variance, mods = ~ DailyOperatingHours_HalfDay + DailyOperatingHours_Mixed + DailyOperatingHours_FullDay, random = list(~
                                                                                                                                     1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(AllDay, digits=3)

#--------------------------------------ECE GARUNTEED YEAR ROUND---------------------
# Not guaranteed year round as reference group 
NotYearRound <- rma.mv(MarginalEffect, Variance, mods = ~ ECEGuaranteedYearRound, random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(NotYearRound, digits=3)

# Guaranteed year round as reference group 
YearRound <- rma.mv(MarginalEffect, Variance, mods = ~ ECENOTGuaranteedYearRound, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                   1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(YearRound, digits=3)

#--------------------------------------COST OF ECE---------------------
#Sliding Scale as Reference Group 
SlidingScale <- rma.mv(MarginalEffect, Variance, mods = ~ CostofECE_Free, random = list(~ 1 | EffectSize_ID, ~
                                                                                                         1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary( SlidingScale, digits=3)

#Free programs as Reference Group 
FreeProgram <- rma.mv(MarginalEffect, Variance, mods = ~ CostofECE_SlidingScale, random = list(~ 1 | EffectSize_ID, ~
                                                                                              1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary( FreeProgram, digits=3)

#---------------------------------------RIGHT TO ACCESS ECE (3 LEVELS)-----------------
#No legal provisions as reference group 
NoProvisions <- rma.mv(MarginalEffect, Variance, mods = ~ AccesstoECE_LegalRight + AccesstoECE_Compulsory, random = list(~
                                                                                                                                 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(NoProvisions, digits=3)

#legal provision as reference group 
LegalProvision <- rma.mv(MarginalEffect, Variance, mods = ~ AccesstoECE_NoLegalProvisions + AccesstoECE_Compulsory, random = list(~
                                                                                                                               1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(LegalProvision, digits=3)

#Compulsory provision as reference group 
Compulsory <- rma.mv(MarginalEffect, Variance, mods = ~ AccesstoECE_NoLegalProvisions + AccesstoECE_LegalRight, random = list(~
                                                                                                                                        1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(Compulsory, digits=3)

#---------------------------------------YEAR OF POLICY IMPLEMENTATION (CONTINUOUS)-----------------

YearofPolicy<- rma.mv(MarginalEffect, Variance, mods = ~ Year.of.Policy.Implementation_AVG2004, random = list(~ 1 | EffectSize_ID, ~
                                                                                                  1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(YearofPolicy, digits=3)

#---------------------------------------AGE OF ELIGIBILTY (3 LEVELS)-----------------
#Younger than 3 y/o as reference group 
ThreeandYounger <- rma.mv(MarginalEffect, Variance, mods = ~ ChildAge_3andOlder + ChildAge_Mixed, random = list(~
                                                                                                                               1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(ThreeandYounger, digits=3)

#Older than 3 y/o as reference group 
ThreeandOlder <- rma.mv(MarginalEffect, Variance, mods = ~ ChildAge_Youngerthan3 + ChildAge_Mixed, random = list(~
                                                                                                                      1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(ThreeandOlder, digits=3)

#Mixed 0 to 6 as reference as reference group 
Mixed0to6 <- rma.mv(MarginalEffect, Variance, mods = ~ ChildAge_Youngerthan3 + ChildAge_3andOlder, random = list(~
                                                                                                                       1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(Mixed0to6, digits=3)

#-------------------------------------------STUDY DESIGN (4 LEVELS)----------------------------
#IV studies dropped because no studies utilize this design 
#DiD as reference group 
DiD <- rma.mv(MarginalEffect, Variance, mods = ~ StudyDesign_TripleDiffandDiff + StudyDesign_RD + StudyDesign_PanelFixedEffects, random = list(~
                                                                                                                      1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(DiD, digits=3)

#Triple DiD as Reference group
TripleDiD <- rma.mv(MarginalEffect, Variance, mods = ~ StudyDesign_DiffandDiff + StudyDesign_RD + StudyDesign_PanelFixedEffects, random = list(~
                                                                                                                                                     1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(TripleDiD, digits=3)

#RD as Reference group
RD <- rma.mv(MarginalEffect, Variance, mods = ~ StudyDesign_DiffandDiff + StudyDesign_TripleDiffandDiff + StudyDesign_PanelFixedEffects, random = list(~
                                                                                                                                               1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(RD, digits=4)

#Panel fixed effects as reference group 
PanelFE <- rma.mv(MarginalEffect, Variance, mods = ~ StudyDesign_DiffandDiff + StudyDesign_TripleDiffandDiff + StudyDesign_RD, random = list(~
                                                                                                                                                             1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(PanelFE, digits=3)

#---------------------------------------------------------CROWDING OUT----------------------------
#No crowding out as reference 
NoCrowdingOut <- rma.mv(MarginalEffect, Variance, mods = ~ CrowdingOutEffect, random = list(~ 1 | EffectSize_ID, ~
                                                                                              1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary( NoCrowdingOut, digits=3)

#No crowding out as reference 
CrowdingOut <- rma.mv(MarginalEffect, Variance, mods = ~ NOCrowdingOutEffect, random = list(~ 1 | EffectSize_ID, ~
                                                                                                  1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary( CrowdingOut, digits=3)

#--------------------------------------------------------ROBUSTNESS CHECK -------------

#No robustness check as reference 
NoRobustness <- rma.mv(MarginalEffect, Variance, mods = ~ RobustnessAnalysis, random = list(~ 1 | EffectSize_ID, ~
                                                                                                  1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(NoRobustness, digits=3)

#Has robustness check as reference 
Robustness <- rma.mv(MarginalEffect, Variance, mods = ~ NORobustnessAnalysis, random = list(~ 1 | EffectSize_ID, ~
                                                                                                  1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(Robustness, digits=3)

#--------------------------------------------------INDICE OF EMPLOYMENT (5 LEVELS)-----------------------
#General Employment as reference group 
GeneralEmploy <- rma.mv(MarginalEffect, Variance, mods = ~ Outcome_LFP, random = list(~
                                                                                                                                                     1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(GeneralEmploy, digits=3)

#LFP as reference group 
LFP <- rma.mv(MarginalEffect, Variance, mods = ~ Outcome_GeneralEmployment, random = list(~
                                                                                                                                                           1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(LFP, digits=3)

#Multiple moderator model
multiplemoderator <- rma.mv(MarginalEffect, Variance, mods = ~ ChildAge_3andOlder + ChildAge_Mixed + TypeofPublication_WorkingPaper, random =
                              list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(multiplemoderator, digits=3)



