#Install packages 
install.packages("metafor")
library(metafor)
install.packages("clubSandwich")
library (clubSandwich)

#Import "HoursWorked_ITT_Longitudinal" dataset into R. Set "Headings" option to "yes" prior to importing. 
HoursWorkedConITT = HoursWorked_ITT_Longitudinal
HoursWorkedConITT <- HoursWorkedConITT[-c(10:11),]
HoursWorkedConITT$Variance <-HoursWorkedConITT$SE^2
options(scipen=999) 

#-----------------------MAIN ANALYSIS AND TEST FOR HETEROGENIETY-----------------------------
#Estimating the overall effect
overalleffect <- rma.mv(OLS, Variance, random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=
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
amountvariancelevel1
amountvariancelevel2
amountvariancelevel3

#------- OUTLIERS----------------------
res <- rma(OLS, Variance, mods = cbind(RobustnessAnalysis + StudyYear_AVRG2016 + ChildAge_Youngerthan3 + ChildAge_3andOlder), data = HoursWorkedConITT)
rstudent(res)

#-------------------------------------ASSESSING PUBLICATION BIAS----------
#PET
overalleffectPET <- rma.mv(OLS, Variance, 
                           mods = ~ SE,
                           random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), 
                           tdist= TRUE, data=HoursWorkedConITT)
overalleffectPET


#Egger's RVE
egger_multi <- rma.mv(OLS, Variance, random = ~ 1 | Study_ID/EffectSize_ID, mods = ~ SE, data = HoursWorkedConITT)
coef_test(egger_multi, vcov = "CR2")

#Funnel plot
funnel(overalleffect,xlab = "Average Marginal Effect", yaxis = "seinv")

#--------------------------------UNIVARIATE MODERATOR ANALYSIS---------------------

#--------STUDY YEAR (CONTINUOUS)--------
studyyear <- rma.mv(OLS, Variance, mods = ~ StudyYear_AVRG2016, random = list(~ 1 | EffectSize_ID, ~
                                                                                           1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(studyyear, digits=3)

# ----PUBLICATION TYPE----
# Working/discussion paper as reference group 
notpublished <- rma.mv(OLS, Variance, mods = ~ Type.of.Publication_Journal, random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(notpublished, digits=3)

# Journal as reference group 

published <- rma.mv(OLS, Variance, mods = ~ TypeofPublication_WorkingPaper, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(published, digits=3)

#-----------------------COUNTRY REGION (Moderator with 4 Levels) -----------
#North America as the reference group 
CountryRegionNorthAm <- rma.mv(OLS, Variance, mods = ~ Country_EuropeCentralAsia, random = list(~
                                                                                                                                                       1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(CountryRegionNorthAm, digits=3)

#Europe/Central Asia as Reference Group

CountryRegionEuropeCenAsia <- rma.mv(OLS, Variance, mods = ~ Country_NorthAmerica, random = list(~
                                                                                                                                                        1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(CountryRegionEuropeCenAsia, digits=3)

# --------------------------------------------------COUNTRY INCOME ----------------------------
#All high income 
#--------------------------------------------MATERNAL AGE (CONTINUOUS)------------------         
#Only 2 Studies 

#----------------------------------------------------MARITIAL STATUS--------------------
#All two Parent 

#-----------------------------------------------------IMMIGRANT---------------
#All non immigrant 
#-------------------------------------------URBAN VS. RURAL REGIONS-------------
#No studies report on it 
#---------------------------------MATERNAL EDUCATION AS AN ORDINAL VARIABLE-------------
# All mixed bachelor 
#---------------------------------MATERNAL EDUCATION IN YEARS- CONTINUOUS VARIABLE-------------
#Only two studies 

#-----------------------------NUMBER OF CHIDREN IN HH 0 to 6 CONTINUOUS VARIABLE---------
#Only 1 study

#-----------------------------NUMBER OF CHIDREN IN HH 0 to 19 CONTINUOUS VARIABLE---------
#Only 2 study


#-------------------------------COUNTRY LEVEL FEMALE LFP RATE (categorical) NEW -------------
#<60 FLFP Rate 
lowerthan60 <- rma.mv(OLS, Variance, mods = ~ FemaleLFPRate_60..65 + FemaleLFPRate_65..70 + FemaleLFPRate_70., random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                          1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(lowerthan60, digits=3)

#60 to<65 FLFP Rate 
from60to65 <- rma.mv(OLS, Variance, mods = ~ FemaleLFPRate_.60 + FemaleLFPRate_65..70 + FemaleLFPRate_70., random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                      1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(from60to65, digits=3)

#65 to<70 FLFP Rate 
from65to70 <- rma.mv(OLS, Variance, mods = ~ FemaleLFPRate_.60 + FemaleLFPRate_60..65 + FemaleLFPRate_70., random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                      1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(from65to70, digits=3)

#70+ FLFP Rate 
from70 <- rma.mv(OLS, Variance, mods = ~ FemaleLFPRate_.60 + FemaleLFPRate_60..65 + FemaleLFPRate_65..70, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                     1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(from70, digits=3)

#-------------------------------COUNTRY LEVEL FEMALE PT Participation RATE (CONTINUOUS) NEW-------------

PartTimeRate <- rma.mv(OLS, Variance, mods = ~ PartTimeRate_Av28, random = list(~ 1 | EffectSize_ID, ~
                                                                                                1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(PartTimeRate, digits=3)

#-----------------------------------YOUNGEST CHILD IN THE HOUSEHOLD---------------------
#Not youngest child dropped because no studies report on it 

#Mixed youngest child as reference group 
MixedYoungest <- rma.mv(OLS, Variance, mods = ~ ISYoungestChildHousehold_1.yes, random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(MixedYoungest, digits=3)

#Is youngest child as reference group 
IsYoungest <- rma.mv(OLS, Variance, mods = ~ MixedYoungestChildHousehold_1.yes, random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(IsYoungest, digits=3)

#-----------------------------------PROGRAM DELIVERY LEVEL (3 LEVELS)---------------------

#National/Federal level as the reference group 
NationalFederal <- rma.mv(OLS, Variance, mods = ~ DeliveryLevel_StateProvincia, random = list(~
                                                                                                                                     1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(NationalFederal, digits=3)

#State level as the reference group 
StateLevel <- rma.mv(OLS, Variance, mods = ~ DeliveryLevel_NationalFederal, random = list(~
                                                                                                                                 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(StateLevel, digits=3)

#--------------------------------------AUSPICE TYPE---------------------
# Majority private delivery dropped because no studies report on it

#Mixed delivery as reference group
MixedAuspice <- rma.mv(OLS, Variance, mods = ~ ProgramDeliveryPublic, random = list(~ 1 | EffectSize_ID, ~
                                                                                                 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(MixedAuspice, digits=3)

#Public delivery as reference group
PublicAuspice <- rma.mv(OLS, Variance, mods = ~ ProgramDeliveryMixed, random = list(~ 1 | EffectSize_ID, ~
                                                                                                 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(PublicAuspice, digits=3)

#--------------------------------------DAILY OPERATING HOURS (4 LEVELS)---------------------
#Only 2 studies 

#--------------------------------------ECE GARUNTEED YEAR ROUND---------------------
# all yes

#--------------------------------------COST OF ECE---------------------

#Sliding Scale as Reference Group 

SlidingScale <- rma.mv(OLS, Variance, mods = ~ CostofECE_Free, random = list(~ 1 | EffectSize_ID, ~
                                                                                          1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary( SlidingScale, digits=3)

#Free programs as Reference Group 

FreeProgram <- rma.mv(OLS, Variance, mods = ~ CostofECE_SlidingScale, random = list(~ 1 | EffectSize_ID, ~
                                                                                                 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary( FreeProgram, digits=3)

#---------------------------------------RIGHT TO ACCESS ECE (3 LEVELS)-----------------

#No legal provisions as reference group 
NoProvisions <- rma.mv(OLS, Variance, mods = ~ AccesstoECE_LegalRight, random = list(~
                                                                                                                           1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(NoProvisions, digits=3)

#legal provision as reference group 
LegalProvision <- rma.mv(OLS, Variance, mods = ~ AccesstoECE_NoLegalProvisions, random = list(~
                                                                                                                                    1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(LegalProvision, digits=3)


#---------------------------------------YEAR OF POLICY IMPLEMENTATION (CONTINUOUS)-----------------
#only two studies 

#---------------------------------------AGE OF ELIGIBILTY (3 LEVELS)-----------------

#Younger than 3 y/o as reference group 
ThreeandYounger <- rma.mv(OLS, Variance, mods = ~ ChildAge_3andOlder + ChildAge_Mixed, random = list(~
                                                                                                                  1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(ThreeandYounger, digits=3)

#Older than 3 y/o as reference group 
ThreeandOlder <- rma.mv(OLS, Variance, mods = ~ ChildAge_Youngerthan3 + ChildAge_Mixed, random = list(~
                                                                                                                   1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(ThreeandOlder, digits=3)

#Mixed 0 to 6 as reference as reference group 
Mixed0to6 <- rma.mv(OLS, Variance, mods = ~ ChildAge_Youngerthan3 + ChildAge_3andOlder, random = list(~
                                                                                                                   1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(Mixed0to6, digits=3)

#------------------------------LONGITUDNAL EFFECTS (SHORT TERM< LONG TERM< MIXED)

#Short Term as reference as reference group 
ShortTerm <- rma.mv(OLS, Variance, mods = ~ LongTerm_4to.7yearsYearsPost + Mixed_1to.7yearsYearsPost, random = list(~
                                                                                                                                 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(ShortTerm, digits=3)

#Long Term as reference as reference group 
LongTerm <- rma.mv(OLS, Variance, mods = ~ ShortTerm_1to.4YearsPost + Mixed_1to.7yearsYearsPost, random = list(~
                                                                                                                            1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(LongTerm, digits=3)

#Mixed Term as reference as reference group 
MixedTerm <- rma.mv(OLS, Variance, mods = ~ ShortTerm_1to.4YearsPost + LongTerm_4to.7yearsYearsPost, random = list(~
                                                                                                                                1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(MixedTerm, digits=3)

#-------------------------------------------STUDY DESIGN (4 LEVELS)----------------------------
#All diff and diff

#---------------------------------------------------------CROWDING OUT----------------------------

#No crowding out as reference 
NoCrowdingOut <- rma.mv(OLS, Variance, mods = ~ CrowdingOutEffect, random = list(~ 1 | EffectSize_ID, ~
                                                                                              1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary( NoCrowdingOut, digits=3)

#No crowding out as reference 
CrowdingOut <- rma.mv(OLS, Variance, mods = ~ NOCrowdingOutEffect, random = list(~ 1 | EffectSize_ID, ~
                                                                                              1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary( CrowdingOut, digits=3)

#--------------------------------------------------------ROBUSTNESS CHECK -------------

#No robustness check as reference 
NoRobustness <- rma.mv(OLS, Variance, mods = ~ RobustnessAnalysis, random = list(~ 1 | EffectSize_ID, ~
                                                                                              1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(NoRobustness, digits=3)

#Has robustness check as reference 
Robustness <- rma.mv(OLS, Variance, mods = ~ NORobustnessAnalysis, random = list(~ 1 | EffectSize_ID, ~
                                                                                              1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(Robustness, digits=3)


#--------------------------------------------------INDICE OF Hours Worked-----------------------

#Hours worked as reference group 
HoursWorked <- rma.mv(OLS, Variance, mods = ~ ContractualHoursWeekly + ActualHoursWeekly, random = list(~
                                                                                                                                         1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(HoursWorked, digits=3)

#Preferred as reference group 
Preferred <- rma.mv(OLS, Variance, mods = ~ ActualHoursWeekly + HoursWeekly, random = list(~
                                                                                                                              1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(Preferred, digits=3)


#---------MULTIPLE MODERATOR MODEL-------------------
multiplemoderator <- rma.mv(OLS, Variance, mods = ~ RobustnessAnalysis + StudyYear_AVRG2016 + ChildAge_Youngerthan3 + ChildAge_3andOlder + FemaleLFPRate_.60 + FemaleLFPRate_60..65 + FemaleLFPRate_70., random =
                              list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=HoursWorkedConITT)
summary(multiplemoderator, digits=3) 





