#Install packages 
install.packages("metafor")
library(metafor)
install.packages("clubSandwich")
library (clubSandwich)

#Import "Employment_ITT_Longitudinal.csv" dataset into R. Set "Headings" option to "yes" prior to importing. 

#Preparing the dataset for analysis
EmployBinaryITT=Employment_ITT_Longitudinal
options(scipen=999)

#-------------MAIN ANALYSIS/TEST FOR HETEROGENEITY-------------------
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

#-------------------------------------ASSESSING PUBLICATION BIAS----------
#Egger's RVE
egger_multi <- rma.mv(MarginalEffect, Variance, random = ~ 1 | Study_ID/EffectSize_ID, mods = ~ SE, data = EmployBinaryITT)
coef_test(egger_multi, vcov = "CR2")

#PET 
overalleffectPET <- rma.mv(MarginalEffect, Variance, 
                           mods = ~ SE,
                           random = list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), 
                           tdist= TRUE, data=EmployBinaryITT)
overalleffectPET

#Funnel Plot
funnel(overalleffect,xlab = "Average Marginal Effect", yaxis = "seinv")

#------ OUTLIERS----------------------
res <- rma(MarginalEffect, Variance, mods = cbind(Study.Year_AVG2016 + Number.of.Other.Children.in.the.Household.Age.0.to..19_AVG1.7 + RobustnessAnalysis), data = EmployBinaryITT)
rstudent(res)

#--------------------------------UNIVARIATE MODERATOR ANALYSIS---------------------

#--------STUDY YEAR (CONTINUOUS)--------
studyyear <- rma.mv(MarginalEffect, Variance, mods = ~ Study.Year_AVG2016, random = list(~ 1 | EffectSize_ID, ~
                                                                                           1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(studyyear, digits=3)

# ----PUBLICATION TYPE----
# Working/discussion paper as reference group 
notpublished <- rma.mv(MarginalEffect, Variance, mods = ~ TypeofPublication_Journal, random = list(~ 1 | EffectSize_ID, ~
                                                                                                     1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(notpublished, digits=3)

# Journal as reference group 

published <- rma.mv(MarginalEffect, Variance, mods = ~ TypeofPublication_WorkingPaper, random = list(~ 1 | EffectSize_ID, ~
                                                                                                       1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(published, digits=3)

#-----------------------COUNTRY REGION (Moderator with 4 Levels) -----------
#North America as the reference group 
CountryRegionNorthAm <- rma.mv(MarginalEffect, Variance, mods = ~ Country_EuropeCentralAsia, random = list(~
                                                                                                                                                       1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(CountryRegionNorthAm, digits=3)

#Europe/Central Asia as Reference Group

CountryRegionEuropeCenAsia <- rma.mv(MarginalEffect, Variance, mods = ~ Country_NorthAmerica, random = list(~
                                                                                                                                                        1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(CountryRegionEuropeCenAsia, digits=3)

# --------------------------------------------------COUNTRY INCOME ----------------------------
#All high income 

#--------------------------------------------MATERNAL AGE (CONTINUOUS)------------------         

MaternalAge <- rma.mv(MarginalEffect, Variance, mods = ~ Maternal.Age_AVG32, random = list(~ 1 | EffectSize_ID, ~
                                                                                             1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(MaternalAge, digits=3)

#----------------------------------------------------MARITIAL STATUS--------------------
#all two parent studies 

#-----------------------------------------------------IMMIGRANT---------------
#All non-immigrant

#-------------------------------------------URBAN VS. RURAL REGIONS-------------
#No studies report on this measure 

#---------------------------------MATERNAL EDUCATION AS AN ORDINAL VARIABLE-------------
#Only two studies reported this measure 

#---------------------------------MATERNAL EDUCATION IN YEARS- CONTINUOUS VARIABLE-------------
#Only two studies reported this measure 

#-----------------------------NUMBER OF CHIDREN IN HH 0 to 6 CONTINUOUS VARIABLE---------
##Only one studies reported this measure 

#-----------------------------NUMBER OF CHIDREN IN HH 0 to 19 CONTINUOUS VARIABLE---------

NumChildren0to19 <- rma.mv(MarginalEffect, Variance, mods = ~ Number.of.Other.Children.in.the.Household.Age.0.to..19_AVG1.7, random = list(~ 1 | EffectSize_ID, ~
                                                                                                                                             1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(NumChildren0to19, digits=3)

#-------------------------------COUNTRY LEVEL FEMALE LFP RATE (categorical) NEW -------------

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

#-------------------------------COUNTRY LEVEL FEMALE PT Participation RATE (CONTINUOUS) NEW-------------

PartTimeRate <- rma.mv(MarginalEffect, Variance, mods = ~ PartTimeRate_AVG22, random = list(~ 1 | EffectSize_ID, ~
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

#-----------------------------------PROGRAM DELIVERY LEVEL (2 LEVELS)---------------------

#National/Federal level as the reference group 
NationalFederal <- rma.mv(MarginalEffect, Variance, mods = ~ DeliveryLevel_StateProvincia, random = list(~
                                                                                                                                     1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(NationalFederal, digits=3)

#State level as the reference group 
StateLevel <- rma.mv(MarginalEffect, Variance, mods = ~ DeliveryLevel_NationalFederal, random = list(~
                                                                                                                                 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(StateLevel, digits=3)

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
#All all-day programs

#--------------------------------------ECE GARUNTEED YEAR ROUND---------------------
# Only two studies reported on this

#--------------------------------------COST OF ECE---------------------

#Sliding Scale as Reference Group 

SlidingScale <- rma.mv(MarginalEffect, Variance, mods = ~ CostofECE_Free, random = list(~ 1 | EffectSize_ID, ~
                                                                                          1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary( SlidingScale, digits=3)

#Free programs as Reference Group 

FreeProgram <- rma.mv(MarginalEffect, Variance, mods = ~ CostofECE_SlidingScale, random = list(~ 1 | EffectSize_ID, ~
                                                                                                 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary( FreeProgram, digits=3)

#---------------------------------------RIGHT TO ACCESS ECE (2 LEVELS)-----------------

#No legal provisions as reference group 
NoProvisions <- rma.mv(MarginalEffect, Variance, mods = ~ AccesstoECE_LegalRight, random = list(~
                                                                                                                           1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(NoProvisions, digits=3)

#legal provision as reference group 
LegalProvision <- rma.mv(MarginalEffect, Variance, mods = ~ AccesstoECE_NoLegalProvisions, random = list(~
                                                                                                                                    1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(LegalProvision, digits=3)

#---------------------------------------YEAR OF POLICY IMPLEMENTATION (CONTINUOUS)-----------------

YearofPolicy<- rma.mv(MarginalEffect, Variance, mods = ~ Year.of.Policy.Implementation_AVG1995, random = list(~ 1 | EffectSize_ID, ~
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

#------------------------------LONGITUDNAL EFFECTS (SHORT TERM< LONG TERM< MIXED)

#Short Term as reference as reference group 
ShortTerm <- rma.mv(MarginalEffect, Variance, mods = ~ LongTerm_4to.7yearsYearsPost + Mixed_1to.7yearsYearsPost, random = list(~
                                                                                                                   1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(ShortTerm, digits=3)

#Long Term as reference as reference group 
LongTerm <- rma.mv(MarginalEffect, Variance, mods = ~ ShortTerm_1to.4YearsPost + Mixed_1to.7yearsYearsPost, random = list(~
                                                                                                                                 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(LongTerm, digits=3)

#Mixed Term as reference as reference group 
MixedTerm <- rma.mv(MarginalEffect, Variance, mods = ~ ShortTerm_1to.4YearsPost + LongTerm_4to.7yearsYearsPost, random = list(~
                                                                                                                            1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(MixedTerm, digits=3)

#-------------------------------------------STUDY DESIGN (2 LEVELS)----------------------------
#IV studies dropped because no studies utlize this design 
#DiD as reference group 
DiD <- rma.mv(MarginalEffect, Variance, mods = ~ StudyDesign_TripleDiffandDiff, random = list(~
                                                                                                                                                 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(DiD, digits=3)

#Triple DiD as Reference group
TripleDiD <- rma.mv(MarginalEffect, Variance, mods = ~ StudyDesign_DiffandDiff, random = list(~
                                                                                                                                                 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(TripleDiD, digits=3)

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


#-------------MULTIPLE MODERATOR MODEL-------------------

multiplemoderator <- rma.mv(MarginalEffect, Variance, mods = ~ Study.Year_AVG2016 + Number.of.Other.Children.in.the.Household.Age.0.to..19_AVG1.7 + RobustnessAnalysis, random =
                              list(~ 1 | EffectSize_ID, ~ 1 | Study_ID), tdist=TRUE, data=EmployBinaryITT)
summary(multiplemoderator, digits=3)



