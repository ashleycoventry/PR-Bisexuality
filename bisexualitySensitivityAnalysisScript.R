#####De-Confounding Sex and Sex of Partner in Mate Preference Research#####
#######Sensitivity Analysis Script######

##load packages
library(lmerTest)
library(reshape2) #to reshape data
library(tidyverse) #for %>% among other things
library(simr) # for sensitivity analysis
library(r2glmm) #for sensitivity analysis

##set seed so results are same each time
set.seed(143)


###Study 1

##load in study 1 data
studyOneData <- read.csv("Human Data/Processed Data/PR Bisexuality Data PROCESSED 12062022 000940.csv")

##processing not added to processing script
#exclude intersex participants
studyOneData <-  studyOneData[studyOneData$sex!=2,]
#exclude people who did not report their sex
studyOneData <- studyOneData[!is.na(studyOneData[,5]), ]

#converting preferred age to preferred age difference
#130 = m_lt_age, 142 = f_lt_age, 154 = f_st_age, 166 = m_st_age

#converting participant age to same scale as ideal
studyOneData$ageLik <- ifelse(studyOneData$age >= 75, 10, 
                              ifelse(studyOneData$age >= 69, 9,
                                     ifelse(studyOneData$age >= 63, 8,
                                            ifelse(studyOneData$age >= 57, 7,
                                                   ifelse(studyOneData$age >= 51, 6, 
                                                          ifelse(studyOneData$age >= 45, 5, 
                                                                 ifelse(studyOneData$age >= 39, 4, 
                                                                        ifelse(studyOneData$age >= 33, 3, 
                                                                               ifelse(studyOneData$age >= 27, 2, 
                                                                                      ifelse(studyOneData$age >= 21, 1, 0))))))))))


#converting ideal partner age to ideal partner age difference
#male ideal LT
studyOneData$m_Lt_AgeLik <- studyOneData$m_lt_age - studyOneData$ageLik
#male ideal ST
studyOneData$m_St_AgeLik <- studyOneData$m_st_age - studyOneData$ageLik
#female ideal LT
studyOneData$f_Lt_AgeLik <- studyOneData$f_lt_age - studyOneData$ageLik
#female ideal ST
studyOneData$f_St_AgeLik <- studyOneData$f_st_age - studyOneData$ageLik

##limiting study 1 data to only long-term preferences & changing to longform

ltDataStudyOne <- studyOneData[,c(167, 5, 178:187, 227, 229)]
ltDataStudyOne <- melt(ltDataStudyOne, id.vars=c("PIN", "sex"))
ltDataStudyOne <- ltDataStudyOne %>% 
  separate("variable", into = c("partnerSex", "x", "trait"), remove = T)
ltDataStudyOne$partnerSex <- ifelse(ltDataStudyOne$partnerSex == "m", 1, 0)
ltDataStudyOne <- ltDataStudyOne[,c(1:3, 5:6)]
ltDataStudyOne$sex  <- as.factor(ltDataStudyOne$sex)
ltDataStudyOne$partnerSex  <- as.factor(ltDataStudyOne$partnerSex)



##Sensitivity Analysis: 3 way interaction of participant sex, partner sex, and trait

#generate lmer with interaction term
model1 <- lmer(value ~ partnerSex*sex*trait + (1|PIN), data = ltDataStudyOne[complete.cases(ltDataStudyOne[,2:5]),])

#find r squared for the overall 3 way interaction
rSquaredModel1 <- r2beta(model1, method = "nsj")

#specify range of effect sizes to test

effectSizesModel1 <- seq(from = -.5, to = .5, by = .05)

##empty data frames to store results
sensitivityResultsModel1 <- data.frame(effectModel1 = numeric(0), 
                                       powerModel1 = numeric(0), 
                                       rSquaredModel1 = numeric(0))
#number of simulations
nSimModel1 <- 100

for(i in 1:length(effectSizesModel1)) {
  #specify the effect size to be changed
  fixef(model1)["partnerSex1:sex1:traithealth"] <- effectSizesModel1[i]
  #run power analysis
  powerModel1 <- powerSim(model1, nsim = nSimModel1, test = fcompare(value~sex*partnerSex+sex*trait+trait*partnerSex))
  rSquaredModel1 <- r2beta(model1, method = "nsj")
  tempModel1 <- expand_grid(effectModel1 = effectSizesModel1[i], 
                            powerModel1 = sum(powerModel1$pval < .05) / nSimModel1, 
                            rSquaredModel1 = rSquaredModel1[rSquaredModel1$Effect == "partnerSex:sex:trait", "Rsq"])
  sensitivityResultsModel1 <- bind_rows(sensitivityResultsModel1, tempModel1)
}


##Sensitivity Analysis: Main effect of partner sex

#do lmer for sensitivity analysis
model2 <- lmer(value ~ partnerSex + trait*sex + (1|PIN), 
               data = ltDataStudyOne[complete.cases(ltDataStudyOne[,2:5]),])


effectSizesModel2 <- seq(from = 0.05, to = .5, by = .01)

#Initialize empty data frames to store results
sensitivityResultsModel2 <- data.frame(effectModel2 = numeric(0), 
                                       powerModel2 = numeric(0), 
                                       rSquaredModel2 = numeric(0))

#set number of simulations to run
nSimModel2 = 100

for(i in 1:length(effectSizesModel2)) {
  #specify what the effect size is
  fixef(model2)["partnerSex1"] <- effectSizesModel2[i]
  #run power analysis
  powerModel2 <- powerSim(model2, nsim = nSimModel2, test = fixed("partnerSex"))
  rSquaredModel2 <- r2beta(model2, method = "nsj")
  tempModel2 <- expand_grid(effectModel2 = effectSizesModel2[i], 
                            powerModel2 = sum(powerModel2$pval < .05) / nSimModel2,
                            rSquaredModel2 = rSquaredModel2[rSquaredModel2$Effect == "partnerSex", "Rsq"])
  sensitivityResultsModel2 <- bind_rows(sensitivityResultsModel2, tempModel2)
}







###Study 2

##loading in data
dataStudy2<-read.csv("Human Data/Processed Data/BisexualityStudy2_DataPROCESSED 20240514 161447.csv")

#longform data
ltDataStudy2 <- dataStudy2[,c(51, 5, 52, 53:62, 64:65)]
ltDataStudy2 <- melt(ltDataStudy2, id.vars=c("PIN", "sex", "sexuality"))
ltDataStudy2 <- ltDataStudy2 %>% 
  separate("variable", into = c("partnerSex", "x", "trait"), remove = T)
ltDataStudy2$partnerSex <- ifelse(ltDataStudy2$partnerSex == "m", 1, 0)
ltDataStudy2 <- ltDataStudy2[,c(1:4, 6:7)]
ltDataStudy2$sex  <- as.factor(ltDataStudy2$sex)
ltDataStudy2$sexuality <- as.factor(ltDataStudy2$sexuality)
ltDataStudy2$partnerSex  <- as.factor(ltDataStudy2$partnerSex)


#only bisexual participants 
ltDataStudy2Bi <- ltDataStudy2[ltDataStudy2$sexuality == 0, ]

#bi and het participants with just opposite sex ratings
ltDataStudy2Combo <- ltDataStudy2[ltDataStudy2$sex != ltDataStudy2$partnerSex, ] 



##Sensitivity Analysis: 3 way interaction of partner sex, trait, and participant sex

#generate lmer with interaction term
model3 <- lmer(value ~ partnerSex*sex*trait + (1|PIN), data = ltDataStudy2Bi[complete.cases(ltDataStudy2Bi[,2:6]),])

#find r squared for the overall 3 way interaction
rSquaredModel3 <- r2beta(model3, method = "nsj")

#specify range of effect sizes to test
effectSizesModel3 <- seq(from = 0.01, to = 1, by = .01)

##empty data frames to store results
sensitivityResultsModel3 <- data.frame(effectModel3 = numeric(0), 
                                       powerModel3 = numeric(0), 
                                       rSquaredModel3 = numeric(0))
#number of simulations
nSimModel3 <- 100

for(i in 1:length(effectSizesModel3)) {
  #specify the effect size to be changed
  fixef(model3)["partnerSex1:sex1:traithealth"] <- effectSizesModel3[i]
  #run power analysis
  powerModel3 <- powerSim(model3, nsim = nSimModel3, test = fcompare(value~sex*partnerSex+sex*trait+trait*partnerSex))
  rSquaredModel3 <- r2beta(model3, method = "nsj")
  tempModel3 <- expand_grid(effectModel3 = effectSizesModel3[i], 
                      powerModel3 = sum(powerModel3$pval < .05) / nSimModel3, 
                      rSquaredModel3 = rSquaredModel3[rSquaredModel3$Effect == "partnerSex:sex:trait", "Rsq"])
  sensitivityResultsModel3 <- bind_rows(sensitivityResultsModel3, tempModel3)
}




##Sensitivity Analysis: main effect of partner sex

#do lmer for sensitivity analysis
model4 <- lmer(value ~ partnerSex + trait*sex + (1|PIN), 
                                 data = ltDataStudy2Bi[complete.cases(ltDataStudy2Bi[,2:6]),])


effectSizesModel4 <- seq(from = 0.001, to = 1, by = .005)

#Initialize empty data frames to store results
sensitivityResultsModel4 <- data.frame(effectModel4 = numeric(0), 
                                     powerModel4 = numeric(0), 
                                     rSquaredModel4 = numeric(0))

#set number of simulations to run
nSimModel4 = 100

for(i in 1:length(effectSizesModel4)) {
  #specify what the effect size is
  fixef(model4)["partnerSex1"] <- effectSizesModel4[i]
  #run power analysis
  powerModel4 <- powerSim(model4, nsim = nSimModel4, test = fixed("partnerSex"))
  rSquaredModel4 <- r2beta(model4, method = "nsj")
  tempModel4 <- expand_grid(effectModel4 = effectSizesModel4[i], 
                                powerModel4 = sum(powerModel4$pval < .05) / nSimModel4,
                                rSquaredModel4 = rSquaredModel4[rSquaredModel4$Effect == "partnerSex", "Rsq"])
  sensitivityResultsModel4 <- bind_rows(sensitivityResultsModel4, tempModel4)
}


##Sensitivity Analysis: 3 way interaction of participant sex, sexual orientation, and trait

#generate lmer with interaction term
model5 <- lmer(value ~ trait*sex*sexuality + (1|PIN), 
               data = ltDataStudy2Combo[complete.cases(ltDataStudy2Combo[,2:6]),]) 

#find r squared for the overall 3 way interaction
rSquaredModel5 <- r2beta(model5, method = "nsj")

#specify range of effect sizes to test
effectSizesModel5 <- seq(from = -.5, to = .5, by = .05)

##empty data frames to store results
sensitivityResultsModel5 <- data.frame(effectModel5 = numeric(0), 
                                       powerModel5 = numeric(0), 
                                       rSquaredModel5 = numeric(0))
#number of simulations
nSimModel5 <- 100

for(i in 1:length(effectSizesModel5)) {
  #specify the effect size to be changed
  fixef(model5)["traithealth:sex1:sexuality1"] <- effectSizesModel5[i]
  #run power analysis
  powerModel5 <- powerSim(model5, nsim = nSimModel5, test = fcompare(value~sex*sexuality+sex*trait+trait*sexuality))
  rSquaredModel5 <- r2beta(model5, method = "nsj")
  tempModel5 <- expand_grid(effectModel5 = effectSizesModel5[i], 
                      powerModel5 = sum(powerModel5$pval < .05) / nSimModel5, 
                      rSquaredModel5 = rSquaredModel5[rSquaredModel5$Effect == "trait:sex:sexuality", "Rsq"])
  sensitivityResultsModel5 <- bind_rows(sensitivityResultsModel5, tempModel5)
}


##Sensitivity Analysis: 2 way interaction of sexual orientation and trait

#generate lmer with interaction term
model6 <- lmer(value ~ trait*sexuality + (1|PIN), 
               data = ltDataStudy2Combo[complete.cases(ltDataStudy2Combo[,2:6]),]) 

#find r squared for the overall 2 way interaction
rSquaredModel6 <- r2beta(model6, method = "nsj")

#specify range of effect sizes to test
effectSizesModel6 <- seq(from = -.5, to = .5, by = .05)

##empty data frames to store results
sensitivityResultsModel6 <- data.frame(effectModel6 = numeric(0), 
                                       powerModel6 = numeric(0), 
                                       rSquaredModel6 = numeric(0))
#number of simulations
nSimModel6 <- 100

for(i in 1:length(effectSizesModel6)) {
  #specify the effect size to be changed
  fixef(model6)["traithealth:sexuality1"] <- effectSizesModel6[i]
  #run power analysis
  powerModel6 <- powerSim(model6, nsim = nSimModel6, test = fcompare(value~sexuality+trait))
  rSquaredModel6 <- r2beta(model6, method = "nsj")
  tempModel6 <- expand_grid(effectModel6 = effectSizesModel6[i], 
                            powerModel6 = sum(powerModel6$pval < .05) / nSimModel6, 
                            rSquaredModel6 = rSquaredModel6[rSquaredModel6$Effect == "trait:sexuality", "Rsq"])
  sensitivityResultsModel6 <- bind_rows(sensitivityResultsModel6, tempModel6)
}


##integrative sensitivity analysis: 3 way interaction of sex, partner sex, trait


#load in pooled bi data

pooledData <- read.csv("Human Data/Processed Data/BisexualityPooledData20241008 101955.csv")
pooledData$sex <- as.factor(pooledData$sex)
pooledData$partnerSex <- as.factor(pooledData$partnerSex)
pooledData$trait <- as.factor(pooledData$trait)

#generate lmer with interaction term
model7 <- lmer(value ~ partnerSex*trait*sex + study + (1|PIN), 
                    data = pooledData[complete.cases(pooledData[,2:5]),])

#find r squared for the overall 3 way interaction
rSquaredModel7 <- r2beta(model7, method = "nsj")

#specify range of effect sizes to test
effectSizesModel7 <- seq(from = -.5, to = .5, by = .05)

##empty data frames to store results
sensitivityResultsModel7 <- data.frame(effectModel7 = numeric(0), 
                                       powerModel7 = numeric(0), 
                                       rSquaredModel7 = numeric(0))
#number of simulations
nSimModel7 <- 100

for(i in 1:length(effectSizesModel7)) {
  #specify the effect size to be changed
  fixef(model7)["partnerSex1:traithealth:sex1"] <- effectSizesModel7[i]
  #run power analysis
  powerModel7 <- powerSim(model7, nsim = nSimModel7, test = fcompare(value~sex*partnerSex+sex*trait+trait*partnerSex))
  rSquaredModel7 <- r2beta(model7, method = "nsj")
  tempModel7 <- expand_grid(effectModel7 = effectSizesModel7[i], 
                            powerModel7 = sum(powerModel7$pval < .05) / nSimModel7, 
                            rSquaredModel7 = rSquaredModel7[rSquaredModel7$Effect == "partnerSex:trait:sex", "Rsq"])
  sensitivityResultsModel7 <- bind_rows(sensitivityResultsModel7, tempModel7)
}