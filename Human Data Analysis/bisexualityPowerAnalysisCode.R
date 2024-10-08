##PR Bisexuality Study 2 power analysis script


#load simr package to do power analysis on lmer 
library(simr)

#set seed so resulst are same each time
set.seed(143)

#create new dataframe with no NAs
powerData <- ltData[complete.cases(ltData$value),]


#do lmer for power analysis
ltOmnibus2 <- lmer(value ~ partnerSex + trait*sex + (1|PIN), 
                   data = powerData)

#do power analysis to determine actual power
powerActual <- powerSim(ltOmnibus2)


#analysis at 200 bisexual participants

ltOmnibus3 <- extend(ltOmnibus2, along = "PIN", n = 200)
power200n <- powerSim(ltOmnibus3) #89.80% power

ltOmnibus4 <- extend(ltOmnibus2, along = "PIN", n = 175)
power175n <- powerSim(ltOmnibus4) # 85.50% power

ltOmnibus5 <- extend(ltOmnibus2, along = "PIN", n = 165)
power165n <- powerSim(ltOmnibus5) #81.40% power


#so overall, need minimum of 165 bi participants, 330 total 
