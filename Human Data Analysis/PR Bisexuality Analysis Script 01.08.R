#####Project Rainbow Analysis Script -- Bisexual Preferences#####
####### Ashley Coventry, Katy Walter, Ben Gelbart, Tamsin German, & Dan Conroy-Beam ########

###load packages###
library(lme4)
library(multilevelTools)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(tidyr)


### set seed ###
set.seed(11182022)




### load data ###
data<-read.csv("Human Data/Processed Data/PR Bisexuality Data PROCESSED 12062022 000940.csv")
    #only bisexual people remain after processing


### Standardize preferences ###
  #phys att; kind; intel; health; resources
  #convert age to age diff
  #how do i standardize these?
  #what does it mean to standardize these lol
  #zscore, scale()
prefs <- scale(data[,178:197])

###converting preferred age to preferred age difference
  #130 = m_lt_age, 142 = f_lt_age, 154 = f_st_age, 166 = m_st_age

##converting participant age to same scale as ideal
data$ageLik <- ifelse(data$age >= 75, 10, 
                      ifelse(data$age >= 69, 9,
                      ifelse(data$age >= 63, 8,
                             ifelse(data$age >= 57, 7,
                                    ifelse(data$age >= 51, 6, 
                                           ifelse(data$age >= 45, 5, 
                                                  ifelse(data$age >= 39, 4, 
                                                         ifelse(data$age >= 33, 3, 
                                                                ifelse(data$age >= 27, 2, 
                                                                       ifelse(data$age >= 21, 1, 0))))))))))
                      


##converting ideal partner age to ideal partner age difference

#male ideal LT
data$mLtAgeLik <- data$m_lt_age - data$ageLik

#male ideal ST
data$mStAgeLik <- data$m_st_age - data$ageLik

#female ideal LT
data$fLtAgeLik <- data$f_lt_age - data$ageLik

#female ideal ST
data$fStAgeLik <- data$f_st_age - data$ageLik


### LT prefs (combined) multilevel model ###
  #looking to see if there is a significant overall effect 
  #of trait + own sex + partner sex on long term prefs
  #nested under participant ID (PIN)

#need to make data long --> tidy with partner sex column


LtData <- data[,c(167, 5, 178:187)]
LtData <- melt(LtData, id.vars=c("PIN", "sex"))
LtData <- LtData %>% 
  separate("variable", into = c("partnerSex", "x", "trait"), remove = T)
LtData$partnerSex <- ifelse(LtData$partnerSex == "m", 0, 1)
LtData <- LtData[,c(1:3, 5:6)]
LtData <- LtData %>%
  pivot_wider(names_from = trait, 
              values_from = value)

LtData$sex  <- as.factor(LtData$sex)



#health
LtHealthMM <- lmer(health + intell + kind + physatt + 
                      resources ~ health + sex + partnerSex + (1|PIN),
                    data = LtData) #is this right? 



#intelligence

LtIntellMM <- lmer(health + intell + kind + physatt + 
                     resources ~ intell + sex + partnerSex + (1|PIN),
                   data = LtData) 

#kindness

LtKindMM <- lmer(health + intell + kind + physatt + 
                     resources ~ kind + sex + partnerSex + (1|PIN),
                   data = LtData) 


#physical attractiveness
LtPhysattMM <- lmer(health + intell + kind + physatt + 
                   resources ~ physatt + sex + partnerSex + (1|PIN),
                 data = LtData) 


#resources

LtResourcesMM <- lmer(health + intell + kind + physatt + 
                   resources ~ resources + sex + partnerSex + (1|PIN),
                 data = LtData) 



### ST prefs (combined) multilevel model ###

#making data long + column for partner sex
StData <- data[,c(167, 5, 188:197)]
StData <- melt(StData, id.vars=c("PIN", "sex"))
StData <- StData %>% 
  separate("variable", into = c("partnerSex", "x", "trait"), remove = T)
StData <- StData[,c(1:3, 5:6)]
StData <- StData %>%
  pivot_wider(names_from = trait, 
              values_from = value)

StData$sex  <- as.factor(StData$sex)


#health
StHealthMM <- lmer(health + intell + kind + physatt + 
                     resources ~ health + sex + partnerSex + (1|PIN),
                   data = StData) 



#intelligence

StIntellMM <- lmer(health + intell + kind + physatt + 
                     resources ~ intell + sex + partnerSex + (1|PIN),
                   data = StData) 

#kindness

StKindMM <- lmer(health + intell + kind + physatt + 
                   resources ~ kind + sex + partnerSex + (1|PIN),
                 data = StData) 


#physical attractiveness
StPhysattMM <- lmer(health + intell + kind + physatt + 
                      resources ~ physatt + sex + partnerSex + (1|PIN),
                    data = StData) 


#resources

StResourcesMM <- lmer(health + intell + kind + physatt + 
                        resources ~ resources + sex + partnerSex + (1|PIN),
                      data = StData) 



### LT prefs (each trait) multilevel models ###


#wait to do until confirm above code is correct




### ST prefs (each trait) multilevel models ###







### main effects (what test? anova?) ###
  #do if not interaction found in 1 or 2
  #outcome variable: trait pref (do separate for LT vs ST)
  #predictor variables: own sex, partner sex
  #nested under participant ID (PIN)

##LT

##men

#health
LtHealthAnova <- aov(health ~ sex + + partnerSex + (1|PIN), 
                      data = LtData) 



### violin plots (LT and ST)
  #pref rating on y axis
  #own sex on x axis (male v female)
  #plot 2 violins for each own sex (partner sex -- male or female)

##LT prefs violin plot


#health
LtHealthPlot <- ggplot(LtData, aes(x=sex, y=health, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Long Term Health Preferences by Sex",x="Participant Sex", y = "LT Health Preference") +
  scale_fill_discrete(name = "Parter Sex")

#intelligence
LtIntelPlot <- ggplot(LtData, aes(x=sex, y=intell, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Long Term Intelligence Preferences by Sex",x="Participant Sex", y = "LT Intelligence Preference") +
  scale_fill_discrete(name = "Parter Sex")

#kindness
LtKindPlot <- ggplot(LtData, aes(x=sex, y=kind, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Long Term Kindness Preferences by Sex",x="Participant Sex", y = "LT Kindness Preference") +
  scale_fill_discrete(name = "Parter Sex")

#physical attractiveness
LtPhysattPlot <- ggplot(LtData, aes(x=sex, y=physatt, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Long Term Physical Attractiveness Preferences by Sex",x="Participant Sex", y = "LT Physical Attractiveness Preference") +
  scale_fill_discrete(name = "Parter Sex")

#resources
LtResourcesPlot <- ggplot(LtData, aes(x=sex, y=resources, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Long Term Resources Preferences by Sex",x="Participant Sex", y = "LT Resources Preference") +
  scale_fill_discrete(name = "Parter Sex")


##ST Prefs violin plot



#health
StHealthPlot <- ggplot(StData, aes(x=sex, y=health, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Short Term Health Preferences by Sex",x="Participant Sex", y = "ST Health Preference") +
  scale_fill_discrete(name = "Parter Sex")

#intelligence
StIntelPlot <- ggplot(StData, aes(x=sex, y=intell, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Short Term Intelligence Preferences by Sex",x="Participant Sex", y = "ST Intelligence Preference") +
  scale_fill_discrete(name = "Parter Sex")

#kindness
StKindPlot <- ggplot(StData, aes(x=sex, y=kind, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Short Term Kindness Preferences by Sex",x="Participant Sex", y = "ST Kindness Preference") +
  scale_fill_discrete(name = "Parter Sex")

#physical attractiveness
StPhysattPlot <- ggplot(StData, aes(x=sex, y=physatt, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Short Term Physical Attractiveness Preferences by Sex",x="Participant Sex", y = "ST Physical Attractiveness Preference") +
  scale_fill_discrete(name = "Parter Sex")

#resources
StResourcesPlot <- ggplot(StData, aes(x=sex, y=resources, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Short Term Resources Preferences by Sex",x="Participant Sex", y = "ST Resources Preference") +
  scale_fill_discrete(name = "Parter Sex")





###Cluster analysis
  #across all traits
  #men rating women; men rating men; women rating men; women rating women
  #ipsatize prefs

