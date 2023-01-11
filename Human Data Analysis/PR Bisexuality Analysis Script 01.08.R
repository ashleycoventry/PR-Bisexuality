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
                      data = LtData) ##error message bc (1|PIN)



### violin plots (LT and ST)
  #pref rating on y axis
  #own sex on x axis (male v female)
  #plot 2 violins for each own sex (partner sex -- male or female)

##LT prefs violin plot

LtData$partnerSex  <- as.factor(LtData$partnerSex)

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

StData$partnerSex  <- as.factor(StData$partnerSex)

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
  #still separate my LT and ST?

##creating separate datasets


##long term prefs
#men rating men
LtMxMData <- subset(LtData, partnerSex == 1 & sex == 1)
naCheckMxM <- apply(LtMxMData[,4:8], 1, function(x) sum(is.na(x))>0)
LtMxMData<- LtMxMData[!naCheckMxM,]

#men rating women
LtWxWData <- subset(LtData, partnerSex == 0 & sex == 1)
naCheckWxW <- apply(LtWxWData[,4:8], 1, function(x) sum(is.na(x))>0)
LtWxWData<- LtWxWData[!naCheckWxW,]
  
LtWxWData <- subset(LtData, partnerSex == 0 & sex == 0)
naCheckWxW <- apply(LtWxWData[,4:8], 1, function(x) sum(is.na(x))>0)
LtWxWData<- LtWxWData[!naCheckWxW,]
  
LtWxMData <- subset(LtData, partnerSex == 1 & sex == 0)
naCheckWxM <- apply(LtWxMData[,4:8], 1, function(x) sum(is.na(x))>0)
LtWxMData<- LtWxMData[!naCheckWxM,] 

##ipsatizing preferences 

#z-score (so takes into account avg value of that trait across ppl) and then ipsatize z-scored value 

#MxW
LtMxWData[,4:8] <- apply(LtMxWData[,4:8],2,scale)
LtMxWData$mean<-sapply(unique(LtMxWData$PIN),function(x) 
  mean(unlist(LtMxWData[LtMxWData$PIN==x,4:8]))
)
LtWxWData[,4:8]<-LtWxWData[,4:8] - LtWxWData$mean

#MxM
LtMxMData[,4:8] <- apply(LtMxMData[,4:8],2,scale)
LtMxMData$mean<-sapply(unique(LtMxMData$PIN),function(x) 
  mean(unlist(LtMxMData[LtMxMData$PIN==x,4:8]))
)
LtMxMData[,4:8]<-LtMxMData[,4:8] - LtMxMData$mean

#WxW
LtWxWData[,4:8] <- apply(LtWxWData[,4:8],2,scale)
LtWxWData$mean<-sapply(unique(LtWxWData$PIN),function(x) 
  mean(unlist(LtWxWData[LtWxWData$PIN==x,4:8]))
)
LtWxWData[,4:8]<-LtWxWData[,4:8] - LtWxWData$mean

#WxM

LtWxMData[,4:8] <- apply(LtWxMData[,4:8],2,scale)
LtWxMData$mean<-sapply(unique(LtWxMData$PIN),function(x) 
  mean(unlist(LtWxMData[LtWxMData$PIN==x,4:8]))
)
LtWxMData[,4:8]<-LtWxMData[,4:8] - LtWxMData$mean



##Cluster Analysis for LT MxW

#extract kmeans wSs
kfitWssMxW<-sapply(1:7,function(x) kmeans(LtMxWData[,4:8],x)$tot.withinss)

#scree plot
screePlotMxW<-qplot(1:7,kfitWssMxW) #idk how to interpret this

#compute differences in within ss across k for k-means clustering
wssDiffsMxW<-diff(kfitWssMxW) 


#add classification to OG dataframe

#create vectors of preference means for each cluster 

#compute variance between trait ratings for each cluster

