#####Project Rainbow Analysis Script -- Bisexual Preferences#####
####### Ashley Coventry, Katy Walter, Ben Gelbart, Tamsin German, & Dan Conroy-Beam ########

###load packages###
library(lmerTest)
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

#exclude intersex participants
data <-  data[data$sex!=2,]

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

###Omnibus Analyses


#longform data

#LT 
LtData <- data[,c(167, 5, 178:187)]
LtData <- melt(LtData, id.vars=c("PIN", "sex"))
LtData <- LtData %>% 
  separate("variable", into = c("partnerSex", "x", "trait"), remove = T)
LtData$partnerSex <- ifelse(LtData$partnerSex == "m", 0, 1)
LtData <- LtData[,c(1:3, 5:6)]
LtData$sex  <- as.factor(LtData$sex)

#ST

StData <- data[,c(167, 5, 188:197)]
StData <- melt(StData, id.vars=c("PIN", "sex"))
StData <- StData %>% 
  separate("variable", into = c("partnerSex", "x", "trait"), remove = T)
StData <- StData[,c(1:3, 5:6)]
StData$sex  <- as.factor(StData$sex)

#ombnibus test

LtOmnibus <- lmer(value ~ trait*partnerSex + (1|PIN), 
                   data = LtData) #sig interaction between resource prefs and partner sex

StOmnibus <- lmer(value ~ trait*partnerSex + (1|PIN), 
                  data = StData) #no sig interactions 



###LT prefs multilevel models
#looking to see if there is a significant overall effect 
#of trait + own sex + partner sex on long term prefs
#nested under participant ID (PIN)

#tidy format for analyses
LtDataTidy <- LtData %>%
  pivot_wider(names_from = trait, 
              values_from = value)

#health
LtHealthMM <- lmer(health  ~ sex * partnerSex + (1|PIN),
                    data = LtDataTidy) 



#intelligence
LtIntellMM <- lmer(intell  ~ sex * partnerSex + (1|PIN),
                   data = LtDataTidy) #significant effect of partner sex on intell pref

#kindness

LtKindMM <- lmer(kind  ~ sex * partnerSex + (1|PIN),
                   data = LtDataTidy) 


#physical attractiveness
LtPhysattMM <- lmer(physatt  ~ sex * partnerSex + (1|PIN),
                 data = LtDataTidy) 


#resources

LtResourcesMM <- lmer(resources  ~ sex * partnerSex + (1|PIN),
                 data = LtDataTidy) #signficant effect of partner sex



### ST prefs multilevel model ###

#tidy format for analyses
StDataTidy <- StData %>%
  pivot_wider(names_from = trait, 
              values_from = value)




#health
StHealthMM <- lmer(health  ~ sex * partnerSex + (1|PIN),
                 data = StDataTidy) 



#intelligence

StIntellMM <- lmer(intell  ~ sex * partnerSex + (1|PIN),
                   data = StDataTidy) 

#kindness

StKindMM <- lmer(kind  ~ sex * partnerSex + (1|PIN),
                   data = StDataTidy)  #sig effect of sex and partner sex,
                                       #sig interaction between sex and partner sex


#physical attractiveness

StPhysattMM <- lmer(physatt  ~ sex * partnerSex + (1|PIN),
                   data = StDataTidy)  #sig effect of partner sex 


#resources

StResourcesMM <- lmer(resources  ~ sex * partnerSex + (1|PIN),
                   data = StDataTidy) #sig effect of partner sex



### main effects - nested ANOVA ###
  #running bc no interaction found for some traits in previous analyses


##LT prefs that found no sig interactions

#health
LtHealthMain <- lmer(health  ~ sex + partnerSex + (1|PIN),
                   data = LtDataTidy) #not sig

#kindness

LtKindMain <- lmer(kind  ~ sex + partnerSex + (1|PIN),
                 data = LtDataTidy) #sig main effect of sex and partner sex


#physical attractiveness
LtPhysattMain <- lmer(physatt  ~ sex + partnerSex + (1|PIN),
                    data = LtDataTidy) #sig main effect of sex but not partner sex



##St prefs that found no sig interactions

#health
StHealthMain <- lmer(health  ~ sex + partnerSex + (1|PIN),
                   data = StDataTidy) #sig main effect of partner sex



#intelligence

StIntellMain <- lmer(intell  ~ sex + partnerSex + (1|PIN),
                   data = StDataTidy) #sig main effect of sex



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

##LT Prefs

#remove NAs from dataframe 

nacheck <- apply(LtDataTidy[,1:8], 1, function(x) sum(is.na(x))>0)
LtDataK<- LtDataTidy[!nacheck,]

#ipsatize traits


#z-score (so takes into account avg value of that trait across ppl)
LtDataK[,4:8] <- apply(LtDataK[,4:8],2,scale)

#ipsatize z-scored value 
#for loop isn't working bc issues with PIN column
#dan: If not, you can try wrapping the mean() function in rep(...,2) so that each participant's mean is just repeated twice. 
#That only works if their two rows are next to each other though

LtDataK <- LtDataK %>% arrange(PIN)

#idk if below will work? 
LtDataK$mean<-if(LtDataK[LtDataK$PIN==LtDataK$PIN[i]]){
  sapply(unique(LtDataK$PIN),function(x) 
    rep(mean(unlist(LtDataK[LtDataK$PIN==x,4:8])), 2)
  )
} else{
  sapply(unique(LtDataK$PIN),function(x) 
    mean(unlist(LtDataK[LtDataK$PIN==x,4:8])))
}





#extract kmeans wSs
kfitWss<-sapply(1:7,function(x) kmeans(LtDataK[,4:8],x)$tot.withinss)

#scree plot
screePlot<-qplot(1:7,kfitWss) 

##compute differences in within ss across k for k-means clustering
wssDiffs<-diff(kfitWss) #4 clusters -- I think?

##Add classification to the original dataframe

kFit<-kmeans(LtDataK[,4:8],4)
LtDataK$kFit <- kFit$cluster





#

