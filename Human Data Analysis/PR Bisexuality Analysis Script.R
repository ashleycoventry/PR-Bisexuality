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
ltData <- data[,c(167, 5, 178:187)]
ltData <- melt(ltData, id.vars=c("PIN", "sex"))
ltData <- ltData %>% 
  separate("variable", into = c("partnerSex", "x", "trait"), remove = T)
ltData$partnerSex <- ifelse(ltData$partnerSex == "m", 0, 1)
ltData <- ltData[,c(1:3, 5:6)]
ltData$sex  <- as.factor(ltData$sex)

#make sure there are no NAs in sex or partner sex columns
nacheckLt <- apply(ltData[,2:3], 1, function(x) sum(is.na(x))>0)
ltData<- ltData[!nacheckLt,]

#ST

stData <- data[,c(167, 5, 188:197)]
stData <- melt(stData, id.vars=c("PIN", "sex"))
stData <- stData %>% 
  separate("variable", into = c("partnerSex", "x", "trait"), remove = T)
stData <- stData[,c(1:3, 5:6)]
stData$sex  <- as.factor(stData$sex)

#make sure there are no NAs in sex or partner sex columns
nacheckSt <- apply(stData[,2:3], 1, function(x) sum(is.na(x))>0)
stData<- stData[!nacheckSt,]

#ombnibus test

ltOmnibus <- lmer(value ~ partnerSex +  trait*sex + (1|PIN), 
                   data = ltData) 

stOmnibus <- lmer(value ~ partnerSex +  trait*sex + (1|PIN), 
                  data = stData) 

#make dataframe with every combo of trait, sex, partner sex and use predict function to predict pref values for each combo
predictDataLt <- expand.grid( unique(ltData$sex), unique(ltData$partnerSex), unique(ltData$trait))
predictDataLt <- predictDataLt %>%
  rename(
    sex = Var1,
    partnerSex = Var2,
    trait = Var3
  )

predictDataSt <- expand.grid(unique(stData$sex), unique(stData$partnerSex), unique(stData$trait))
predictDataSt <- predictDataSt %>%
  rename(
    sex = Var1,
    partnerSex = Var2,
    trait = Var3
  )

#use predict function
predictDataLt$predictedValues <- predict(ltOmnibus, newdata = predictDataLt) 
#will not work with or without PIN :( (with PIN, error says too many levels)



###LT prefs (using nested anova to look for main effects)

#tidy format for analyses
ltDataTidy <- ltData %>%
  pivot_wider(names_from = trait, 
              values_from = value)


#health
ltHealthMain <- lmer(scale(health)  ~ sex + partnerSex + (1|PIN),
                     data = ltDataTidy) #not sig

#kindness
ltKindMain <- lmer(scale(kind)  ~ sex + partnerSex + (1|PIN),
                   data = ltDataTidy) #sig main effect of sex and partner sex


#physical attractiveness
ltPhysattMain <- lmer(scale(physatt)  ~ sex + partnerSex + (1|PIN),
                      data = ltDataTidy) #sig main effect of sex but not partner sex

#intell
ltIntellMain <- lmer(scale(intell)  ~ sex + partnerSex + (1|PIN),
                     data = ltDataTidy) #sig effect of partner sex, not sex

#resources
ltResourceMain <- lmer(scale(resources)  ~ sex + partnerSex + (1|PIN),
                       data = ltDataTidy) #effect of partner sex, not sex


### ST prefs main effects ###

#tidy format for analyses
stDataTidy <- stData %>%
  pivot_wider(names_from = trait, 
              values_from = value)

#kindness
stKindMain <- lmer(kind ~ sex + partnerSex + (1|PIN), 
                   data = stDataTidy) #sig main effect of partner sex and sex

#physical attractiveness
stPhysattMain <- lmer(physatt ~ sex + partnerSex + (1|PIN), 
                   data = stDataTidy) #sig main effect of partner sex

#health
stHealthMain <- lmer(health  ~ sex + partnerSex + (1|PIN),
                     data = stDataTidy) #sig main effect of partner sex

#intelligence

stIntellMain <- lmer(intell  ~ sex + partnerSex + (1|PIN),
                     data = stDataTidy) #sig main effect of sex, not partner sex

#resources
stResourceMain <- lmer(scale(resources)  ~ sex + partnerSex + (1|PIN),
                       data = stDataTidy) #sig main effect of partner sex






### violin plots (LT and ST)
  #pref rating on y axis
  #own sex on x axis (male v female)
  #plot 2 violins for each own sex (partner sex -- male or female)

##LT prefs violin plot

ltDataTidy$partnerSex  <- as.factor(ltDataTidy$partnerSex)

#health


ltHealthPlot <- ggplot(ltDataTidy, aes(x=sex, y=health, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Long Term Health Preferences by Sex",x="Participant Sex", y = "LT Health Preference") +
  scale_fill_discrete(name = "Parter Sex")

#intelligence
ltIntelPlot <- ggplot(ltDataTidy, aes(x=sex, y=intell, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Long Term Intelligence Preferences by Sex",x="Participant Sex", y = "LT Intelligence Preference") +
  scale_fill_discrete(name = "Parter Sex")

#kindness
ltKindPlot <- ggplot(ltDataTidy, aes(x=sex, y=kind, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Long Term Kindness Preferences by Sex",x="Participant Sex", y = "LT Kindness Preference") +
  scale_fill_discrete(name = "Parter Sex")

#physical attractiveness
ltPhysattPlot <- ggplot(ltDataTidy, aes(x=sex, y=physatt, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Long Term Physical Attractiveness Preferences by Sex",x="Participant Sex", y = "LT Physical Attractiveness Preference") +
  scale_fill_discrete(name = "Parter Sex")

#resources
ltResourcesPlot <- ggplot(ltDataTidy, aes(x=sex, y=resources, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Long Term Resources Preferences by Sex",x="Participant Sex", y = "LT Resources Preference") +
  scale_fill_discrete(name = "Parter Sex")


##ST Prefs violin plot

stDataTidy$partnerSex  <- as.factor(stDataTidy$partnerSex)

#health
stHealthPlot <- ggplot(stDataTidy, aes(x=sex, y=health, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Short Term Health Preferences by Sex",x="Participant Sex", y = "ST Health Preference") +
  scale_fill_discrete(name = "Parter Sex")

#intelligence
stIntelPlot <- ggplot(stDataTidy, aes(x=sex, y=intell, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Short Term Intelligence Preferences by Sex",x="Participant Sex", y = "ST Intelligence Preference") +
  scale_fill_discrete(name = "Parter Sex")

#kindness
stKindPlot <- ggplot(stDataTidy, aes(x=sex, y=kind, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Short Term Kindness Preferences by Sex",x="Participant Sex", y = "ST Kindness Preference") +
  scale_fill_discrete(name = "Parter Sex")

#physical attractiveness
stPhysattPlot <- ggplot(stDataTidy, aes(x=sex, y=physatt, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Short Term Physical Attractiveness Preferences by Sex",x="Participant Sex", y = "ST Physical Attractiveness Preference") +
  scale_fill_discrete(name = "Parter Sex")

#resources
stResourcesPlot <- ggplot(stDataTidy, aes(x=sex, y=resources, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Short Term Resources Preferences by Sex",x="Participant Sex", y = "ST Resources Preference") +
  scale_fill_discrete(name = "Parter Sex")




###Cluster analysis
  #across all traits
  #men rating women; men rating men; women rating men; women rating women

##LT Prefs

#remove NAs from dataframe 

nacheckLt <- apply(ltDataTidy[,1:8], 1, function(x) sum(is.na(x))>0)
ltDataK<- ltDataTidy[!nacheckLt,]

#ipsatize traits


#z-score (so takes into account avg value of that trait across ppl)
ltDataK[,4:8] <- apply(ltDataK[,4:8],2,scale)

#ipsatize z-scored value 


#adding empty columns to dataframe
avColsLt <- c('ipHealth', 'ipIntell', 'ipKind', 'ipPhysatt', 'ipResources')
ltDataK[ , avColsLt] <- NA


#take means of traits for every PIN and subtract mean from indiv traits & place in new columns
for (i in 1:nrow(ltDataK)){
  focalPrefsLt <- ltDataK[i,4:8]
  avPrefsLt <- rowMeans(ltDataK[ltDataK$PIN == ltDataK$PIN[i],4:8], na.rm = T)
  focalPrefsLt <- focalPrefsLt - avPrefsLt
  ltDataK[i,9:13] <- focalPrefsLt
  
}




#extract kmeans wSs
kfitWssLt<-sapply(1:7,function(x) kmeans(ltDataK[,9:13],x)$tot.withinss)

#scree plot
screePlotLt<-qplot(1:7,kfitWssLt) 

##compute differences in within ss across k for k-means clustering
wssDiffsLt<-diff(kfitWssLt) #4 clusters -- I think?

##Add classification to the original dataframe

kFitLt<-kmeans(ltDataK[,9:13],4)
ltDataK$kFitLt <- kFitLt$cluster


##Create vectors of preference means for each cluster 
clustCentersLt<-kFitLt$centers

##Look at breakdown by cluster, sex, and partner sex #0 = women, #1 = men
clustSexLt<-table(ltDataK$sex, ltDataK$kFitLt, ltDataK$partnerSex)/rowSums(table(ltDataK$sex, ltDataK$kFitLt, ltDataK$partnerSex))
#make two sep tables based on sex and get proportions

#are men and women are choosing clusters at diff rates?
chisqSexLt<-chisq.test(table(ltDataK$sex,ltDataK$kFitLt)) #yes 

#are participants choosing clusters at diff rates based on ideal partner sex?
chisqPSexLt <- chisq.test(table(ltDataK$partnerSex,ltDataK$kFitLt)) #no

### Plotting ###

##plot bar graph with each trait mean for each 3 clusters (# clusters depends on scree)
meanTraitLt <- c(clustCentersLt[1,], clustCentersLt[2,], clustCentersLt[3,], clustCentersLt[4,])
mateTypeLt <-c(rep("1", 5), rep("2", 5), rep("3", 5), rep("4", 5))
traitLt <- c(rep(c("health", "intelligence", "kindness", "physical attractiveness", "resources"), 4))  
plottingLt <- data.frame(meanTraitLt, mateTypeLt, traitLt)
kFitPlotLt <- ggplot(data=plottingLt, aes(x=mateTypeLt, y=meanTraitLt, fill=traitLt)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal(base_size = 15) + xlab("Type of Mate") + ylab("Desired Trait Level") +
  scale_fill_discrete(name = "Trait")





##ST Prefs

#remove NAs from dataframe 

nacheckSt <- apply(stDataTidy[,1:8], 1, function(x) sum(is.na(x))>0)
stDataK<- stDataTidy[!nacheckSt,]

#ipsatize traits


#z-score (so takes into account avg value of that trait across ppl)
stDataK[,4:8] <- apply(stDataK[,4:8],2,scale)

#ipsatize z-scored value 


#adding empty columns to dataframe
avColsSt <- c('ipHealth', 'ipIntell', 'ipKind', 'ipPhysatt', 'ipResources')
stDataK[ , avColsSt] <- NA


#take means of traits for every PIN and subtract mean from indiv traits & place in new columns
for (i in 1:nrow(stDataK)){
  focalPrefsSt <- stDataK[i,4:8]
  avPrefsSt <- rowMeans(stDataK[stDataK$PIN == stDataK$PIN[i],4:8], na.rm = T)
  focalPrefsSt <- focalPrefsSt - avPrefsSt
  stDataK[i,9:13] <- focalPrefsSt
  
}




#extract kmeans wSs
kfitWssSt<-sapply(1:7,function(x) kmeans(stDataK[,9:13],x)$tot.withinss)

#scree plot
screePlotSt<-qplot(1:7,kfitWssSt) 

##compute differences in within ss across k for k-means clustering
wssDiffsSt<-diff(kfitWssSt) #4 clusters?

##Add classification to the original dataframe

kFitSt<-kmeans(stDataK[,9:13],4)
stDataK$kFitSt <- kFitSt$cluster


##Create vectors of preference means for each cluster 
clustCentersSt<-kFitSt$centers

##Look at breakdown by cluster, sex, and partner sex #0 = women, #1 = men
clustSexSt<-table(stDataK$sex,stDataK$kFitSt, stDataK$partnerSex)


#are men and women are choosing clusters at diff rates?
chisqSexSt<-chisq.test(table(stDataK$sex,stDataK$kFitSt)) #yes 

#are participants choosing clusters at diff rates based on ideal partner sex?
chisqPSexSt <- chisq.test(table(stDataK$partnerSex,stDataK$kFitSt)) #yes

### Plotting ###

##plot bar graph with each trait mean for each 3 clusters (# clusters depends on scree)
meanTraitSt <- c(clustCentersSt[1,], clustCentersSt[2,], clustCentersSt[3,], clustCentersSt[4,])
mateTypeSt <-c(rep("1", 5), rep("2", 5), rep("3", 5), rep("4", 5))
traitSt <- c(rep(c("health", "intelligence", "kindness", "physical attractiveness", "resources"), 4))  
plottingSt <- data.frame(meanTraitSt, mateTypeSt, traitSt)
kFitPlotSt <- ggplot(data=plottingSt, aes(x=mateTypeSt, y=meanTraitSt, fill=traitSt)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal(base_size = 15) + xlab("Type of Mate") + ylab("Desired Trait Level") +
  scale_fill_discrete(name = "Trait")


