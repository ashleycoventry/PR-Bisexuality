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
data$m_Lt_AgeLik <- data$m_lt_age - data$ageLik

#male ideal ST
data$m_St_AgeLik <- data$m_st_age - data$ageLik

#female ideal LT
data$f_Lt_AgeLik <- data$f_lt_age - data$ageLik

#female ideal ST
data$f_St_AgeLik <- data$f_st_age - data$ageLik

##add avg of other pref ratings to age 
  #so that when prefs + age are standardized, age is centered around 0

#data$m_Lt_AgeLik <- mean(c(data[,178:187],na.rm=T)) + data$m_Lt_AgeLik #(adding Lt pref avgs only)
#data$f_Lt_AgeLik  <- mean(c(data[,178:187],na.rm=T)) + data$f_Lt_AgeLik
#data$m_St_AgeLik <- mean(c(data[,188:197],na.rm = T)) + data$m_St_AgeLik #(adding st pref avg only)
#data$f_St_AgeLik <- mean(c(data[,188:197],na.rm = T)) + f_St_AgeLik


###Omnibus Analyses


#longform data

#LT 
ltData <- data[,c(167, 5, 178:187, 227, 229)]
ltData <- melt(ltData, id.vars=c("PIN", "sex"))
ltData <- ltData %>% 
  separate("variable", into = c("partnerSex", "x", "trait"), remove = T)
ltData$partnerSex <- ifelse(ltData$partnerSex == "m", 1, 0)
ltData <- ltData[,c(1:3, 5:6)]
ltData$sex  <- as.factor(ltData$sex)

#make sure there are no NAs in sex or partner sex columns
nacheckLt <- apply(ltData[,2:3], 1, function(x) sum(is.na(x))>0)
ltData<- ltData[!nacheckLt,]

#Standardize preferences
ltData$value<-scale(ltData$value)

#ST

stData <- data[,c(167, 5, 188:197, 228, 230)]
stData <- melt(stData, id.vars=c("PIN", "sex"))
stData <- stData %>% 
  separate("variable", into = c("partnerSex", "x", "trait"), remove = T)
stData$partnerSex <- ifelse(stData$partnerSex == "m", 1, 0)
stData <- stData[,c(1:3, 5:6)]
stData$sex  <- as.factor(stData$sex)

#make sure there are no NAs in sex or partner sex columns
nacheckSt <- apply(stData[,2:3], 1, function(x) sum(is.na(x))>0)
stData<- stData[!nacheckSt,]

#Standardize preferencse
stData$value<-scale(stData$value)

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


##LT predict 

#use predict function
predictDataLt$predictedValues <- predict(ltOmnibus, newdata = predictDataLt, re.form = NA) 

#plot predicted values splitting by sex
predictPlotSexLt <- ggplot(data = predictDataLt, aes(x=trait, y=predictedValues, fill=sex))  + 
  geom_bar(stat = "identity", position=position_dodge()) + facet_wrap(~partnerSex)


##St predict
#use predict function
predictDataSt$predictedValues <- predict(stOmnibus, newdata = predictDataSt, re.form = NA) 

#plot predicted values splitting by sex
predictPlotSexSt <- ggplot(data = predictDataSt, aes(x=trait, y=predictedValues, fill=sex))  + 
  geom_bar(stat = "identity", position=position_dodge()) + facet_wrap(~partnerSex)




###LT prefs (using nested anova to look for main effects)

#tidy format for analyses
ltDataTidy <- ltData %>%
  pivot_wider(names_from = trait, 
              values_from = value)


#health
ltHealthMain <- lmer(health  ~ sex + partnerSex + (1|PIN),
                     data = ltDataTidy) #not sig

#kindness
ltKindMain <- lmer(kind  ~ sex + partnerSex + (1|PIN),
                   data = ltDataTidy) #sig main effect of sex


#physical attractiveness
ltPhysattMain <- lmer(physatt  ~ sex + partnerSex + (1|PIN),
                      data = ltDataTidy) #sig main effect of sex 

#intell
ltIntellMain <- lmer(intell  ~ sex + partnerSex + (1|PIN),
                     data = ltDataTidy) #no sig effects 

#resources
ltResourceMain <- lmer(resources  ~ sex + partnerSex + (1|PIN),
                       data = ltDataTidy) #no sig effects

#ideal age
ltAgeMain <- lmer(AgeLik ~ sex + partnerSex + (1|PIN), 
                  data = ltDataTidy) #effect of partner sex and sex 

### ST prefs main effects ###

#tidy format for analyses
stDataTidy <- stData %>%
  pivot_wider(names_from = trait, 
              values_from = value)

#kindness
stKindMain <- lmer(kind ~ sex + partnerSex + (1|PIN), 
                   data = stDataTidy) #sig main effect of sex

#physical attractiveness
stPhysattMain <- lmer(physatt ~ sex + partnerSex + (1|PIN), 
                   data = stDataTidy) #no sig effects 

#health
stHealthMain <- lmer(health  ~ sex + partnerSex + (1|PIN),
                     data = stDataTidy) #no sig effects

#intelligence

stIntellMain <- lmer(intell  ~ sex + partnerSex + (1|PIN),
                     data = stDataTidy) #sig main effect of sex, not partner sex

#resources
stResourceMain <- lmer(resources  ~ sex + partnerSex + (1|PIN),
                       data = stDataTidy) #no sig effects after standardizing


#age
stAgeMain <- lmer(AgeLik ~ sex + partnerSex + (1|PIN), 
                  data = stDataTidy) #sig main effect of sex



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
  scale_fill_discrete(name = "Partner Sex")

#intelligence
ltIntelPlot <- ggplot(ltDataTidy, aes(x=sex, y=intell, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Long Term Intelligence Preferences by Sex",x="Participant Sex", y = "LT Intelligence Preference") +
  scale_fill_discrete(name = "Partner Sex")

#kindness
ltKindPlot <- ggplot(ltDataTidy, aes(x=sex, y=kind, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Long Term Kindness Preferences by Sex",x="Participant Sex", y = "LT Kindness Preference") +
  scale_fill_discrete(name = "Partner Sex")

#physical attractiveness
ltPhysattPlot <- ggplot(ltDataTidy, aes(x=sex, y=physatt, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Long Term Physical Attractiveness Preferences by Sex",x="Participant Sex", y = "LT Physical Attractiveness Preference") +
  scale_fill_discrete(name = "Partner Sex")

#resources
ltResourcesPlot <- ggplot(ltDataTidy, aes(x=sex, y=resources, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Long Term Resources Preferences by Sex",x="Participant Sex", y = "LT Resources Preference") +
  scale_fill_discrete(name = "Partner Sex")

#age
ltAgePlot <- ggplot(ltDataTidy, aes(x=sex, y=AgeLik, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Long Term Resources Preferences by Sex",x="Participant Sex", y = "LT Age Preference") +
  scale_fill_discrete(name = "Partner Sex")

##ST Prefs violin plot

stDataTidy$partnerSex  <- as.factor(stDataTidy$partnerSex)

#health
stHealthPlot <- ggplot(stDataTidy, aes(x=sex, y=health, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Short Term Health Preferences by Sex",x="Participant Sex", y = "ST Health Preference") +
  scale_fill_discrete(name = "Partner Sex")

#intelligence
stIntelPlot <- ggplot(stDataTidy, aes(x=sex, y=intell, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Short Term Intelligence Preferences by Sex",x="Participant Sex", y = "ST Intelligence Preference") +
  scale_fill_discrete(name = "Partner Sex")

#kindness
stKindPlot <- ggplot(stDataTidy, aes(x=sex, y=kind, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Short Term Kindness Preferences by Sex",x="Participant Sex", y = "ST Kindness Preference") +
  scale_fill_discrete(name = "Partner Sex")

#physical attractiveness
stPhysattPlot <- ggplot(stDataTidy, aes(x=sex, y=physatt, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Short Term Physical Attractiveness Preferences by Sex",x="Participant Sex", y = "ST Physical Attractiveness Preference") +
  scale_fill_discrete(name = "Partner Sex")

#resources
stResourcesPlot <- ggplot(stDataTidy, aes(x=sex, y=resources, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Short Term Resources Preferences by Sex",x="Participant Sex", y = "ST Resources Preference") +
  scale_fill_discrete(name = "Partner Sex")

#age
stAgePlot <- ggplot(stDataTidy, aes(x=sex, y=AgeLik, fill = partnerSex)) +
  geom_violin() + 
  scale_x_discrete(limits=c("0", "1")) +
  labs(title="Plot Long Term Resources Preferences by Sex",x="Participant Sex", y = "LT Age Preference") +
  scale_fill_discrete(name = "Partner Sex")


###Cluster analysis
  #across all traits, excluding age 
  #men rating women; men rating men; women rating men; women rating women

##LT Prefs

#remove NAs from dataframe 

nacheckLt <- apply(ltDataTidy[,1:9], 1, function(x) sum(is.na(x))>0)
ltDataK<- ltDataTidy[!nacheckLt,1:8] #excludes age also

###ipsatize traits


##z-score (so takes into account avg value of that trait across ppl)
ltDataK[,4:8] <- apply(ltDataK[,4:8],2,scale)

##ipsatize z-scored values

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
wssDiffsLt<-diff(kfitWssLt) 

##Add classification to the original dataframe

kFitLt<-kmeans(ltDataK[,9:13],4)
ltDataK$kFitLt <- kFitLt$cluster


##Create vectors of preference means for each cluster 
clustCentersLt<-kFitLt$centers

##Look at breakdown by cluster, sex, and partner sex
clustSexLt<-table(ltDataK$sex, ltDataK$kFitLt, ltDataK$partnerSex)/rowSums(table(ltDataK$sex, ltDataK$kFitLt, ltDataK$partnerSex))

#cluster breakdown of women
clustSexLtF <- table(ltDataK$partnerSex[ltDataK$sex == 0], ltDataK$kFitLt[ltDataK$sex == 0])/
  rowSums(table(ltDataK$partnerSex[ltDataK$sex == 0], ltDataK$kFitLt[ltDataK$sex == 0]))

#cluster breakdown of men
clustSexLtM <- table(ltDataK$partnerSex[ltDataK$sex == 1], ltDataK$kFitLt[ltDataK$sex == 1])/
  rowSums(table(ltDataK$partnerSex[ltDataK$sex == 1], ltDataK$kFitLt[ltDataK$sex == 1]))


#are men and women are choosing clusters at diff rates? 
  #have to separate based on ideal male v female partners bc independence assumption
chisqSexLtIdealF<-chisq.test(table(ltDataK$sex[ltDataK$partnerSex == 0],ltDataK$kFitLt[ltDataK$partnerSex == 0])) #yes 
chisqSexLtIdealM<-chisq.test(table(ltDataK$sex[ltDataK$partnerSex == 1],ltDataK$kFitLt[ltDataK$partnerSex == 1])) #no


#do clusters differ by partner sex?
ltDataK$kFitLt <-as.factor(ltDataK$kFitLt)
logRegModelLt <- glmer(partnerSex ~ kFitLt + (1|PIN), data = ltDataK, family = "binomial") 
 



### Plotting ###


##plot bar graph with each trait mean for each 4 clusters (# clusters depends on scree)
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

nacheckSt <- apply(stDataTidy[,1:9], 1, function(x) sum(is.na(x))>0)
stDataK<- stDataTidy[!nacheckSt,1:8]

#ipsatize traits


#z-score (so takes into account avg value of that trait across ppl)
stDataK[,4:8] <- apply(stDataK[,4:8],2,scale)


#adding empty columns to dataframe
avColsSt <- c('ipHealth', 'ipIntell', 'ipKind', 'ipPhysatt', 'ipResources')
stDataK[ , avColsSt] <- NA


#take means of traits for every PIN and subtract mean from indiv traits & place in new columns
for (i in 1:nrow(stDataK)){
  focalPrefsSt <- stDataK[i,4:8]
  avPrefsSt <- rowMeans(stDataK[stDataK$PIN == stDataK$PIN[i],4:8], na.rm = T)
  focalPrefsSt <- focalPrefsSt - avPrefsSt
  stDataK[i,10:14] <- focalPrefsSt
  
}



##st prefs cluster analysis 


#extract kmeans wSs
kfitWssSt<-sapply(1:7,function(x) kmeans(stDataK[,10:14],x)$tot.withinss)

#scree plot
screePlotSt<-qplot(1:7,kfitWssSt) 

##compute differences in within ss across k for k-means clustering
wssDiffsSt<-diff(kfitWssSt) #4 clusters?

##Add classification to the original dataframe

kFitSt<-kmeans(stDataK[,10:14],4)
stDataK$kFitSt <- kFitSt$cluster


##Create vectors of preference means for each cluster 
clustCentersSt<-kFitSt$centers

##Look at breakdown by cluster, sex, and partner sex #0 = women, #1 = men
clustSexSt<-table(stDataK$sex,stDataK$kFitSt, stDataK$partnerSex)

#cluster choice for female participants
clustSexStF <- table(stDataK$partnerSex[stDataK$sex == 0], stDataK$kFitSt[stDataK$sex == 0])/
  rowSums(table(stDataK$partnerSex[stDataK$sex == 0], stDataK$kFitSt[stDataK$sex == 0]))

#cluster choice for male participants
clustSexStM <- table(stDataK$partnerSex[stDataK$sex == 1], stDataK$kFitSt[stDataK$sex == 1])/
  rowSums(table(stDataK$partnerSex[stDataK$sex == 1], stDataK$kFitSt[stDataK$sex == 1]))



#are men and women are choosing clusters at diff rates? 
#have to separate based on ideal male v female partners bc independence assumption
chisqSexStIdealF<-chisq.test(table(stDataK$sex[stDataK$partnerSex == 0],stDataK$kFitSt[stDataK$partnerSex == 0])) #no 
chisqSexStIdealM<-chisq.test(table(stDataK$sex[stDataK$partnerSex == 1],stDataK$kFitSt[stDataK$partnerSex == 1])) #no


#are participants choosing clusters at diff rates based on ideal partner sex
#log reg predicting psex from kfit and sex?

logRegModelSt <- glmer(partnerSex ~ kFitSt + sex + (1|PIN), data = stDataK, family = "binomial") 





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

