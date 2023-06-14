#####Project Rainbow Analysis Script -- Bisexual Preferences#####
####### Ashley Coventry, Katy Walter, Ben Gelbart, Tamsin German, & Dan Conroy-Beam ########

###load packages###
library(lmerTest)
library(multilevelTools)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(tidyr)
library(ggpubr) #for panel plot
library(ggpattern)
library(ggridges) #for ridgeline plots


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
#hashtagged out bc we currently aren't standardizing

#data$m_Lt_AgeLik <- mean(unlist(data[,178:187]),na.rm=T) + data$m_Lt_AgeLik #(adding Lt pref avgs only)
#data$f_Lt_AgeLik  <- mean(unlist(data[,178:187]),na.rm=T) + data$f_Lt_AgeLik
#data$m_St_AgeLik <- mean(unlist(data[,188:197]),na.rm = T) + data$m_St_AgeLik #(adding st pref avg only)
#data$f_St_AgeLik <- mean(unlist(data[,188:197]),na.rm = T) + data$f_St_AgeLik


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
ltData$partnerSex  <- as.factor(ltData$partnerSex)

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
stData$partnerSex  <- as.factor(stData$partnerSex)

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


#create new group variable  (0 = male participant/male target, 1 = male/female, 2 = female/male, 3 = female/female)
predictDataLt$group <- ifelse(predictDataLt$sex == 1 & predictDataLt$partnerSex == 1, 0, 
                       ifelse(predictDataLt$sex == 1 & predictDataLt$partnerSex == 0, 1, 
                              ifelse(predictDataLt$sex == 0 & predictDataLt$partnerSex == 1, 2,
                                     ifelse(predictDataLt$sex == 0 & predictDataLt$partnerSex == 0, 3, NA))))

predictDataLt$group <- as.factor(predictDataLt$group)

predictPlotLt <- ggplot(data = predictDataLt, aes(x=trait, y=predictedValues, fill=group))+  
  geom_bar(stat = "identity", position=position_dodge())+ 
  scale_fill_manual(values = c("0" = "darkblue", "1" = "yellow", "2" ="lightblue", "3" = "lightyellow"), 
                    labels = c("male & male target", "male & female target", "female & male target", "female & female target")) +
  labs(y= "Predicted Trait Values", x = "Trait")



#ggsave("predictPlotLt.jpeg", plot=last_plot(), width=225, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)


##St predict
#use predict function
predictDataSt$predictedValues <- predict(stOmnibus, newdata = predictDataSt, re.form = NA) 


#create new group variable  (0 = male participant/male target, 1 = male/female, 2 = female/male, 3 = female/female)
predictDataSt$group <- ifelse(predictDataSt$sex == 1 & predictDataSt$partnerSex == 1, 0, 
                              ifelse(predictDataSt$sex == 1 & predictDataSt$partnerSex == 0, 1, 
                                     ifelse(predictDataSt$sex == 0 & predictDataSt$partnerSex == 1, 2,
                                            ifelse(predictDataSt$sex == 0 & predictDataSt$partnerSex == 0, 3, NA))))

predictDataSt$group <- as.factor(predictDataSt$group)


predictPlotSt <- ggplot(data = predictDataSt, aes(x=trait, y=predictedValues, fill=group))+  
  geom_bar(stat = "identity", position=position_dodge())+ 
  scale_fill_manual(values = c("0" = "darkblue", "1" = "yellow", "2" ="lightblue", "3" = "lightyellow"), 
                    labels = c("male & male target", "male & female target", "female & male target", "female & female target")) +
  labs(y= "Predicted Trait Values", x = "Trait")
#ggsave("predictPlotSt.jpeg", plot=last_plot(), width=225, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)



###LT prefs (using nested anova to look for interaction effects)

#tidy format for analyses
ltDataTidy <- ltData %>%
  pivot_wider(names_from = trait, 
              values_from = value)


#health
ltHealthInt <- lmer(health  ~ sex*partnerSex + (1|PIN),
                     data = ltDataTidy) #not sig

ltHealthMain <- lmer(health  ~ sex+partnerSex + (1|PIN),
                     data = ltDataTidy) #not sig
#kindness 
ltKindInt <- lmer(kind  ~ sex*partnerSex + (1|PIN),
                   data = ltDataTidy) #no interaction

ltKindMain <- lmer(kind  ~ sex+partnerSex + (1|PIN),
                  data = ltDataTidy) #sig main of sex and psex

#physical attractiveness
ltPhysattInt <- lmer(physatt  ~ sex*partnerSex + (1|PIN),
                      data = ltDataTidy) #not sig

ltPhysattMain <- lmer(physatt  ~ sex+partnerSex + (1|PIN),
                     data = ltDataTidy) #sig main effect of sex

#intell
ltIntellInt <- lmer(intell  ~ sex*partnerSex + (1|PIN),
                     data = ltDataTidy) #not sig

ltIntellMain <- lmer(intell  ~ sex+partnerSex + (1|PIN),
                    data = ltDataTidy) #sig effect of sex

#resources
ltResourceInt <- lmer(resources  ~ sex*partnerSex + (1|PIN),
                       data = ltDataTidy) #not sig

ltResourceMain <- lmer(resources  ~ sex+partnerSex + (1|PIN),
                      data = ltDataTidy)  #sig main effect of psex, not sex

#ideal age
ltAgeInt <- lmer(AgeLik ~ sex*partnerSex + (1|PIN), 
                  data = ltDataTidy) #not sig 

ltAgeMain <- lmer(AgeLik ~ sex+partnerSex + (1|PIN), 
                 data = ltDataTidy) #sig main effect of sex and partner sex

### ST prefs main effects ###

#tidy format for analyses
stDataTidy <- stData %>%
  pivot_wider(names_from = trait, 
              values_from = value)

#kindness
stKindInt <- lmer(kind ~ sex*partnerSex + (1|PIN), 
                   data = stDataTidy) 
                #sig interaction

#physical attractiveness
stPhysattInt <- lmer(physatt ~ sex*partnerSex + (1|PIN), 
                   data = stDataTidy) 

stPhysattMain <- lmer(physatt ~ sex+partnerSex + (1|PIN), 
                      data = stDataTidy) #only sig main effect of partner sex


#health
stHealthInt <- lmer(health  ~ sex*partnerSex + (1|PIN),
                     data = stDataTidy)

stHealthMain <- lmer(health  ~ sex+partnerSex + (1|PIN),
                     data = stDataTidy) #sig main effect of partner sex


#intelligence

stIntellInt <- lmer(intell  ~ sex*partnerSex + (1|PIN),
                     data = stDataTidy)  

stIntellMain <- lmer(intell  ~ sex+partnerSex + (1|PIN),
                    data = stDataTidy) #sig main effect of partner sex

#resources
stResourceInt <- lmer(resources  ~ sex*partnerSex + (1|PIN),
                       data = stDataTidy) 

stResourceMain <- lmer(resources  ~ sex+partnerSex + (1|PIN),
                      data = stDataTidy) #sig effect of partner sex

#age
stAgeInt <- lmer(AgeLik ~ sex*partnerSex + (1|PIN), 
                  data = stDataTidy) 

stAgeMain <- lmer(AgeLik ~ sex+partnerSex + (1|PIN), 
                  data = stDataTidy) #sig main effect of sex and partner sex



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




###Ridgeline Plots
#traits on Y axis; values on X axis with different colors for each group (male/male, male/female, female/male, female/female)

###lt prefs

#create new group variable  (0 = male participant/male target, 1 = male/female, 2 = female/male, 3 = female/female)
ltData$group <- ifelse(ltData$sex == 1 & ltData$partnerSex == 1, 0, 
                           ifelse(ltData$sex == 1 & ltData$partnerSex == 0, 1, 
                                  ifelse(ltData$sex == 0 & ltData$partnerSex == 1, 2,
                                         ifelse(ltData$sex == 0 & ltData$partnerSex == 0, 3, NA))))
ltData$group <- as.factor(ltData$group)

#separate dataframe by men and women and then male vs female partners
ltDataMale <- subset(ltData, ltData$sex == 1)
ltDataFemale <- subset(ltData, ltData$sex ==0)
ltDataMalePartner <- subset(ltData, ltData$partnerSex == 1)
ltDataFemalePartner <- subset(ltData, ltData$partnerSex == 0)

#overall plot (everyone together)

ltRidgeplot <- ggplot(ltData, aes(x = value[,1], y = trait, fill = group)) +
  geom_density_ridges(scale = 1, alpha = 0.6) +
  scale_fill_manual(values = c("0" = "darkblue", "1" = "yellow", "2" ="lightblue", "3" = "lightyellow"), 
                    labels = c("male & male target", "male & female target", "female & male target", "female & female target")) +
  labs(x = "Preference Value", y = "Trait") +
  theme_ridges()+
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5))
#ggsave("ridgelineLt.jpeg", plot=last_plot(), width=225, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)


#only men

ltRidgeplotMen <- ggplot(ltDataMale, aes(x = value[,1], y = trait, fill = group)) +
  geom_density_ridges(scale = 1, alpha = 0.6) +
  scale_fill_manual(values = c("0" = "darkblue", "1" = "yellow"), 
                    labels = c("male & male target", "male & female target")) +
  labs(x = "Preference Value", y = "Trait") +
  theme_ridges()+
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5))


#only women
ltRidgeplotWomen <- ggplot(ltDataFemale, aes(x = value[,1], y = trait, fill = group)) +
  geom_density_ridges(scale = 1, alpha = 0.6) +
  scale_fill_manual(values = c("2" ="lightblue", "3" = "lightyellow"), 
                    labels = c("female & male target", "female & female target")) +
  labs(x = "Preference Value", y = "Trait") +
  theme_ridges()+
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5))

#only male partners
ltRidgeplotMalePartners <- ggplot(ltDataMalePartner, aes(x = value[,1], y = trait, fill = group)) +
  geom_density_ridges(scale = 1, alpha = 0.6) +
  scale_fill_manual(values = c("0" = "darkblue", "2" ="lightblue"), 
                    labels = c("male & male target", "female & male target")) +
  labs(x = "Preference Value", y = "Trait") +
  theme_ridges()+
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5))

#only female partners
ltRidgeplotFemalePartners <- ggplot(ltDataFemalePartner, aes(x = value[,1], y = trait, fill = group)) +
  geom_density_ridges(scale = 1, alpha = 0.6) +
  scale_fill_manual(values = c("1" = "yellow", "3" = "lightyellow"), 
                    labels = c("male & female target", "female & female target")) +
  labs(x = "Preference Value", y = "Trait") +
  theme_ridges()+
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5))


##Overall plot to match predict plot

OverallPlotLt <- ggplot(data = ltData, aes(x=trait, y=value[,1], fill=group))+  
  geom_bar(stat = "identity", position=position_dodge())+ 
  scale_fill_manual(values = c("0" = "darkblue", "1" = "yellow", "2" ="lightblue", "3" = "lightyellow"), 
                    labels = c("male & male target", "male & female target", "female & male target", "female & female target")) +
  labs(y= "Trait Values", x = "Trait")








##st prefs

#create new group variable  (0 = male participant/male target, 1 = male/female, 2 = female/male, 3 = female/female)
stData$group <- ifelse(stData$sex == 1 & stData$partnerSex == 1, 0, 
                       ifelse(stData$sex == 1 & stData$partnerSex == 0, 1, 
                              ifelse(stData$sex == 0 & stData$partnerSex == 1, 2,
                                     ifelse(stData$sex == 0 & stData$partnerSex == 0, 3, NA))))


stData$group <- as.factor(stData$group)

#separate dataframe by men and women and then male vs female partners
stDataMale <- subset(stData, stData$sex == 1)
stDataFemale <- subset(stData, stData$sex ==0)
stDataMalePartner <- subset(stData, stData$partnerSex == 1)
stDataFemalePartner <- subset(stData, stData$partnerSex == 0)




#overall plot

stRidgeplot <- ggplot(stData, aes(x = value[,1], y = trait, fill = group)) +
  geom_density_ridges(scale = 1, alpha = 0.6) +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "darkblue", "2" = "yellow", "3" = "lightyellow"), 
                    labels = c("male & male target", "male & female target", "female & male target", "female & female target")) +
  labs(x = "Preference Value", y = "Trait") +
  theme_ridges()+
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5))



#only men

stRidgeplotMen <- ggplot(stDataMale, aes(x = value[,1], y = trait, fill = group)) +
  geom_density_ridges(scale = 1, alpha = 0.6) +
  scale_fill_manual(values = c("0" = "darkblue", "1" = "yellow"), 
                    labels = c("male & male target", "male & female target")) +
  labs(x = "Preference Value", y = "Trait") +
  theme_ridges()+
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5))


#only women
stRidgeplotWomen <- ggplot(stDataFemale, aes(x = value[,1], y = trait, fill = group)) +
  geom_density_ridges(scale = 1, alpha = 0.6) +
  scale_fill_manual(values = c("2" ="lightblue", "3" = "lightyellow"), 
                    labels = c("female & male target", "female & female target")) +
  labs(x = "Preference Value", y = "Trait") +
  theme_ridges()+
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5))

#only male partners
stRidgeplotMalePartners <- ggplot(stDataMalePartner, aes(x = value[,1], y = trait, fill = group)) +
  geom_density_ridges(scale = 1, alpha = 0.6) +
  scale_fill_manual(values = c("0" = "darkblue", "2" ="lightblue"), 
                    labels = c("male & male target", "female & male target")) +
  labs(x = "Preference Value", y = "Trait") +
  theme_ridges()+
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5))

#only female partners
stRidgeplotFemalePartners <- ggplot(stDataFemalePartner, aes(x = value[,1], y = trait, fill = group)) +
  geom_density_ridges(scale = 1, alpha = 0.6) +
  scale_fill_manual(values = c("1" = "yellow", "3" = "lightyellow"), 
                    labels = c("male & female target", "female & female target")) +
  labs(x = "Preference Value", y = "Trait") +
  theme_ridges()+
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5))



##Overall plot to match predict plot

OverallPlotSt <- ggplot(data = stData, aes(x=trait, y=value[,1], fill=group))+  
  geom_bar(stat = "identity", position=position_dodge())+ 
  scale_fill_manual(values = c("0" = "darkblue", "1" = "yellow", "2" ="lightblue", "3" = "lightyellow"), 
                    labels = c("male & male target", "male & female target", "female & male target", "female & female target")) +
  labs(y= "Trait Values", x = "Trait")







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

##Look at breakdown by cluster, sex, and partner sex (divider is the last listed thing)
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
chisqSexLtIdealM<-chisq.test(table(ltDataK$sex[ltDataK$partnerSex == 1],ltDataK$kFitLt[ltDataK$partnerSex == 1])) #yes


##chisq -- male partner cluster x female partner cluster

#create separate DF with pin + cluster + partner sex
columns <- c(1, 3, 14)
chisqDataLt <- ltDataK[,columns]

#create 2 new blank columns: male cluster and female cluster
chisqDataLt$MaleClust <- NA
chisqDataLt$FemaleClust <- NA

#add cluster to column corresponding to correct partner sex

for(i in 1:nrow(chisqDataLt)){
  if(chisqDataLt$partnerSex[i] == 1){
    chisqDataLt$MaleClust[i] <- chisqDataLt$kFitLt[i]
  } else {
    chisqDataLt$FemaleClust[i] <- chisqDataLt$kFitLt[i]
  }
  
}

#make df wide so each ID is only one row
chisqDataLt <- pivot_wider(chisqDataLt, id_cols = PIN, names_from = partnerSex, values_from = c("kFitLt", "MaleClust", "FemaleClust"))
#subset so only relevant columns are kept
chisqDataLt <- subset(chisqDataLt, select = c(PIN, MaleClust_1, FemaleClust_0))
#rename columns
names(chisqDataLt) <- c("PIN", "maleClust", "femaleClust")

#run chisq,excluding NAs
pSexClustChisqLt <- chisq.test(table(chisqDataLt$maleClust, chisqDataLt$femaleClust)) #sig

###Matrix Tables

##ideal female partners

#create matrix dataframe
LtMatrixDataFemale <- data.frame((table(ltDataK$sex[ltDataK$partnerSex == 0],ltDataK$kFitLt[ltDataK$partnerSex == 0])/rowSums(table(ltDataK$sex[ltDataK$partnerSex == 0],ltDataK$kFitLt[ltDataK$partnerSex == 0])))*100)
#relabel column names
colnames(LtMatrixDataFemale) <- c("sex", "cluster", "clusterFrequency")
#round all numbers to 2 decimal places
LtMatrixDataFemale[,3] <-round(LtMatrixDataFemale[,3],2) 


#plot matrix
LtMatrixPlotFemale <- ggplot(LtMatrixDataFemale, aes(x= sex, y = cluster, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = LtMatrixDataFemale$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "darkgreen") +
  scale_x_discrete(labels = c('Women','Men')) +
  scale_y_discrete(labels = c('Rich & Kind','Kind & Smart','Hot & Healthy', 'Smart')) 

#ggsave("LtMatrixPlotFemale.jpeg", plot=last_plot(), width=225, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)

##ideal male partners

#create matrix dataframe
LtMatrixDataMale <- data.frame((table(ltDataK$sex[ltDataK$partnerSex == 1],ltDataK$kFitLt[ltDataK$partnerSex == 1])/rowSums(table(ltDataK$sex[ltDataK$partnerSex == 1],ltDataK$kFitLt[ltDataK$partnerSex == 1])))*100)
#relabel column names
colnames(LtMatrixDataMale) <- c("sex", "cluster", "clusterFrequency")
#round all numbers to 2 decimal places
LtMatrixDataMale[,3] <-round(LtMatrixDataMale[,3],2) 


#plot matrix
LtMatrixPlotMale <- ggplot(LtMatrixDataMale, aes(x= sex, y = cluster, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = LtMatrixDataMale$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "darkgreen") +
  scale_x_discrete(labels = c('Women','Men')) +
  scale_y_discrete(labels = c('Rich & Kind','Kind & Smart','Hot & Healthy', 'Smart')) 


##male compared to female partners

#create matrix dataframe
LtMatrixDataPartners <- data.frame((table(chisqDataLt$maleClust, chisqDataLt$femaleClust))/sum(table(chisqDataLt$maleClust, chisqDataLt$femaleClust))*100)
#relabel column names
colnames(LtMatrixDataPartners) <- c("maleClust", "femaleClust", "clusterFrequency")
#round all numbers to 2 decimal places
LtMatrixDataPartners[,3] <-round(LtMatrixDataPartners[,3],2) 


#plot matrix
LtMatrixPlotPartners<- ggplot(LtMatrixDataPartners, aes(x= femaleClust, y = maleClust, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = LtMatrixDataPartners$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "darkgreen") +
  scale_x_discrete(labels = c('Rich & Kind','Kind & Smart','Hot & Healthy', 'Smart')) +
  scale_y_discrete(labels = c('Rich & Kind','Kind & Smart','Hot & Healthy', 'Smart')) 







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

#Multipanel figure


#cluster 1 (title will change based on clusters)
meanTrait1Lt <- clustCentersLt[1,]
trait1Lt <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting1Lt <- data.frame(meanTrait1Lt, trait1Lt)
plot1Lt <- ggplot(data=plotting1Lt, aes(x=trait1Lt, y=meanTrait1Lt)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "red")+
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level")  +ylim(-0.7,0.7) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 2 
meanTrait2Lt <- clustCentersLt[2,]
trait2Lt <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting2Lt <- data.frame(meanTrait2Lt, trait2Lt)
plot2Lt <- ggplot(data=plotting2Lt, aes(x=trait2Lt, y=meanTrait2Lt)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "forestgreen")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-0.7,0.7) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 3 
meanTrait3Lt <- clustCentersLt[3,]
trait3Lt <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting3Lt <- data.frame(meanTrait3Lt, trait3Lt)
plot3Lt <- ggplot(data=plotting3Lt, aes(x=trait3Lt, y=meanTrait3Lt)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "purple")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-0.7,0.7) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 4
meanTrait4Lt <- clustCentersLt[4,]
trait4Lt <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting4Lt <- data.frame(meanTrait4Lt, trait4Lt)
plot4Lt <- ggplot(data=plotting4Lt, aes(x=trait4Lt, y=meanTrait4Lt)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "yellow")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-0.7,0.7) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))



#combine clusters into one graph
PRpanelPlotLt<-ggarrange(plot1Lt,plot2Lt,plot3Lt, plot4Lt,labels=c("A","B","C", "D"), nrow=1, ncol=4,font.label = list(size = 14, color = "black"))



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
  stDataK[i,9:13] <- focalPrefsSt
  
}



##st prefs cluster analysis 


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
clustSexSt<-table(stDataK$sex,stDataK$kFitSt, stDataK$partnerSex)/rowSums(table(stDataK$sex, stDataK$kFitSt, stDataK$partnerSex))

#cluster choice for female participants
clustSexStF <- table(stDataK$partnerSex[stDataK$sex == 0], stDataK$kFitSt[stDataK$sex == 0])/
  rowSums(table(stDataK$partnerSex[stDataK$sex == 0], stDataK$kFitSt[stDataK$sex == 0]))

#cluster choice for male participants
clustSexStM <- table(stDataK$partnerSex[stDataK$sex == 1], stDataK$kFitSt[stDataK$sex == 1])/
  rowSums(table(stDataK$partnerSex[stDataK$sex == 1], stDataK$kFitSt[stDataK$sex == 1]))


###Chisq Analyses

##are men and women are choosing clusters at diff rates? 
#have to separate based on ideal male v female partners bc independence assumption
fisherSexStIdealF <- fisher.test(table(stDataK$sex[stDataK$partnerSex == 0],stDataK$kFitSt[stDataK$partnerSex == 0]))
  #needed fisher bc warning with chi square

chisqSexStIdealM<-chisq.test(table(stDataK$sex[stDataK$partnerSex == 1],stDataK$kFitSt[stDataK$partnerSex == 1])) #no


##chisq -- male partner cluster x female partner cluster

#create separate DF with pin + cluster + partner sex
columns <- c(1, 3, 14)
chisqDataSt <- stDataK[,columns]

#create 2 new blank columns: male cluster and female cluster
chisqDataSt$MaleClust <- NA
chisqDataSt$FemaleClust <- NA

#add cluster to column corresponding to correct partner sex

for(i in 1:nrow(chisqDataSt)){
  if(chisqDataSt$partnerSex[i] == 1){
    chisqDataSt$MaleClust[i] <- chisqDataSt$kFitSt[i]
  } else {
    chisqDataSt$FemaleClust[i] <- chisqDataSt$kFitSt[i]
  }
  
}

#make df wide so each ID is only one row
chisqDataSt <- pivot_wider(chisqDataSt, id_cols = PIN, names_from = partnerSex, values_from = c("kFitSt", "MaleClust", "FemaleClust"))
#subset so only relevant columns are kept
chisqDataSt <- subset(chisqDataSt, select = c(PIN, MaleClust_1, FemaleClust_0))
#rename columns
names(chisqDataSt) <- c("PIN", "maleClust", "femaleClust")

#run fisher instead of chisq bc some cells have 0
pSexClustFisherSt <- fisher.test(table(chisqDataSt$maleClust, chisqDataSt$femaleClust),
                                 simulate.p.value = T)
  



###Matrix Tables

##ideal female partners

#create matrix dataframe
StMatrixDataFemale <- data.frame((table(stDataK$sex[stDataK$partnerSex == 0],stDataK$kFitSt[stDataK$partnerSex == 0])/rowSums(table(stDataK$sex[stDataK$partnerSex == 0],stDataK$kFitSt[stDataK$partnerSex == 0])))*100)
#relabel column names
colnames(StMatrixDataFemale) <- c("sex", "cluster", "clusterFrequency")
#round all numbers to 2 decimal places
StMatrixDataFemale[,3] <-round(StMatrixDataFemale[,3],2) 


#plot matrix
StMatrixPlotFemale <- ggplot(StMatrixDataFemale, aes(x= sex, y = cluster, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = StMatrixDataFemale$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "darkgreen") +
  scale_x_discrete(labels = c('Women','Men')) +
  scale_y_discrete(labels = c('Hot & Healthy','Smart & Hot','Rich & Kind', 'Rich & Healthy (& Kind)')) 

#ggsave("StMatrixPlotFemale.jpeg", plot=last_plot(), width=225, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)

##ideal male partners

#create matrix dataframe
StMatrixDataMale <- data.frame((table(stDataK$sex[stDataK$partnerSex == 1],stDataK$kFitSt[stDataK$partnerSex == 1])/rowSums(table(stDataK$sex[stDataK$partnerSex == 1],stDataK$kFitSt[stDataK$partnerSex == 1])))*100)
#relabel column names
colnames(StMatrixDataMale) <- c("sex", "cluster", "clusterFrequency")
#round all numbers to 2 decimal places
StMatrixDataMale[,3] <-round(StMatrixDataMale[,3],2) 


#plot matrix
StMatrixPlotMale <- ggplot(StMatrixDataMale, aes(x= sex, y = cluster, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = StMatrixDataMale$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "darkgreen") +
  scale_x_discrete(labels = c('Women','Men')) +
  scale_y_discrete(labels = c('Hot & Healthy','Smart & Hot','Rich & Kind', 'Rich & Healthy (& Kind)')) 


##male compared to female partners

#create matrix dataframe
StMatrixDataPartners <- data.frame((table(chisqDataSt$maleClust, chisqDataSt$femaleClust))/sum(table(chisqDataSt$maleClust, chisqDataSt$femaleClust))*100)
#relabel column names
colnames(StMatrixDataPartners) <- c("maleClust", "femaleClust", "clusterFrequency")
#round all numbers to 2 decimal places
StMatrixDataPartners[,3] <-round(StMatrixDataPartners[,3],2) 


#plot matrix
StMatrixPlotPartners<- ggplot(StMatrixDataPartners, aes(x= femaleClust, y = maleClust, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = StMatrixDataPartners$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "darkgreen") +
  scale_x_discrete(labels = c('Hot & Healthy','Smart & Hot','Rich & Kind', 'Rich & Healthy (& Kind)')) +
  scale_y_discrete(labels = c('Hot & Healthy','Smart & Hot','Rich & Kind', 'Rich & Healthy (& Kind)')) 





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


#Multipanel figure


#cluster 1 (title will change based on clusters)
meanTrait1St <- clustCentersSt[1,]
trait1St <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting1St <- data.frame(meanTrait1St, trait1St)
plot1St <- ggplot(data=plotting1St, aes(x=trait1St, y=meanTrait1St)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "red")+
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level")  +ylim(-1.6,1.6) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 2 
meanTrait2St <- clustCentersSt[2,]
trait2St <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting2St <- data.frame(meanTrait2St, trait2St)
plot2St <- ggplot(data=plotting2St, aes(x=trait2St, y=meanTrait2St)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "forestgreen")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-1.6,1.6) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 3 
meanTrait3St <- clustCentersSt[3,]
trait3St <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting3St <- data.frame(meanTrait3St, trait3St)
plot3St <- ggplot(data=plotting3St, aes(x=trait3St, y=meanTrait3St)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "purple")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-1.6,1.6) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 4
meanTrait4St <- clustCentersSt[4,]
trait4St <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting4St <- data.frame(meanTrait4St, trait4St)
plot4St <- ggplot(data=plotting4St, aes(x=trait4St, y=meanTrait4St)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "yellow")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-1.6,1.6) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))



#combine clusters into one graph
PRpanelPlotSt<-ggarrange(plot1St,plot2St,plot3St, plot4St,labels=c("A","B","C", "D"), nrow=1, ncol=4,font.label = list(size = 14, color = "black"))








###supplemental stuff

#st prefs cluster analysis with 3 clusters


##Add classification (3) to the original dataframe

kFitSt3<-kmeans(stDataK[,9:13],3)
stDataK$kFitSt3 <- kFitSt3$cluster


##Create vectors of preference means for each cluster 
clustCentersSt3<-kFitSt3$centers

##Look at breakdown by cluster, sex, and partner sex #0 = women, #1 = men
clustSexSt3<-table(stDataK$sex,stDataK$kFitSt3, stDataK$partnerSex)/rowSums(table(stDataK$sex, stDataK$kFitSt3, stDataK$partnerSex))

#cluster choice for female participants
clustSexStF3 <- table(stDataK$partnerSex[stDataK$sex == 0], stDataK$kFitSt3[stDataK$sex == 0])/
  rowSums(table(stDataK$partnerSex[stDataK$sex == 0], stDataK$kFitSt3[stDataK$sex == 0]))

#cluster choice for male participants
clustSexStM3 <- table(stDataK$partnerSex[stDataK$sex == 1], stDataK$kFitSt3[stDataK$sex == 1])/
  rowSums(table(stDataK$partnerSex[stDataK$sex == 1], stDataK$kFitSt3[stDataK$sex == 1]))


###Chisq Analyses

##are men and women are choosing clusters at diff rates? 
#have to separate based on ideal male v female partners bc independence assumption
chisqSexStIdealF <- chisq.test(table(stDataK$sex[stDataK$partnerSex == 0],stDataK$kFitSt3[stDataK$partnerSex == 0]))

chisqSexStIdealM<-chisq.test(table(stDataK$sex[stDataK$partnerSex == 1],stDataK$kFitSt3[stDataK$partnerSex == 1]))


##chisq -- male partner cluster x female partner cluster

#create separate DF with pin + cluster + partner sex
columns <- c(1, 3, 15) 
chisqDataSt3 <- stDataK[,columns]

#create 2 new blank columns: male cluster and female cluster
chisqDataSt3$MaleClust <- NA
chisqDataSt3$FemaleClust <- NA

#add cluster to column corresponding to correct partner sex

for(i in 1:nrow(chisqDataSt3)){
  if(chisqDataSt3$partnerSex[i] == 1){
    chisqDataSt3$MaleClust[i] <- chisqDataSt3$kFitSt3[i]
  } else {
    chisqDataSt3$FemaleClust[i] <- chisqDataSt3$kFitSt3[i]
  }
  
}

#make df wide so each ID is only one row
chisqDataSt3 <- pivot_wider(chisqDataSt3, id_cols = PIN, names_from = partnerSex, values_from = c("kFitSt3", "MaleClust", "FemaleClust"))
#subset so only relevant columns are kept
chisqDataSt3 <- subset(chisqDataSt3, select = c(PIN, MaleClust_1, FemaleClust_0))
#rename columns
names(chisqDataSt3) <- c("PIN", "maleClust", "femaleClust")

#chisq test, male vs female partners
pSexClustChisqSt3 <- chisq.test(table(chisqDataSt3$maleClust, chisqDataSt3$femaleClust))




###Matrix Tables

##ideal female partners

#create matrix dataframe
StMatrixDataFemale3 <- data.frame((table(stDataK$sex[stDataK$partnerSex == 0],stDataK$kFitSt3[stDataK$partnerSex == 0])/rowSums(table(stDataK$sex[stDataK$partnerSex == 0],stDataK$kFitSt3[stDataK$partnerSex == 0])))*100)
#relabel column names
colnames(StMatrixDataFemale3) <- c("sex", "cluster", "clusterFrequency")
#round all numbers to 2 decimal places
StMatrixDataFemale3[,3] <-round(StMatrixDataFemale3[,3],2) 


#plot matrix
StMatrixPlotFemale3 <- ggplot(StMatrixDataFemale3, aes(x= sex, y = cluster, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = StMatrixDataFemale3$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "darkgreen") +
  scale_x_discrete(labels = c('Women','Men')) +
  scale_y_discrete(labels = c('Attractive','Well-Rounded','Healthy, Kind, & Wealthy')) 

#ggsave("StMatrixPlotFemale.jpeg", plot=last_plot(), width=225, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)



##ideal male partners

#create matrix dataframe
StMatrixDataMale3 <- data.frame((table(stDataK$sex[stDataK$partnerSex == 1],stDataK$kFitSt3[stDataK$partnerSex == 1])/rowSums(table(stDataK$sex[stDataK$partnerSex == 1],stDataK$kFitSt3[stDataK$partnerSex == 1])))*100)
#relabel column names
colnames(StMatrixDataMale3) <- c("sex", "cluster", "clusterFrequency")
#round all numbers to 2 decimal places
StMatrixDataMale3[,3] <-round(StMatrixDataMale3[,3],2) 


#plot matrix
StMatrixPlotMale3 <- ggplot(StMatrixDataMale3, aes(x= sex, y = cluster, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = StMatrixDataMale3$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "darkgreen") +
  scale_x_discrete(labels = c('Women','Men')) +
  scale_y_discrete(labels = c('Attractive','Well-Rounded','Healthy, Kind, & Wealthy')) 


##male compared to female partners

#create matrix dataframe
StMatrixDataPartners3 <- data.frame((table(chisqDataSt3$maleClust, chisqDataSt3$femaleClust))/sum(table(chisqDataSt3$maleClust, chisqDataSt3$femaleClust))*100)
#relabel column names
colnames(StMatrixDataPartners3) <- c("maleClust", "femaleClust", "clusterFrequency")
#round all numbers to 2 decimal places
StMatrixDataPartners3[,3] <-round(StMatrixDataPartners3[,3],2) 


#plot matrix
StMatrixPlotPartners3<- ggplot(StMatrixDataPartners3, aes(x= femaleClust, y = maleClust, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = StMatrixDataPartners3$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "darkgreen") +
  scale_x_discrete(labels = c('Attractive','Well-Rounded','Healthy, Kind, & Wealthy')) +
  scale_y_discrete(labels = c('Attractive','Well-Rounded','Healthy, Kind, & Wealthy')) 





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


#Multipanel figure


#cluster 1 (title will change based on clusters)
meanTrait1St3 <- clustCentersSt3[1,]
trait1St3 <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting1St3 <- data.frame(meanTrait1St3, trait1St3)
plot1St3 <- ggplot(data=plotting1St3, aes(x=trait1St3, y=meanTrait1St3)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "red")+
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level")  +ylim(-1.6,1.6) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 2 
meanTrait2St3 <- clustCentersSt3[2,]
trait2St3 <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting2St3 <- data.frame(meanTrait2St3, trait2St3)
plot2St3 <- ggplot(data=plotting2St3, aes(x=trait2St3, y=meanTrait2St3)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "forestgreen")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-1.6,1.6) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 3 
meanTrait3St3 <- clustCentersSt3[3,]
trait3St3 <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting3St3 <- data.frame(meanTrait3St3, trait3St3)
plot3St3 <- ggplot(data=plotting3St3, aes(x=trait3St3, y=meanTrait3St3)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "purple")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-1.6,1.6) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))



#combine clusters into one graph
PRpanelPlotSt3<-ggarrange(plot1St3,plot2St3,plot3St3,labels=c("A","B","C"), nrow=1, ncol=3,font.label = list(size = 14, color = "black"))


