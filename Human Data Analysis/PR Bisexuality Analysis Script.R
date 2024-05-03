#####De-Confounding Sex and Sex of Partner in Mate Preference Research#####
################Analysis Script##############

###load packages###
library(lmerTest)
library(multilevelTools)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(tidyr)
library(ggpubr) #for panel plot
library(ggpattern)


### set seed ###
#for cluster analyses
set.seed(11182022)


### load data ###
data<-read.csv("Human Data/Processed Data/PR Bisexuality Data PROCESSED 12062022 000940.csv")
    #only bisexual people remain after processing

#exclude intersex participants
data <-  data[data$sex!=2,]
#exclude people who did not report their sex
data <- data[!is.na(data[,5]), ]


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




#ombnibus test

ltOmnibus <- lmer(value ~ partnerSex +  trait*sex + (1|PIN), 
                   data = ltData) 

stOmnibus <- lmer(value ~ partnerSex +  trait*sex + (1|PIN), 
                  data = stData) 


###LT prefs

#tidy format for analyses
ltDataTidy <- ltData %>%
  pivot_wider(names_from = trait, 
              values_from = value)

#standardizing outcome variable (traits)

#health 

ltHealthMain <- lmer(scale(health)  ~ sex+partnerSex + (1|PIN),
                     data = ltDataTidy) #not sig
#kindness 

ltKindMain <- lmer(scale(kind)  ~ sex+partnerSex + (1|PIN),
                  data = ltDataTidy) #sig main of sex and psex

#physical attractiveness

ltPhysattMain <- lmer(scale(physatt)  ~ sex+partnerSex + (1|PIN),
                     data = ltDataTidy) #sig main effect of sex

#intell

ltIntellMain <- lmer(scale(intell)  ~ sex+partnerSex + (1|PIN),
                    data = ltDataTidy) #sig effect of psex

#resources

ltResourceMain <- lmer(scale(resources)  ~ sex+partnerSex + (1|PIN),
                      data = ltDataTidy)  #sig main effect of psex, not sex

#ideal age (NOT STANDARDIZED)


ltAgeMain <- lmer(AgeLik ~ sex+partnerSex + (1|PIN), 
                 data = ltDataTidy) #sig main effect of sex and partner sex



### ST prefs main effects ###

#tidy format for analyses
stDataTidy <- stData %>%
  pivot_wider(names_from = trait, 
              values_from = value)

#kindness


stKindMain <- lmer(scale(kind) ~ sex+partnerSex + (1|PIN), 
                  data = stDataTidy) 

#physical attractiveness


stPhysattMain <- lmer(scale(physatt) ~ sex+partnerSex + (1|PIN), 
                      data = stDataTidy) #only sig main effect of partner sex


#health


stHealthMain <- lmer(scale(health)  ~ sex+partnerSex + (1|PIN),
                     data = stDataTidy) #sig main effect of partner sex


#intelligence


stIntellMain <- lmer(scale(intell)  ~ sex+partnerSex + (1|PIN),
                    data = stDataTidy) #sig main effect of partner sex

#resources


stResourceMain <- lmer(scale(resources)  ~ sex+partnerSex + (1|PIN),
                      data = stDataTidy) #sig effect of partner sex

#age (NOT STANDARDIZED)


stAgeMain <- lmer(AgeLik ~ sex+partnerSex + (1|PIN), 
                  data = stDataTidy) #sig main effect of sex and partner sex




###Density Plots

###lt prefs

#create new group variable  (0 = male participant/male target, 1 = male/female, 2 = female/male, 3 = female/female)
ltData$group <- ifelse(ltData$sex == 1 & ltData$partnerSex == 1, 0, 
                           ifelse(ltData$sex == 1 & ltData$partnerSex == 0, 1, 
                                  ifelse(ltData$sex == 0 & ltData$partnerSex == 1, 2,
                                         ifelse(ltData$sex == 0 & ltData$partnerSex == 0, 3, NA))))
ltData$group <- as.factor(ltData$group)


##overall plot faceted by partner sex

#labelling facet fxn
facetNames <- list(
  "AgeLik" = "(A) Age Difference",
  "health" = " (B) Health",
  "intell" = " (C) Intelligence",
  "kind" = " (D) Kindness",
  "physatt" = " (E) Physical Attractiveness",
  "resources" = " (F) Resources"
)

labellerFacet <- function(variable, value){
  return(facetNames[value])
}


##facted mirror density plot

ltMirrorPlot <- ggplot(ltData, aes(x = value, fill = group)) +
  #top
  geom_density(aes(y = after_stat(density)),
               data = ltData[ltData$partnerSex == 0,], alpha = 0.8) + #female targets
  #bottom
  geom_density(aes(y = -after_stat(density)),
               data = ltData[ltData$partnerSex == 1,], alpha = 0.8) + #male targets
  facet_wrap(~trait, ncol = 3, scales = "free", labeller = labellerFacet)+
  scale_fill_manual(values = c("1" = "darkblue", "3" = "orangered", "0" = "lightblue", "2" ="orange"), 
                    labels = c("Male Participant/Female Target", "Female Participant/Female Target",
                               "Male Participant/Male Target",  "Female Participant/Male Target"),
                    breaks = c("1", "3", "0", "2")) +
  labs(x = "Preference Value", y = "Density", fill = "Participant Sex/Target Sex") +
  theme_grey(base_size = 20) +
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5)) +
  ggtitle("Preferences for Ideal Partners") +
  geom_text(aes(x = .75, y = Inf, label = "Female \n Partners"),
            hjust = 0, vjust = 2.5, color = "black", size = 6) +
  geom_text(aes(x = .75, y = -Inf, label = "Male \n Partners"),
            hjust = 0, vjust = -2.5, color = "black", size = 6) 


#ggsave("densityLt.jpeg", plot=last_plot(), width=400, height=450, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)



##st prefs

#create new group variable  (0 = male participant/male target, 1 = male/female, 2 = female/male, 3 = female/female)
stData$group <- ifelse(stData$sex == 1 & stData$partnerSex == 1, 0, 
                       ifelse(stData$sex == 1 & stData$partnerSex == 0, 1, 
                              ifelse(stData$sex == 0 & stData$partnerSex == 1, 2,
                                     ifelse(stData$sex == 0 & stData$partnerSex == 0, 3, NA))))


stData$group <- as.factor(stData$group)




##faceted density plot (mirrored)

stMirrorPlot <- ggplot(stData, aes(x = value, fill = group)) +
  #top
  geom_density(aes(y = after_stat(density)),
               data = stData[stData$partnerSex == 0,], alpha = 0.8) + #female targets
  #bottom
  geom_density(aes(y = -after_stat(density)),
               data = stData[stData$partnerSex == 1,], alpha = 0.8) + #male targets
  facet_wrap(~trait, ncol = 3, scales = "free", labeller = labellerFacet)+
  scale_fill_manual(values = c("1" = "darkblue", "3" = "orangered", "0" = "lightblue", "2" ="orange"), 
                    labels = c("Male Participant/Female Target", "Female Participant/Female Target",
                               "Male Participant/Male Target",  "Female Participant/Male Target"),
                    breaks = c("1", "3", "0", "2")) +
  labs(x = "Preference Value", y = "Density", fill = "Participant Sex/Target Sex") +
  theme_grey(base_size = 20) +
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5)) +
  ggtitle("Preferences for Ideal Partners") +
  geom_text(aes(x = .85, y = Inf, label = "Female \n Partners"),
            hjust = 0, vjust = 2.5, color = "black", size = 6) +
  geom_text(aes(x = .85, y = -Inf, label = "Male \n Partners"),
            hjust = 0, vjust = -2.5, color = "black", size = 6) 

#ggsave("densitySt.jpeg", plot=last_plot(), width=400, height=450, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)




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
  #within person, across targets
  avPrefsLt <- mean(rowMeans(ltDataK[ltDataK$PIN == ltDataK$PIN[i],4:8], na.rm = T))
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
  scale_fill_gradient(low = "white", high = "springgreen4") +
  scale_x_discrete(labels = c('Female','Male')) +
  scale_y_discrete(labels = c("Kind \n& Smart","Wealthy \n& Kind","Attractive \n& Healthy", "Smart")) +
  ggtitle("A") +
  labs(x = "Participant Sex", y = "Cluster of Female Targets", fill = "Cluster \nFrequency") +
  theme(legend.position = "none")  # Remove legend for this plot


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
  scale_fill_gradient(low = "white", high = "springgreen4") +
  scale_x_discrete(labels = c('Female','Male')) +
  scale_y_discrete(labels = c("Kind \n& Smart","Wealthy \n& Kind","Attractive \n& Healthy", "Smart")) +
  ggtitle("B")+
  labs(x = "Participant Sex", y = "Cluster of Male Targets", fill = "Cluster \nFrequency") +
  theme(legend.position = "none")  # Remove legend for this plot


#panel plot of both of these graphs
ltMatrixPanel <- ggarrange(LtMatrixPlotFemale, LtMatrixPlotMale, nrow=1, ncol=2, 
                           common.legend = TRUE, legend = "right")
#ggsave("LtMatrixPlotPanel.jpeg", plot=last_plot(), width=200, height=110, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)



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
  scale_fill_gradient(low = "white", high = "springgreen4") +
  scale_x_discrete(labels = c("Kind & Smart","Wealthy & Kind","Attractive & Healthy", "Smart")) +
  scale_y_discrete(labels = c("Kind & Smart","Wealthy & Kind","Attractive & Healthy", "Smart")) +
  labs(x = "Ideal Female Partner Cluster", y = "Ideal Male Partner Cluster", fill = "Cluster Frequency")

#ggsave("LtMatrixPlotPartners.jpeg", plot=last_plot(), width=250, height=250, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)






### Plotting the clusters ###


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
PRpanelPlotLt<-ggarrange(plot1Lt,plot2Lt,plot3Lt, plot4Lt,labels=c("Kind & Smart","Wealthy & Kind","Attractive & Healthy", "Smart"), nrow=1, ncol=4,font.label = list(size = 14, color = "black"))

#ggsave("PRpanelPlotLt.jpeg", plot=last_plot(), width=300, height=200, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)




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
  avPrefsSt <- mean(rowMeans(stDataK[stDataK$PIN == stDataK$PIN[i],4:8], na.rm = T))
  focalPrefsSt <- focalPrefsSt - avPrefsSt
  stDataK[i,9:13] <- focalPrefsSt
  
}



##st prefs cluster analysis 


#extract kmeans wSs
kfitWssSt<-sapply(1:7,function(x) kmeans(stDataK[,9:13],x)$tot.withinss)

#scree plot
screePlotSt<-qplot(1:7,kfitWssSt) 

##compute differences in within ss across k for k-means clustering
wssDiffsSt<-diff(kfitWssSt)

##Add classification (3) to the original dataframe

kFitSt3<-kmeans(stDataK[,9:13],3)
stDataK$kFitSt3 <- kFitSt3$cluster


##Create vectors of preference means for each cluster 
clustCentersSt3<-kFitSt3$centers


###Chisq Analyses

##are men and women are choosing clusters at diff rates? 
#have to separate based on ideal male v female partners bc independence assumption
chisqSexStIdealF3 <- chisq.test(table(stDataK$sex[stDataK$partnerSex == 0],stDataK$kFitSt3[stDataK$partnerSex == 0]))

chisqSexStIdealM3 <-chisq.test(table(stDataK$sex[stDataK$partnerSex == 1],stDataK$kFitSt3[stDataK$partnerSex == 1]))


##chisq -- male partner cluster x female partner cluster

#create separate DF with pin + cluster + partner sex
columns <- c(1, 3, 14) 
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
  scale_fill_gradient(low = "white", high = "springgreen4") +
  scale_x_discrete(labels = c('Female','Male')) +
  scale_y_discrete(labels = c("Attractive \n& Healthy","Smart \n& Kind","Wealthy")) +
  ggtitle("A")+
  labs(x = "Participant Sex", y = "Cluster of Female Targets", fill = "Cluster \nFrequency")+
  theme(legend.position = "none")

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
  scale_fill_gradient(low = "white", high = "springgreen4") +
  scale_x_discrete(labels = c('Female','Male')) +
  scale_y_discrete(labels = c("Attractive \n& Healthy","Smart \n& Kind","Wealthy")) +
  ggtitle("B")+
  labs(x = "Participant Sex", y = "Cluster of Male Targets", fill = "Clustern \nFrequency") +
  theme(legend.position = "none")


#panel plot of both of these graphs
stMatrixPanel3 <- ggarrange(StMatrixPlotFemale3, StMatrixPlotMale3, nrow=1, ncol=2,
                            common.legend = TRUE, legend = "right")
#ggsave("StMatrixPlotPanel3.jpeg", plot=last_plot(), width=200, height=110, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)



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
  scale_fill_gradient(low = "white", high = "springgreen4") +
  scale_x_discrete(labels = c("Attractive & Healthy","Smart and Kind","Wealthy")) +
  scale_y_discrete(labels = c("Attractive & Healthy","Smart and Kind","Wealthy")) +
  labs(x = "Ideal Female Partner Cluster", y = "Ideal Male Partner Cluster", fill = "Cluster Frequency")

#ggsave("StMatrixPlotPartners.jpeg", plot=last_plot(), width=250, height=250, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)






### Plotting ###


#Multipanel figure


#cluster 1 (title will change based on clusters)
meanTrait1St3 <- clustCentersSt3[1,]
trait1St3 <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting1St3 <- data.frame(meanTrait1St3, trait1St3)
plot1St3 <- ggplot(data=plotting1St3, aes(x=trait1St3, y=meanTrait1St3)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "red")+
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level")  +ylim(-.7,.7) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 2 
meanTrait2St3 <- clustCentersSt3[2,]
trait2St3 <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting2St3 <- data.frame(meanTrait2St3, trait2St3)
plot2St3 <- ggplot(data=plotting2St3, aes(x=trait2St3, y=meanTrait2St3)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "forestgreen")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-.7,.7) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 3 
meanTrait3St3 <- clustCentersSt3[3,]
trait3St3 <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting3St3 <- data.frame(meanTrait3St3, trait3St3)
plot3St3 <- ggplot(data=plotting3St3, aes(x=trait3St3, y=meanTrait3St3)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "purple")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-.7,.7) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))



#combine clusters into one graph
PRpanelPlotSt3<-ggarrange(plot1St3,plot2St3,plot3St3,labels=c("Attractive & Healthy","Smart and Kind","Wealthy"), nrow=1, ncol=3,font.label = list(size = 14, color = "black"))

#ggsave("PRpanelPlotSt3.jpeg", plot=last_plot(), width=300, height=200, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)






###supplemental stuff

#st prefs cluster analysis with 4 clusters

##Add classification to the original dataframe

kFitSt4<-kmeans(stDataK[,9:13],4)
stDataK$kFitSt4 <- kFitSt4$cluster


##Create vectors of preference means for each cluster 
clustCentersSt4<-kFitSt4$centers


###Chisq Analyses

##are men and women are choosing clusters at diff rates? 
#have to separate based on ideal male v female partners bc independence assumption
chisqSexStIdealF4 <- chisq.test(table(stDataK$sex[stDataK$partnerSex == 0],stDataK$kFitSt4[stDataK$partnerSex == 0]))
chisqSexStIdealM4<-chisq.test(table(stDataK$sex[stDataK$partnerSex == 1],stDataK$kFitSt4[stDataK$partnerSex == 1])) 


##chisq -- male partner cluster x female partner cluster

#create separate DF with pin + cluster + partner sex
columns <- c(1, 3, 15)
chisqDataSt4 <- stDataK[,columns]

#create 2 new blank columns: male cluster and female cluster
chisqDataSt4$MaleClust <- NA
chisqDataSt4$FemaleClust <- NA

#add cluster to column corresponding to correct partner sex

for(i in 1:nrow(chisqDataSt4)){
  if(chisqDataSt4$partnerSex[i] == 1){
    chisqDataSt4$MaleClust[i] <- chisqDataSt4$kFitSt4[i]
  } else {
    chisqDataSt4$FemaleClust[i] <- chisqDataSt4$kFitSt4[i]
  }
  
}

#make df wide so each ID is only one row
chisqDataSt4 <- pivot_wider(chisqDataSt4, id_cols = PIN, names_from = partnerSex, values_from = c("kFitSt4", "MaleClust", "FemaleClust"))
#subset so only relevant columns are kept
chisqDataSt4 <- subset(chisqDataSt4, select = c(PIN, MaleClust_1, FemaleClust_0))
#rename columns
names(chisqDataSt4) <- c("PIN", "maleClust", "femaleClust")

#fisher test (some cell have 3 or less, so can't do chisq)
pSexClustFisherSt4 <- fisher.test(table(chisqDataSt4$maleClust, chisqDataSt4$femaleClust), simulate.p.value = T)





###Matrix Tables

##ideal female partners

#create matrix dataframe
StMatrixDataFemale4 <- data.frame((table(stDataK$sex[stDataK$partnerSex == 0],stDataK$kFitSt4[stDataK$partnerSex == 0])/
                                     rowSums(table(stDataK$sex[stDataK$partnerSex == 0],stDataK$kFitSt4[stDataK$partnerSex == 0])))*100)
#relabel column names
colnames(StMatrixDataFemale4) <- c("sex", "cluster", "clusterFrequency")
#round all numbers to 2 decimal places
StMatrixDataFemale4[,3] <-round(StMatrixDataFemale4[,3],2) 


#plot matrix
StMatrixPlotFemale4 <- ggplot(StMatrixDataFemale4, aes(x= sex, y = cluster, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = StMatrixDataFemale4$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "springgreen4") +
  scale_x_discrete(labels = c('Female','Male')) +
  scale_y_discrete(labels = c("Wealthy","Smart \n&Kind","Below Average", "Attractive \n&Healthy")) +
  ggtitle("A")+
  labs(x = "Participant Sex", y = "Percentage of Female Targets in Each Cluster", fill = "Cluster \nFrequency")+
  theme(legend.position = "none")

#ggsave("StMatrixPlotFemale.jpeg", plot=last_plot(), width=225, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)

##ideal male partners

#create matrix dataframe
StMatrixDataMale4 <- data.frame((table(stDataK$sex[stDataK$partnerSex == 1],stDataK$kFitSt4[stDataK$partnerSex == 1])/
                                   rowSums(table(stDataK$sex[stDataK$partnerSex == 1],stDataK$kFitSt4[stDataK$partnerSex == 1])))*100)
#relabel column names
colnames(StMatrixDataMale4) <- c("sex", "cluster", "clusterFrequency")
#round all numbers to 2 decimal places
StMatrixDataMale4[,3] <-round(StMatrixDataMale4[,3],2) 


#plot matrix
StMatrixPlotMale4 <- ggplot(StMatrixDataMale4, aes(x= sex, y = cluster, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = StMatrixDataMale4$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "springgreen4") +
  scale_x_discrete(labels = c('Female','Male')) +
  scale_y_discrete(labels = c("Wealthy","Smart \n&Kind","Below Average", "Attractive \n&Healthy")) +
  ggtitle("B")+
  labs(x = "Participant Sex", y = "Percentage of Male Targets in Each Cluster", fill = "Cluster \nFrequency")+
  theme(legend.position = "none")


# PANEL PLOT


stMatrixPanel4 <- ggarrange(StMatrixPlotFemale4, StMatrixPlotMale4, nrow=1, ncol=2, 
                            common.legend = TRUE, legend = "right")

#ggsave("StMatrixPlotPanel4.jpeg", plot=last_plot(), width=200, height=110, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)


##male compared to female partners

#create matrix dataframe
StMatrixDataPartners4 <- data.frame((table(chisqDataSt4$maleClust, chisqDataSt4$femaleClust))/
                                      sum(table(chisqDataSt4$maleClust, chisqDataSt4$femaleClust))*100)
#relabel column names
colnames(StMatrixDataPartners4) <- c("maleClust", "femaleClust", "clusterFrequency")
#round all numbers to 2 decimal places
StMatrixDataPartners4[,3] <-round(StMatrixDataPartners4[,3],2) 


#plot matrix
StMatrixPlotPartners4<- ggplot(StMatrixDataPartners4, aes(x= femaleClust, y = maleClust, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = StMatrixDataPartners4$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "springgreen4") +
  scale_x_discrete(labels = c("Wealthy","Smart and Kind","Below Average", "Attractive and Healthy")) +
  scale_y_discrete(labels = c("Wealthy","Smart and Kind","Below Average", "Attractive and Healthy")) +
  labs(x = "Ideal Female Partner Cluster", y = "Ideal Male Partner Cluster", fill = "Cluster Frequency")
#ggsave("StMatrixPlotPartners4.jpeg", plot=last_plot(), width=200, height=250, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)




### Plotting ###

#Multipanel figure


#cluster 1 (title will change based on clusters)
meanTrait1St4 <- clustCentersSt4[1,]
trait1St4 <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting1St4 <- data.frame(meanTrait1St4, trait1St4)
plot1St4 <- ggplot(data=plotting1St4, aes(x=trait1St4, y=meanTrait1St4)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "red")+
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level")  +ylim(-1.5,1.5) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 2 
meanTrait2St4 <- clustCentersSt4[2,]
trait2St4 <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting2St4 <- data.frame(meanTrait2St4, trait2St4)
plot2St4 <- ggplot(data=plotting2St4, aes(x=trait2St4, y=meanTrait2St4)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "forestgreen")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-1.5,1.5) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 3 
meanTrait3St4 <- clustCentersSt4[3,]
trait3St4 <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting3St4 <- data.frame(meanTrait3St4, trait3St4)
plot3St4 <- ggplot(data=plotting3St4, aes(x=trait3St4, y=meanTrait3St4)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "purple")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-1.5,1.5) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 4
meanTrait4St4 <- clustCentersSt4[4,]
trait4St4 <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting4St4 <- data.frame(meanTrait4St4, trait4St4)
plot4St4 <- ggplot(data=plotting4St4, aes(x=trait4St4, y=meanTrait4St4)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "yellow")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-1.5,1.5) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))



#combine clusters into one graph
PRpanelPlotSt4<-ggarrange(plot1St4,plot2St4,plot3St4, plot4St4,
                         labels=c("Wealthy","Smart and Kind","Below Average", "Attractive and Healthy"), 
                         nrow=1, ncol=4,font.label = list(size = 13, color = "black")) 


#ggsave("PRpanelPlotSt4.jpeg", plot=last_plot(), width=300, height=200, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)





###Supplemental Materials 2

##Analyses excluding people who still id-ed as heterosexual & hetero-romantic


#exclude those participants who id-ed as het*
dataBiOnly <- subset(data, !(sex_orient_best == 4 & rom_orient_best ==3)) 


###Omnibus Analyses


#longform data

#LT 
ltDataBi <- dataBiOnly[,c(167, 5, 178:187, 227, 229)]
ltDataBi <- melt(ltDataBi, id.vars=c("PIN", "sex"))
ltDataBi <- ltDataBi %>% 
  separate("variable", into = c("partnerSex", "x", "trait"), remove = T)
ltDataBi$partnerSex <- ifelse(ltDataBi$partnerSex == "m", 1, 0)
ltDataBi <- ltDataBi[,c(1:3, 5:6)]
ltDataBi$sex  <- as.factor(ltDataBi$sex)
ltDataBi$partnerSex  <- as.factor(ltDataBi$partnerSex)

#make sure there are no NAs in sex or partner sex columns
nacheckLtBi <- apply(ltDataBi[,2:3], 1, function(x) sum(is.na(x))>0)
ltDataBi<- ltDataBi[!nacheckLtBi,]


#ST

stDataBi <- dataBiOnly[,c(167, 5, 188:197, 228, 230)]
stDataBi <- melt(stDataBi, id.vars=c("PIN", "sex"))
stDataBi <- stDataBi %>% 
  separate("variable", into = c("partnerSex", "x", "trait"), remove = T)
stDataBi$partnerSex <- ifelse(stDataBi$partnerSex == "m", 1, 0)
stDataBi <- stDataBi[,c(1:3, 5:6)]
stDataBi$sex  <- as.factor(stDataBi$sex)
stDataBi$partnerSex  <- as.factor(stDataBi$partnerSex)

#make sure there are no NAs in sex or partner sex columns
nacheckStBi <- apply(stDataBi[,2:3], 1, function(x) sum(is.na(x))>0)
stDataBi<- stDataBi[!nacheckStBi,]




#ombnibus test

ltOmnibusBi <- lmer(value ~ partnerSex +  trait*sex + (1|PIN), 
                  data = ltDataBi) 

stOmnibusBi <- lmer(value ~ partnerSex +  trait*sex + (1|PIN), 
                  data = stDataBi) 


###LT prefs (using nested anova to look for interaction effects)

#tidy format for analyses
ltDataBiTidy <- ltDataBi %>%
  pivot_wider(names_from = trait, 
              values_from = value)

#standardizing outcome variable (traits)

#health 
ltHealthIntBi <- lmer(scale(health)  ~ sex*partnerSex + (1|PIN),
                    data = ltDataBiTidy)#not sig

ltHealthMainBi <- lmer(scale(health)  ~ sex+partnerSex + (1|PIN),
                     data = ltDataBiTidy) #sig main effect of sex (different)
#kindness 
ltKindIntBi <- lmer(scale(kind)  ~ sex*partnerSex + (1|PIN),
                  data = ltDataBiTidy) #no interaction

ltKindMainBi <- lmer(scale(kind)  ~ sex+partnerSex + (1|PIN),
                   data = ltDataBiTidy) #sig main of sex and psex (same)

#physical attractiveness
ltPhysattIntBi <- lmer(scale(physatt)  ~ sex*partnerSex + (1|PIN),
                     data = ltDataBiTidy) #not sig

ltPhysattMainBi <- lmer(scale(physatt)  ~ sex+partnerSex + (1|PIN),
                      data = ltDataBiTidy) #sig main effect of sex (same)

#intell
ltIntellIntBi <- lmer(scale(intell)  ~ sex*partnerSex + (1|PIN),
                    data = ltDataBiTidy) #not sig

ltIntellMainBi <- lmer(scale(intell)  ~ sex+partnerSex + (1|PIN),
                     data = ltDataBiTidy) #sig effect of psex (same)

#resources
ltResourceIntBi <- lmer(scale(resources)  ~ sex*partnerSex + (1|PIN),
                      data = ltDataBiTidy) #not sig

ltResourceMainBi <- lmer(scale(resources)  ~ sex+partnerSex + (1|PIN),
                       data = ltDataBiTidy)  #sig main effect of psex, not sex (same)

#ideal age (NOT STANDARDIZED)
ltAgeIntBi <- lmer(AgeLik ~ sex*partnerSex + (1|PIN), 
                 data = ltDataBiTidy) #not sig 

ltAgeMainBi <- lmer(AgeLik ~ sex+partnerSex + (1|PIN), 
                  data = ltDataBiTidy) #sig main effect of sex and partner sex (same)

### ST prefs main effects ###

#tidy format for analyses
stDataBiTidy <- stDataBi %>%
  pivot_wider(names_from = trait, 
              values_from = value)

#kindness
stKindIntBi <- lmer(scale(kind) ~ sex*partnerSex + (1|PIN), 
                  data = stDataBiTidy) #sig interaction (same)

stKindMainBi <- lmer(scale(kind) ~ sex + partnerSex + (1|PIN), 
                     data = stDataBiTidy)


#physical attractiveness
stPhysattIntBi <- lmer(scale(physatt) ~ sex*partnerSex + (1|PIN), 
                     data = stDataBiTidy) 

stPhysattMainBi <- lmer(scale(physatt) ~ sex+partnerSex + (1|PIN), 
                      data = stDataBiTidy) #only sig main effect of partner sex (same)


#health
stHealthIntBi <- lmer(scale(health)  ~ sex*partnerSex + (1|PIN),
                    data = stDataBiTidy)

stHealthMainBi <- lmer(scale(health)  ~ sex+partnerSex + (1|PIN),
                     data = stDataBiTidy) #sig main effect of partner sex (same)


#intelligence

stIntellIntBi <- lmer(scale(intell)  ~ sex*partnerSex + (1|PIN),
                    data = stDataBiTidy)  

stIntellMainBi <- lmer(scale(intell)  ~ sex+partnerSex + (1|PIN),
                     data = stDataBiTidy) #nothing significant (different)

#resources
stResourceIntBi <- lmer(scale(resources)  ~ sex*partnerSex + (1|PIN),
                      data = stDataBiTidy) 

stResourceMainBi <- lmer(scale(resources)  ~ sex+partnerSex + (1|PIN),
                       data = stDataBiTidy) #sig effect of partner sex (same)

#age (NOT STANDARDIZED)
stAgeIntBi <- lmer(AgeLik ~ sex*partnerSex + (1|PIN), 
                 data = stDataBiTidy) 

stAgeMainBi <- lmer(AgeLik ~ sex+partnerSex + (1|PIN), 
                  data = stDataBiTidy) #sig main effect of sex and partner sex (same)



###Visualizing data

##Long-Term Prefs

#create new group variable  (0 = male participant/male target, 1 = male/female, 2 = female/male, 3 = female/female)
ltDataBi$group <- ifelse(ltDataBi$sex == 1 & ltDataBi$partnerSex == 1, 0, 
                         ifelse(ltDataBi$sex == 1 & ltDataBi$partnerSex == 0, 1, 
                                ifelse(ltDataBi$sex == 0 & ltDataBi$partnerSex == 1, 2,
                                       ifelse(ltDataBi$sex == 0 & ltDataBi$partnerSex == 0, 3, NA))))


ltDataBi$group <- as.factor(ltDataBi$group)


##faceted density plot (mirrored)

ltMirrorPlotBi <- ggplot(ltDataBi, aes(x = value, fill = group)) +
  #top
  geom_density(aes(y = after_stat(density)),
               data = ltDataBi[ltDataBi$partnerSex == 0,], alpha = 0.8) + #female targets
  #bottom
  geom_density(aes(y = -after_stat(density)),
               data = ltDataBi[ltDataBi$partnerSex == 1,], alpha = 0.8) + #male targets
  facet_wrap(~trait, ncol = 3, scales = "free", labeller = labellerFacet)+
  scale_fill_manual(values = c("1" = "darkblue", "3" = "orangered", "0" = "lightblue", "2" ="orange"), 
                    labels = c("Male Participant/Female Target", "Female Participant/Female Target",
                               "Male Participant/Male Target",  "Female Participant/Male Target"),
                    breaks = c("1", "3", "0", "2")) +
  labs(x = "Preference Value", y = "Density", fill = "Participant Sex/Target Sex") +
  theme_grey(base_size = 20) +
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5)) +
  ggtitle("Preferences for Ideal Partners") +
  geom_text(aes(x = 1, y = Inf, label = "Female \n Partners"),
            hjust = 0, vjust = 1.5, color = "black", size = 6) +
  geom_text(aes(x = 1, y = -Inf, label = "Male \n Partners"),
            hjust = 0, vjust = -1.5, color = "black", size = 6)
  

#ggsave("densityLtBi.jpeg", plot=last_plot(), width=400, height=450, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)





##short term prefs

#create new group variable  (0 = male participant/male target, 1 = male/female, 2 = female/male, 3 = female/female)
stDataBi$group <- ifelse(stDataBi$sex == 1 & stDataBi$partnerSex == 1, 0, 
                       ifelse(stDataBi$sex == 1 & stDataBi$partnerSex == 0, 1, 
                              ifelse(stDataBi$sex == 0 & stDataBi$partnerSex == 1, 2,
                                     ifelse(stDataBi$sex == 0 & stDataBi$partnerSex == 0, 3, NA))))


stDataBi$group <- as.factor(stDataBi$group)




##faceted density plot (mirrored)

stMirrorPlotBi <- ggplot(stDataBi, aes(x = value, fill = group)) +
  #top
  geom_density(aes(y = after_stat(density)),
               data = stDataBi[stDataBi$partnerSex == 0,], alpha = 0.8) + #female targets
  #bottom
  geom_density(aes(y = -after_stat(density)),
               data = stDataBi[stDataBi$partnerSex == 1,], alpha = 0.8) + #male targets
  facet_wrap(~trait, ncol = 3, scales = "free", labeller = labellerFacet)+
  scale_fill_manual(values = c("1" = "darkblue", "3" = "orangered", "0" = "lightblue", "2" ="orange"), 
                    labels = c("Male Participant/Female Target", "Female Participant/Female Target",
                               "Male Participant/Male Target",  "Female Participant/Male Target"),
                    breaks = c("1", "3", "0", "2")) +
  labs(x = "Preference Value", y = "Density", fill = "Participant Sex/Target Sex") +
  theme_grey(base_size = 20) +
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5)) +
  ggtitle("Preferences for Ideal Partners") +
  geom_text(aes(x = 1.2, y = Inf, label = "Female \n Partners"),
            hjust = 0, vjust = 1.25, color = "black", size = 6) +
  geom_text(aes(x = 1.2, y = -Inf, label = "Male \n Partners"),
            hjust = 0, vjust = -1.25, color = "black", size = 6)

#ggsave("densityStBi.jpeg", plot=last_plot(), width=400, height=450, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)






###Cluster analysis
#across all traits, excluding age 
#men rating women; men rating men; women rating men; women rating women

##LT Prefs

#remove NAs from dataframe 

nacheckLtBi <- apply(ltDataBiTidy[,1:9], 1, function(x) sum(is.na(x))>0)
ltDataBiK<- ltDataBiTidy[!nacheckLtBi,1:8] #excludes age also

###ipsatize traits


##z-score (so takes into account avg value of that trait across ppl)
ltDataBiK[,4:8] <- apply(ltDataBiK[,4:8],2,scale)

##ipsatize z-scored values

#adding empty columns to dataframe
avColsLtBi <- c('ipHealth', 'ipIntell', 'ipKind', 'ipPhysatt', 'ipResources')
ltDataBiK[ , avColsLtBi] <- NA


#take means of traits for every PIN and subtract mean from indiv traits & place in new columns
for (i in 1:nrow(ltDataBiK)){
  focalPrefsLtBi <- ltDataBiK[i,4:8]
  #within person, across targets
  avPrefsLtBi <- mean(rowMeans(ltDataBiK[ltDataBiK$PIN == ltDataBiK$PIN[i],4:8], na.rm = T))
  focalPrefsLtBi <- focalPrefsLtBi - avPrefsLtBi
  ltDataBiK[i,9:13] <- focalPrefsLtBi
  
}


#extract kmeans wSs
kfitWssLtBi<-sapply(1:7,function(x) kmeans(ltDataBiK[,9:13],x)$tot.withinss)

#scree plot
screePlotLtBi<-qplot(1:7,kfitWssLtBi) 

##compute differences in within ss across k for k-means clustering
wssDiffsLtBi<-diff(kfitWssLtBi) 

##Add classification to the original dataframe

kFitLtBi<-kmeans(ltDataBiK[,9:13],4)
ltDataBiK$kFitLtBi <- kFitLtBi$cluster


##Create vectors of preference means for each cluster 
clustCentersLtBi<-kFitLtBi$centers


#are men and women are choosing clusters at diff rates? 
#have to separate based on ideal male v female partners bc independence assumption
chisqSexLtBiIdealF<-chisq.test(table(ltDataBiK$sex[ltDataBiK$partnerSex == 0],ltDataBiK$kFitLtBi[ltDataBiK$partnerSex == 0])) #yes (same)
chisqSexLtBiIdealM<-chisq.test(table(ltDataBiK$sex[ltDataBiK$partnerSex == 1],ltDataBiK$kFitLtBi[ltDataBiK$partnerSex == 1])) #yes (same)


##chisq -- male partner cluster x female partner cluster

#create separate DF with pin + cluster + partner sex
columns <- c(1, 3, 14)
chisqDataLtBi <- ltDataBiK[,columns]

#create 2 new blank columns: male cluster and female cluster
chisqDataLtBi$MaleClust <- NA
chisqDataLtBi$FemaleClust <- NA

#add cluster to column corresponding to correct partner sex

for(i in 1:nrow(chisqDataLtBi)){
  if(chisqDataLtBi$partnerSex[i] == 1){
    chisqDataLtBi$MaleClust[i] <- chisqDataLtBi$kFitLtBi[i]
  } else {
    chisqDataLtBi$FemaleClust[i] <- chisqDataLtBi$kFitLtBi[i]
  }
  
}

#make df wide so each ID is only one row
chisqDataLtBi <- pivot_wider(chisqDataLtBi, id_cols = PIN, names_from = partnerSex, values_from = c("kFitLtBi", "MaleClust", "FemaleClust"))
#subset so only relevant columns are kept
chisqDataLtBi <- subset(chisqDataLtBi, select = c(PIN, MaleClust_1, FemaleClust_0))
#rename columns
names(chisqDataLtBi) <- c("PIN", "maleClust", "femaleClust")

#run chisq,excluding NAs
pSexClustFisherLtBi <- fisher.test(table(chisqDataLtBi$maleClust, chisqDataLtBi$femaleClust), simulate.p.value = T) #sig (same)
  #have to do fisher bc some cells only contain 1



###Matrix Tables

##ideal female partners

#create matrix dataframe
LtMatrixDataBiFemale <- data.frame((table(ltDataBiK$sex[ltDataBiK$partnerSex == 0],ltDataBiK$kFitLtBi[ltDataBiK$partnerSex == 0])/
                                      rowSums(table(ltDataBiK$sex[ltDataBiK$partnerSex == 0],ltDataBiK$kFitLtBi[ltDataBiK$partnerSex == 0])))*100)
#relabel column names
colnames(LtMatrixDataBiFemale) <- c("sex", "cluster", "clusterFrequency")
#round all numbers to 2 decimal places
LtMatrixDataBiFemale[,3] <-round(LtMatrixDataBiFemale[,3],2) 


#plot matrix
LtMatrixPlotBiFemale <- ggplot(LtMatrixDataBiFemale, aes(x= sex, y = cluster, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = LtMatrixDataBiFemale$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "springgreen4") +
  scale_x_discrete(labels = c('Female','Male')) +
  scale_y_discrete(labels = c("Kind \n& Smart","Smart \n& Healthy","Attractive \n& Healthy", "Smart")) +
  ggtitle("A") +
  labs(x = "Participant Sex", y = "Cluster of Female Targets", fill = "Cluster \nFrequency")+
  theme(legend.position = "none")

#ggsave("LtMatrixPlotBiFemale.jpeg", plot=last_plot(), width=225, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)

##ideal male partners

#create matrix dataframe
LtMatrixDataBiMale <- data.frame((table(ltDataBiK$sex[ltDataBiK$partnerSex == 1],ltDataBiK$kFitLtBi[ltDataBiK$partnerSex == 1])/
                                    rowSums(table(ltDataBiK$sex[ltDataBiK$partnerSex == 1],ltDataBiK$kFitLtBi[ltDataBiK$partnerSex == 1])))*100)
#relabel column names
colnames(LtMatrixDataBiMale) <- c("sex", "cluster", "clusterFrequency")
#round all numbers to 2 decimal places
LtMatrixDataBiMale[,3] <-round(LtMatrixDataBiMale[,3],2) 


#plot matrix
LtMatrixPlotBiMale <- ggplot(LtMatrixDataBiMale, aes(x= sex, y = cluster, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = LtMatrixDataBiMale$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "springgreen4") +
  scale_x_discrete(labels = c('Female','Male')) +
  scale_y_discrete(labels = c("Kind \n& Smart","Smart \n& Healthy","Attractive \n& Healthy", "Smart")) +
  ggtitle("B")+
  labs(x = "Participant Sex", y = "Cluster of Male Targets", fill = "Cluster \nFrequency") +
  theme(legend.position = "none")


#panel plot of both of these graphs
ltMatrixPanelBi <- ggarrange(LtMatrixPlotBiFemale, LtMatrixPlotBiMale, nrow=1, ncol=2, 
                             common.legend = TRUE, legend = "right")
#ggsave("LtMatrixPlotPanelBi.jpeg", plot=last_plot(), width=200, height=110, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)




##male compared to female partners

#create matrix dataframe
LtMatrixDataBiPartners <- data.frame((table(chisqDataLtBi$maleClust, chisqDataLtBi$femaleClust))/
                                       sum(table(chisqDataLtBi$maleClust, chisqDataLtBi$femaleClust))*100)
#relabel column names
colnames(LtMatrixDataBiPartners) <- c("maleClust", "femaleClust", "clusterFrequency")
#round all numbers to 2 decimal places
LtMatrixDataBiPartners[,3] <-round(LtMatrixDataBiPartners[,3],2) 


#plot matrix
LtMatrixPlotBiPartners<- ggplot(LtMatrixDataBiPartners, aes(x= femaleClust, y = maleClust, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = LtMatrixDataBiPartners$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "springgreen4") +
  scale_x_discrete(labels = c("Kind & Smart","Smart & Healthy","Attractive & Healthy", "Smart")) +
  scale_y_discrete(labels = c("Kind & Smart","Smart & Healthy","Attractive & Healthy", "Smart")) +
  labs(x = "Ideal Female Partner Cluster", y = "Ideal Male Partner Cluster", fill = "Cluster Frequency")

#ggsave("LtMatrixPlotBiPartners.jpeg", plot=last_plot(), width=200, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)






### Plotting ###


#Multipanel figure


#cluster 1 (title will change based on clusters)
meanTrait1LtBi <- clustCentersLtBi[1,]
trait1LtBi <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting1LtBi <- data.frame(meanTrait1LtBi, trait1LtBi)
plot1LtBi <- ggplot(data=plotting1LtBi, aes(x=trait1LtBi, y=meanTrait1LtBi)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "red")+
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level")  +ylim(-1,1) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 2 
meanTrait2LtBi <- clustCentersLtBi[2,]
trait2LtBi <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting2LtBi <- data.frame(meanTrait2LtBi, trait2LtBi)
plot2LtBi <- ggplot(data=plotting2LtBi, aes(x=trait2LtBi, y=meanTrait2LtBi)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "forestgreen")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-1,1) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 3 
meanTrait3LtBi <- clustCentersLtBi[3,]
trait3LtBi <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting3LtBi <- data.frame(meanTrait3LtBi, trait3LtBi)
plot3LtBi <- ggplot(data=plotting3LtBi, aes(x=trait3LtBi, y=meanTrait3LtBi)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "purple")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-1,1) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 4
meanTrait4LtBi <- clustCentersLtBi[4,]
trait4LtBi <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting4LtBi <- data.frame(meanTrait4LtBi, trait4LtBi)
plot4LtBi <- ggplot(data=plotting4LtBi, aes(x=trait4LtBi, y=meanTrait4LtBi)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "yellow")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-1,1) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))



#combine clusters into one graph
PRpanelPlotLtBi<-ggarrange(plot1LtBi,plot2LtBi,plot3LtBi, plot4Lt,
                           labels=c("Kind & Smart","Smart & Healthy","Attractive & Healthy", "Smart"), nrow=1, ncol=4,font.label = list(size = 14, color = "black"))



##ST Prefs 

#remove NAs from dataframe 

nacheckStBi <- apply(stDataBiTidy[,1:9], 1, function(x) sum(is.na(x))>0)
stDataBiK<- stDataBiTidy[!nacheckStBi,1:8]

#ipsatize traits


#z-score (so takes into account avg value of that trait across ppl)
stDataBiK[,4:8] <- apply(stDataBiK[,4:8],2,scale)


#adding empty columns to dataframe
avColsStBi <- c('ipHealth', 'ipIntell', 'ipKind', 'ipPhysatt', 'ipResources')
stDataBiK[ , avColsStBi] <- NA


#take means of traits for every PIN and subtract mean from indiv traits & place in new columns
for (i in 1:nrow(stDataBiK)){
  focalPrefsStBi <- stDataBiK[i,4:8]
  avPrefsStBi <- mean(rowMeans(stDataBiK[stDataBiK$PIN == stDataBiK$PIN[i],4:8], na.rm = T))
  focalPrefsStBi <- focalPrefsStBi - avPrefsStBi
  stDataBiK[i,9:13] <- focalPrefsStBi
  
}



##st prefs cluster analysis 


#extract kmeans wSs
kfitWssStBi<-sapply(1:7,function(x) kmeans(stDataBiK[,9:13],x)$tot.withinss)

#scree plot
screePlotStBi<-qplot(1:7,kfitWssStBi) 

##compute differences in within ss across k for k-means clustering
wssDiffsSt<-diff(kfitWssSt)

##Add classification to the original dataframe
kFitStBi<-kmeans(stDataBiK[,9:13],3)
stDataBiK$kFitStBi <- kFitStBi$cluster


##Create vectors of preference means for each cluster 
clustCentersStBi<-kFitStBi$centers


###Chisq Analyses

##are men and women are choosing clusters at diff rates? 
#have to separate based on ideal male v female partners bc independence assumption
chisqSexStBiIdealF <- chisq.test(table(stDataBiK$sex[stDataBiK$partnerSex == 0],stDataBiK$kFitStBi[stDataBiK$partnerSex == 0]))

chisqSexStBiIdealM<-chisq.test(table(stDataBiK$sex[stDataBiK$partnerSex == 1],stDataBiK$kFitStBi[stDataBiK$partnerSex == 1])) 



##chisq -- male partner cluster x female partner cluster

#create separate DF with pin + cluster + partner sex
columnsBi <- c(1, 3, 14)
chisqDataStBi <- stDataBiK[,columns]

#create 2 new blank columns: male cluster and female cluster
chisqDataStBi$MaleClust <- NA
chisqDataStBi$FemaleClust <- NA

#add cluster to column corresponding to correct partner sex

for(i in 1:nrow(chisqDataStBi)){
  if(chisqDataStBi$partnerSex[i] == 1){
    chisqDataStBi$MaleClust[i] <- chisqDataStBi$kFitStBi[i]
  } else {
    chisqDataStBi$FemaleClust[i] <- chisqDataStBi$kFitStBi[i]
  }
  
}

#make df wide so each ID is only one row
chisqDataStBi <- pivot_wider(chisqDataStBi, id_cols = PIN, names_from = partnerSex, values_from = c("kFitStBi", "MaleClust", "FemaleClust"))
#subset so only relevant columns are kept
chisqDataStBi <- subset(chisqDataStBi, select = c(PIN, MaleClust_1, FemaleClust_0))
#rename columns
names(chisqDataStBi) <- c("PIN", "maleClust", "femaleClust")

#run chisq
pSexClustChisqStBi <- chisq.test(table(chisqDataStBi$maleClust, chisqDataStBi$femaleClust))


###Matrix Tables

##ideal female partners

#create matrix dataframe
StMatrixDataBiFemale <- data.frame((table(stDataBiK$sex[stDataBiK$partnerSex == 0],stDataBiK$kFitStBi[stDataBiK$partnerSex == 0])/
                                      rowSums(table(stDataBiK$sex[stDataBiK$partnerSex == 0],stDataBiK$kFitStBi[stDataBiK$partnerSex == 0])))*100)
#relabel column names
colnames(StMatrixDataBiFemale) <- c("sex", "cluster", "clusterFrequency")
#round all numbers to 2 decimal places
StMatrixDataBiFemale[,3] <-round(StMatrixDataBiFemale[,3],2) 


#plot matrix
StMatrixPlotBiFemale <- ggplot(StMatrixDataBiFemale, aes(x= sex, y = cluster, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = StMatrixDataBiFemale$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "springgreen4") +
  scale_x_discrete(labels = c('Female','Male')) +
  scale_y_discrete(labels = c("Wealthy","Attractive \n& Healthy","Smart \n& Kind")) +
  ggtitle("A")+
  labs(x = "Participant Sex", y = "Cluster of Female Targets", fill = "Cluster \nFrequency") +
  theme(legend.position = "none")

#ggsave("StMatrixPlotBiFemale.jpeg", plot=last_plot(), width=225, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)

##ideal male partners

#create matrix dataframe
StMatrixDataBiMale <- data.frame((table(stDataBiK$sex[stDataBiK$partnerSex == 1],stDataBiK$kFitStBi[stDataBiK$partnerSex == 1])/
                                    rowSums(table(stDataBiK$sex[stDataBiK$partnerSex == 1],stDataBiK$kFitStBi[stDataBiK$partnerSex == 1])))*100)
#relabel column names
colnames(StMatrixDataBiMale) <- c("sex", "cluster", "clusterFrequency")
#round all numbers to 2 decimal places
StMatrixDataBiMale[,3] <-round(StMatrixDataBiMale[,3],2) 


#plot matrix
StMatrixPlotBiMale <- ggplot(StMatrixDataBiMale, aes(x= sex, y = cluster, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = StMatrixDataBiMale$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "springgreen4") +
  scale_x_discrete(labels = c('Female','Male')) +
  scale_y_discrete(labels = c("Wealthy","Attractive \n& Healthy","Smart \n& Kind")) +
  ggtitle("B")+
  labs(x = "Participant Sex", y = "Cluster of Male Targets", fill = "Cluster \nFrequency")+
  theme(legend.position = "none")


# PANEL PLOT


stMatrixPanelBi <- ggarrange(StMatrixPlotBiFemale, StMatrixPlotBiMale, nrow=1, ncol=2,
                             common.legend = TRUE, legend = "right")
#ggsave("StMatrixPlotPanelBi.jpeg", plot=last_plot(), width=300, height=150, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)



##male compared to female partners

#create matrix dataframe
StMatrixDataBiPartners <- data.frame((table(chisqDataStBi$maleClust, chisqDataStBi$femaleClust))/
                                       sum(table(chisqDataStBi$maleClust, chisqDataStBi$femaleClust))*100)
#relabel column names
colnames(StMatrixDataBiPartners) <- c("maleClust", "femaleClust", "clusterFrequency")
#round all numbers to 2 decimal places
StMatrixDataBiPartners[,3] <-round(StMatrixDataBiPartners[,3],2) 


#plot matrix
StMatrixPlotBiPartners<- ggplot(StMatrixDataBiPartners, aes(x= femaleClust, y = maleClust, fill = clusterFrequency)) +
  geom_tile(color = "white") +
  geom_text(label = StMatrixDataBiPartners$clusterFrequency)+
  scale_fill_gradient(low = "white", high = "springgreen4") +
  scale_x_discrete(labels = c("Wealthy","Attractive & Healthy","Smart & Kind")) +
  scale_y_discrete(labels = c("Wealthy","Attractive & Healthy","Smart & Kind")) +
  labs(x = "Ideal Female Partner Cluster", y = "Ideal Male Partner Cluster", fill = "Cluster Frequency")




### Plotting ###


#Multipanel figure


#cluster 1 (title will change based on clusters)
meanTrait1StBi <- clustCentersStBi[1,]
trait1StBi <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting1StBi <- data.frame(meanTrait1StBi, trait1StBi)
plot1StBi <- ggplot(data=plotting1StBi, aes(x=trait1StBi, y=meanTrait1StBi)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "red")+
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level")  +ylim(-.7,.7) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 2 
meanTrait2StBi <- clustCentersStBi[2,]
trait2StBi <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting2StBi <- data.frame(meanTrait2StBi, trait2StBi)
plot2StBi <- ggplot(data=plotting2StBi, aes(x=trait2StBi, y=meanTrait2StBi)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "forestgreen")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-.7,.7) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))

#cluster 3 
meanTrait3StBi <- clustCentersStBi[3,]
trait3StBi <- c("Health", "Intelligence", "Kindness", "Physical Attractiveness", "Resources")
plotting3StBi <- data.frame(meanTrait3StBi, trait3StBi)
plot3StBi <- ggplot(data=plotting3StBi, aes(x=trait3StBi, y=meanTrait3StBi)) +
  geom_bar(stat="identity", color="black", position=position_dodge(), fill = "purple")+ 
  theme_minimal(base_size = 14) + xlab("Trait") + ylab("Relative Desired Trait Level") +ylim(-.7,.7) +
  theme(plot.title = element_text(size = 14), axis.text.x = element_text(angle = 90))


#combine clusters into one graph
PRpanelPlotStBi<-ggarrange(plot1StBi,plot2StBi,plot3StBi,
                         labels=c("Wealthy","Attractive & Healthy","Smart & Kind"), 
                         nrow=1, ncol=3,font.label = list(size = 13, color = "black")) 

#ggsave("PRpanelPlotStBi.jpeg", plot=last_plot(), width=300, height=200, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)





########Generating descriptives of trait pref values
healthDescriptives <- summary(ltDataTidy$health)
healthSD <- sd(ltDataTidy$health, na.rm = TRUE)

intellDescriptives <- summary(ltDataTidy$intell)
intellSD <- sd(ltDataTidy$intell, na.rm = TRUE)

kindDescriptives <- summary(ltDataTidy$kind)
kindSD <- sd(ltDataTidy$kind, na.rm = TRUE)

physattDescriptives <- summary(ltDataTidy$physatt)
physattSD <- sd(ltDataTidy$physatt, na.rm = TRUE)

resourcesDescriptives <- summary(ltDataTidy$resources)
resourcesSD <- sd(ltDataTidy$resources, na.rm = TRUE)




