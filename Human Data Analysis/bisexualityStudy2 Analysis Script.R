#####De-Confounding Sex and Sex of Partner in Mate Preference Research#####
################Study 2 Analysis Script##############

###load packages###
library(lmerTest)
library(reshape2) #to reshape data
library(tidyverse) #for %>% among other things
library(ggplot2)
library(sjPlot) #for interaction plots


### set seed ###
set.seed(040524)


#load data
data<-read.csv("Human Data/Processed Data/BisexualityStudy2_DataPROCESSED 20240514 161447.csv")



###Omnibus Analyses


#longform data

#LT 
ltData <- data[,c(51, 5, 52, 53:62, 64:65)]
ltData <- melt(ltData, id.vars=c("PIN", "sex", "sexuality"))
ltData <- ltData %>% 
  separate("variable", into = c("partnerSex", "x", "trait"), remove = T)
ltData$partnerSex <- ifelse(ltData$partnerSex == "m", 1, 0)
ltData <- ltData[,c(1:4, 6:7)]
ltData$sex  <- as.factor(ltData$sex)
ltData$sexuality <- as.factor(ltData$sexuality)
ltData$partnerSex  <- as.factor(ltData$partnerSex)


###mixed effects models

##only bisexual participants (replicating study 1 analyses)
ltDataBi <- ltData[ltData$sexuality == 0, ]

omnibusBi <- lmer(value ~ partnerSex + trait*sex + (1|PIN), 
                  data = ltDataBi)
omnibusBi2 <- lmer(value ~ partnerSex*trait*sex + (1|PIN), 
                  data = ltDataBi)


##only using people's opposite sex ratings (so het people + bi opp sex)
ltDataCombo <- ltData[ltData$sex != ltData$partnerSex, ] #this makes it so we only keep rows where participnt sex is different than partner sex


omnibusCombo <- lmer(value ~ trait*sex*sexuality + (1|PIN), 
                     data = ltDataCombo) 



###Trait by sex interactions 

##tidy format for analyses

#Bi-only data
ltDataBiTidy <- ltDataBi %>%
  pivot_wider(names_from = trait, 
              values_from = value)

ltDataComboTidy <- ltDataCombo %>%
  pivot_wider(names_from = trait, 
              values_from = value)

#health 

ltHealthBi <- lmer(scale(health)  ~ sex+partnerSex + (1|PIN), 
                     data = ltDataBiTidy) #not sig

ltHealthCombo <- lm(scale(health) ~ sex*sexuality, data = ltDataComboTidy)



#kindness 

ltKindBi <- lmer(scale(kind)  ~ sex+partnerSex + (1|PIN),
                   data = ltDataBiTidy) #no sig main effects (different than study1)


ltKindCombo <- lm(scale(kind)  ~ sex*sexuality,
                  data = ltDataComboTidy)

#plotting that interaction

kindIntPlot <- plot_model(ltKindCombo, type = "pred", terms = c("sexuality", "sex"))




#physical attractiveness

ltPhysattBi <- lmer(scale(physatt)  ~ sex+partnerSex + (1|PIN),
                      data = ltDataBiTidy) #sig main effect of sex

ltPhysattCombo <- lm(scale(physatt)  ~ sex*sexuality,
                     data = ltDataComboTidy) 


#intell

ltIntellBi <- lmer(scale(intell)  ~ sex+partnerSex + (1|PIN),
                     data = ltDataBiTidy) #no sig effects (diff to study 1)


ltIntellCombo <- lm(scale(intell)  ~ sex*sexuality,
                    data = ltDataComboTidy)



#resources

ltResourceBi <- lmer(scale(resources)  ~ sex+partnerSex + (1|PIN),
                       data = ltDataBiTidy)  #sig main effect of psex (same as study 1)

ltResourceCombo <- lm(scale(resources)  ~ sex*sexuality,
                      data = ltDataComboTidy)


#bisexual preferences for resources in opposite sex partners only
oppSexData <- ltDataBiTidy %>%
  filter(ltDataBiTidy[[2]] != ltDataBiTidy[[4]])

ltResourceOppSex <- lm(scale(resources)  ~ sex,
                       data = oppSexData) 



#ideal age (NOT STANDARDIZED)

ltAgeBi <- lmer(AgeLik ~ sex+partnerSex + (1|PIN), 
                  data = ltDataBiTidy) #sig main effect of sex and partner sex


ltAgeCombo <- lm(AgeLik ~ sex*sexuality, 
                 data = ltDataComboTidy)



###Integrative sample analyses
#pooling bisexual study 1 and 2 samples 
#will replicate bisexual analyses w pooled sample

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

#make sure there are no NAs in sex or partner sex columns
nacheckLtStudyOne <- apply(ltDataStudyOne[,2:3], 1, function(x) sum(is.na(x))>0)
ltDataStudyOne<- ltDataStudyOne[!nacheckLtStudyOne,]

##add study variable for study 1 data
ltDataStudyOne$study <- 1


##new study 2 data frame w/ bi participants, no sexuality or group columns
cols <- c(1,2,4,5,6)
ltDataBiStudyTwo <- ltDataBi[,cols]

##add study variable to study 2 data
ltDataBiStudyTwo$study <- 2

##merge 2 dataframes for combined data
pooledBiData <- rbind(ltDataStudyOne, ltDataBiStudyTwo)


##omnibus test
omnibusPooled <- lmer(value ~ partnerSex + study + trait*sex + (1|PIN), 
                  data = pooledBiData)
omnibusPooled2 <- lmer(value ~ partnerSex*trait*sex + study + (1|PIN), 
                   data = pooledBiData)

##exploring the trait*sex interactions

#tidy format for analyses
pooledBiDataTidy <- pooledBiData %>%
  pivot_wider(names_from = trait, 
              values_from = value)


healthBiPooled <- lmer(scale(health)  ~ sex+partnerSex + study + (1|PIN), 
                   data = pooledBiDataTidy) #sig

kindBiPooled <- lmer(scale(kind)  ~ sex+partnerSex + study + (1|PIN),
                 data = pooledBiDataTidy) #sig main effect of sex and partner sex

physattBiPooled <- lmer(scale(physatt)  ~ sex+partnerSex + study + (1|PIN),
                    data = pooledBiDataTidy) #sig main effect of sex

intellBiPooled <- lmer(scale(intell)  ~ sex+partnerSex + study + (1|PIN),
                   data = pooledBiDataTidy) #sig main effect of partner sex

resourcesBiPooled <- lmer(scale(resources)  ~ sex+partnerSex + study + (1|PIN),
                     data = pooledBiDataTidy)  #sig main effect of psex (same as study 1)

oppSexDataPooled <- pooledBiDataTidy %>% #bisexual preferences for resources in opposite sex partners only
  filter(pooledBiDataTidy[[2]] != pooledBiDataTidy[[3]])

resourcesOppSexPooled <- lm(scale(resources)  ~ sex + study,
                       data = oppSexDataPooled)

ageBiPooled <- lmer(AgeLik ~ sex+partnerSex + study + (1|PIN), #ideal age (NOT STANDARDIZED)
                data = pooledBiDataTidy) #sig main effect of sex and partner sex



###plotting

#overall plot faceted by partner sex

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

###facted mirror density plot

##Bi participants only (replication of study 1)



#create new group variable  (0 = male participant/male target, 1 = male/female, 2 = female/male, 3 = female/female)
ltDataBi$group <- ifelse(ltDataBi$sex == 1 & ltDataBi$partnerSex == 1, 0, 
                       ifelse(ltDataBi$sex == 1 & ltDataBi$partnerSex == 0, 1, 
                              ifelse(ltDataBi$sex == 0 & ltDataBi$partnerSex == 1, 2,
                                     ifelse(ltDataBi$sex == 0 & ltDataBi$partnerSex == 0, 3, NA))))
ltDataBi$group <- as.factor(ltDataBi$group)




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
  ggtitle("Preferences for Ideal Partners (Bisexual Participants Only)") +
  geom_text(aes(x = 1.3, y = Inf, label = "Female \n Partners"),
            hjust = 0, vjust = 2.5, color = "black", size = 6) +
  geom_text(aes(x = 1.3, y = -Inf, label = "Male \n Partners"),
            hjust = 0, vjust = -2.5, color = "black", size = 6) 



#ggsave("densityLtBi.jpeg", plot=last_plot(), width=400, height=450, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)



#plotting trait levels for each trait faceted by sexuality (0 = bisexual, 1 = heterosexual)
sexualityTraitPlot <- ggplot(data = ltData, aes(x = trait, y = value, fill = sex)) +
  stat_summary(fun = "mean", geom = "bar", position = position_dodge(), color = "black") +
  facet_wrap(~sexuality, labeller = as_labeller(c("0" = "Bisexual Participants", "1" = "Heterosexual Participants")))+
  xlab("Trait") +
  ylab("Trait Value")+
  scale_x_discrete(labels = c("Age", "Health", "Intell.", "Kind.", "Phys. Att.", "Resources"))+
  labs(fill = "Participant Sex") +
  scale_fill_manual(values = c("0" = "maroon", "1" = "blue"), 
                    labels = c("Female", "Male"))


########Generating descriptives of trait pref values
healthDescriptives <- summary(ltDataComboTidy$health)
healthSD <- sd(ltDataComboTidy$health, na.rm = TRUE)

intellDescriptives <- summary(ltDataComboTidy$intell)
intellSD <- sd(ltDataComboTidy$intell, na.rm = TRUE)

kindDescriptives <- summary(ltDataComboTidy$kind)
kindSD <- sd(ltDataComboTidy$kind, na.rm = TRUE)

physattDescriptives <- summary(ltDataComboTidy$physatt)
physattSD <- sd(ltDataComboTidy$physatt, na.rm = TRUE)

resourcesDescriptives <- summary(ltDataComboTidy$resources)
resourcesSD <- sd(ltDataComboTidy$resources, na.rm = TRUE)


###supplemental materials

###exploring a potential main effect of relationship status

#create dataframe with rel status variable

#longform data

#LT 
ltDataRelStat <- data[,c(51, 5, 2, 52, 53:62, 64:65)]
ltDataRelStat <- melt(ltDataRelStat, id.vars=c("PIN", "sex", "rel_status", "sexuality"))
ltDataRelStat <- ltDataRelStat %>% 
  separate("variable", into = c("partnerSex", "x", "trait"), remove = T)
ltDataRelStat$partnerSex <- ifelse(ltDataRelStat$partnerSex == "m", 1, 0)
ltDataRelStat <- ltDataRelStat[,c(1:5, 7:8)]
ltDataRelStat$sex  <- as.factor(ltDataRelStat$sex)
ltDataRelStat$sexuality <- as.factor(ltDataRelStat$sexuality)
ltDataRelStat$partnerSex  <- as.factor(ltDataRelStat$partnerSex)
ltDataRelStat$rel_status  <- as.factor(ltDataRelStat$rel_status)

#make sure there are no NAs in sex or partner sex columns
colNum<- c(2,5)
nacheckLtRelStat <- apply(ltDataRelStat[,colNum], 1, function(x) sum(is.na(x))>0)
ltDataRelStat<- ltDataRelStat[!nacheckLtRelStat,]


###mixed effects models

##only bisexual participants (replicating study 1 analyses)
ltDataRelStatBi <- ltDataRelStat[ltDataRelStat$sexuality == 0, ]

omnibusBiRelStat <- lmer(value ~ partnerSex + trait*sex*rel_status +
                            (1|PIN), 
                  data = ltDataRelStatBi)



#exploring effect trait*trait
#tidy format for analyses
ltDataRelStatBiTidy <- ltDataRelStatBi %>%
  pivot_wider(names_from = trait, 
              values_from = value)

#standardizing outcome variable (traits)

#health 
ltHealthRelStatBi <- lmer(scale(health)  ~ partnerSex + sex*rel_status + (1|PIN),
                        data = ltDataRelStatBiTidy)
ltHealthRelStatBi2 <- lmer(scale(health)  ~ partnerSex + sex + rel_status + (1|PIN),
                          data = ltDataRelStatBiTidy)

#kindness 


ltKindRelStatBi <- lmer(scale(kind)  ~ partnerSex + sex*rel_status + (1|PIN),
                      data = ltDataRelStatBiTidy)
ltKindRelStatBi2 <- lmer(scale(kind)  ~ partnerSex + sex + rel_status + (1|PIN),
                        data = ltDataRelStatBiTidy)

#physical attractiveness

ltPhysattRelStatBi <- lmer(scale(physatt)  ~ partnerSex + sex*rel_status + (1|PIN),
                         data = ltDataRelStatBiTidy)

ltPhysattRelStatBi2 <- lmer(scale(physatt)  ~ partnerSex + sex + rel_status + (1|PIN),
                           data = ltDataRelStatBiTidy)

#intell

ltIntellRelStatBi <- lmer(scale(intell) ~ partnerSex + sex*rel_status + (1|PIN),
                        data = ltDataRelStatBiTidy)
ltIntellRelStatBi2 <- lmer(scale(intell) ~ partnerSex + sex + rel_status + (1|PIN),
                          data = ltDataRelStatBiTidy)

#resources

ltResourceRelStatBi <- lmer(scale(resources)  ~ partnerSex + sex*rel_status + (1|PIN),
                          data = ltDataRelStatBiTidy)

ltResourceRelStatBi2 <- lmer(scale(resources)  ~ partnerSex + sex + rel_status + (1|PIN),
                          data = ltDataRelStatBiTidy)

#ideal age (NOT STANDARDIZED)
ltAgeRelStatBi <- lmer(AgeLik ~ partnerSex + sex*rel_status + (1|PIN),
                     data = ltDataRelStatBiTidy)

#plotting interaction
AgeRelStatIntPlot <- plot_model(ltAgeRelStatBi, type = "pred", terms = c("rel_status", "sex"))

AgeRelStatIntPlot2 <- ggplot(data = ltDataRelStatBiTidy, aes(x = rel_status, y = AgeLik, fill = sex)) +
  geom_boxplot(position = position_dodge(), color = "black")+
  xlab("Relationship Status") +
  ylab("Ideal Age Difference")+
  scale_x_discrete(labels = c("Single", "Casually Dating", "Seriously Dating", "Engaged", "Married"))+
  labs(fill = "Participant Sex") +
  scale_fill_manual(values = c("0" = "maroon", "1" = "blue"), 
                    labels = c("Female", "Male"))


