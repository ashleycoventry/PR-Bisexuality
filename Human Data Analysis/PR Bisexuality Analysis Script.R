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


#resources model -- with same/opp sex partner as predictor

#add same or opp variable to lt data tidy (0=same, 1=opp)
ltDataTidy$sameOrOppSex <- ifelse(ltDataTidy$sex == ltDataTidy$partnerSex, 0, 1) 
ltDataTidy$sameOrOppSex <- as.factor(ltDataTidy$sameOrOppSex)

ltResourceSameOpp <- lmer(scale(resources) ~ sex*sameOrOppSex + (1|PIN),
                          data = ltDataTidy)

#plot interaction
resourceLtIntPlot <- plot_model(ltResourceSameOpp, type = "pred", terms = c("sex", "sameOrOppSex"))

resourceLtIntPlot2 <- ggplot(ltDataTidy, aes(x = sex, y = resources, fill = sameOrOppSex))+
  stat_summary(fun = "mean", geom = "bar", position = position_dodge(), color = "black") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", position = position_dodge(width = 0.9),
    width = 0.25) +
  scale_fill_manual(
    values = c("0" = "#9DC183", "1" = "#FFBF00"), 
    labels = c("Same Sex", "Opposite Sex")) +
  scale_x_discrete(labels = c("0" = "Male", "1" = "Female")) +
  labs(x = "Participant Sex", y = "Resource Preference", 
       fill = "Target Sex (Same Or Opposite Participant)") +
  theme_minimal()

resourceLtIntPlot3 <- ggplot(ltDataTidy, aes(x = sex, y = resources, fill = sameOrOppSex))+
  geom_violin(position = position_dodge(width = 0.9), color = "black", alpha = 0.7) + 
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.2, color = "black", outlier.shape = NA) + 
  scale_fill_manual(values = c("0" = "#9DC183", "1" = "#FFBF00"), 
    labels = c("Same Sex", "Opposite Sex")) +
  scale_x_discrete(labels = c("0" = "Male", "1" = "Female")) +
  labs(x = "Participant Sex", y = "Resource Preference", 
       fill = "Target Sex (Same Or Opposite Participant)") +
  theme_minimal()

resourceLtIntPlot4 <- ggplot(ltDataTidy, aes(x = sex, y = resources, fill = sameOrOppSex))+
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.5, color = "black", outlier.shape = NA) + 
  scale_fill_manual(values = c("0" = "#9DC183", "1" = "#FFBF00"), 
                    labels = c("Same Sex", "Opposite Sex")) +
  scale_x_discrete(labels = c("0" = "Male", "1" = "Female")) +
  labs(x = "Participant Sex", y = "Resource Preference", 
       fill = "Target Sex (Same Or Opposite Participant)") +
  theme_minimal()

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


#add same or opp variable to lt data tidy (0=same, 1=opp)
stDataTidy$sameOrOppSex <- ifelse(stDataTidy$sex == stDataTidy$partnerSex, 0, 1) 
stDataTidy$sameOrOppSex <- as.factor(stDataTidy$sameOrOppSex)

stResourceSameOpp <- lmer(scale(resources) ~ sex*sameOrOppSex + (1|PIN),
                          data = stDataTidy)

#plot interaction
resourceStIntPlot <- plot_model(stResourceSameOpp, type = "pred", terms = c("sex", "sameOrOppSex"))

resourceStIntPlot2 <- ggplot(stDataTidy, aes(x = sex, y = resources, fill = sameOrOppSex))+
  stat_summary(fun = "mean", geom = "bar", position = position_dodge(), color = "black") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", position = position_dodge(width = 0.9),
               width = 0.25) +
  scale_fill_manual(
    values = c("0" = "#9DC183", "1" = "#FFBF00"), 
    labels = c("Same Sex", "Opposite Sex")) +
  scale_x_discrete(labels = c("0" = "Male", "1" = "Female")) +
  labs(x = "Participant Sex", y = "Resource Preference", 
       fill = "Target Sex (Same Or Opposite Participant)") +
  theme_minimal()

resourceStIntPlot3 <- ggplot(stDataTidy, aes(x = sex, y = resources, fill = sameOrOppSex))+
  geom_violin(position = position_dodge(width = 0.9), color = "black", alpha = 0.7) + 
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.2, color = "black", outlier.shape = NA) + 
  scale_fill_manual(values = c("0" = "#9DC183", "1" = "#FFBF00"), 
                    labels = c("Same Sex", "Opposite Sex")) +
  scale_x_discrete(labels = c("0" = "Male", "1" = "Female")) +
  labs(x = "Participant Sex", y = "Resource Preference", 
       fill = "Target Sex (Same Or Opposite Participant)") +
  theme_minimal()

resourceStIntPlot4 <- ggplot(stDataTidy, aes(x = sex, y = resources, fill = sameOrOppSex))+
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.5, color = "black", outlier.shape = NA) + 
  scale_fill_manual(values = c("0" = "#9DC183", "1" = "#FFBF00"), 
                    labels = c("Same Sex", "Opposite Sex")) +
  scale_x_discrete(labels = c("0" = "Male", "1" = "Female")) +
  labs(x = "Participant Sex", y = "Resource Preference", 
       fill = "Target Sex (Same Or Opposite Participant)") +
  theme_minimal()



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



########Generating descriptives of long term trait pref values
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



###Supplemental Materials

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

ltHealthMainBi <- lmer(scale(health)  ~ sex+partnerSex + (1|PIN),
                     data = ltDataBiTidy) #sig main effect of sex (different)
#kindness 

ltKindMainBi <- lmer(scale(kind)  ~ sex+partnerSex + (1|PIN),
                   data = ltDataBiTidy) #sig main of sex and psex (same)

#physical attractiveness

ltPhysattMainBi <- lmer(scale(physatt)  ~ sex+partnerSex + (1|PIN),
                      data = ltDataBiTidy) #sig main effect of sex (same)

#intell

ltIntellMainBi <- lmer(scale(intell)  ~ sex+partnerSex + (1|PIN),
                     data = ltDataBiTidy) #sig effect of psex (same)

#resources

ltResourceMainBi <- lmer(scale(resources)  ~ sex+partnerSex + (1|PIN),
                       data = ltDataBiTidy)  #sig main effect of psex, not sex (same)

#ideal age (NOT STANDARDIZED)

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

stPhysattMainBi <- lmer(scale(physatt) ~ sex+partnerSex + (1|PIN), 
                      data = stDataBiTidy) #only sig main effect of partner sex (same)


#health

stHealthMainBi <- lmer(scale(health)  ~ sex+partnerSex + (1|PIN),
                     data = stDataBiTidy) #sig main effect of partner sex (same)


#intelligence

stIntellMainBi <- lmer(scale(intell)  ~ sex+partnerSex + (1|PIN),
                     data = stDataBiTidy) #nothing significant (different)

#resources

stResourceMainBi <- lmer(scale(resources)  ~ sex+partnerSex + (1|PIN),
                       data = stDataBiTidy) #sig effect of partner sex (same)

#age (NOT STANDARDIZED)


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





##exploring a potential main effect of relationship status

#create dataframe with rel status variable
#longform data

#LT 
ltDataRelStat <- data[,c(167, 5, 2, 178:187, 227, 229)]
ltDataRelStat <- melt(ltDataRelStat, id.vars=c("PIN", "sex", "rel_status"))
ltDataRelStat <- ltDataRelStat %>% 
  separate("variable", into = c("partnerSex", "x", "trait"), remove = T)
ltDataRelStat$partnerSex <- ifelse(ltDataRelStat$partnerSex == "m", 1, 0)
ltDataRelStat <- ltDataRelStat[,c(1:4, 6:7)]
ltDataRelStat$sex  <- as.factor(ltDataRelStat$sex)
ltDataRelStat$partnerSex  <- as.factor(ltDataRelStat$partnerSex)
ltDataRelStat$rel_status  <- as.factor(ltDataRelStat$rel_status)

#make sure there are no NAs in sex or partner sex columns
colNum<- c(2,4)
nacheckLtRelStat <- apply(ltDataRelStat[,colNum], 1, function(x) sum(is.na(x))>0)
ltDataRelStat<- ltDataRelStat[!nacheckLtRelStat,]


#omnibus

ltOmnibusRelStat <- lmer(value ~ trait*rel_status + trait*sex + partnerSex + (1|PIN), 
                  data = ltDataRelStat) 


#exploring effect trait*trait
#tidy format for analyses
ltDataRelStatTidy <- ltDataRelStat %>%
  pivot_wider(names_from = trait, 
              values_from = value)

#standardizing outcome variable (traits)

#health 
ltHealthRelStat <- lmer(scale(health)  ~ sex + partnerSex + rel_status + (1|PIN),
                      data = ltDataRelStatTidy)

#kindness 


ltKindRelStat <- lmer(scale(kind)  ~ sex + partnerSex + rel_status + (1|PIN),
                     data = ltDataRelStatTidy)

#physical attractiveness

ltPhysattRelStat <- lmer(scale(physatt)  ~ sex + partnerSex + rel_status + (1|PIN),
                        data = ltDataRelStatTidy)

#intell

ltIntellRelStat <- lmer(scale(intell) ~ sex + partnerSex + rel_status + (1|PIN),
                        data = ltDataRelStatTidy)

#resources

ltResourceRelStat <- lmer(scale(resources)  ~ sex + partnerSex + rel_status + (1|PIN),
                         data = ltDataRelStatTidy)

#ideal age (NOT STANDARDIZED)
ltAgeRelStat <- lmer(AgeLik ~ sex + partnerSex + rel_status + (1|PIN),
                    data = ltDataRelStatTidy)


