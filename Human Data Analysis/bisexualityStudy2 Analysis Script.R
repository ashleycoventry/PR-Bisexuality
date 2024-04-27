#####De-Confounding Sex and Sex of Partner in Mate Preference Research#####
################Study 2 Analysis Script##############

###load packages###
library(lmerTest)
library(reshape2) #to reshape data
library(tidyverse) #for %>% among other things
library(ggplot2)

### set seed ###
set.seed(040524)


#load data
data<-read.csv(file.choose())



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


##only using people's opposite sex ratings (so het people + bi opp sex)
ltDataCombo <- ltData[ltData$sex != ltData$partnerSex, ] #this makes it so we only keep rows where participnt sex is different than partner sex

omnibusCombo <- lmer(value ~ trait*sex + sexuality + (1|PIN), 
                 data = ltDataCombo) #started with 3 way interaction of trait, sex, sexuality, and then reduced from there



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


#kindness 

ltKindBi <- lmer(scale(kind)  ~ sex+partnerSex + (1|PIN),
                   data = ltDataBiTidy) #no sig main effects (different than study1)


#physical attractiveness

ltPhysattBi <- lmer(scale(physatt)  ~ sex+partnerSex + (1|PIN),
                      data = ltDataBiTidy) #sig main effect of sex



#intell

ltIntellBi <- lmer(scale(intell)  ~ sex+partnerSex + (1|PIN),
                     data = ltDataBiTidy) #no sig effects (diff to study 1)



#resources

ltResourceBi <- lmer(scale(resources)  ~ sex+partnerSex + (1|PIN),
                       data = ltDataBiTidy)  #sig main effect of psex (same as study 1)




#ideal age (NOT STANDARDIZED)

ltAgeBi <- lmer(AgeLik ~ sex+partnerSex + (1|PIN), 
                  data = ltDataBiTidy) #sig main effect of sex and partner sex




##plotting

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

##facted mirror density plot
#faceted by group: 
  #bi male participant/male target = 0
  #bi male participant/female target = 1
  #bi female participant/male target = 2
  #bi female participant/female target = 3
  #het male participant/female target = 4
  #het female participant/male target = 5


#create group variable
#(0 = male participant/male target, 1 = male/female, 2 = female/male, 3 = female/female)
ltData$group <- ifelse(ltData$sexuality == 0 & ltData$sex == 1 & ltData$partnerSex == 1, 0, 
                       ifelse(ltData$sexuality == 0 & ltData$sex == 1 & ltData$partnerSex == 0, 1, 
                              ifelse(ltData$sexuality == 0 & ltData$sex == 0 & ltData$partnerSex == 1, 2,
                                     ifelse(ltData$sexuality == 0 & ltData$sex == 0 & ltData$partnerSex == 0, 3, 
                                            ifelse(ltData$sexuality == 1 & ltData$sex == 1, 4,
                                                   ifelse(ltData$sexuality == 1 & ltData$sex == 0, 5, NA))))))


ltData$group <- as.factor(ltData$group)


#make plot

ltMirrorPlot <- ggplot(ltData, aes(x = value, fill = group)) +
  #top
  geom_density(aes(y = after_stat(density)),
               data = ltData[ltData$group %in% c("1", "3", "4"),], alpha = 0.6) + #female targets
  #bottom
  geom_density(aes(y = -after_stat(density)),
               data = ltData[ltData$group %in% c("0", "2", "5"),], alpha = 0.6) + #male targets
  facet_wrap(~trait, ncol = 3, scales = "free", labeller = labellerFacet)+
  scale_fill_manual(values = c("1" = "darkblue", "3" = "orangered", "4" = "forestgreen", "0" = "lightblue", "2" ="orange", "5" = "#9ACD32"), 
                    labels = c("Bi Male Participant/Female Target", "Bi Female Participant/Female Target","Het Male Participant/Female Target", 
                               "Bi Male Participant/Male Target",  "Bi Female Participant/Male Target", "Het Female Participant/Male Target"),
                      breaks = c("1", "3", "4", "0", "2", "5")) +
  labs(x = "Preference Value", y = "Density", fill = "Participant Sex/Target Sex") +
  theme_grey(base_size = 20) +
  theme(
    axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
    axis.title.y = element_text(hjust = 0.5)) +
  ggtitle("Preferences for Ideal Partners")+
  geom_hline(yintercept = 0, color = "black", linetype = "solid")+
  geom_text(aes(x = .75, y = Inf, label = "Female \n Partners"),
            hjust = 0, vjust = 2.5, color = "black", size = 5) +
  geom_text(aes(x = .75, y = -Inf, label = "Male \n Partners"),
            hjust = 0, vjust = -.5, color = "black", size = 5) 



#ggsave("densityLt.jpeg", plot=last_plot(), width=400, height=450, units="mm", path ="/Users/ashle/Desktop", scale = 1, dpi=300, limitsize=TRUE)


#plotting trait levels for each trait faceted by sexuality (0 = bisexual, 1 = heterosexual)
sexualityTraitPlot <- ggplot(data = ltData, aes(x = trait, y = value, fill = sex)) +
  stat_summary(fun = "mean", geom = "bar", position = position_dodge(), color = "black") +
  facet_wrap(~sexuality, labeller = as_labeller(c("0" = "Bisexual Participants", "1" = "Heterosexual Participants")))+
  xlab("Trait") +
  ylab("Trait Value")+
  scale_x_discrete(labels = c("Age", "Health", "Intelligence", "Kindness", "Phys. Att.", "Resources"))+
  labs(fill = "Sex") +
  scale_fill_manual(values = c("0" = "maroon", "1" = "blue"), 
                    labels = c("Female", "Male"))


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



