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



