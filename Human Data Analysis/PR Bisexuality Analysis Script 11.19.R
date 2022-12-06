#####Project Rainbow Analysis Script -- Bisexual Preferences#####
####### Ashley Coventry, Katy Walter, Ben Gelbart, Tamsin German, & Dan Conroy-Beam ########

###load packages###



### set seed ###
set.seed(11182022)




### load data ###
data<-read.csv(file.choose())
    #only bisexual people remain after processing


### Standardize preferences ###
  #phys att; kind; intel; health; resources
  #convert age to age diff
  #how do i standardize these?
  #what does it mean to standardize these lol
  #zscore, scale()
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
data$m_lt_age_lik <- data$m_lt_age - data$agelik

#male ideal ST
data$m_st_age_lik <- data$m_st_age - data$agelik

#female ideal LT
data$f_lt_age_lik <- data$f_lt_age - data$agelik

#female ideal ST
data$f_st_age_lik <- data$f_st_age - data$agelik


### LT prefs multilevel model ###
  #looking to see if there is a significant overall effect 
  #of trait + own sex + partner sex on long term prefs
  #nested under participant ID (PIN)







### ST prefs (combined) multilevel model ###







### LT prefs (each trait) multilevel models ###







### ST prefs (each trait) multilevel models ###







### main effects (what test?) ###
  #do if not interaction found in 1 or 2
  #outcome variable: trait pref (do separate for LT vs ST)
  #predictor variables: own sex, partner sex
  #nested under participant ID (PIN)





### violin plots (LT and ST)
  #pref rating on y axis
  #own sex on x axis (male v female)
  #plot 2 violins for each own sex (partner sex -- male or female)









###Cluster analysis
  #across all traits
  #men rating women; men rating men; women rating men; women rating women
  #ipsatize prefs
