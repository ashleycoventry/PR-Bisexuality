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


##age conversion (ideal age -> ideal age diff)
  #data$AGELIK <- ifelse(data$age) >= 75, 10, 
    #ifelse......
  #data$partnerage - data$agelik



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
