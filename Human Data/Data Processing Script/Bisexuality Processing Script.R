#####Project Rainbow Processing Script -- Bisexual Preferences#####
      ##NOTE: same processing script as General Project Rainbow, just with non-bisexuals excluded
### load packages ###
library(car)
library(psych)




### set seed ###
set.seed(11062022)


### load data ###
#will have two datasets from Qualtrics and Prolific
qdata<-read.csv(file.choose())
pdata<-read.csv(file.choose())



#eliminate title rows 
qdata<-qdata[-c(1:2),]
pdata<-pdata[-c(1:2),]


#keep only participants who passed the attention check
qdata<-qdata[qdata$attn_check=="1",]
pdata<-pdata[pdata$attn_check=="1",]

#column order was changed due to clerical issues
#need to reorder columns so that they are in right order for indexing 
qdata<-qdata[,c(1:63,68:95,166:185,186:380,140:143,144:152,153:165,64,129:139,65,118:128,67,96:106,66,107:117,381)]
pdata<-pdata[,c(1:63,68:95,166:185,186:380,140:143,144:152,153:165,64,129:139,65,118:128,67,96:106,66,107:117,381)]

#create a Participant Identification Number (PIN) for each participant
#assign systematically different IDs to Qualtrics vs. Prolific participants
qdata$PIN<-paste0("q",sample(1:nrow(qdata)))
pdata$PIN<-paste0("p",sample(1:nrow(pdata)))


#keep only columns needed for analyses
qdata<-qdata[,c(18:28,30:38,40:111,307:380,382)]
pdata<-pdata[,c(18:28,30:38,40:111,307:380,382)]

###only keep bi participants
#create separate variables for bisexual identity (LT and ST) so we can separate out for analyses
qdata$bisexualLT <- ifelse(qdata$pref_check_1 == 1 & qdata$pref_check_2 == 1, 1, 0)
qdata$bisexualST <- ifelse(qdata$pref_check_3 == 1 & qdata$pref_check_4 == 1, 1, 0)

pdata$bisexualLT <- ifelse(pdata$pref_check_1 == 1 & pdata$pref_check_2 == 1, 1, 0)
pdata$bisexualST <- ifelse(pdata$pref_check_3 == 1 & pdata$pref_check_4 == 1, 1, 0)
#eliminate ppl with 0s in both columns (so not attracted LT/ST to both sexes)
qdata <- subset(qdata, qdata$bisexualLT == 1 | qdata$bisexualST == 1)
pdata <- subset(pdata, pdata$bisexualLT == 1 | pdata$bisexualST == 1)

#merge datasets
data<-rbind(qdata,pdata)


#change column names
colnames(data)[1:118]<-c("age","rel_status","gender","gender_text","sex","transgender","transgender_text","masc_feel","fem_feel","masc_present","fem_present","gender_transition",
                         "hormones","hormones_text","surgeries","surgeries_text","sex_orient","sex_orient_text",
                         "rom_orient","rom_orient_text","sex_orient_best","sex_orient_best_text","asexual","bisexual","demisexual","gay","hetero","lesbian","omnisexual","pansexual","queer","other_sexuality", "other_sexuality_text",
                         "rom_orient_best","rom_orient_best_text","arom","birom","demirom","heterorom","homorom","omnirom","panrom","other_rom","other_rom_text","sexual_role","race_asian","race_black","race_hispanic",
                         "race_nativeam","race_mideast","race_white","race_na","rel_protestant","rel_catholic","rel_mormon","rel_jeworthodox","rel_jewconserve","rel_jewreform","rel_muslim",
                         "rel_buddhist","rel_hindu","rel_athiest","rel_agnostic","rel_spirit","rel_na","height_feet","height_inches","education","pol_orient",
                         "pol_party","sex_with_males","sex_with_females","sexatt_ident_woman","sexatt_ident_man","sexatt_ident_nonbinary","sexatt_pres_feminine","sexatt_pres_masculine",
                         "sexatt_pres_neither","sexatt_pres_both","sexatt_sex_female","sexatt_sex_male","sexatt_sex_intersex","romatt_ident_woman",
                         "romatt_ident_man","romatt_ident_nonbinary","romatt_pres_feminine","romatt_pres_masculine",
                         "romatt_pres_neither","romatt_pres_both","romatt_sex_female","romatt_sex_male","romatt_sex_intersex",
                         "jealous_imagined_1","jealous_imagined_2","jealous_imagined_3","jealous_imagined_4","soi_partnernum","soi_oneocc",
                         "soi_short_interest","soi_nolove","soi_casualsex","soi_longterm","soi_fantasies_extrapair","soi_arousal","soi_fantasies_stranger",
                         "variety_partnum_5yrs","variety_partnum_1yr","variety_partnum_month","sex_5yrs","sex_2yrs","sex_1yr","sex_6months","sex_3months",
                         "sex_1month","sex_1week","sex_1day","sex_1evening","sex_1hour")


#due to clerical issue, change in name of pref check columns
#rename pref check columns to match pre-reg
colnames(data)[c(119,131,143,155)]<-c("m_lt_check","f_lt_check","f_st_check","m_st_check")

#change variables to numeric
data[,c(1:3,5:6,8:13,15,17,19,21,23:32,34,36:43,45:166)]<-as.numeric(unlist(data[,c(1:3,5:6,8:13,15,17,19,21,23:32,34,36:43,45:166)]))



### remove non-responders ###

#check and only keep participants that filled out at least 80% of all mating measures 
#measures are: st-prefs, lt-prefs, soi, sv_time, sv_partnum, jealousy
#participants filled out different mate preference surveys depending on their orientations
#those who said yes to female long-term
#those who said yes to male long-term
#those who said yes to female short-term 
#those who said yes to male short-term

# find proportion of NAs to female LT preferences
data$fltprop <- rowMeans(is.na(data[,132:142])) 
# find proportion of NAs to female ST preferences
data$fstprop <- rowMeans(is.na(data[,144:154])) 
# find proportion of NAs to male LT preferences
data$mltprop <- rowMeans(is.na(data[,120:130]))
# find proportion of NAs to male ST preferences
data$mstprop <- rowMeans(is.na(data[,156:166])) 
#find proportion of NAs to all other measures (SOI, sv_time, sv_partnum,jealousy)
data$matingprop <- rowMeans(is.na(data[,93:118])) 

#change to NAs if didn't fill out this section (e.g. said were not interested in female long-term partners)
data$fltprop<-ifelse(data$f_lt_check==1,data$fltprop,NA)
data$mltprop<-ifelse(data$m_lt_check==1,data$mltprop,NA)
data$fstprop<-ifelse(data$f_st_check==1,data$fstprop,NA)
data$mstprop<-ifelse(data$m_st_check==1,data$mstprop,NA)

#get mean of proportion of preference NAs for each person
data$prefNaCheck<-rowMeans(data[,168:171],na.rm=T)

#figure out total proportion of NAs for each person for all mating measures
#some people have NaNs because they did not indicate that they were attracted to men or women, for short-term or long-term
data$NAcheck<- (data$matingprop * ncol(data[,93:118]) + data$prefNaCheck*11*rowSums(data[,168:171],na.rm=T))/((ncol(data[,93:118])+11*rowSums(data[,168:171],na.rm=T)))

#remove participants from data who have greater than .20 proportion of NAs
data<-data[data$NAcheck<.2,]
#also remove NaN participants
data<-subset(data, (!is.na(data$NAcheck)))






### create composite SOI variable ###

#first recode reverse scored item asking about interest in long-term
data$soi_longterm<-recode(data$soi_longterm,'1=9;2=8;3=7;4=6;5=5;6=4;7=3;8=2;9=1')
#create composite SOI variable by taking the mean of all soi responses, ignore missing values
data$soi_comp<-rowMeans(data[,97:105],na.rm=T) 






### compute mate preference variables ###

### change preferences from 1-11 to 0-10 scale
#subtract 1 from all columns
data[,c(120:130,132:142,144:154,156:166)]<-apply(data[,c(120:130,132:142,144:154,156:166)],2,function(x) x-1)


#mate preference composite measures
#Create a dataframe of just preference ratings
ratings<-data[,c(120:129,132:141,144:153,156:165)]
#Compute composite ratings
comps<-sapply(seq(1,40,2),function(x) rowMeans(ratings[,x:(x+1)]))
#Generate composite names
compnames<-colnames(ratings)[seq(1,40,2)]
compnames<-gsub("1","",compnames)
colnames(comps)<-compnames
#Cbind composite ratings into data
data<-cbind(data,comps)






### calculate overall sexual jealousy score ###

#overall jealousy score is the sum of the responses to the four scenarios
#higher scores are associated with greater jealousy for sexual infidelity
data$jealousy_comp<-rowSums(data[,93:96],na.rm=T) 





### Creating Preference Sets ###

#we only will analyze one set of prefs at a time (one long-term and one short-term)
#but those attracted to more than one gender have 2 sets of preferences
#one where those attracted to more than one gender have same sex prefs
#one where those attracted to more than one gender have opposite sex prefs
#will have two sets of preferences (opp sex and same sex prefs for multisexuals)

#determine everyone's first set of prefs (one long-term and one short-term) 
#same sex prefs

#Set 1#

#create long-term prefs for same sex
#intersex people - male prefs for set 1
#health
data$ssp_lt_health<-ifelse(data$sex==0,data$f_lt_health,
                                ifelse(data$sex==1,data$m_lt_health,
                                       ifelse(data$sex==2,data$m_lt_health,NA)))
#kindness
data$ssp_lt_kind<-ifelse(data$sex==0,data$f_lt_kind,
                              ifelse(data$sex==1,data$m_lt_kind,
                                     ifelse(data$sex==2,data$m_lt_kind,NA)))
#intelligence
data$ssp_lt_intell<-ifelse(data$sex==0,data$f_lt_intell,
                                ifelse(data$sex==1,data$m_lt_intell,
                                       ifelse(data$sex==2,data$m_lt_intell,NA)))
#resources
data$ssp_lt_resources<-ifelse(data$sex==0,data$f_lt_resources,
                                   ifelse(data$sex==1,data$m_lt_resources,
                                          ifelse(data$sex==2,data$m_lt_resources,NA)))
#physical attractiveness
data$ssp_lt_physatt<-ifelse(data$sex==0,data$f_lt_physatt,
                                 ifelse(data$sex==1,data$m_lt_physatt,
                                        ifelse(data$sex==2,data$m_lt_physatt,NA)))
#age
data$ssp_lt_age<-ifelse(data$sex==0,data$f_lt_age,
                             ifelse(data$sex==1,data$m_lt_age,
                                    ifelse(data$sex==2,data$m_lt_age,NA)))



#create short-term prefs for same sex
#intersex people - male prefs for set 1
#health
data$ssp_st_health<-ifelse(data$sex==0,data$f_st_health,
                                ifelse(data$sex==1,data$m_st_health,
                                       ifelse(data$sex==2,data$m_st_health,NA)))
#kindness
data$ssp_st_kind<-ifelse(data$sex==0,data$f_st_kind,
                              ifelse(data$sex==1,data$m_st_kind,
                                     ifelse(data$sex==2,data$m_st_kind,NA)))
#intelligence
data$ssp_st_intell<-ifelse(data$sex==0,data$f_st_intell,
                                ifelse(data$sex==1,data$m_st_intell,
                                       ifelse(data$sex==2,data$m_st_intell,NA)))
#resources
data$ssp_st_resources<-ifelse(data$sex==0,data$f_st_resources,
                                   ifelse(data$sex==1,data$m_st_resources,
                                          ifelse(data$sex==2,data$m_st_resources,NA)))
#physical attractiveness
data$ssp_st_physatt<-ifelse(data$sex==0,data$f_st_physatt,
                                 ifelse(data$sex==1,data$m_st_physatt,
                                        ifelse(data$sex==2,data$m_st_physatt,NA)))
#age
data$ssp_st_age<-ifelse(data$sex==0,data$f_st_age,
                             ifelse(data$sex==1,data$m_st_age,
                                    ifelse(data$sex==2,data$m_st_age,NA)))






#Set 2 for Multisexuals#

#create long-term prefs for opposite sex
#intersex people - female prefs for set 2
#health
data$osp_lt_health<-ifelse(data$sex==0,data$m_lt_health,
                                ifelse(data$sex==1,data$f_lt_health,
                                       ifelse(data$sex==2,data$f_lt_health,NA)))
#kindness
data$osp_lt_kind<-ifelse(data$sex==0,data$m_lt_kind,
                              ifelse(data$sex==1,data$f_lt_kind,
                                     ifelse(data$sex==2,data$f_lt_kind,NA)))
#intelligence
data$osp_lt_intell<-ifelse(data$sex==0,data$m_lt_intell,
                                ifelse(data$sex==1,data$f_lt_intell,
                                       ifelse(data$sex==2,data$f_lt_intell,NA)))
#resources
data$osp_lt_resources<-ifelse(data$sex==0,data$m_lt_resources,
                                   ifelse(data$sex==1,data$f_lt_resources,
                                          ifelse(data$sex==2,data$f_lt_resources,NA)))
#physical attractiveness
data$osp_lt_physatt<-ifelse(data$sex==0,data$m_lt_physatt,
                                 ifelse(data$sex==1,data$f_lt_physatt,
                                        ifelse(data$sex==2,data$f_lt_physatt,NA)))
#age
data$osp_lt_age<-ifelse(data$sex==0,data$m_lt_age,
                             ifelse(data$sex==1,data$f_lt_age,
                                    ifelse(data$sex==2,data$f_lt_age,NA)))




#create short-term prefs for opposite sex
#intersex people - female prefs for set 2
#health
data$osp_st_health<-ifelse(data$sex==0,data$m_st_health,
                                ifelse(data$sex==1,data$f_st_health,
                                       ifelse(data$sex==2,data$f_st_health,NA)))
#kindness
data$osp_st_kind<-ifelse(data$sex==0,data$m_st_kind,
                              ifelse(data$sex==1,data$f_st_kind,
                                     ifelse(data$sex==2,data$f_st_kind,NA)))
#intelligence
data$osp_st_intell<-ifelse(data$sex==0,data$m_st_intell,
                                ifelse(data$sex==1,data$f_st_intell,
                                       ifelse(data$sex==2,data$f_st_intell,NA)))
#resources
data$osp_st_resources<-ifelse(data$sex==0,data$m_st_resources,
                                   ifelse(data$sex==1,data$f_st_resources,
                                          ifelse(data$sex==2,data$f_st_resources,NA)))
#physical attractiveness
data$osp_st_physatt<-ifelse(data$sex==0,data$m_st_physatt,
                                 ifelse(data$sex==1,data$f_st_physatt,
                                        ifelse(data$sex==2,data$f_st_physatt,NA)))
#age
data$osp_st_age<-ifelse(data$sex==0,data$m_st_age,
                             ifelse(data$sex==1,data$f_st_age,
                                    ifelse(data$sex==2,data$f_st_age,NA)))


### clean desired partner number ###

#calculate the 95th percentile and then truncate there
#find 95th percentile
outputquantile1<-quantile(data$variety_partnum_5yrs, seq(0,1,by=0.05),na.rm=T)
outputquantile2<-quantile(data$variety_partnum_1yr, seq(0,1,by=0.05),na.rm=T)
outputquantile3<-quantile(data$variety_partnum_month, seq(0,1,by=0.05),na.rm=T)

#truncate at 95th percentile 
data[,106]<-ifelse(data[,106]>outputquantile1[[20]],outputquantile1[[20]],data[,106])
data[,107]<-ifelse(data[,107]>outputquantile2[[20]],outputquantile2[[20]],data[,107])
data[,108]<-ifelse(data[,108]>outputquantile3[[20]],outputquantile3[[20]],data[,108])

#create scree plot to find number of components
partnumtimecor <- cor(data[,106:108], use = "pairwise.complete.obs") 
screepartnumtimecor<-scree(partnumtimecor, factors = FALSE) 

#do a PCA to get a smaller number of PCs, based on scree, retain x components
partnumpca<-pca(data[,106:108],nfactors=1,missing=T)
partnumpcascores<-data.frame(partnumpca$scores)

#save new component(s) to data
data$partnum_pca<-partnumpcascores$PC1






### clean likelihood of sex ###

#create scree plot to find number of components
timecor <- cor(data[,109:118], use = "pairwise.complete.obs") 
screetimecor<-scree(timecor, factors = FALSE) 

#do a PCA to get a smaller number of PCs, based on scree, retain x components
timepca<-pca(data[,109:118],nfactors=2,missing=T)
timepcascores<-data.frame(timepca$scores)

#save new components to data
data$sex_time_pca1<-timepcascores$RC1
data$sex_time_pca2<-timepcascores$RC2


#change columns to factors
data$gender<-as.factor(data$gender)
data$sex<-as.factor(data$sex)




#basic data check
#demographics and frequency tables
#age
meanage<-mean(as.numeric(data$age),na.rm=T)
agerange<-range(as.numeric(data$age),na.rm=T)
mdage<-median(as.numeric(data$age),na.rm=T)
#sex
tablesex<-table(data$sex)
#gender
tablegender<-table(data$gender)


###Save Data###

#Determine the date and time
date<-format(Sys.time(),format="%Y%m%d %H%M%S")

#Save the data

write.csv(data,paste0("/Users/ashle/Desktop/Research/Project Rainbow/PR-Bisexuality.nosync/Human Data/Processed Data/",date,".csv"), row.names = FALSE)




format<-".csv"
date<-format(Sys.time(),format="%m%d%Y %H%M%S")
file<-file.path(paste0("/Users/ashle/Desktop/Research/Project Rainbow/PR-Bisexuality.nosync/Human Data/Processed Data/","/PR Bisexuality Data PROCESSED ",date,format))

write.csv(data,file=file,row.names=F)



