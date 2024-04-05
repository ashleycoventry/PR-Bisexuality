#####Bisexuality Study 2 Processing Script#####
####### Ashley Coventry, Selina Mixner, Katy Walter, Ben Gelbart, Tamsin German, & Dan Conroy-Beam ########
##NOTE: same processing script as General Project Rainbow, just with non-bisexuals excluded


##load data
data<-read.csv(file.choose())


##eliminate title rows 
data<-data[-c(1:2),]

##eliminate those who failed attention check
data<-data[data$attn_check=="1",]

##create a Participant Identification Number (PIN) for each participant
data$PIN<-paste0(sample(1:nrow(data)))


##label some participants as Bi and some as Het

#pref_check_1 = lt attraction to men, pref_check_2 = lt attraction to women
#pref_check_3 = st attraction to men, pref_check_4 = st attraction to women
#sex_orient_best = 4 is heterosexual, rom_orient_best = 3 is hetero-romantic

data$sexuality <- ifelse(data$pref_check_1 == 1 & data$pref_check_2 == 1, 0,
                         ifelse(data$pref_check_1 == 1 & data$pref_check_2 == 2, 1, 
                                ifelse(data$pref_check_1 == 2 & data$pref_check_2 ==1, 1, 
                                       2)))
                         #so in data$sexuality, 0 = bisexual, 1 = heterosexual, 2 = other


#change preference rating variables to numeric
data[,29:50]<-as.numeric(unlist(data[,29:50]))

### compute mate preference variables ###

### change preferences from 1-11 to 0-10 scale
#subtract 1 from all columns
data[,29:50]<-apply(data[,29:50],2,function(x) x-1)


#mate preference composite measures

#Create a dataframe of just preference ratings (not age)
ratings<-data[,c(29:38, 40:49)]

#Compute composite ratings
comps<-sapply(seq(1,20,2),function(x) rowMeans(ratings[,x:(x+1)]))
#Generate composite names
compnames<-colnames(ratings)[seq(1,20,2)]
compnames<-gsub("1","",compnames)
colnames(comps)<-compnames

#Cbind composite ratings into data
data<-cbind(data,comps)


###Save Data###

#Determine the date and time
date<-format(Sys.time(),format="%Y%m%d %H%M%S")


format<-".csv"
date<-format(Sys.time(),format="%m%d%Y %H%M%S")
file<-file.path(paste0("/Users/ashle/Desktop/Research/Project Rainbow/PR-Bisexuality.nosync/PR-Bisexuality.nosync/Human Data/Processed Data/","/BisexualityStudy2_DataPROCESSED ",date,format))

write.csv(data,file=file,row.names=F)

