
dir <- "/Users/Jeylan/Documents/Migration-Project/"
setwd(dir)

#reading in table

data <- read.csv("turkish2013_2.csv", header=T, as.is=F, na.strings=c("","NA"))

#install.packages("devtools")
#devtools::install_github("ropensci/rdhs")

#import libraries
library(corrplot)
library(ggplot2)
library(dplyr)
library(eha)
library(survival)
library(ggpubr)
library(ranger)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rms)
library(survminer)
library(survMisc)
library(ggplot2)
#Description of Data Set
#Available online: https://dhsprogram.com/What-We-Do/survey-search.cfm?pgtype=main&SrvyTp=country
#The Turkish DHS is a nationally representative survey administered by Hacettepe University Institute of 
#Population Studies (HUIPS). A weighted, multistage, stratified cluster sampling technique is used to 
#select households. All eligible women in the household—those between the ages of 15 and 49 who usually 
#reside in the household and were present the night prior to the interview—are interviewed. 
#Detailed demographic, social and economic characteristics are collected  Turkey is one of the 
#few international surveys that also collects migration histories. 
#The aim of this study is to understand the predictors of women's fertility transitions based on available features.

###### caseid   "case identification"
## v000     "country code and phase"
## v001     "cluster number"
## v002     "household number"
## v003     "respondent's line number"
## v004     "ultimate area unit"
## v005     "women's individual sample weight (6 decimals)"
## v006     "month of interview"
## v007     "year of interview"
## v008     "date of interview (cmc)"
## v009     "respondent's month of birth"
## v010     "respondent's year of birth"
## v011     "date of birth (cmc)"
## v012     "respondent's current age"
## v013     "age in 5-year groups"  
## v014     "completeness of age information"  
## v015     "result of individual interview"  
## v016     "day of interview"
## v017     "cmc start of calendar"
## v018     "row of month of interview"
## v019     "length of calendar"
## v019a    "number of calendar columns"
## v020     "ever-married sample"
## v021     "primary sampling unit"
## v022     "sample strata for sampling errors"
## v023     "stratification used in sample design"
## v024     "region"
## v025     "type of place of residence"
## v026     "na - de facto place of residence"
## v027     "number of visits"
## v028     "interviewer identification"
## v029      "na - keyer identification"
## v030      "field supervisor"
## seduc     "highest education"
## v501      "marital status"
## v508      "first marriage year"
## v133      "years of schooling"
## s107      "ever attended school"
## 1130c     "childhood region of residence"
## s130d     "childhood province of residence"
## s130e     "ever changed childhood place of residence"
## s130f     "total number of migrations"
# s130jy_01  "year of first migration
# s130k_01   "reason of first migration"
# s130h_01   "province of residence"
# s130h_02   "second province of residence"
# s130g_01   "place of residence"
# s130g_02   "second place of residence"
# v201      "Total number of births"
#######     b2_01-b2_20  "child1-child20"




#DATA WRANGLING

str(data)
#Rename column headings 
####
dim(data)

migdata=data[c(1:34, 3255, 3264:3265, 174:193, 454, 2902:2904, 2909, 3279:3285,4248,4272,4152:4153, 4128:4129)]
head(migdata)

#Rename variables
names(migdata) <- c("caseid", "countrycode", "clusternumber", "householdnumber", "resplinenumber", "areaunit", "sampleweight", "intmonth", "intyear", "intdate", "birthmonth", "birthyear", "datebirthcmc", "age", "agegroup", "agecompleteness", "intresult", "dayint", "cmcstartofcalendar", "rowmonthint", "lenghtcalendar", "numbercalendarcolumns", "evermarriedsample", "psu", "samplestrata", "stratification", "region", "typeresidence", "defactoplace", "numbervisits", "intid", "keyerid", "fieldsupervisor", "fieldeditor",  "mothertongue", "tonguemother", "tonguefather", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "b13", "b14", "b15", "b16", "b17", "b18", "b19", "b20", "totalchildren", "maritalstatus", "maritalstatus2", "nrunions" , "yearunion", "birthplace", "birthprovince", "childhoodplace", "childhoodprov", "evermoved", "numbermigrations", "seduc", "firstyearmig", "reasonmig", "provorigin", "provincemig1", "placeorigin", "placemig1")
head(migdata)

str(migdata) 
dim(migdata)  # 9746 observations, 72 variables

sapply(migdata, function(x) {sum(is.na(x))})  #missings in births, marital status, nrunions (due to unmarried), residence variables
summary(migdata)


##Removing observations with missing place of residence information

migdata <- migdata[!(is.na(migdata$childhoodprov)),]  #removing missing childhood province of residence
migdata <- migdata[!(is.na(migdata$childhoodplace)),]   #removing missing childhood place of residence

migdata[is.na(migdata$numbermigrations), "numbermigrations"] <- 0

#Replacing residence variables with missings for those who never moved

migdata$firstyearmig <- ifelse(migdata$evermoved == "no", 9999, as.numeric(as.character(migdata$firstyearmig)))
migdata$reasonmig <- ifelse(migdata$evermoved == "no", 9999, as.character(migdata$reasonmig))
migdata$provorigin <- ifelse(migdata$evermoved == "no", 9999, as.character(migdata$provorigin))
migdata$provincemig1 <- ifelse(migdata$evermoved == "no", 9999, as.character(migdata$provincemig1))
migdata$firstplace <- ifelse(migdata$evermoved == "no", 9999, migdata$firstplace)
migdata$placemig1 <- ifelse(migdata$evermoved == "no", 9999, migdata$placemig1)

migdata[is.na(migdata$b1), "b1"] <- 9999
migdata[is.na(migdata$b2), "b2"] <- 9999
migdata[is.na(migdata$b3), "b3"] <- 9999
migdata[is.na(migdata$b4), "b4"] <- 9999
migdata[is.na(migdata$b5), "b5"] <- 9999
migdata[is.na(migdata$b6), "b6"] <- 9999
migdata[is.na(migdata$b7), "b7"] <- 9999
migdata[is.na(migdata$b8), "b8"] <- 9999
migdata[is.na(migdata$b9), "b9"] <- 9999
migdata[is.na(migdata$b10), "b10"] <- 9999
migdata[is.na(migdata$b11), "b11"] <- 9999
migdata[is.na(migdata$b12), "b12"] <- 9999
migdata[is.na(migdata$b13), "b13"] <- 9999
migdata[is.na(migdata$b14), "b14"] <- 9999
migdata[is.na(migdata$b15), "b15"] <- 9999


migdata <- migdata[!(is.na(migdata$firstyearmig)),]   #removing missings
migdata <- migdata[!(is.na(migdata$provincemig1)),] 
migdata <- migdata[!(is.na(migdata$placemig1)),]   

summary(migdata)
dim(migdata)


# CONSTRUCTING VARIABLES

## Type of migration: based a comparison of childhood place of residence and current place of residence
## 6 characters: rural and urban nonmigrannts, urban to urban, rural to urban, urban to rural and rural to rural

migdata$migrationtype <- ifelse(migdata$evermoved == "yes" & migdata$childhoodplace == "subdistrict/village" & (migdata$placemig1 == 2 | migdata$placemig1 == 3), "rural to urban", 
                                ifelse(migdata$evermoved == "yes" & migdata$childhoodplace == "subdistrict/village" & (migdata$placemig1 == 4), "rural to rural", 
                                       ifelse(migdata$evermoved == "yes" & (migdata$childhoodplace == "district" | migdata$childhoodplace == "province") & (migdata$placemig1 == 2 |migdata$placemig1 == 3), "urban to urban",             
                                              ifelse(migdata$evermoved == "yes" & (migdata$childhoodplace == "district" | migdata$childhoodplace == "province") & (migdata$placemig1 == 4), "urban to rural",  
                                                    ifelse(migdata$evermoved == "no" & migdata$typeresidence=="rural", "rural nonmig",
                                                          ifelse(migdata$evermoved == "no" & migdata$typeresidence=="urban", "urban nonmig", "other"))))))

migdata$migrationtypecurrent <- ifelse(migdata$evermoved == "yes" & migdata$childhoodplace == "subdistrict/village" & migdata$typeresidence=="urban", "rural to urban", 
                                ifelse(migdata$evermoved == "yes" & migdata$childhoodplace == "subdistrict/village" & migdata$typeresidence=="rural", "rural to rural", 
                                       ifelse(migdata$evermoved == "yes" & (migdata$childhoodplace == "district" | migdata$childhoodplace == "province") & migdata$typeresidence=="urban", "urban to urban",             
                                              ifelse(migdata$evermoved == "yes" & (migdata$childhoodplace == "district" | migdata$childhoodplace == "province") & migdata$typeresidence=="rural", "urban to rural",  
                                                     ifelse(migdata$evermoved == "no" & migdata$typeresidence=="rural", "rural nonmig",
                                                            ifelse(migdata$evermoved == "no" & migdata$typeresidence=="urban", "urban nonmig", "other"))))))


migdata$migrationtype <-as.factor(migdata$migrationtype)
migdata$migrationtypecurrent <-as.factor(migdata$migrationtypecurrent)

head(migdata)
tail(migdata)


## Year of first migration

summary(migdata$firstyearmig)

migdata$firstyearmig <- ifelse(migdata$firstyearmig==9999, NA, migdata$firstyearmig)

head(migdata)


## Migrant cohort 
migdata$migcohort <- ifelse(migdata$evermoved == "yes" & migdata$firstyearmig >= 1970 & migdata$firstyearmig<= 1979, "1970-1979", 
                                       ifelse(migdata$evermoved == "yes" & migdata$firstyearmig >= 1980 & migdata$firstyearmig<= 1989, "1980-1989", 
                                              ifelse(migdata$evermoved == "yes" &  migdata$firstyearmig >= 1990 & migdata$firstyearmig<= 1999, "1990-1999",             
                                                     ifelse(migdata$evermoved == "yes" &  migdata$firstyearmig >= 2000 & migdata$firstyearmig<= 2013, "2000-2009", NA))))  

migdata$migcohort <- as.factor(migdata$migcohort)                                                            

## Year of first birth

migdata$firstbirth<- with(migdata, pmin(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15))

# Year of second birth
library(dplyr)

birthdata <-  migdata[c(38:52)]

birthdata$firstbirth<- with(migdata, pmin(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15))

head(birthdata)

birthdata <- birthdata %>% 
  mutate(b1 = ifelse(birthdata$b1 == birthdata$firstbirth, 9999, b1),
         b2 = ifelse(birthdata$b2 == birthdata$firstbirth, 9999, b2),
         b3 = ifelse(birthdata$b3 == birthdata$firstbirth, 9999, b3), 
         b4 = ifelse(birthdata$b4 == birthdata$firstbirth, 9999, b4), 
         b5 = ifelse(birthdata$b5 == birthdata$firstbirth, 9999, b5), 
         b6 = ifelse(birthdata$b6 == birthdata$firstbirth, 9999, b6), 
         b7 = ifelse(birthdata$b7 == birthdata$firstbirth, 9999, b7), 
         b8 = ifelse(birthdata$b8 == birthdata$firstbirth, 9999, b8), 
         b9 = ifelse(birthdata$b9 == birthdata$firstbirth, 9999, b9), 
         b10 = ifelse(birthdata$b10 == birthdata$firstbirth, 9999, b10), 
         b11 = ifelse(birthdata$b11 == birthdata$firstbirth, 9999, b11), 
         b12 = ifelse(birthdata$b12 == birthdata$firstbirth, 9999, b12), 
         b13 = ifelse(birthdata$b13 == birthdata$firstbirth, 9999, b13), 
         b14 = ifelse(birthdata$b14 == birthdata$firstbirth, 9999, b14), 
         b15 = ifelse(birthdata$b15 == birthdata$firstbirth, 9999, b15))

head(birthdata)

birthdata$secondbirth<- with(birthdata, pmin(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15))

migdata$secondbirth<- birthdata$secondbirth

head(migdata)

##Year of third birth

birthdata <- birthdata %>% 
  mutate(b1 = ifelse(birthdata$b1 == birthdata$secondbirth, 9999, b1),
         b2 = ifelse(birthdata$b2 == birthdata$secondbirth, 9999, b2),
         b3 = ifelse(birthdata$b3 == birthdata$secondbirth, 9999, b3), 
         b4 = ifelse(birthdata$b4 == birthdata$secondbirth, 9999, b4), 
         b5 = ifelse(birthdata$b5 == birthdata$secondbirth, 9999, b5), 
         b6 = ifelse(birthdata$b6 == birthdata$secondbirth, 9999, b6), 
         b7 = ifelse(birthdata$b7 == birthdata$secondbirth, 9999, b7), 
         b8 = ifelse(birthdata$b8 == birthdata$secondbirth, 9999, b8), 
         b9 = ifelse(birthdata$b9 == birthdata$secondbirth, 9999, b9), 
         b10 = ifelse(birthdata$b10 == birthdata$secondbirth, 9999, b10), 
         b11 = ifelse(birthdata$b11 == birthdata$secondbirth, 9999, b11), 
         b12 = ifelse(birthdata$b12 == birthdata$secondbirth, 9999, b12), 
         b13 = ifelse(birthdata$b13 == birthdata$secondbirth, 9999, b13), 
         b14 = ifelse(birthdata$b14 == birthdata$secondbirth, 9999, b14), 
         b15 = ifelse(birthdata$b15 == birthdata$secondbirth, 9999, b15))

birthdata$thirdbirth<- with(birthdata, pmin(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15))

migdata$thirdbirth<- with(birthdata, pmin(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15))

head(migdata)


## Birth cohort
summary(migdata$birthyear)

migdata$cohort <- ifelse(migdata$birthyear >= 1960 & migdata$birthyear<=1969, 1, 
                                ifelse(migdata$birthyear >= 1970 & migdata$birthyear<=1979, 2, 
                                       ifelse(migdata$birthyear >= 1980 & migdata$birthyear<=1989, 3,             
                                              ifelse(migdata$birthyear >= 1990 & migdata$birthyear<=1999,  4, 9999))))  
migdata$cohort <- as.numeric(migdata$cohort)                                                      



##Ever married
summary(migdata$maritalstatus)

migdata$evermarried <- ifelse(migdata$maritalstatus == "never in union", 0, 1)
summary(migdata$evermarried)
migdata$evermarried<- ifelse(is.na(migdata$evermarried), 1, as.numeric(migdata$evermarried))
summary(migdata$evermarried)

migdata$evermarried <- as.factor(migdata$evermarried)

#Region of childhood origin
summary(migdata$childhoodprov)

migdata$childregion <- ifelse(migdata$childhoodprov == "aydin" | migdata$childhoodprov == "baliksehir" | migdata$childhoodprov == "bursa"| migdata$childhoodprov == "bitlis"| migdata$childhoodprov == "denizli" | migdata$childhoodprov == "burdur" |  migdata$childhoodprov == "edirne"| migdata$childhoodprov == "istanbul"| migdata$childhoodprov == "izmir"| migdata$childhoodprov == "kirklareli"| migdata$childhoodprov == "kocaeli"| migdata$childhoodprov == "manisa"| migdata$childhoodprov == "mugla"| migdata$childhoodprov == "sakarya"| migdata$childhoodprov == "tekirdag"| migdata$childhoodprov == "yalova"| migdata$childhoodprov == "bilecik" , "west", 
                               ifelse(migdata$childhoodprov == "adana"| migdata$childhoodprov == "antalya"| migdata$childhoodprov == "hatay"| migdata$childhoodprov == "isparta"| migdata$childhoodprov == "icel "| migdata$childhoodprov == "k. maras"| migdata$childhoodprov == "osmaniye", "south",
                                       ifelse(migdata$childhoodprov == "afyon"| migdata$childhoodprov == "amasya"| migdata$childhoodprov == "ankara"| migdata$childhoodprov == "bolu"| migdata$childhoodprov == "cankiri"| migdata$childhoodprov == "corum"| migdata$childhoodprov == "eskisehir"| migdata$childhoodprov == "kayseri"| migdata$childhoodprov == "kirsehir"| migdata$childhoodprov == "konya"| migdata$childhoodprov == "kutahya"| migdata$childhoodprov == "nevsehir"| migdata$childhoodprov == "sivas"| migdata$childhoodprov == "tokat"| migdata$childhoodprov == "usak"| migdata$childhoodprov == "yozgat"| migdata$childhoodprov == "aksaray"| migdata$childhoodprov == "karaman" | migdata$childhoodprov == "kirikkale", "central",
                                           ifelse(migdata$childhoodprov == "artvin"| migdata$childhoodprov == "giresun"| migdata$childhoodprov == "gumushane"| migdata$childhoodprov == "kastamonu"| migdata$childhoodprov == "ordu"| migdata$childhoodprov == "rize"| migdata$childhoodprov == "samsun"| migdata$childhoodprov == "sinop"| migdata$childhoodprov == "trabzon"| migdata$childhoodprov == "bartin"| migdata$childhoodprov == "karabuk"| migdata$childhoodprov == "zonguldak", "north",
                                                  ifelse(migdata$childhoodprov == "adiyaman"| migdata$childhoodprov == "agri"| migdata$childhoodprov == "bing\xe3\xb6l"| migdata$childhoodprov == "canakkale"| migdata$childhoodprov == "diyarbakir"| migdata$childhoodprov == "elazig"| migdata$childhoodprov == "erzincan"| migdata$childhoodprov == "erzurum"| migdata$childhoodprov == "gaziantep"| migdata$childhoodprov == "hakkari"| migdata$childhoodprov == "kars"| migdata$childhoodprov == "malatya"| migdata$childhoodprov == "mardin"| migdata$childhoodprov == "mus"| migdata$childhoodprov == "siirt"| migdata$childhoodprov == "tunceli"| migdata$childhoodprov == "sanliurfa"| migdata$childhoodprov == "van"| migdata$childhoodprov == "bayburt"| migdata$childhoodprov == "batman"| migdata$childhoodprov == "sirnak"| migdata$childhoodprov == "ardahan"| migdata$childhoodprov == "igdir"| migdata$childhoodprov == "kilis", "east", "other")))))

migdata$childregion <- as.factor(migdata$childregion)                                  

#Education  

migdata$seduc <- as.factor(migdata$seduc)


#Age of first marriage 

summary(migdata$yearunion)

migdata$agemarriage<- ifelse(!is.na(migdata$yearunion), migdata$yearunion - migdata$birthyear, "NA")
migdata$agemarriage<- as.numeric(migdata$agemarriage)
summary(migdata$agemarriage)


#Kurdish background 

migdata$kurdish<- ifelse(migdata$mothertongue == "kurdish" | migdata$tonguemother == "kurdish" | migdata$tonguefather == "kurdish", "kurdish", "other")
migdata$kurdish <- as.factor(migdata$kurdish)
summary(migdata$kurdish)
migdata$kurdish <- ifelse(is.na(migdata$kurdish), "other", as.character(migdata$kurdish))   #removing missings
migdata$kurdish <- as.factor(migdata$kurdish)
summary(migdata$kurdish)


#Total children
migdata$totalchildren <- ifelse(migdata$totalchildren>=3, "3+", as.character(migdata$totalchildren) )


##Variables for event history models 


#Year when resp. turned age 15

migdata$year15 <- migdata$birthyear + 15

#Year when resp. turned age 14
migdata$year14 <- migdata$birthyear + 14

#Age of first birth

migdata$agefirst <- ifelse(migdata$firstbirth !=9999 & migdata$birthyear!=9999, migdata$firstbirth - migdata$birthyear, 9999)
summary(migdata$agefirst)
migdata$agefirst <- ifelse(migdata$agefirst < 15 & migdata$agefirst !=9999 , 9999, as.numeric(migdata$agefirst)) ##REcoding if age below 15, which isn't common

#migdata$agefirst2 <- ifelse(migdata$firstbirth !=9999 & migdata$birthyear!=9999, migdata$firstbirth - migdata$birthyear, 9999)

#Age of second birth
migdata$agesecond <- ifelse(migdata$secondbirth !=9999 & migdata$birthyear!=9999, migdata$secondbirth - migdata$birthyear, 9999)

#Age of third birth
migdata$agethird <- ifelse(migdata$thirdbirth !=9999 & migdata$birthyear!=9999, migdata$thirdbirth - migdata$birthyear, 9999)


#Age of first migration
#migdata$agefirstmig <- ifelse(migdata$firstyearmig !=9999 & migdata$birthyear!=9999, migdata$firstyearmig - migdata$birthyear, 9999)


#Dichotomous measure for first birth event
migdata$birth1 <- ifelse(migdata$totalchildren>=1, 1, 0)

#Dichotomous measure for second birth event
migdata$birth2 <- ifelse(migdata$totalchildren>=2, 1, 0)

#Dichotomous measure for third birth event
migdata$birth3 <- ifelse(migdata$totalchildren>=3, 1, 0)

#Time until first birth
migdata$time1 <- ifelse(migdata$birth1==1, migdata$firstbirth- migdata$year14, 
                      ifelse(migdata$birth1==0 & migdata$age<=40, migdata$age-14, 
                             ifelse(migdata$birth1==0 & migdata$age>40, 26, NA)))
                        
migdata$time1 <- as.numeric(migdata$time1)

summary(migdata$time1)

migdata[migdata$time1<0,]
migdata[migdata$time1==0,]
migdata[migdata$time1>26,]

#Risk period between 14-40
migdata$time1 <- ifelse(migdata$time1<0, NA, as.character(migdata$time1)) #getting rid of those who had children below 14
migdata$time1 <- as.numeric(migdata$time1)
migdata$time1 <- ifelse(migdata$time1==0, NA, as.character(migdata$time1)) #getting rid of those who had their first child at age 14
migdata$time1 <- as.numeric(migdata$time1)
migdata$time1 <- ifelse(migdata$time1>26, NA, as.character(migdata$time1)) #getting rid of those who had their first child above age 40
migdata$time1 <- as.numeric(migdata$time1)

summary(migdata$time1)

migdata[migdata$time1<0,]
migdata[migdata$time1==0,]
migdata[migdata$time1>26,]

#migdata$time1 <- ifelse(migdata$agebirth2<15 , migdata$firstbirth- migdata$year15, 
 #                       ifelse(migdata$birth1==0 & migdata$age<=40, migdata$age-15, 
                               
#migdata2 <- migdata[!(is.na(migdata$time2)),]   #removing missing childhood place of residence


#Time until second birth 
migdata$time2 <- ifelse(migdata$birth2==1, migdata$secondbirth- migdata$firstbirth, 
                        ifelse(migdata$birth2==0 & migdata$age<=40, 2013 - migdata$firstbirth, 
                               ifelse(migdata$birth2==0 & migdata$age>40, 40-migdata$agefirst, 
                                    ifelse(migdata$birth1==0, NA, NA))))

migdata[migdata$time2<0,]  #those who didn't have a first birth
migdata[migdata$time2==0,] #those who had a first birth in 2013
migdata[migdata$time2>26,] #These are twins


migdata$time2 <- as.numeric(migdata$time2)
summary(migdata$time2)
head(migdata)
migdata$birth2<- ifelse(migdata$time2<0, NA, as.numeric(migdata$birth2))
migdata$birth2 <- as.numeric(migdata$birth2)
migdata$birth2<- ifelse(migdata$time2>26, NA, as.numeric(migdata$birth2))
migdata$birth2 <- as.numeric(migdata$birth2)
migdata$birth2<- ifelse(migdata$time2==0, NA, as.numeric(migdata$birth2))
migdata$birth2 <- as.numeric(migdata$birth2)

migdata$time2 <- ifelse(migdata$time2 <0, NA, as.numeric(migdata$time2) )
migdata$time2 <- as.numeric(migdata$time2)
summary(migdata$time2)

migdata$time2 <- ifelse(migdata$time2 ==0, NA, as.numeric(migdata$time2) )
migdata$time2 <- as.numeric(migdata$time2)
summary(migdata$time2)

migdata$time2 <- ifelse(migdata$time2 >26, NA, as.numeric(migdata$time2) )
migdata$time2 <- as.numeric(migdata$time2)
summary(migdata$time2)

head(migdata)

summary(migdata$time2)
summary(migdata$birth2)


#Time until third birth  
migdata$time3 <- ifelse(migdata$birth3==1, migdata$thirdbirth- migdata$secondbirth, 
                        ifelse(migdata$birth3==0 & migdata$age<=40, 2013-migdata$secondbirth, 
                               ifelse(migdata$birth3==0 & migdata$age>40, 40-migdata$agesecond,
                                      ifelse(migdata$birth1==0, NA, 
                                             ifelse(migdata$birth2==0, NA, NA)))))
                                                    
migdata$time3 <- as.numeric(migdata$time3)

summary(migdata$time3)

migdata[migdata$time3<0,]  #those who didn't have a second birth
migdata[migdata$time3==0,] #those who had a second birth in 2013
migdata[migdata$time3>26,] #These are twins


migdata$birth3<- ifelse(migdata$time3<0, NA, as.numeric(migdata$birth3)) 
migdata$time3 <- ifelse(migdata$time3<0, NA, as.numeric(migdata$time3))

migdata$time3 <- as.numeric(migdata$time3)
migdata$birth3 <- as.numeric(migdata$birth3)

migdata$birth3<- ifelse(migdata$time3>26, NA, as.numeric(migdata$birth3)) 
migdata$time3 <- ifelse(migdata$time3>26, NA, as.numeric(migdata$time3))

migdata$time3 <- as.numeric(migdata$time3)
migdata$birth3 <- as.numeric(migdata$birth3)

migdata$time3<- ifelse(migdata$birth1==0, NA, as.numeric(migdata$time3))
migdata$time3 <- as.numeric(migdata$time3)
migdata$time3<- ifelse(migdata$birth2==0, NA, as.numeric(migdata$time3))
migdata$time3 <- as.numeric(migdata$time3)

summary(migdata$time3)
migdata$birth3<- ifelse(migdata$birth1==0, NA, as.numeric(migdata$birth3))
migdata$birth3 <- as.numeric(migdata$birth3)
migdata$birth3<- ifelse(migdata$birth2==0, NA, as.numeric(migdata$birth3))
migdata$birth3 <- as.numeric(migdata$birth3)
summary(migdata$birth3)



##EXPLORATORY DATA ANALYSIS

###  Migration types by year of migration 

# Stacked

mig  <- migdata[!(is.na(migdata$migcohort)),]   
ggplot(mig, aes(fill=mig$migrationtype, y=100, x=mig$migcohort)) + 
  geom_bar(position="fill", stat="identity") + scale_fill_brewer(palette="GnBu") + scale_linetype_manual(guide_legend(reverse = TRUE))+ 
  ggtitle("Percentage distribution of migrant groups across cohorts") + xlab("Year of migration") + ylab("Percentage") + labs(fill = "Migrant type")

#Caution in interpreting this, data more heavily constituted by recent migrants 

ggplot(mig, aes(fill=mig$migrationtype, y=1, x=mig$migcohort)) + 
  geom_bar(stat="identity") + scale_fill_brewer(palette="GnBu") + scale_linetype_manual(guide_legend(reverse = TRUE))+ 
  ggtitle("Frequency of migrant groups by cohort") + xlab("Year of migration") + ylab("Frequency") + labs(fill = "Migrant type")


##Total children by migrant type

ggplot(mig, aes(fill=mig$totalchildren, y=100, x=mig$migrationtype)) + 
  geom_bar(position="fill", stat="identity") + scale_fill_brewer(palette="PiYG") + scale_linetype_manual(guide_legend(reverse = TRUE))+ 
  ggtitle("Percentage distribution of # children across migrant groups") + xlab("Migrant type") + ylab("Percentage") + labs(fill = "Number children")


##Education by migrant type

ggplot(migdata, aes(fill=migdata$seduc, y=100, x=migdata$migrationtype)) + 
  geom_bar(position="fill", stat="identity") + scale_fill_brewer(palette="PiYG") + scale_linetype_manual(guide_legend(reverse = TRUE))+ 
  ggtitle("Percentage distribution of education across migrant groups") + xlab("Migrant type") + ylab("Percentage") + labs(fill = "Education")

### Kaplan meier survival curve - First birth transition
        #Kaplan Meier (non-parametric method to estimate survival probability)
        #Using ggsurvplot()

summary(migdata)
summary(migdata$time1)

migdata <- migdata[!(is.na(migdata$time1)),]   #removing missings

migdata$SurvObj <- with(migdata, Surv(time1, birth1))

km.as.one <- survfit(SurvObj ~ 1, data = migdata)

km.by.cohort <- survfit(SurvObj ~ cohort, data = migdata)
km.by.migtype <- survfit(SurvObj ~ migrationtype, data = migdata)
km.by.seduc <- survfit(SurvObj ~ seduc, data = migdata)
km.by.marr <- survfit(SurvObj ~ evermarried, data = migdata)
km.by.kurdish <- survfit(SurvObj ~ kurdish, data = migdata)
km.by.region <- survfit(SurvObj ~ childregion, data = migdata)

ggsurvplot(km.as.one, data = migdata, risk.table = TRUE)
ggsurvplot(km.by.migtype, data = migdata, risk.table = FALSE)
ggsurvplot(km.by.cohort, data = migdata, risk.table = FALSE)
ggsurvplot(km.by.seduc, data = migdata, risk.table = FALSE)
ggsurvplot(km.by.marr, data = migdata, risk.table = FALSE)
ggsurvplot(km.by.kurdish, data = migdata, risk.table = FALSE)
ggsurvplot(km.by.region, data = migdata, risk.table = FALSE)

#plot(km.as.one)
#plot(km.by.cohort)
#plot(km.by.migtype)
#plot(km.by.seduc)

km.by.cohort <- coxph(Surv(time1, birth1) ~ cohort, data = migdata)
km.by.migtype <- coxph(Surv(time1, birth1) ~ migrationtype, data = migdata)
km.by.seduc <- coxph(Surv(time1, birth1) ~ seduc, data = migdata)
km.by.marr <- coxph(Surv(time1, birth1) ~ evermarried, data = migdata)
km.by.kurdish <- coxph(Surv(time1, birth1) ~ kurdish, data = migdata)
km.by.region <- coxph(Surv(time1, birth1) ~ childregion, data = migdata)

# Log rank test to test equality of survival function 
summary(km.by.cohort)  #Differences between cohorts is highly significant
summary(km.by.migtype) #Differences between migration types is highly significant
summary(km.by.seduc) #Differences between education levels is highly significant
summary(km.by.kurdish)  #Differences between ethnicities is highly significant
summary(km.by.region)  #Differences between ethnicities is highly significant


##MODEL DEVELOPMENT  AND EVALUATION

#Cox model
migdata$cohort<- as.factor(migdata$cohort)  #make factor 
migdata <- within(migdata, migrationtype <- relevel(migrationtype, ref = 3)) #set refrence categories
migdata <- within(migdata, seduc <- relevel(seduc, ref = 3))
migdata <- within(migdata, kurdish <- relevel(kurdish, ref = 2))

fit.cr1 <- coxreg(Surv(time1, birth1) ~ cohort , data= migdata)
fit.cr2 <- coxreg(Surv(time1, birth1) ~ cohort + childregion , data= migdata)
fit.cr3 <- coxreg(Surv(time1, birth1) ~ cohort + childregion + kurdish , data= migdata)
fit.cr4 <- coxreg(Surv(time1, birth1) ~ cohort + childregion + kurdish + migrationtype, data= migdata)
fit.cr5 <- coxreg(Surv(time1, birth1) ~ cohort + childregion + kurdish + migrationtype + seduc, data= migdata)

plot(fit.cr1)  #cumulative hazard function: probability of failure at time x given survival until time x 
summary(fit.cr1)
summary(fit.cr2)
summary(fit.cr3)
summary(fit.cr4)
summary(fit.cr5)

library(lmtest)  #likelihood ratio tests 
lrtest(fit.cr1,fit.cr2 )
lrtest(fit.cr2,fit.cr3 )
lrtest(fit.cr3,fit.cr4 )
lrtest(fit.cr4,fit.cr5 )  # each model is better fit than previous model 


## Parametric models

#Weibull model
fit.w <- phreg(Surv(time1, birth1) ~ cohort + childregion + kurdish + migrationtype + seduc , data = migdata)

#Lognormal model
fit.log <- phreg(Surv(time1, birth1) ~ cohort + childregion + kurdish + migrationtype + seduc , data = migdata, dist ="lognormal")

#Piecewise constant hazards model (more flexible)
fit.pch <- phreg(Surv(time1, birth1) ~ cohort + childregion + kurdish + migrationtype + seduc , data = migdata, dist ="pch")

summary(fit.pch)
summary(fit.w)
summary(fit.log)

#Log normal model is best fit according to comparison of maximum log likelihoods

#Examining through graphs

check.dist(fit.cr5, fit.w ) 
check.dist(fit.cr5, fit.log ) 
check.dist(fit.cr5, fit.pch ) 

#fit.pch2 <- phreg(Surv(time1, birth1) ~ cohort + childregion + kurdish + migrationtype + seduc , data = migdata, dist ="pch", cuts = c(2, 6, 12, 16, 20))
#fit.pch3 <- phreg(Surv(time1, birth1) ~ cohort + childregion + kurdish + migrationtype + seduc , data = migdata, dist ="pch", cuts = c(2, 6, 12, 16, 20, 24))
fit.pch4 <- phreg(Surv(time1, birth1) ~ cohort + childregion + kurdish + migrationtype + seduc , data = migdata, dist ="pch", cuts = c(2, 4, 8, 16, 20, 24))
#fit.pch5 <- phreg(Surv(time1, birth1) ~ cohort + childregion + kurdish + migrationtype + seduc , data = migdata, dist ="pch", cuts = c(2, 4, 8, 12, 16, 20, 24))
#fit.pch6 <- phreg(Surv(time1, birth1) ~ cohort + childregion + kurdish + migrationtype + seduc , data = migdata, dist ="pch", cuts = c(1, 3, 5, 8, 12, 16, 20, 23))

#check.dist(fit.cr5, fit.pch2 ) 
#check.dist(fit.cr5, fit.pch3 ) 
check.dist(fit.cr5, fit.pch4 )  #this seems like best parameterization
#check.dist(fit.cr5, fit.pch5 ) 
#check.dist(fit.cr5, fit.pch6 ) 

summary(fit.pch6)
summary(fit.log)

#Estimated hazard function 

plot(fit.pch2, fn="haz")



## SECOND BIRTH

migdata2 <- migdata[!(is.na(migdata$time2)),]   

dim(migdata2)

head(migdata2)

summary(migdata2$time2)
summary(migdata2$birth2)

migdata2$SurvObj2 <- with(migdata2, Surv(time2, birth2))

km.as.one2 <- survfit(SurvObj2 ~ 1, data = migdata2)
km.by.cohort2 <- survfit(SurvObj2 ~ cohort, data = migdata2)
km.by.migtype2 <- survfit(SurvObj2 ~ migrationtype, data = migdata2)
km.by.seduc2 <- survfit(SurvObj2 ~ seduc, data = migdata2)

ggsurvplot(km.as.one2, data = migdata2, risk.table = TRUE)
ggsurvplot(km.by.migtype2, data = migdata2, risk.table = FALSE)
ggsurvplot(km.by.cohort2, data = migdata2, risk.table = FALSE)
ggsurvplot(km.by.seduc2, data = migdata2, risk.table = FALSE)

plot(km.as.one)


#### Third Birth

head(migdata2)
#migdata2$time3[migdata2$time3 == "NA"] <- NA
migdata2 <- migdata[!(is.na(migdata$time2)),]   
migdata3 <- migdata2[!(is.na(migdata2$time3)),]   #removing missings

dim(migdata3)

head(migdata3)

summary(migdata3$time3)
summary(migdata3$birth3)

migdata3$SurvObj3 <- with(migdata3, Surv(time3, birth3))

#km.as.one3 <- survfit(SurvObj3 ~ 1, data = migdata3)
#km.by.cohort3 <- survfit(SurvObj3 ~ cohort, data = migdata3)
km.by.migtype3 <- survfit(SurvObj3 ~ migrationtype, data = migdata3)
#km.by.seduc3 <- survfit(SurvObj3 ~ seduc, data = migdata3)

#ggsurvplot(km.as.one3, data = migdata3, risk.table = TRUE)
ggsurvplot(km.by.migtype3, data = migdata3, risk.table = FALSE)
#ggsurvplot(km.by.cohort3, data = migdata3, risk.table = FALSE)
#ggsurvplot(km.by.seduc3, data = migdata3, risk.table = FALSE)

plot(km.as.one)


add2<- function(x, y) {
        x + y
}

add2(3, 5)




