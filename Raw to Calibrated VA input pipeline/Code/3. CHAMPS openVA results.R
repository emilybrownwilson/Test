# Last edited: 2 Aug 2020
# Last run:    8 Aug 2020

# Objective: run InterVA and Insilico on CHAMPS data, after mapping variables 

rm(list = ls())

library(rJava)
library(openVA)
library(readr)
library(dplyr)
library(CrossVA)
library(readxl)
library(haven)
library(data.table)

'%!in%' <- function(x,y)!('%in%'(x,y))

############################################# run InterVA5 for 2016 cases with MITS
start <- Sys.time()
file <- getwd()
file

CHAMPSdataNew <- read.csv(file.path(file,"/Data/CHAMPS_MITS_VA_20191115/ASR-114_JHU_MITS_VA/asr_114_va2016.csv"), header=TRUE, stringsAsFactors = FALSE, na.strings=c("NULL"))
CHAMPSdataJune <- read.csv(file.path(file,"Data/CHAMPS_MITS_VA_20200610/ASR-138_JHU_MITS_VA_V2/asr114_va2016.csv"), header=TRUE, stringsAsFactors = FALSE, na.strings=c("NULL"))

dim(CHAMPSdataNew)
dim(CHAMPSdataJune)

# find June VAs that aren't in previoust traunch
JuneOnly <- anti_join(CHAMPSdataJune, CHAMPSdataNew, by = c("champs_deid"))

original.cols <- setdiff(colnames(CHAMPSdataNew),colnames(JuneOnly))
Jun.cols <- setdiff(colnames(JuneOnly),colnames(CHAMPSdataNew))
original.cols
Jun.cols

# combine VAs for complete set to-date
CHAMPSdata <- rbind(CHAMPSdataNew,JuneOnly)

dim(CHAMPSdata)
class(CHAMPSdata$champs_deid)

names(CHAMPSdata) <- tolower(colnames(CHAMPSdata))

CHAMPSvariables <- as.vector(colnames(CHAMPSdata))

################################## Map Columns
###### Wet/Dry
table(CHAMPSdata$id10004, exclude=NULL)
CHAMPSdata$id10004a <- ifelse(CHAMPSdata$id10004 %in% c("wet","WET SEASON"), "Y", ifelse(CHAMPSdata$id10004 %!in% c("dk","DK","ref","Ref",".","",NA), "N", "."))
CHAMPSdata$id10004b <- ifelse(CHAMPSdata$id10004 %in% c("dry","DRY SEASON"), "Y", ifelse(CHAMPSdata$id10004 %!in% c("dk","DK","ref","Ref",".","",NA), "N", "."))
table(CHAMPSdata$id10004a, CHAMPSdata$id10004, exclude = NULL)
table(CHAMPSdata$id10004b, CHAMPSdata$id10004, exclude = NULL)

###### Male/Female
table(CHAMPSdata$id10019, exclude=NULL)
CHAMPSdata$id10019a <- ifelse(CHAMPSdata$id10019 %in% c("male","MALE","Male"), "Y", ifelse(CHAMPSdata$id10019 %!in% c("dk",".","","ambiguous/intersex",NA), "N", "."))
CHAMPSdata$id10019b <- ifelse(CHAMPSdata$id10019 %in% c("female","FEMALE","Female"), "Y", ifelse(CHAMPSdata$id10019 %!in% c("dk",".","","ambiguous/intersex",NA), "N", "."))
table(CHAMPSdata$id10019a, CHAMPSdata$id10019, exclude = NULL)
table(CHAMPSdata$id10019b, CHAMPSdata$id10019, exclude = NULL)

###### Age
table(CHAMPSdata$id10022, exclude=NULL)        
table(CHAMPSdata$ageindays, exclude=NULL)      
table(CHAMPSdata$ageinmonths, exclude = NULL)  
sum(table(CHAMPSdata$ageinyears))
table(CHAMPSdata$isneonatal, CHAMPSdata$age_group, exclude=NULL)            
table(CHAMPSdata$age_neonate_minutes, CHAMPSdata$ageinmonths, exclude=NULL) 
table(CHAMPSdata$age_neonate_days, CHAMPSdata$ageinmonths, exclude=NULL)    
table(CHAMPSdata$age_neonate_hours)            
table(CHAMPSdata$age_child_months)             
table(CHAMPSdata$age_child_years)              
table(CHAMPSdata$age_adult)                    
table(CHAMPSdata$age_adult, CHAMPSdata$id10022, exclude = NULL)  

# revised age group based on WHO 2016 format
CHAMPSdata$ageatdeath <- NULL
CHAMPSdata$ageatdeath <- CHAMPSdata$ageindays
CHAMPSdata <- CHAMPSdata %>% mutate( ageatdeath = ifelse(is.na(ageatdeath) & !is.na(age_neonate_minutes) & !is.na(isneonatal), age_neonate_minutes/(60*24), ageatdeath))
table(is.na(CHAMPSdata$ageatdeath))
CHAMPSdata <- CHAMPSdata %>% mutate( ageatdeath = ifelse(is.na(ageatdeath) & !is.na(age_neonate_hours) & !is.na(isneonatal), age_neonate_hours/(24), ageatdeath))
table(is.na(CHAMPSdata$ageatdeath))
CHAMPSdata <- CHAMPSdata %>% mutate( ageatdeath = ifelse(is.na(ageatdeath) & !is.na(age_neonate_days) & !is.na(isneonatal), age_neonate_days, ageatdeath))
table(is.na(CHAMPSdata$ageatdeath))
CHAMPSdata <- CHAMPSdata %>% mutate( ageatdeath = ifelse(is.na(ageatdeath) & !is.na(age_child_months) & !is.na(ischild), age_child_months*30.4, ageatdeath))
table(is.na(CHAMPSdata$ageatdeath))
CHAMPSdata <- CHAMPSdata %>% mutate( ageatdeath = ifelse(is.na(ageatdeath) & !is.na(age_child_years) & !is.na(ischild), age_child_years*365.25, ageatdeath))
table(is.na(CHAMPSdata$ageatdeath))
CHAMPSdata <- CHAMPSdata %>% mutate( ageatdeath = ifelse(is.na(ageatdeath) & !is.na(age_adult) & !is.na(isadult), age_adult*365.25, ageatdeath))
table(is.na(CHAMPSdata$ageatdeath))
CHAMPSdata <- CHAMPSdata %>% mutate( ageatdeath = ifelse(is.na(ageatdeath) & !is.na(ageinyears) & !is.na(isadult), ageinyears*365.25, ageatdeath))
table(is.na(CHAMPSdata$ageatdeath))
CHAMPSdata <- CHAMPSdata %>% mutate( ageatdeath = ifelse(is.na(ageatdeath) & !is.na(ageinmonths) & !is.na(isadult), ageinmonths*30.4, ageatdeath))
table(is.na(CHAMPSdata$ageatdeath))

CHAMPSdata$id10022a <- ifelse(CHAMPSdata$ageatdeath  >= 65*365.25, "Y", "N") 
CHAMPSdata$id10022b <- ifelse(CHAMPSdata$ageatdeath >= 50*365.25 & CHAMPSdata$ageatdeath < 65*365.25, "Y","N") 
CHAMPSdata$id10022c <- ifelse(CHAMPSdata$ageatdeath >= 15*365.25 & CHAMPSdata$ageatdeath < 50*365.25, "Y", "N")
CHAMPSdata$id10022d <- ifelse(CHAMPSdata$ageatdeath >= 5*365.25 & CHAMPSdata$ageatdeath < 15*365.25, "Y", "N")
CHAMPSdata$id10022e <- ifelse(CHAMPSdata$ageatdeath >= 1*365.25 & CHAMPSdata$ageatdeath < 5*365.25, "Y", "N")
CHAMPSdata$id10022f <- ifelse(CHAMPSdata$ageatdeath >= 28 & CHAMPSdata$ageatdeath < 365.25, "Y", "N")
CHAMPSdata$id10022g <- ifelse(CHAMPSdata$ageatdeath < 28, "Y", "N")
CHAMPSdata$id10022h <- ifelse(CHAMPSdata$ageatdeath < 1, "Y", "N")
CHAMPSdata$id10022i <- ifelse(CHAMPSdata$ageatdeath %in% c(1:2), "Y", "N")
CHAMPSdata$id10022j <- ifelse(CHAMPSdata$ageatdeath %in% c(3:7), "Y", "N")
CHAMPSdata$id10022k <- ifelse(CHAMPSdata$ageatdeath %in% c(8:27), "Y", "N")
CHAMPSdata$id10022l <- ifelse(CHAMPSdata$id10019b == "Y" & CHAMPSdata$ageatdeath >= 12*365.25 & CHAMPSdata$ageatdeath < 20*365.25, "Y", "N")
CHAMPSdata$id10022m <- ifelse(CHAMPSdata$id10019b == "Y" & CHAMPSdata$ageatdeath >= 20*365.25 & CHAMPSdata$ageatdeath < 35*365.25, "Y", "N")
CHAMPSdata$id10022n <- ifelse(CHAMPSdata$id10019b == "Y" & CHAMPSdata$ageatdeath >= 35*365.25 & CHAMPSdata$ageatdeath < 49*365.25, "Y", "N")

table(CHAMPSdata$id10022a, exclude=NULL)
table(CHAMPSdata$id10022b, exclude=NULL)
table(CHAMPSdata$id10022c, exclude=NULL)
table(CHAMPSdata$id10022d, exclude=NULL)
table(CHAMPSdata$id10022e, exclude=NULL)
table(CHAMPSdata$id10022f, exclude=NULL)
table(CHAMPSdata$id10022g, exclude=NULL)
table(CHAMPSdata$id10022h, exclude=NULL)
table(CHAMPSdata$id10022i, exclude=NULL)
table(CHAMPSdata$id10022j, exclude=NULL)
table(CHAMPSdata$id10022k, exclude=NULL)
table(CHAMPSdata$id10022l, exclude=NULL)
table(CHAMPSdata$id10022m, exclude=NULL)
table(CHAMPSdata$id10022n, exclude=NULL)

###### Women
table(CHAMPSdata$id10059, exclude=NULL)
CHAMPSdata$id10059o <- ifelse(CHAMPSdata$id10059 %in% c("married","partner","life_partner"), "Y", ifelse(CHAMPSdata$id10059 %!in% c("dk","DK","ref","Ref",".","",NA), "N", "."))
table(CHAMPSdata$id10059, CHAMPSdata$id10059o, exclude=NULL)

table(CHAMPSdata$id10106, exclude=NULL)
CHAMPSdata$id10106a <- ifelse(CHAMPSdata$id10106 %in% c(98,99) | is.na(CHAMPSdata$id10106), ".", ifelse(CHAMPSdata$id10106 > 5 & CHAMPSdata$id10106 < 98, "Y", "N"))
table(CHAMPSdata$id10106,CHAMPSdata$id10106a)

table(CHAMPSdata$id10108, exclude=NULL)
CHAMPSdata$id10108a <- ifelse(CHAMPSdata$id10108 %in% c(1:97), "Y", ifelse(CHAMPSdata$id10108 %!in% c("dk","DK","ref","Ref",".","",99,9999,NA), "N", "."))
table(CHAMPSdata$id10108, CHAMPSdata$id10108a, exclude = NULL)

table(CHAMPSdata$illness_length, exclude = NULL)
table(CHAMPSdata$id10120_unit, exclude = NULL)
table(CHAMPSdata$illness_length, CHAMPSdata$id10120_unit, exclude = NULL)

table(CHAMPSdata$id10120, CHAMPSdata$id10120_unit)
table(CHAMPSdata$id10121, CHAMPSdata$id10120_unit)
table(CHAMPSdata$id10122, CHAMPSdata$id10120_unit)

table(CHAMPSdata$id10120, CHAMPSdata$illness_length, exclude=NULL)
table(CHAMPSdata$id10121, CHAMPSdata$illness_length, exclude=NULL)
table(CHAMPSdata$id10122, CHAMPSdata$illness_length, exclude=NULL)

table(CHAMPSdata$id10120)  # Days
table(CHAMPSdata$id10121)  # Months
table(CHAMPSdata$id10122)  # Weeks

table(CHAMPSdata$id10120, CHAMPSdata$id10121) # This conflicts
table(CHAMPSdata$id10122, CHAMPSdata$id10121) 
table(CHAMPSdata$id10120, CHAMPSdata$id10122) # So does this

CHAMPSdata$illdur <- NULL
CHAMPSdata$illdur <- CHAMPSdata$id10120
table(is.na(CHAMPSdata$illdur))
CHAMPSdata <- CHAMPSdata %>% mutate( illdur = ifelse(is.na(illdur) & !is.na(id10122), id10122*7, illdur))
table(is.na(CHAMPSdata$illdur))
CHAMPSdata$id10121[CHAMPSdata$id10121=="Months"] <- NA
CHAMPSdata$id10121 <- as.integer(CHAMPSdata$id10121)
CHAMPSdata <- CHAMPSdata %>% mutate( illdur = ifelse(is.na(illdur) & !is.na(id10121), id10121*30.4, illdur))
table(is.na(CHAMPSdata$illdur)) 
CHAMPSdata$id10120a <- ifelse(is.na(CHAMPSdata$illdur), ".", ifelse(CHAMPSdata$illdur < 21, "Y", "N"))
table(CHAMPSdata$illdur, CHAMPSdata$id10120a, exclude=NULL)
CHAMPSdata$id10120b <- ifelse(is.na(CHAMPSdata$illdur), ".", ifelse(CHAMPSdata$illdur >= 21, "Y", "N"))
table(CHAMPSdata$illdur, CHAMPSdata$id10120b, exclude=NULL)

table(CHAMPSdata$id10148_units, CHAMPSdata$id10148, exclude=NULL)
table(CHAMPSdata$id10148_a, exclude=NULL)
table(CHAMPSdata$id10148_b, exclude=NULL)
table(CHAMPSdata$id10148_c, exclude=NULL)
CHAMPSdata$id10148a <- ifelse(is.na(CHAMPSdata$id10148),".", ifelse(CHAMPSdata$id10148 <7, "Y","N"))
CHAMPSdata$id10148b <- ifelse(is.na(CHAMPSdata$id10148),".", ifelse(CHAMPSdata$id10148 %in% c(7:13), "Y","N"))
CHAMPSdata$id10148c <- ifelse(is.na(CHAMPSdata$id10148),".", ifelse(CHAMPSdata$id10148 >=14, "Y","N"))
table(CHAMPSdata$id10148, CHAMPSdata$id10148a, exclude = NULL)
table(CHAMPSdata$id10148, CHAMPSdata$id10148b, exclude = NULL)
table(CHAMPSdata$id10148, CHAMPSdata$id10148c, exclude = NULL)

table(CHAMPSdata$id10150)
CHAMPSdata$id10150a <- ifelse(CHAMPSdata$id10150 %in% c("severe"), "Y",ifelse(is.na(CHAMPSdata$id10150) | CHAMPSdata$id10150 %in% c("dk","DK","ref","Ref",".",""),".","N"))
table(CHAMPSdata$id10150, CHAMPSdata$id10150a, exclude=NULL)

table(CHAMPSdata$id10151)
CHAMPSdata$id10151a <- ifelse(CHAMPSdata$id10151 %in% c("continuous"), "Y",ifelse(is.na(CHAMPSdata$id10151) | CHAMPSdata$id10151 %in% c("dk","DK","ref","Ref",".",""),".","N"))
table(CHAMPSdata$id10151, CHAMPSdata$id10151a, exclude=NULL)

table(CHAMPSdata$id10154)
CHAMPSdata$id10154a <- ifelse(is.na(CHAMPSdata$id10154),".",ifelse(CHAMPSdata$id10154 < 21, "Y","N"))
CHAMPSdata$id10154b <- ifelse(is.na(CHAMPSdata$id10154),".",ifelse(CHAMPSdata$id10154 >= 21, "Y","N"))
table(CHAMPSdata$id10154, CHAMPSdata$id10154a, exclude=NULL)
table(CHAMPSdata$id10154, CHAMPSdata$id10154b, exclude=NULL)

table(CHAMPSdata$id10161)
CHAMPSdata$id10161a <- ifelse(CHAMPSdata$id10161 %in% c(99,999,9999) | is.na(CHAMPSdata$id10161), ".", ifelse(CHAMPSdata$id10161 >= 3, "Y", "N"))
table(CHAMPSdata$id10161, CHAMPSdata$id10161a, exclude=NULL)

table(CHAMPSdata$id10165)
CHAMPSdata$id10165a <- ifelse(CHAMPSdata$id10165 %in% c("continuous"), "Y",ifelse(CHAMPSdata$id10165 %in% c("NULL","DK",NA),".","N"))
table(CHAMPSdata$id10165, CHAMPSdata$id10165a, exclude=NULL)

table(CHAMPSdata$id10167)
CHAMPSdata$id10167a <- ifelse(is.na(CHAMPSdata$id10167),".",ifelse(CHAMPSdata$id10167 < 14,"Y","N"))
CHAMPSdata$id10167b <- ifelse(is.na(CHAMPSdata$id10167),".",ifelse(CHAMPSdata$id10167 >= 14,"Y","N"))
table(CHAMPSdata$id10167, CHAMPSdata$id10167a, exclude=NULL)
table(CHAMPSdata$id10167, CHAMPSdata$id10167b, exclude=NULL)

table(CHAMPSdata$id10169)
CHAMPSdata$id10169a <- ifelse(is.na(CHAMPSdata$id10169),".",ifelse(CHAMPSdata$id10169 < 14,"Y","N"))
CHAMPSdata$id10169b <- ifelse(is.na(CHAMPSdata$id10169),".",ifelse(CHAMPSdata$id10169 >= 14,"Y","N"))
table(CHAMPSdata$id10169, CHAMPSdata$id10169a, exclude=NULL)
table(CHAMPSdata$id10169, CHAMPSdata$id10169b, exclude=NULL)

table(CHAMPSdata$id10173)
CHAMPSdata$id10173a <- ifelse(CHAMPSdata$id10173 %in% c("grunting","grunting dk","grunting wheezing","stridor grunting","stridor grunting wheezing","stridor wheezing","wheezing","YES"), "Y",ifelse(CHAMPSdata$id10173 %in% c("NULL","dk",NA),".","N"))
table(CHAMPSdata$id10173,CHAMPSdata$id10173a, exclude=NULL)

table(CHAMPSdata$id10176)
CHAMPSdata$id10176a <- ifelse(is.na(CHAMPSdata$id10176),".",ifelse(CHAMPSdata$id10176 >=3,"Y","N"))
table(CHAMPSdata$id10176, CHAMPSdata$id10176a, exclude=NULL)

table(CHAMPSdata$id10178)
CHAMPSdata$id10178a <- ifelse(is.na(CHAMPSdata$id10178), ".", ifelse(CHAMPSdata$id10178 >= 30, "Y", "N"))
table(CHAMPSdata$id10178,CHAMPSdata$id10178a, exclude=NULL)

table(CHAMPSdata$id10182)
CHAMPSdata$id10182a <- ifelse(is.na(CHAMPSdata$id10182) | CHAMPSdata$id10182==99, ".", ifelse(CHAMPSdata$id10182 < 14, "Y", "N"))
CHAMPSdata$id10182b <- ifelse(is.na(CHAMPSdata$id10182) | CHAMPSdata$id10182==99, ".", ifelse(CHAMPSdata$id10182 %in% c(14:27), "Y", "N"))
CHAMPSdata$id10182c <- ifelse(is.na(CHAMPSdata$id10182) | CHAMPSdata$id10182==99, ".", ifelse(CHAMPSdata$id10182 >= 28, "Y", "N"))
table(CHAMPSdata$id10182, CHAMPSdata$id10182a, exclude=NULL)
table(CHAMPSdata$id10182, CHAMPSdata$id10182b, exclude=NULL)
table(CHAMPSdata$id10182, CHAMPSdata$id10182c, exclude=NULL)

table(CHAMPSdata$id10183)
CHAMPSdata$id10183a <- ifelse(CHAMPSdata$id10183 %in% c(99,999) | is.na(CHAMPSdata$id10183), ".", ifelse(CHAMPSdata$id10183 >= 4, "Y", "N"))
table(CHAMPSdata$id10183, CHAMPSdata$id10183a, exclude=NULL)

table(CHAMPSdata$id10184)
CHAMPSdata$id10184a <- ifelse(CHAMPSdata$id10184 %in% c(99,999) | is.na(CHAMPSdata$id10184),".",ifelse(CHAMPSdata$id10184 >= 3, "Y", "N"))
table(CHAMPSdata$id10184, CHAMPSdata$id10184a, exclude=NULL)

table(CHAMPSdata$id10190)
CHAMPSdata$id10190o <- ifelse(CHAMPSdata$id10190 %in% c(99,999) | is.na(CHAMPSdata$id10190), ".", ifelse(CHAMPSdata$id10190 >= 3, "Y", "N"))
table(CHAMPSdata$id10190, CHAMPSdata$id10190o, exclude=NULL)


CHAMPSdata$pain_duration[CHAMPSdata$pain_duration=="DAYS"]<-"days"
table(CHAMPSdata$pain_duration, CHAMPSdata$id10196) # Hours
table(CHAMPSdata$pain_duration, CHAMPSdata$id10197) # Days
table(CHAMPSdata$pain_duration, CHAMPSdata$id10197b) # Weeks
table(CHAMPSdata$pain_duration, CHAMPSdata$id10198) # Months
CHAMPSdata$paindur <- NULL
CHAMPSdata$paindur <- CHAMPSdata$id10196
table(is.na(CHAMPSdata$paindur))
CHAMPSdata <- CHAMPSdata %>% mutate( paindur = ifelse(is.na(paindur) & !is.na(id10197), id10197, paindur))
table(is.na(CHAMPSdata$paindur))
CHAMPSdata <- CHAMPSdata %>% mutate( paindur = ifelse(is.na(paindur) & !is.na(id10197b), id10197b*7, paindur))
table(is.na(CHAMPSdata$paindur))
CHAMPSdata <- CHAMPSdata %>% mutate( paindur = ifelse(is.na(paindur) & !is.na(id10198), id10198*30.4, paindur))
table(is.na(CHAMPSdata$paindur))
CHAMPSdata$id10197a <- ifelse(is.na(CHAMPSdata$paindur), ".", ifelse(CHAMPSdata$paindur <14, "Y", "N"))
CHAMPSdata$id10197b <- ifelse(is.na(CHAMPSdata$paindur), ".", ifelse(CHAMPSdata$paindur>=14 , "Y", "N"))
table(CHAMPSdata$paindur, CHAMPSdata$id10197a, exclude=NULL)
table(CHAMPSdata$paindur, CHAMPSdata$id10197b, exclude=NULL)

table(CHAMPSdata$id10199, exclude = NULL)
CHAMPSdata$id10199a <- ifelse(CHAMPSdata$id10199 %in% c("upper_abdomen","upper and lower abdomen","upper_lower_abdomen"), "Y",ifelse(is.na(CHAMPSdata$id10199) | CHAMPSdata$id10199 %in% c("dk","DK","ref","Ref",".",""),".","N"))
CHAMPSdata$id10199b <- ifelse(CHAMPSdata$id10199 %in% c("lower abdomen","lower_abdomen","upper_lower_abdomen"), "Y",ifelse(is.na(CHAMPSdata$id10199) | CHAMPSdata$id10199 %in% c("dk","DK","ref","Ref",".",""),".","N"))
table(CHAMPSdata$id10199, CHAMPSdata$id10199a , exclude = NULL)
table(CHAMPSdata$id10199, CHAMPSdata$id10199b , exclude = NULL)

table(CHAMPSdata$id10201)
CHAMPSdata$id10201a <- ifelse(CHAMPSdata$id10201 %in% c(99,999,9999) | is.na(CHAMPSdata$id10201), ".", ifelse(CHAMPSdata$id10201 <14, "Y", "N"))
CHAMPSdata$id10201b <- ifelse(CHAMPSdata$id10201 %in% c(99,999,9999) | is.na(CHAMPSdata$id10201), ".", ifelse(CHAMPSdata$id10201 >=14, "Y", "N"))
table(CHAMPSdata$id10201, CHAMPSdata$id10201a, exclude=NULL)
table(CHAMPSdata$id10201, CHAMPSdata$id10201b, exclude=NULL)

table(CHAMPSdata$id10203)
CHAMPSdata$id10203a <- ifelse(CHAMPSdata$id10203 %in% c("rapidly"), "Y", ifelse(CHAMPSdata$id10203 %!in% c("dk","DK","ref",".",""), "N", "."))
table(CHAMPSdata$id10203a, CHAMPSdata$id10203, exclude = NULL)

table(CHAMPSdata$id10205)
CHAMPSdata$id10205a <- ifelse(CHAMPSdata$id10205 %in% c(99,999,9999) | is.na(CHAMPSdata$id10205), ".", ifelse(CHAMPSdata$id10205 <14, "Y", "N"))
CHAMPSdata$id10205b <- ifelse(CHAMPSdata$id10205 %in% c(99,999,9999) | is.na(CHAMPSdata$id10205), ".", ifelse(CHAMPSdata$id10205 >=14, "Y", "N"))
table(CHAMPSdata$id10205a, CHAMPSdata$id10205, exclude = NULL)
table(CHAMPSdata$id10205b, CHAMPSdata$id10205, exclude = NULL)

table(CHAMPSdata$id10209)
table(CHAMPSdata$id10209_a, CHAMPSdata$id10209_units)
table(CHAMPSdata$id10209_b, CHAMPSdata$id10209_units)
table(CHAMPSdata$id10209, CHAMPSdata$id10209_a)
table(CHAMPSdata$id10209, CHAMPSdata$id10209_b)
CHAMPSdata$id10209a <- ifelse(CHAMPSdata$id10209 %in% c(99,999,9999) | is.na(CHAMPSdata$id10209), ".", ifelse(CHAMPSdata$id10209 < 7, "Y", "N"))
CHAMPSdata$id10209b <- ifelse(CHAMPSdata$id10209 %in% c(99,999,9999) | is.na(CHAMPSdata$id10209), ".", ifelse(CHAMPSdata$id10209 >= 7, "Y", "N"))
table(CHAMPSdata$id10209a, CHAMPSdata$id10209, exclude = NULL)
table(CHAMPSdata$id10209b, CHAMPSdata$id10209, exclude = NULL)

table(CHAMPSdata$id10211, exclude = NULL)
table(CHAMPSdata$id10211_units, CHAMPSdata$id10211, exclude=NULL)
table(CHAMPSdata$id10211, CHAMPSdata$id10211_a, exclude=NULL) # days
table(CHAMPSdata$id10211, CHAMPSdata$id10211_b, exclude=NULL) # months
CHAMPSdata$id10211a <- ifelse(CHAMPSdata$id10211 %in% c(99,999,9999) | is.na(CHAMPSdata$id10211), ".", ifelse(CHAMPSdata$id10211 >= 7, "Y", "N"))
table(CHAMPSdata$id10211a, CHAMPSdata$id10211, exclude = NULL)

table(CHAMPSdata$id10213_units, CHAMPSdata$id10213, exclude=NULL)
table(CHAMPSdata$id10213_units, CHAMPSdata$id10213_a, exclude=NULL) # days
table(CHAMPSdata$id10213_units, CHAMPSdata$id10213_b, exclude=NULL) # months
table(CHAMPSdata$id10213, CHAMPSdata$id10213_a, exclude=NULL) # days
table(CHAMPSdata$id10213, CHAMPSdata$id10213_b, exclude=NULL) # days
CHAMPSdata$id10213o <- ifelse(CHAMPSdata$id10213 %in% c(99,999,9999) | is.na(CHAMPSdata$id10213), ".", ifelse(CHAMPSdata$id10213 >= 3, "Y", "N"))
table(CHAMPSdata$id10213o, CHAMPSdata$id10213, exclude = NULL)

table(CHAMPSdata$id10216, exclude=NULL)
CHAMPSdata$id10216a <- ifelse(CHAMPSdata$id10216 %in% c(99,999,9999) | is.na(CHAMPSdata$id10216), ".", ifelse(CHAMPSdata$id10216 >= 6, "Y", "N"))
table(CHAMPSdata$id10216a, CHAMPSdata$id10216, exclude = NULL)

table(CHAMPSdata$id10221, exclude = NULL)
CHAMPSdata$id10221a <- ifelse(CHAMPSdata$id10221 %in% c(99,999,9999) | is.na(CHAMPSdata$id10221), ".", ifelse(CHAMPSdata$id10221 < 10, "Y", "N"))
CHAMPSdata$id10221b <- ifelse(CHAMPSdata$id10221 %in% c(99,999,9999) | is.na(CHAMPSdata$id10221), ".", ifelse(CHAMPSdata$id10221 >= 10, "Y", "N"))
table(CHAMPSdata$id10221a, CHAMPSdata$id10221, exclude = NULL)
table(CHAMPSdata$id10221b, CHAMPSdata$id10221, exclude = NULL)

table(CHAMPSdata$id10232, CHAMPSdata$id10232_a, exclude = NULL)
table(CHAMPSdata$id10232, CHAMPSdata$id10232_b, exclude = NULL)
CHAMPSdata$id10232a <- ifelse(CHAMPSdata$id10232 %in% c(99,999,9999) | is.na(CHAMPSdata$id10232), ".", ifelse(CHAMPSdata$id10232 >= 14, "Y", "N"))
table(CHAMPSdata$id10232a, CHAMPSdata$id10232, exclude = NULL)

table(CHAMPSdata$id10234, exclude = NULL)
CHAMPSdata$id10234a <- ifelse(CHAMPSdata$id10234 %in% c(99,999,9999) | is.na(CHAMPSdata$id10234), ".", ifelse(CHAMPSdata$id10234 < 7, "Y", "N"))
CHAMPSdata$id10234b <- ifelse(CHAMPSdata$id10234 %in% c(99,999,9999) | is.na(CHAMPSdata$id10234), ".", ifelse(CHAMPSdata$id10234 >=7, "Y", "N"))
table(CHAMPSdata$id10234a, CHAMPSdata$id10234, exclude = NULL)
table(CHAMPSdata$id10234b, CHAMPSdata$id10234, exclude = NULL)

table(CHAMPSdata$id10235, exclude=NULL)
CHAMPSdata$id10235a <- ifelse(CHAMPSdata$id10235 %in% c("face","face trunk","face trunk extremities","face trunk extremities everywhere"), "Y", ifelse(CHAMPSdata$id10235 %!in% c("dk","DK","ref","Ref",".","",NA), "N", "."))
CHAMPSdata$id10235b <- ifelse(CHAMPSdata$id10235 %in% c("face trunk","trunk","face trunk extremities","trunk extremities","face trunk extremities everywhere"), "Y", ifelse(CHAMPSdata$id10235 %!in% c("dk","DK","ref","Ref",".","",NA), "N", "."))
CHAMPSdata$id10235c <- ifelse(CHAMPSdata$id10235 %in% c("extremities","face trunk extremities","trunk extremities"), "Y", ifelse(CHAMPSdata$id10235 %!in% c("dk","DK","ref","Ref",".","",NA), "N", "."))
CHAMPSdata$id10235d <- ifelse(CHAMPSdata$id10235 %in% c("everywhere","face trunk extremities everywhere"), "Y", ifelse(CHAMPSdata$id10235 %!in% c("dk","DK","ref","Ref",".","",NA), "N", "."))
table(CHAMPSdata$id10235a, CHAMPSdata$id10235, exclude = NULL)
table(CHAMPSdata$id10235b, CHAMPSdata$id10235, exclude = NULL)
table(CHAMPSdata$id10235c, CHAMPSdata$id10235, exclude = NULL)
table(CHAMPSdata$id10235d, CHAMPSdata$id10235, exclude = NULL)

table(CHAMPSdata$id10248)
table(CHAMPSdata$id10248_units, CHAMPSdata$id10248, exclude = NULL)
table(CHAMPSdata$id10248_units, CHAMPSdata$id10248_a, exclude = NULL) # days
table(CHAMPSdata$id10248_units, CHAMPSdata$id10248_b, exclude = NULL) # months
table(CHAMPSdata$id10248_a, CHAMPSdata$id10248, exclude=NULL) # id10248 seems to have been converted to months
table(CHAMPSdata$id10248_b, CHAMPSdata$id10248, exclude=NULL)
CHAMPSdata$id10248 <- signif(30*CHAMPSdata$id10248)
CHAMPSdata$id10248a <- ifelse(CHAMPSdata$id10248 %in% c(99,999,9999) | is.na(CHAMPSdata$id10248), ".", ifelse(CHAMPSdata$id10248 >= 7, "Y", "N"))
table(CHAMPSdata$id10248, CHAMPSdata$id10248a, exclude = NULL)

table(CHAMPSdata$id10250)
table(CHAMPSdata$id10250_units, CHAMPSdata$id10250)
CHAMPSdata$id10250_a[CHAMPSdata$id10250_a=="Days"]<-NA
CHAMPSdata$id10250_a <- as.integer(CHAMPSdata$id10250_a)
table(CHAMPSdata$id10250_units, CHAMPSdata$id10250_a)
table(CHAMPSdata$id10250_units, CHAMPSdata$id10250_b)
table(CHAMPSdata$id10250_a, CHAMPSdata$id10250, exclude=NULL) # id10250 seems to have been converted to months
table(CHAMPSdata$id10250_b, CHAMPSdata$id10250, exclude=NULL)
CHAMPSdata$id10250 <- signif(30*CHAMPSdata$id10250)
CHAMPSdata$id10250a <- ifelse(CHAMPSdata$id10250 %in% c(99,999,9999) | is.na(CHAMPSdata$id10250), ".", ifelse(CHAMPSdata$id10250 >= 3, "Y", "N"))
table(CHAMPSdata$id10250a, CHAMPSdata$id10250, exclude = NULL)

table(CHAMPSdata$id10260)
CHAMPSdata$id10260a <- ifelse(CHAMPSdata$id10260 %in% c("right_side","right_side    one_leg_only one_arm_only"), "Y", ifelse(CHAMPSdata$id10260 %!in% c("dk","DK","ref","Ref",".",""), "N", "."))
CHAMPSdata$id10260b <- ifelse(CHAMPSdata$id10260 %in% c("left side","left_side"), "Y", ifelse(CHAMPSdata$id10260 %!in% c("dk","DK","ref","Ref",".",""), "N", "."))
CHAMPSdata$id10260c <- ifelse(CHAMPSdata$id10260 %in% c("lower_part_of_body","lower_part_of_body upper_part_of_body"), "Y", ifelse(CHAMPSdata$id10260 %!in% c("dk","DK","ref","Ref",".",""), "N", "."))
CHAMPSdata$id10260d <- ifelse(CHAMPSdata$id10260 %in% c("upper_part_of_body","lower_part_of_body upper_part_of_body"), "Y", ifelse(CHAMPSdata$id10260 %!in% c("dk","DK","ref","Ref",".",""), "N", "."))
CHAMPSdata$id10260e <- ifelse(CHAMPSdata$id10260 %in% c("one_leg_only","one_leg_only one_arm_only","right_side    one_leg_only one_arm_only"), "Y", ifelse(CHAMPSdata$id10260 %!in% c("dk","DK","ref","Ref",".",""), "N", "."))
CHAMPSdata$id10260f <- ifelse(CHAMPSdata$id10260 %in% c("one_arm_only","one_leg_only one_arm_only ","right_side    one_leg_only one_arm_only"), "Y", ifelse(CHAMPSdata$id10260 %!in% c("dk","DK","ref","Ref",".",""), "N", "."))
CHAMPSdata$id10260g <- ifelse(CHAMPSdata$id10260 %in% c("whole_body"), "Y", ifelse(CHAMPSdata$id10260 %!in% c("dk","DK","ref","Ref",".",""), "N", "."))
table(CHAMPSdata$id10260, CHAMPSdata$id10260a, exclude = NULL)
table(CHAMPSdata$id10260, CHAMPSdata$id10260b, exclude = NULL)
table(CHAMPSdata$id10260, CHAMPSdata$id10260c, exclude = NULL)
table(CHAMPSdata$id10260, CHAMPSdata$id10260d, exclude = NULL)
table(CHAMPSdata$id10260, CHAMPSdata$id10260e, exclude = NULL)
table(CHAMPSdata$id10260, CHAMPSdata$id10260f, exclude = NULL)
table(CHAMPSdata$id10260, CHAMPSdata$id10260g, exclude = NULL)

table(CHAMPSdata$id10262, exclude = NULL)
table(CHAMPSdata$id10262_a, CHAMPSdata$id10262, exclude=NULL) # id10262 seems to have been converted to months
table(CHAMPSdata$id10262_b, CHAMPSdata$id10262, exclude=NULL)
CHAMPSdata$id10262 <- signif(30*CHAMPSdata$id10262,1)
CHAMPSdata$id10262a <- ifelse(CHAMPSdata$id10262 %in% c(99,999,9999) | is.na(CHAMPSdata$id10262), ".", ifelse(CHAMPSdata$id10262 >=7, "Y", "N"))
table(CHAMPSdata$id10262a, CHAMPSdata$id10262, exclude = NULL)

table(CHAMPSdata$id10263, exclude = NULL)
CHAMPSdata$id10263a <- ifelse(CHAMPSdata$id10263 %in% c("solids","both"), "Y", ifelse(CHAMPSdata$id10263 %!in% c("dk","DK","ref","Ref",".",""), "N", "."))
CHAMPSdata$id10263b <- ifelse(CHAMPSdata$id10263 %in% c("liquids","both"), "Y", ifelse(CHAMPSdata$id10263 %!in% c("dk","DK","ref","Ref",".",""), "N", "."))
table(CHAMPSdata$id10263a, CHAMPSdata$id10263, exclude = NULL)
table(CHAMPSdata$id10263b, CHAMPSdata$id10263, exclude = NULL)

table(CHAMPSdata$id10266, exclude = NULL)
table(CHAMPSdata$id10266_a, CHAMPSdata$id10266, exclude=NULL) # id10266 seems to have been converted to months
table(CHAMPSdata$id10266_b, CHAMPSdata$id10266, exclude=NULL)
CHAMPSdata$id10266 <- signif(30*CHAMPSdata$id10266,1)
CHAMPSdata$id10266a <- ifelse(CHAMPSdata$id10266 %in% c(99,999,9999) | is.na(CHAMPSdata$id10266), ".", ifelse(CHAMPSdata$id10266 >=21, "Y", "N"))
table(CHAMPSdata$id10266a, CHAMPSdata$id10266, exclude = NULL)

table(CHAMPSdata$id10274, exclude = NULL)
CHAMPSdata$id10274a <- ifelse(CHAMPSdata$id10274 %in% c(99,999,9999) | is.na(CHAMPSdata$id10274), ".", ifelse(CHAMPSdata$id10274 >=2, "Y", "N"))
table(CHAMPSdata$id10274a, CHAMPSdata$id10274, exclude = NULL)

table(CHAMPSdata$id10285, exclude = NULL)
CHAMPSdata$id10285a <- ifelse(CHAMPSdata$id10285 %in% c(99,999,9999) | is.na(CHAMPSdata$id10285), ".", ifelse(CHAMPSdata$id10285 >3, "Y", "N"))
table(CHAMPSdata$id10285a, CHAMPSdata$id10285, exclude = NULL)

table(CHAMPSdata$id10303, exclude = NULL)
CHAMPSdata$id10303a <- ifelse(CHAMPSdata$id10303 %in% c(99,999,9999) | is.na(CHAMPSdata$id10303), ".", ifelse(CHAMPSdata$id10303 >=4, "Y", "N"))
table(CHAMPSdata$id10303a, CHAMPSdata$id10303, exclude = NULL)

table(CHAMPSdata$id10309, exclude = NULL)
CHAMPSdata$id10309o <- ifelse(CHAMPSdata$id10309 %in% c(99,999,9999) | is.na(CHAMPSdata$id10309), ".", ifelse(CHAMPSdata$id10309 <6, "Y", "N"))
table(CHAMPSdata$id10309o, CHAMPSdata$id10309, exclude = NULL)

table(CHAMPSdata$id10319, exclude = NULL)
CHAMPSdata$id10319a <- ifelse(CHAMPSdata$id10319 %in% c(99,999,9999) | is.na(CHAMPSdata$id10319), ".", ifelse(CHAMPSdata$id10319 ==0, "Y", "N"))
CHAMPSdata$id10319b <- ifelse(CHAMPSdata$id10319 %in% c(99,999,9999) | is.na(CHAMPSdata$id10319), ".", ifelse(CHAMPSdata$id10319 >=4, "Y", "N"))
table(CHAMPSdata$id10319a, CHAMPSdata$id10319, exclude = NULL)
table(CHAMPSdata$id10319b, CHAMPSdata$id10319, exclude = NULL)

table(CHAMPSdata$id10332, exclude = NULL)
CHAMPSdata$id10332a <- ifelse(CHAMPSdata$id10332 %in% c(99,999,9999) | is.na(CHAMPSdata$id10332), ".", ifelse(CHAMPSdata$id10332 >24, "Y", "N"))
table(CHAMPSdata$id10332a, CHAMPSdata$id10332, exclude = NULL)

table(CHAMPSdata$id10337, exclude = NULL)
CHAMPSdata$id10337a <- ifelse(CHAMPSdata$id10337 %in% c("hospital","other_health_facility"), "Y", ifelse(CHAMPSdata$id10337 %!in% c("dk","DK","ref","Ref",".","",NA), "N", "."))
CHAMPSdata$id10337b <- ifelse(CHAMPSdata$id10337 %in% c("home"), "Y", ifelse(CHAMPSdata$id10337 %!in% c("dk","DK","ref","Ref",".","",NA), "N", "."))
CHAMPSdata$id10337c <- ifelse(CHAMPSdata$id10337 %in% c("other", "on_route_to_hospital_or_facility"), "Y", ifelse(CHAMPSdata$id10337 %!in% c("dk","DK","ref","Ref",".","",NA), "N", "."))
table(CHAMPSdata$id10337a, CHAMPSdata$id10337, exclude = NULL)
table(CHAMPSdata$id10337b, CHAMPSdata$id10337, exclude = NULL)
table(CHAMPSdata$id10337c, CHAMPSdata$id10337, exclude = NULL)

table(CHAMPSdata$id10355, exclude = NULL)
CHAMPSdata$id10355a <- ifelse(CHAMPSdata$id10355 %in% c("first"), "Y", ifelse(CHAMPSdata$id10355 %!in% c("dk","DK","ref","Ref",".","",NA), "N", "."))
table(CHAMPSdata$id10355a, CHAMPSdata$id10355, exclude = NULL)

table(CHAMPSdata$id10357, exclude=NULL)
CHAMPSdata$id10357o <- ifelse(CHAMPSdata$id10357 %in% c("during_delivery","after_delivery","after delivery"), "Y", ifelse(CHAMPSdata$id10357 %!in% c("dk","DK","ref","Ref",".","",NA), "N", "."))
table(CHAMPSdata$id10357o, CHAMPSdata$id10357, exclude = NULL)

table(CHAMPSdata$id10358, exclude=NULL)
CHAMPSdata$id10358a <- ifelse(CHAMPSdata$id10358 %in% c(99,999,9999) | is.na(CHAMPSdata$id10358), ".", ifelse(CHAMPSdata$id10358 <=12, "Y", "N"))
table(CHAMPSdata$id10358a, CHAMPSdata$id10358, exclude = NULL)

table(CHAMPSdata$id10360, exclude=NULL)
CHAMPSdata$id10360a <- ifelse(CHAMPSdata$id10360 %in% c("Government clinic/health center","Government health post","Government hospital","HEALTH FACILITY","hospital","HOSPITAL","other_health_facility"), "Y", ifelse(CHAMPSdata$id10360 %!in% c("dk","DK","ref","Ref",".","",NA), "N", "."))
CHAMPSdata$id10360b <- ifelse(CHAMPSdata$id10360 %in% c("home","The mother's home"), "Y", ifelse(CHAMPSdata$id10360 %!in% c("dk","DK","ref","Ref",".","",NA), "N", "."))
CHAMPSdata$id10360c <- ifelse(CHAMPSdata$id10360 %in% c("other","On route to a health provider or facility","on_route_to_hospital_facility","on_route_to_hospital_or_facility"), "Y", ifelse(CHAMPSdata$id10360 %!in% c("dk","DK","ref","Ref",".","",NA), "N", "."))
table(CHAMPSdata$id10360, CHAMPSdata$id10360a, exclude = NULL)
table(CHAMPSdata$id10360, CHAMPSdata$id10360b, exclude = NULL)
table(CHAMPSdata$id10360, CHAMPSdata$id10360c, exclude = NULL)

table(CHAMPSdata$id10367, exclude=NULL)
CHAMPSdata$id10367a <- ifelse(CHAMPSdata$id10367 %in% c(99,999,9999) | is.na(CHAMPSdata$id10367), ".", ifelse(CHAMPSdata$id10367 >=9 & CHAMPSdata$id10367 <= 12, "Y", "N"))
CHAMPSdata$id10367b <- ifelse(CHAMPSdata$id10367 %in% c(99,999,9999) | is.na(CHAMPSdata$id10367), ".", ifelse(CHAMPSdata$id10367 ==8, "Y", "N"))
CHAMPSdata$id10367c <- ifelse(CHAMPSdata$id10367 %in% c(99,999,9999) | is.na(CHAMPSdata$id10367), ".", ifelse(CHAMPSdata$id10367 <=7, "Y", "N"))
table(CHAMPSdata$id10367a, CHAMPSdata$id10367, exclude = NULL)
table(CHAMPSdata$id10367b, CHAMPSdata$id10367, exclude = NULL)
table(CHAMPSdata$id10367c, CHAMPSdata$id10367, exclude = NULL)

table(CHAMPSdata$id10382, exclude=NULL)
CHAMPSdata$id10382a <- ifelse(CHAMPSdata$id10382 %in% c(99,999,9999) | is.na(CHAMPSdata$id10382), ".", ifelse(CHAMPSdata$id10382 >24, "Y", "N"))
table(CHAMPSdata$id10382a, CHAMPSdata$id10382, exclude = NULL)

table(CHAMPSdata$id10385, exclude=NULL)
CHAMPSdata$id10385a <- ifelse(CHAMPSdata$id10385 %in% c("green_or_brown"), "Y", ifelse(CHAMPSdata$id10385 %!in% c("dk","DK","ref","Ref",".","",NA), "N", "."))
table(CHAMPSdata$id10385a, CHAMPSdata$id10385, exclude = NULL)

table(CHAMPSdata$id10394, exclude = NULL)
CHAMPSdata$id10394a <- ifelse(CHAMPSdata$id10394 %in% c(99,999,9999) | is.na(CHAMPSdata$id10394), ".", ifelse(CHAMPSdata$id10394 ==0, "Y", "N"))
CHAMPSdata$id10394b <- ifelse(CHAMPSdata$id10394 %in% c(99,999,9999) | is.na(CHAMPSdata$id10394), ".", ifelse(CHAMPSdata$id10394 >=4, "Y", "N"))
table(CHAMPSdata$id10394a, CHAMPSdata$id10394, exclude = NULL)
table(CHAMPSdata$id10394b, CHAMPSdata$id10394, exclude = NULL)

table(CHAMPSdata$id10414, exclude=NULL)
CHAMPSdata$id10414a <- ifelse(CHAMPSdata$id10414 %in% c("chewing_tobacco"), "Y", ifelse(CHAMPSdata$id10414 %!in% c("dk","DK","ref","Ref",".",""), "N", "."))
table(CHAMPSdata$id10414a, CHAMPSdata$id10414, exclude = NULL)

table(CHAMPSdata$id10415, exclude=NULL)
CHAMPSdata$id10415a <- ifelse(CHAMPSdata$id10415 %in% c(99,999,9999) | is.na(CHAMPSdata$id10415), ".", ifelse(CHAMPSdata$id10415 >=10, "Y", "N"))
table(CHAMPSdata$id10415a, CHAMPSdata$id10415, exclude = NULL)

table(CHAMPSdata$id10114)
CHAMPSdata$id10114[CHAMPSdata$id10114 == "Dead"]<-"yes"
CHAMPSdata$id10114[CHAMPSdata$id10114 == "Alive"]<-"no"

################################## CONVERT ALL Y/N INTO BINARY FORMAT AT ONCE
# for(i in c(1:227,229:ncol(CHAMPSdata))){
for(i in c(1:ncol(CHAMPSdata))){
  CHAMPSdata[i][CHAMPSdata[i] == "NA"] <- "."
  CHAMPSdata[i][CHAMPSdata[i] == "DK"] <- "."
  CHAMPSdata[i][CHAMPSdata[i] == "dk"] <- "."
  CHAMPSdata[i][CHAMPSdata[i] == "Doesn't know"] <- "."
  CHAMPSdata[i][CHAMPSdata[i] == "ref"] <- "."
  CHAMPSdata[i][CHAMPSdata[i] == "Ref"] <- "."
  CHAMPSdata[i][CHAMPSdata[i] == ""] <- "."
  CHAMPSdata[i][is.na(CHAMPSdata[i])] <- "."
  CHAMPSdata[i][CHAMPSdata[i] == "NULL"] <- "."
  CHAMPSdata[i][CHAMPSdata[i] == "yes"] <- "Y"
  CHAMPSdata[i][CHAMPSdata[i] == "Yes"] <- "Y"
  CHAMPSdata[i][CHAMPSdata[i] == "YES"] <- "Y"
  CHAMPSdata[i][CHAMPSdata[i] == "no"] <- "N"
  CHAMPSdata[i][CHAMPSdata[i] == "No"] <- "N"
  CHAMPSdata[i][CHAMPSdata[i] == "NO"] <- "N"
}

names(CHAMPSdata)[names(CHAMPSdata) == 'champs_deid'] <- 'ID'

################################## KEEP ONLY COLUMNS NEEDED TO RUN ALGORITHMS
records <- read.csv(file.path(file, "/Data/who151_va_output.csv"))

sample_test <- odk2openVA_v151(records)
colnames(sample_test)

test <- CHAMPSdata
names(test) <- sub("id10", "i", names(test))
COMSAonly <- setdiff(colnames(test), colnames(sample_test))
sampleonly <- setdiff(colnames(sample_test),colnames(test))
sampleonly <- sub("i", "id10", sampleonly)
sampleonly <- sub("o", "", sampleonly)

for(j in 1:ncol(CHAMPSdata)){
  if(names(CHAMPSdata[j]) %in% sampleonly){
    print(colnames(CHAMPSdata)[j])
    store <- paste0(colnames(CHAMPSdata)[j],"o", sep="")
    print(store)
    names(CHAMPSdata)[names(CHAMPSdata) == colnames(CHAMPSdata)[j]] <- store
  }
}

names(CHAMPSdata) <- sub("id10", "i", names(CHAMPSdata))

IV5data.neonatal <- subset(CHAMPSdata, case_type_desc %in% c("Death in the first 24 hours","Early Neonate (1 to 6 days)","Late Neonate (7 to 27 days)"))
dim(IV5data.neonatal)
IV5data.childu5 <- subset(CHAMPSdata, case_type_desc %in% c("Child (12 months to less than 60 Months)","Infant (28 days to less than 12 months)"))
dim(IV5data.childu5)

names.use <- names(CHAMPSdata)[(names(CHAMPSdata) %in% colnames(sample_test))]

IV5data.neonatal.subset <- IV5data.neonatal[, c(names.use)]
IV5data.childu5.subset <- IV5data.childu5[, c(names.use)]

COMSAonly <- setdiff(colnames(IV5data.neonatal.subset), colnames(sample_test)) 
sampleonly <- setdiff(colnames(sample_test),colnames(IV5data.neonatal.subset)) 

length(COMSAonly)
length(sampleonly)

IV5neonate <- setcolorder(IV5data.neonatal.subset, names(sample_test))
IV5childu5 <- setcolorder(IV5data.childu5.subset, names(sample_test))

################################## Run models for CHAMPS
# InterVA5
set.seed(123)
InterVA5.neonate <- InterVA5(IV5neonate, HIV = "h", Malaria = "h", write=FALSE,
                             directory = tempdir(), filename = "VA5_result", output = "extended", append = FALSE)
set.seed(123)
InterVA5.childu5 <- InterVA5(IV5childu5, HIV = "h", Malaria = "h", write=FALSE,
                             directory = tempdir(), filename = "VA5_result", output = "extended", append = FALSE)

# Insilico
set.seed(123)
codeVAInsilico.neonate <- codeVA(IV5neonate, data.type = "WHO2016", model = "InSilicoVA",
                                 Nsim=10000, version = "5.0", HIV = "h", Malaria = "h")
set.seed(123)
codeVAInsilico.childu5 <- codeVA(IV5childu5, data.type = "WHO2016", model = "InSilicoVA",
                                 Nsim=10000, version = "5.0", HIV = "h", Malaria = "h")


stop.time <- Sys.time()
save.image(file=file.path(file,"/Data/openVA_champs.Rdata"))

