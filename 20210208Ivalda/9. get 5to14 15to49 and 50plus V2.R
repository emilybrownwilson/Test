# Last edited: 11 Feb 2021
# Last run:    11 Feb 2021

# Objective: Produce broad cause graphs from individual COD, for each method

rm(list = ls())

library(openVA)

file <- getwd()
load(file.path(file,"/Data/openVA_comsa.Rdata"))

'%!in%' <- function(x,y)!('%in%'(x,y))

### keep only objects we're using
rm(list = ls()[!ls() %in% c("codeVAInsilico.ch5_14y", "codeVAInsilico.ad15_49", "codeVAInsilico.ad50")])

file <- getwd()
file

library(stringr)

########################################################### Start here for data visualization - no calibration for these age categories
# InterVA.COD.ch5_14 <- data.frame(lapply(getTopCOD(InterVA5.ch5_14),as.character), stringsAsFactors = FALSE) 
# InterVA.COD.ad15_49 <- data.frame(lapply(getTopCOD(InterVA5.ad15_49),as.character), stringsAsFactors = FALSE) 
# InterVA.COD.ad50 <- data.frame(lapply(getTopCOD(InterVA5.ad50),as.character), stringsAsFactors = FALSE) 

Insilico.COD.ch5_14 <- data.frame(lapply(getTopCOD(codeVAInsilico.ch5_14y),as.character), stringsAsFactors = FALSE) 
Insilico.COD.ad15_49 <- data.frame(lapply(getTopCOD(codeVAInsilico.ad15_49),as.character), stringsAsFactors = FALSE) 
Insilico.COD.ad50 <- data.frame(lapply(getTopCOD(codeVAInsilico.ad50),as.character), stringsAsFactors = FALSE) 

# Put InterVA and Insilico together:
# head(InterVA.COD.ch5_14)
# head(InterVA.COD.ad15_49)
# head(InterVA.COD.ad50)

head(Insilico.COD.ch5_14)
head(Insilico.COD.ad15_49)
head(Insilico.COD.ad50)

################################## reformat original COD
# names(InterVA.COD.ch5_14)[names(InterVA.COD.ch5_14) == 'cause'] <- 'InterVA'
names(Insilico.COD.ch5_14)[names(Insilico.COD.ch5_14) == 'cause'] <- 'InsilicoVA'

# names(InterVA.COD.ad15_49)[names(InterVA.COD.ad15_49) == 'cause'] <- 'InterVA'
names(Insilico.COD.ad15_49)[names(Insilico.COD.ad15_49) == 'cause'] <- 'InsilicoVA'

# names(InterVA.COD.ad50)[names(InterVA.COD.ad50) == 'cause'] <- 'InterVA'
names(Insilico.COD.ad50)[names(Insilico.COD.ad50) == 'cause'] <- 'InsilicoVA'

# InterVA.COD.ch5_14$ID <- trimws(InterVA.COD.ch5_14$ID)
Insilico.COD.ch5_14$ID <- trimws(Insilico.COD.ch5_14$ID)

# InterVA.COD.ad15_49$ID <- trimws(InterVA.COD.ad15_49$ID)
Insilico.COD.ad15_49$ID <- trimws(Insilico.COD.ad15_49$ID)

# InterVA.COD.ad50$ID <- trimws(InterVA.COD.ad50$ID)
Insilico.COD.ad50$ID <- trimws(Insilico.COD.ad50$ID)
################################## Compile and write out results

# COD.ch5_14 <- merge(InterVA.COD.ch5_14,Insilico.COD.ch5_14, by=c("ID"))
# 
# COD.ad15_49 <- merge(InterVA.COD.ad15_49,Insilico.COD.ad15_49, by=c("ID"))
# 
# COD.ad50 <- merge(InterVA.COD.ad50,Insilico.COD.ad50, by=c("ID"))

# head(COD.ch5_14)
# head(COD.ad15_49)
# head(COD.ad50)
# 
dim(Insilico.COD.ch5_14)
dim(Insilico.COD.ad15_49)
dim(Insilico.COD.ad50)

############## ADD sex, province
COMSAdata <- read.csv(file.path(file,"Data/all_WHO_wgt.csv"), stringsAsFactors = FALSE)
# COMSAdata$ID <- trimws(COMSAdata$ID)

names(COMSAdata)[names(COMSAdata) == 'comsa_id'] <- 'ID'
names(COMSAdata)[names(COMSAdata) == 'id10019'] <- 'sex'

COMSAdata <- COMSAdata[,c("ID","sex","province")]

COMSAdata$ID <- as.character(COMSAdata$ID)
COD.ch5to14 <- merge(Insilico.COD.ch5_14,COMSAdata, by=c("ID"))
COD.ad15to49 <- merge(Insilico.COD.ad15_49,COMSAdata, by=c("ID"))
COD.ad50 <- merge(Insilico.COD.ad50,COMSAdata, by=c("ID"))

dim(COD.ch5to14)
dim(Insilico.COD.ad15_49)
dim(Insilico.COD.ad50)

############## ADD age
COMSAdata_age <- read.csv(file.path(file,"Data/all_WHO_with_age.csv"), stringsAsFactors = FALSE)
hist(COMSAdata_age$ageatdeath)
names(COMSAdata_age)[names(COMSAdata_age) == 'comsa_id'] <- 'ID'
names(COMSAdata_age)[names(COMSAdata_age) == 'ageatdeath'] <- 'age_in_days'
names(COMSAdata_age)[names(COMSAdata_age) == 'id10023'] <- 'date_of_death'

COMSAdata_age <- COMSAdata_age[,c("ID","age_in_days","date_of_death")]
COMSAdata <- merge(COMSAdata, COMSAdata_age, by=c("ID"))

COD.5to14 <- merge(COD.ch5to14,COMSAdata, by=c("ID","sex","province"))
COD.15to49 <- merge(COD.ad15to49,COMSAdata, by=c("ID","sex","province"))
COD.50 <- merge(COD.ad50,COMSAdata, by=c("ID","sex","province"))

dim(COD.5to14)
COD.5to14.1 <- COD.5to14 %>%filter(str_detect(date_of_death, "2019"))
COD.5to14.2 <- COD.5to14 %>% filter(str_detect(date_of_death, "2020"))
dim(COD.5to14.1)
head(COD.5to14.1)
dim(COD.5to14.2)
head(COD.5to14.2)
COD.5to14 <- rbind(COD.5to14.1,COD.5to14.2)
head(COD.5to14)
dim(COD.5to14)

dim(COD.15to49)
COD.15to49.1 <- COD.15to49 %>%filter(str_detect(date_of_death, "2019"))
COD.15to49.2 <- COD.15to49 %>% filter(str_detect(date_of_death, "2020"))
dim(COD.15to49.1)
head(COD.15to49.1)
dim(COD.15to49.2)
head(COD.15to49.2)
COD.15to49 <- rbind(COD.15to49.1,COD.15to49.2)
dim(COD.15to49)

dim(COD.50)
COD.50.1 <- COD.50 %>%filter(str_detect(date_of_death, "2019"))
COD.50.2 <- COD.50 %>% filter(str_detect(date_of_death, "2020"))
dim(COD.50.1)
head(COD.50.1)
dim(COD.50.2)
head(COD.50.2)
COD.50 <- rbind(COD.50.1,COD.50.2)
dim(COD.50)

############ check ages are in range
summary(COD.5to14)  # age range approx: 1826-5410
summary(COD.15to49) # age range approx: 5479-18260
summary(COD.50)    # age range approx: >=18262
############

#### define cause categories for all categories
Maternal <- c("Abortion-related death", "Anaemia of pregnancy","Ectopic pregnancy",
              "Obstetric haemorrhage","Obstetric hemorrhage", "Pregnancy-induced hypertension", "Other and unspecified maternal CoD", "Pregnancy-induced sepsis",
              'Pregnancy-related sepsis',"Ruptured uterus",
              "Severe anaemia","Obstructed labour")
Cancer <- c("Breast neoplasms","Digestive neoplasms","Leukemia/lymphoma","Neoplasms","Oral neoplasms","Other and unspecified neoplasms","Reproductive neoplasms MF","Respiratory neoplasms")
Injury <- c("Accid drowning and submersion","Accid expos to smoke fire & flame","Accid fall","Accid poisoning & noxious subs","Assault",
            "Contact with venomous plant/animal","Intentional self-harm",
            "Other and unspecified external CoD", "Other injuries", "Exposure to force of nature", "Other injuries",
            "Road traffic accident")
Tuberculosis <- c("Pulmonary tuberculosis")
HIV <- c("HIV/AIDS","HIV/AIDS related death")
Other_adult <- c("Other and unspecified NCD", "Acute abdomen", "Acute cardiac disease", "Asthma", "Chronic obstructive pulmonary dis", "Chronic respiratory", 
                 "Diabetes","Diabetes mellitus","Epilepsy", "Ischemic heart disease", "Other and unspecified NCD", "Liver cirrhosis", "Renal failure", "Sickle cell with crisis", 
                 "Other and unspecified cardiac dis", "Severe malnutrition")
Malaria <- c("Malaria")
Diarrhea <- c("Diarrhoeal diseases")
Pneumonia <- c("Acute resp infect incl pneumonia")
Other_infections_adult <- c("Meningitis and encephalitis", "Other and unspecified infections", "Pertussis", "Sepsis (non-obstetric)", "Hemorrhagic fever (non-dengue)","Haemorrhagic fever (non-dengue)",
                            "Other and unspecified infect dis","Dengue fever","Measles","Tetanus")
Stroke <- c("Stroke")









########### categorize deaths, adults 5-14
# unique(COD.5to14$InsilicoVA)

# COD.5to14 <- subset(COD.5to14, sex == "male")
# COD.5to14 <- subset(COD.5to14, sex == "female")

# COD.5to14$InterVA_sm_cause <- COD.5to14$InterVA
COD.5to14$InsilicoVA_sm_cause <- COD.5to14$InsilicoVA

# # COD.5to14$InterVA[COD.5to14$InterVA %in% Maternal] <- "Maternal"
# COD.5to14$InterVA[COD.5to14$InterVA %in% Maternal] <- "Other"
# COD.5to14$InterVA[COD.5to14$InterVA %in% Cancer] <- "Cancer"
# COD.5to14$InterVA[COD.5to14$InterVA %in% Injury] <- "Injury"
# COD.5to14$InterVA[COD.5to14$InterVA %in% Tuberculosis] <- "Tuberculosis"
# COD.5to14$InterVA[COD.5to14$InterVA %in% HIV] <- "HIV"
# COD.5to14$InterVA[COD.5to14$InterVA %in% Other_adult] <- "Other"
# COD.5to14$InterVA[COD.5to14$InterVA %in% Malaria] <- "Malaria"
# COD.5to14$InterVA[COD.5to14$InterVA %in% Diarrhea] <- "Diarrhea"
# COD.5to14$InterVA[COD.5to14$InterVA %in% Pneumonia] <- "Pneumonia"
# COD.5to14$InterVA[COD.5to14$InterVA %in% Stroke] <- "Other"
# COD.5to14$InterVA[COD.5to14$InterVA %in% Other_infections_adult] <- "Other infections"
# COD.5to14$InterVA[COD.5to14$InterVA=="Undetermined"] <- "Unspecified"
# table(COD.5to14$InterVA,COD.5to14$InterVA, exclude = NULL)

# COD.5to14$InsilicoVA[COD.5to14$InsilicoVA %in% Maternal] <- "Maternal"
COD.5to14$InsilicoVA[COD.5to14$InsilicoVA %in% Maternal] <- "Other"
COD.5to14$InsilicoVA[COD.5to14$InsilicoVA %in% Cancer] <- "Cancer"
COD.5to14$InsilicoVA[COD.5to14$InsilicoVA %in% Injury] <- "Injury"
COD.5to14$InsilicoVA[COD.5to14$InsilicoVA %in% Tuberculosis] <- "Tuberculosis"
COD.5to14$InsilicoVA[COD.5to14$InsilicoVA %in% HIV] <- "HIV"
COD.5to14$InsilicoVA[COD.5to14$InsilicoVA %in% Other_adult] <- "Other"
COD.5to14$InsilicoVA[COD.5to14$InsilicoVA %in% Malaria] <- "Malaria"
COD.5to14$InsilicoVA[COD.5to14$InsilicoVA %in% Diarrhea] <- "Diarrhea"
COD.5to14$InsilicoVA[COD.5to14$InsilicoVA %in% Pneumonia] <- "Pneumonia"
COD.5to14$InsilicoVA[COD.5to14$InsilicoVA %in% Stroke] <- "Other"
COD.5to14$InsilicoVA[COD.5to14$InsilicoVA %in% Other_infections_adult] <- "Other infections"
COD.5to14$InsilicoVA[COD.5to14$InsilicoVA=="Undetermined"] <- "Unspecified"
table(COD.5to14$InsilicoVA,COD.5to14$InsilicoVA, exclude = NULL)

head(COD.5to14)

COD.5to14 <- COD.5to14[,c("ID","InsilicoVA_sm_cause","InsilicoVA","sex","province","age_in_days","date_of_death")]
dim(COD.5to14)
write.csv(COD.5to14, file.path(file,"/Results/COD.5to14.csv"), row.names = FALSE)













########### categorize deaths, adults 15-49
# unique(COD.15to49$InsilicoVA)

# COD.15to49 <- subset(COD.15to49, sex == "male")
# COD.15to49 <- subset(COD.15to49, sex == "female")

# COD.15to49$InterVA_sm_cause <- COD.15to49$InterVA
COD.15to49$InsilicoVA_sm_cause <- COD.15to49$InsilicoVA

# COD.15to49$InterVA[COD.15to49$InterVA %in% Maternal] <- "Maternal"
# COD.15to49$InterVA[COD.15to49$InterVA %in% Cancer] <- "Cancer"
# COD.15to49$InterVA[COD.15to49$InterVA %in% Injury] <- "Injury"
# COD.15to49$InterVA[COD.15to49$InterVA %in% Tuberculosis] <- "Tuberculosis"
# COD.15to49$InterVA[COD.15to49$InterVA %in% HIV] <- "HIV"
# COD.15to49$InterVA[COD.15to49$InterVA %in% Other_adult] <- "Other"
# COD.15to49$InterVA[COD.15to49$InterVA %in% Stroke] <- "Other"
# COD.15to49$InterVA[COD.15to49$InterVA %in% Malaria] <- "Other infections"
# COD.15to49$InterVA[COD.15to49$InterVA %in% Diarrhea] <- "Other infections"
# COD.15to49$InterVA[COD.15to49$InterVA %in% Pneumonia] <- "Other infections"
# COD.15to49$InterVA[COD.15to49$InterVA %in% Other_infections_adult] <- "Other infections"
# COD.15to49$InterVA[COD.15to49$InterVA=="Undetermined"] <- "Unspecified"
# table(COD.15to49$InterVA,COD.15to49$InterVA, exclude = NULL)

COD.15to49$InsilicoVA[COD.15to49$InsilicoVA %in% Maternal] <- "Maternal"
COD.15to49$InsilicoVA[COD.15to49$InsilicoVA %in% Cancer] <- "Cancer"
COD.15to49$InsilicoVA[COD.15to49$InsilicoVA %in% Injury] <- "Injury"
COD.15to49$InsilicoVA[COD.15to49$InsilicoVA %in% Tuberculosis] <- "Tuberculosis"
COD.15to49$InsilicoVA[COD.15to49$InsilicoVA %in% HIV] <- "HIV"
COD.15to49$InsilicoVA[COD.15to49$InsilicoVA %in% Other_adult] <- "Other"
COD.15to49$InsilicoVA[COD.15to49$InsilicoVA %in% Stroke] <- "Other"
COD.15to49$InsilicoVA[COD.15to49$InsilicoVA %in% Malaria] <- "Other infections"
COD.15to49$InsilicoVA[COD.15to49$InsilicoVA %in% Diarrhea] <- "Other infections"
COD.15to49$InsilicoVA[COD.15to49$InsilicoVA %in% Pneumonia] <- "Other infections"
COD.15to49$InsilicoVA[COD.15to49$InsilicoVA %in% Other_infections_adult] <- "Other infections"
COD.15to49$InsilicoVA[COD.15to49$InsilicoVA=="Undetermined"] <- "Unspecified"
table(COD.15to49$InsilicoVA,COD.15to49$InsilicoVA, exclude = NULL)

head(COD.15to49)

COD.15to49 <- COD.15to49[,c("ID","InsilicoVA_sm_cause","InsilicoVA","sex","province","age_in_days","date_of_death")]
dim(COD.15to49)
write.csv(COD.15to49, file.path(file,"/Results/COD.15to49.csv"), row.names = FALSE)

















########### categorize deaths, adults 50plus
# unique(COD.50$InsilicoVA)

# COD.50 <- subset(COD.50, sex == "male")
# COD.50 <- subset(COD.50, sex == "female")

# COD.50$InterVA_sm_cause <- COD.50$InterVA
COD.50$InsilicoVA_sm_cause <- COD.50$InsilicoVA

# COD.50$InterVA[COD.50$InterVA %in% Stroke] <- "Stroke"
# COD.50$InterVA[COD.50$InterVA %in% Cancer] <- "Cancer"
# COD.50$InterVA[COD.50$InterVA %in% Injury] <- "Injury"
# COD.50$InterVA[COD.50$InterVA %in% Tuberculosis] <- "Tuberculosis"
# COD.50$InterVA[COD.50$InterVA %in% HIV] <- "HIV"
# COD.50$InterVA[COD.50$InterVA %in% Other_adult] <- "Other"
# COD.50$InterVA[COD.50$InterVA %in% Maternal] <- "Other"
# COD.50$InterVA[COD.50$InterVA %in% Malaria] <- "Other infections"
# COD.50$InterVA[COD.50$InterVA %in% Diarrhea] <- "Other infections"
# COD.50$InterVA[COD.50$InterVA %in% Pneumonia] <- "Other infections"
# COD.50$InterVA[COD.50$InterVA %in% Other_infections_adult] <- "Other infections"
# COD.50$InterVA[COD.50$InterVA=="Undetermined"] <- "Unspecified"
# table(COD.50$InterVA, COD.50$InterVA, exclude=NULL)


COD.50$InsilicoVA[COD.50$InsilicoVA %in% Stroke] <- "Stroke"
COD.50$InsilicoVA[COD.50$InsilicoVA %in% Cancer] <- "Cancer"
COD.50$InsilicoVA[COD.50$InsilicoVA %in% Injury] <- "Injury"
COD.50$InsilicoVA[COD.50$InsilicoVA %in% Tuberculosis] <- "Tuberculosis"
COD.50$InsilicoVA[COD.50$InsilicoVA %in% HIV] <- "HIV"
COD.50$InsilicoVA[COD.50$InsilicoVA %in% Other_adult] <- "Other"
COD.50$InsilicoVA[COD.50$InsilicoVA %in% Maternal] <- "Other"
COD.50$InsilicoVA[COD.50$InsilicoVA %in% Malaria] <- "Other infections"
COD.50$InsilicoVA[COD.50$InsilicoVA %in% Diarrhea] <- "Other infections"
COD.50$InsilicoVA[COD.50$InsilicoVA %in% Pneumonia] <- "Other infections"
COD.50$InsilicoVA[COD.50$InsilicoVA %in% Other_infections_adult] <- "Other infections"
COD.50$InsilicoVA[COD.50$InsilicoVA=="Undetermined"] <- "Unspecified"
table(COD.50$InsilicoVA, COD.50$InsilicoVA, exclude=NULL)

head(COD.50)

COD.50 <- COD.50[,c("ID","InsilicoVA_sm_cause","InsilicoVA","sex","province","age_in_days","date_of_death")]
dim(COD.50)
write.csv(COD.50, file.path(file,"/Results/COD.50plus.csv"), row.names = FALSE)







