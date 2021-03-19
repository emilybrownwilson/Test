# Last edited: 26 Jan 2021
# Last run:    26 Jan 2021

# Objective: Produce broad cause graphs from individual COD, for each method

rm(list = ls())

library(openVA)
library(dplyr)
library(stringr)

file <- getwd()
load(file.path(file,"/Data/openVA_comsa.Rdata"))

'%!in%' <- function(x,y)!('%in%'(x,y))

### keep only objects we're using
rm(list = ls()[!ls() %in% c("codeVAInsilico.ch5_19y")])

file <- getwd()
file
########################################################### Start here for data visualization - no calibration for these age categories
Insilico.COD.ch5_19 <- data.frame(lapply(getTopCOD(codeVAInsilico.ch5_19y),as.character), stringsAsFactors = FALSE) 

# Put InterVA and Insilico together:
head(Insilico.COD.ch5_19)
COD.ch5to19 <- Insilico.COD.ch5_19
head(COD.ch5to19)

############## ADD sex, province
COMSAdata <- read.csv(file.path(file,"Data/all_WHO_wgt.csv"), stringsAsFactors = FALSE)
names(COMSAdata)[names(COMSAdata) == 'comsa_id'] <- 'ID'
names(COMSAdata)[names(COMSAdata) == 'id10019'] <- 'sex'

COMSAdata <- COMSAdata[,c("ID","sex","province")]

COD.5to19 <- merge(COD.ch5to19,COMSAdata, by=c("ID"))
head(COD.5to19)
############## ADD age
COMSAdata_age <- read.csv(file.path(file,"Data/all_WHO_with_age.csv"), stringsAsFactors = FALSE)
hist(COMSAdata_age$ageatdeath)
names(COMSAdata_age)[names(COMSAdata_age) == 'comsa_id'] <- 'ID'
names(COMSAdata_age)[names(COMSAdata_age) == 'ageatdeath'] <- 'age_in_days'
names(COMSAdata_age)[names(COMSAdata_age) == 'id10023'] <- 'date_of_death'

COMSAdata_age <- COMSAdata_age[,c("ID","age_in_days","date_of_death")]
COMSAdata <- merge(COMSAdata, COMSAdata_age, by=c("ID"))

COD.5to19 <- merge(COD.5to19,COMSAdata, by=c("ID","sex","province"))

# COD.5to19 <- COD.5to19 %>%
#   filter(str_detect(date_of_death, "2019"))


############ check ages are in range
summary(COD.5to19)  # age range approx: 1826-7296
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








########### categorize deaths, adults 5-19
names(COD.5to19)[names(COD.5to19) == 'cause'] <- 'InsilicoVA_sm_cause'

# COD.5to19$InsilicoVA[COD.5to19$InsilicoVA %in% Maternal] <- "Maternal"
COD.5to19$InsilicoVA[COD.5to19$InsilicoVA %in% Maternal] <- "Other"
COD.5to19$InsilicoVA[COD.5to19$InsilicoVA %in% Cancer] <- "Cancer"
COD.5to19$InsilicoVA[COD.5to19$InsilicoVA %in% Injury] <- "Injury"
COD.5to19$InsilicoVA[COD.5to19$InsilicoVA %in% Tuberculosis] <- "Tuberculosis"
COD.5to19$InsilicoVA[COD.5to19$InsilicoVA %in% HIV] <- "HIV"
COD.5to19$InsilicoVA[COD.5to19$InsilicoVA %in% Other_adult] <- "Other"
COD.5to19$InsilicoVA[COD.5to19$InsilicoVA %in% Malaria] <- "Malaria"
COD.5to19$InsilicoVA[COD.5to19$InsilicoVA %in% Diarrhea] <- "Diarrhea"
COD.5to19$InsilicoVA[COD.5to19$InsilicoVA %in% Pneumonia] <- "Pneumonia"
COD.5to19$InsilicoVA[COD.5to19$InsilicoVA %in% Stroke] <- "Other"
COD.5to19$InsilicoVA[COD.5to19$InsilicoVA %in% Other_infections_adult] <- "Other infections"
COD.5to19$InsilicoVA[COD.5to19$InsilicoVA=="Undetermined"] <- "Unspecified"
table(COD.5to19$InsilicoVA,COD.5to19$InsilicoVA, exclude = NULL)

head(COD.5to19)

COD.5to19$age_in_years <- COD.5to19$age_in_days/365.25
COD.5to19 <- COD.5to19[,c("ID","InsilicoVA_sm_cause","InsilicoVA","sex","province","age_in_days","age_in_years","date_of_death")]
head(COD.5to19)
write.csv(COD.5to19, file.path(file,"/Results/COD.5to19years.csv"), row.names = FALSE)

















