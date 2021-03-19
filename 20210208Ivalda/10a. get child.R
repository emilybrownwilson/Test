# Last edited: 11 Feb 2021
# Last run:    11 Feb 2021

# Objective: get input for VA analysis

rm(list = ls())

library(reshape2)
library(dplyr)
library(stringr)

'%!in%' <- function(x,y)!('%in%'(x,y))

########################################################### Reformat single cause matrices to get VA analysis data
file <- getwd()
file
#### COMSA
InsilicoVA <- readRDS(file ="Results/single_insilicova_child_comsa.rds")
class(InsilicoVA)
dim(InsilicoVA)
head(InsilicoVA)
InsilicoVA <- melt(InsilicoVA, id.vars=c())
InsilicoVA <- subset(InsilicoVA, value==1)
names(InsilicoVA)[names(InsilicoVA) == 'Var1'] <- 'ID'
names(InsilicoVA)[names(InsilicoVA) == 'Var2'] <- 'InsilicoVA'
InsilicoVA$value <- NULL

data <- InsilicoVA

head(data)

data$ID <- as.character(data$ID)




############## ADD sex, province
COMSAdata <- read.csv(file.path(file,"Data/all_WHO_wgt.csv"), stringsAsFactors = FALSE)
names(COMSAdata)[names(COMSAdata) == 'comsa_id'] <- 'ID'
names(COMSAdata)[names(COMSAdata) == 'id10019'] <- 'sex'

COMSAdata <- COMSAdata[,c("ID","sex","province")]

data.all <- data
dim(data.all)
data <- merge(data,COMSAdata, by=c("ID"))
dim(data)






############## Why are some cases getting dropped?
# library(dplyr)
# COMSAdata$ID <- as.character(COMSAdata$ID)
# COMSAdata$ID <- trimws(COMSAdata$ID)
# cases.not.merging <- anti_join(data.all,COMSAdata)
# dim(cases.not.merging)
# these.ids <- cases.not.merging$ID
# 
# OG <- read.csv(file.path(file,"Data/all_WHO.csv"), stringsAsFactors = FALSE)
# class(OG$comsa_id)
# OG$comsa_id <- as.character(OG$comsa_id)
# are.they.there <- subset(OG, comsa_id %in% these.ids)
# 
# 
# OG.2 <- read.csv(file.path(file,"Data/all_WHO_wgt.csv"), stringsAsFactors = FALSE)
# class(OG.2$comsa_id)
# OG.2$comsa_id <- as.character(OG.2$comsa_id)
# 
# are.they.there <- subset(OG, comsa_id %in% these.ids)



############## ADD age
COMSAdata_age <- read.csv(file.path(file,"Data/all_WHO_with_age.csv"), stringsAsFactors = FALSE)
hist(COMSAdata_age$ageatdeath)
names(COMSAdata_age)[names(COMSAdata_age) == 'comsa_id'] <- 'ID'
names(COMSAdata_age)[names(COMSAdata_age) == 'ageatdeath'] <- 'age_in_days'
names(COMSAdata_age)[names(COMSAdata_age) == 'id10023'] <- 'date_of_death'

COMSAdata_age <- COMSAdata_age[,c("ID","age_in_days","date_of_death")]
COMSAdata <- merge(COMSAdata, COMSAdata_age, by=c("ID"))

data <- merge(data,COMSAdata, by=c("ID","sex","province"))
dim(data)
data1 <- data %>%filter(str_detect(date_of_death, "2019"))
data2 <- data %>% filter(str_detect(date_of_death, "2020"))
dim(data1)
head(data1)
dim(data2)
head(data2)

data <- rbind(data1,data2)
dim(data)
############ check ages are in range
summary(data)  # age range approx: 28-1814
############

data <- data[,c("ID","InsilicoVA","sex","province","age_in_days","date_of_death")]
dim(data)
write.csv(data, "Results/COD.1to59months.csv", row.names = FALSE)









############ add small causes

rm(list = ls())

library(openVA)
library(reshape2)
library(dplyr)

file <- getwd()

load(file.path(file,"/Data/openVA_comsa.Rdata"))

'%!in%' <- function(x,y)!('%in%'(x,y))

### keep only objects we're using
rm(list = ls()[!ls() %in% c("codeVAInsilico.neonate", "codeVAInsilico.ch1_59m")])

file <- getwd()
file

Insilico.COD.ch1_59 <- data.frame(lapply(getTopCOD(codeVAInsilico.ch1_59m),as.character), stringsAsFactors = FALSE) 

################################## reformat original COD
# names(InterVA.COD.ch1_59)[names(InterVA.COD.ch1_59) == 'cause'] <- 'InterVA_sm_cause'
# InterVA.COD.ch1_59$ID <- trimws(InterVA.COD.ch1_59$ID)
names(Insilico.COD.ch1_59)[names(Insilico.COD.ch1_59) == 'cause'] <- 'InsilicoVA_sm_cause'
Insilico.COD.ch1_59$ID <- trimws(Insilico.COD.ch1_59$ID)
# names(EAVA.COD.ch1_59)[names(EAVA.COD.ch1_59) == 'allexpertdxs'] <- 'EAVA_sm_cause'
# EAVA.COD.ch1_59$ID <- trimws(EAVA.COD.ch1_59$ID)

################################## Compile and write out results
COD.ch1_59 <- read.csv(file.path(file,"/Results/COD.1to59months.csv"), stringsAsFactors = FALSE)

# COD.ch1_59 <- merge(COD.ch1_59,InterVA.COD.ch1_59, by=c("ID"))
COD.ch1_59 <- merge(COD.ch1_59,Insilico.COD.ch1_59, by=c("ID"))
# COD.ch1_59 <- merge(COD.ch1_59,EAVA.COD.ch1_59, by=c("ID"))

head(COD.ch1_59)
COD.ch1_59$age_in_years <- COD.ch1_59$age_in_days/365.25
COD.ch1_59 <- COD.ch1_59[,c("ID","InsilicoVA_sm_cause","InsilicoVA","sex","province","age_in_days","date_of_death")]

head(COD.ch1_59)
############ check ages are in range
summary(COD.ch1_59)  # age range approx: 28-1782
############
head(COD.ch1_59)
dim(COD.ch1_59)
write.csv(COD.ch1_59, "Results/COD.1to59months.csv", row.names = FALSE)



