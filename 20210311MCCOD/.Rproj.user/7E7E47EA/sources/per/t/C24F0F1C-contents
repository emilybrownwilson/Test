# Last edited: 12 Mar 2021
# Last run:    12 Mar 2021

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
InsilicoVA <- readRDS(file ="Results/single_insilicova_neonate_comsa.rds")
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

EAVA <- readRDS(file ="Results/single_eava_neonate_comsa.rds")
class(EAVA)
head(EAVA)
EAVA$undecided <- NA
EAVA$undecided <- ifelse(EAVA$congenital_malformation %!in% c(0,1),1,0)
EAVA <- as.matrix(EAVA)
dim(EAVA)
head(EAVA)
EAVA <- melt(EAVA, id.vars=c())
EAVA <- subset(EAVA, value==1)
names(EAVA)[names(EAVA) == 'Var1'] <- 'ID'
names(EAVA)[names(EAVA) == 'Var2'] <- 'EAVA'
EAVA$value <- NULL

data <- merge(data,EAVA,by=c("ID"))

head(data)

data$ID <- as.character(data$ID)

data <- data %>% select(ID, InsilicoVA, EAVA)

dim(InsilicoVA)
dim(EAVA)
dim(data)
############## ADD sex, province
# COMSAdata <- read.csv(file.path(file,"Data/all_WHO_wgt.csv"), stringsAsFactors = FALSE)
# names(COMSAdata)[names(COMSAdata) == 'comsa_id'] <- 'ID'
# names(COMSAdata)[names(COMSAdata) == 'id10019'] <- 'sex'
# 
# COMSAdata <- COMSAdata[,c("ID","sex","province")]
# data <- merge(data,COMSAdata, by=c("ID"))

############## ADD age
COMSAdata_age <- read.csv(file.path(file,"Data/all_WHO_with_age.csv"), stringsAsFactors = FALSE)
hist(COMSAdata_age$ageatdeath)
names(COMSAdata_age)[names(COMSAdata_age) == 'comsa_id'] <- 'ID'
names(COMSAdata_age)[names(COMSAdata_age) == 'ageatdeath'] <- 'age_in_days'
# names(COMSAdata_age)[names(COMSAdata_age) == 'id10023'] <- 'date_of_death'
names(COMSAdata_age)[names(COMSAdata_age) == 'id10023_a'] <- 'date_of_death'

names(COMSAdata_age)[names(COMSAdata_age) == 'id10058'] <- 'place_of_death'


COMSAdata_age <- COMSAdata_age[,c("ID","age_in_days","date_of_death","place_of_death")]
COMSAdata <- merge(data, COMSAdata_age, by=c("ID"))

head(COMSAdata)
data <- COMSAdata
data <- data %>%
  filter(str_detect(date_of_death, "2019"))

head(data)
############ check ages are in range
summary(data)  # age range approx: 0-27
############

data <- data[,c("ID","InsilicoVA","EAVA","age_in_days","date_of_death","place_of_death")]

write.csv(data, "Results/COD.0to27days.csv", row.names = FALSE)

