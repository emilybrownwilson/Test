# Last edited: 8 Aug 2020
# Last run:    8 Aug 2020

# Objective: get input for VA analysis

rm(list = ls())

library(reshape2)

'%!in%' <- function(x,y)!('%in%'(x,y))

########################################################### Reformat single cause matrices to get VA analysis data
file <- getwd()

#### COMSA
InsilicoVA <- readRDS(file ="Data/single_insilicova_neonate_comsa.rds")
class(InsilicoVA)
dim(InsilicoVA)
head(InsilicoVA)
InsilicoVA <- melt(InsilicoVA, id.vars=c())
InsilicoVA <- subset(InsilicoVA, value==1)
names(InsilicoVA)[names(InsilicoVA) == 'Var1'] <- 'ID'
names(InsilicoVA)[names(InsilicoVA) == 'Var2'] <- 'InsilicoVA'
InsilicoVA$value <- NULL

InterVA <- readRDS(file ="Data/single_interva_neonate_comsa.rds")
class(InterVA)
InterVA <- as.matrix(InterVA)
dim(InterVA)
head(InterVA)
InterVA <- melt(InterVA, id.vars=c())
InterVA <- subset(InterVA, value==1)
names(InterVA)[names(InterVA) == 'Var1'] <- 'ID'
names(InterVA)[names(InterVA) == 'Var2'] <- 'InterVA'
InterVA$value <- NULL

EAVA <- readRDS(file ="Data/single_eava_neonate_comsa.rds")
class(EAVA)
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

data <- merge(InterVA,InsilicoVA,by=c("ID"))
data$ID <- trimws(data$ID)
data <- merge(data,EAVA,by=c("ID"))
dim(data)
head(data)

data$ID <- as.character(data$ID)

data <- data %>% select(ID, InterVA, InsilicoVA, EAVA)


############## ADD sex, province
COMSAdata <- read.csv(file.path(file,"Data/all_WHO_wgt.csv"), stringsAsFactors = FALSE)
names(COMSAdata)[names(COMSAdata) == 'comsa_id'] <- 'ID'
names(COMSAdata)[names(COMSAdata) == 'id10019'] <- 'sex'

COMSAdata <- COMSAdata[,c("ID","sex","province")]
data <- merge(data,COMSAdata, by=c("ID"))

############## ADD age
COMSAdata_age <- read.csv(file.path(file,"Data/all_WHO_with_age.csv"), stringsAsFactors = FALSE)
hist(COMSAdata_age$ageatdeath)
names(COMSAdata_age)[names(COMSAdata_age) == 'comsa_id'] <- 'ID'
names(COMSAdata_age)[names(COMSAdata_age) == 'ageatdeath'] <- 'age_in_days'

COMSAdata_age <- COMSAdata_age[,c("ID","age_in_days")]
COMSAdata <- merge(COMSAdata, COMSAdata_age, by=c("ID"))

data <- merge(data,COMSAdata, by=c("ID","sex","province"))

head(data)
############ check ages are in range
summary(data)  # age range approx: <=27 days
############

data <- data[,c("ID","InterVA","InsilicoVA","EAVA","sex","province","age_in_days")]

write.csv(data, "Results/COD.0to27days.csv", row.names = FALSE)



############ add small causes

rm(list = ls())

library(openVA)
library(reshape2)
library(dplyr)

file <- getwd()

load(file.path(file,"/Data/openVA_comsa.Rdata"))

'%!in%' <- function(x,y)!('%in%'(x,y))

### keep only objects we're using
rm(list = ls()[!ls() %in% c("InterVA5.neonate", "InterVA5.ch1_59",
                            "codeVAInsilico.neonate", "codeVAInsilico.ch1_59")])

file <- getwd()
file

InterVA.COD.neonate <- data.frame(lapply(getTopCOD(InterVA5.neonate),as.character), stringsAsFactors = FALSE) 
Insilico.COD.neonate <- data.frame(lapply(getTopCOD(codeVAInsilico.neonate),as.character), stringsAsFactors = FALSE) 
EAVA.COD.neonate <- read.csv(file.path(file,"/Data/eava_neonate_comsa.csv"), stringsAsFactors = FALSE)

# Put InterVA and Insilico and EAVA together:
head(InterVA.COD.neonate)

head(Insilico.COD.neonate)

EAVA.COD.neonate <- EAVA.COD.neonate[,c("ID","allexpertdxs1")]
head(EAVA.COD.neonate)

dim(InterVA.COD.neonate)
dim(Insilico.COD.neonate)
dim(EAVA.COD.neonate)
################################## reformat original COD
names(InterVA.COD.neonate)[names(InterVA.COD.neonate) == 'cause'] <- 'InterVA_sm_cause'
InterVA.COD.neonate$ID <- trimws(InterVA.COD.neonate$ID)
names(Insilico.COD.neonate)[names(Insilico.COD.neonate) == 'cause'] <- 'InsilicoVA_sm_cause'
Insilico.COD.neonate$ID <- trimws(Insilico.COD.neonate$ID)
names(EAVA.COD.neonate)[names(EAVA.COD.neonate) == 'allexpertdxs1'] <- 'EAVA_sm_cause'
EAVA.COD.neonate$ID <- trimws(EAVA.COD.neonate$ID)

################################## Compile and write out results
COD.neonate <- read.csv(file.path(file,"/Results/COD.0to27days.csv"), stringsAsFactors = FALSE)
dim(COD.neonate)

COD.neonate <- merge(COD.neonate,InterVA.COD.neonate, by=c("ID"))
COD.neonate <- merge(COD.neonate,Insilico.COD.neonate, by=c("ID"))
COD.neonate <- merge(COD.neonate,EAVA.COD.neonate, by=c("ID"))

head(COD.neonate)

COD.neonate <- COD.neonate[,c("ID","InterVA_sm_cause","InsilicoVA_sm_cause","EAVA_sm_cause","sex","province","age_in_days",
                            "InterVA","InsilicoVA","EAVA")]

head(COD.neonate)
############ check ages are in range
summary(COD.neonate)  # age range approx: 0-27
############

write.csv(COD.neonate, "Results/COD.0to27days.specific.causes.csv", row.names = FALSE)






######### 2 Dec 2020 - RE: Henry

neonate <- read.csv("Results/COD.0to27days.specific.causes.csv",stringsAsFactors = FALSE)
child <- read.csv("Results/COD.1to59months.specific.causes.csv",stringsAsFactors = FALSE)

head(neonate)
head(child)

# question 1
unique(neonate$EAVA_sm_cause)
unique(child$EAVA_sm_cause)

file <- getwd()
EAVA.COD.neonate <- read.csv(file.path(file,"/Data/eava_neonate_comsa.csv"), stringsAsFactors = FALSE)
EAVA.COD.child <- read.csv(file.path(file,"/Data/eava_child_comsa.csv"), stringsAsFactors = FALSE)

unique(EAVA.COD.neonate$allexpertdxs1)
unique(EAVA.COD.child$allexpertdxs)

NN_small_X_broad_cause_table <- table(neonate$EAVA_sm_cause, neonate$EAVA)
write.csv(NN_small_X_broad_cause_table,"Results/NN_small_X_broad_cause_table.csv")

CH_small_X_broad_cause_table <- table(child$EAVA_sm_cause, child$EAVA)
write.csv(CH_small_X_broad_cause_table,"Results/child_small_X_broad_cause_table.csv")


# question 2 - corrected


######### 4 Dec 2020 - RE: Henry
file <- getwd()
data <- read.csv(file.path(file,"/Data/mits_neonate_champs.csv"))


######### 15 Feb 2021 - RE: Henry - NEONATES
data <- read.csv(file.path(file,"/Results/COD.0to27days.specific.causes.csv"))
dim(data)
head(data)
table(data$InsilicoVA_sm_cause, data$InsilicoVA)

COD.stillbirths <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210126Li/Results/COD.stillbirths.csv")
head(COD.stillbirths)

test <- merge(COD.stillbirths,data,by=c("ID"))
dim(test)

# merge: id10104 (did the baby ever cry?), id10109 (did the baby ever move?), id10110 (did the baby ever breathe?)
Stillbirth.vars <- read.csv(file.path(file,"Data/all_WHO.csv"), stringsAsFactors = FALSE)
# Stillbirth.vars <- Stillbirth.vars[,c("comsa_id","id10104","id10109","id10110")]
Stillbirth.vars <- Stillbirth.vars[,c("comsa_id","ageInDays","age_group","age_neonate_days")]
head(Stillbirth.vars)
names(Stillbirth.vars)[names(Stillbirth.vars) == 'comsa_id'] <- 'ID'

test <- merge(data,Stillbirth.vars, by=c("ID"))
dim(test)
head(test)
write.csv(test,"/Users/EWilson/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210215Henry/COD.0to27days.specific.causes.stillbirth.vars.csv", row.names = FALSE)

######### 15 Feb 2021 - RE: Henry - CHILD
data <- read.csv(file.path(file,"/Results/COD.1to59months.specific.causes.csv"))
dim(data)
head(data)
table(data$InsilicoVA_sm_cause, data$InsilicoVA)

COD.stillbirths <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210126Li/Results/COD.stillbirths.csv")
head(COD.stillbirths)

test <- merge(COD.stillbirths,data,by=c("ID"))
dim(test)

# merge: id10104 (did the baby ever cry?), id10109 (did the baby ever move?), id10110 (did the baby ever breathe?)
Stillbirth.vars <- read.csv(file.path(file,"Data/all_WHO.csv"), stringsAsFactors = FALSE)
# Stillbirth.vars <- Stillbirth.vars[,c("comsa_id","id10104","id10109","id10110")]
Stillbirth.vars <- Stillbirth.vars[,c("comsa_id","ageInDays","ageInMonths","ageInYears","age_group","age_child_unit","age_child_months","age_child_years")]
head(Stillbirth.vars)
names(Stillbirth.vars)[names(Stillbirth.vars) == 'comsa_id'] <- 'ID'

test <- merge(data,Stillbirth.vars, by=c("ID"))
dim(test)
write.csv(test,"/Users/EWilson/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210215Henry/COD.1to59months.specific.causes.stillbirth.vars.csv", row.names = FALSE)


