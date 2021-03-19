# Last edited: 1 Dec 2020
# Last run:    1 Dec 2020

# Objective: get input for VA analysis

rm(list = ls())

library(reshape2)

'%!in%' <- function(x,y)!('%in%'(x,y))

########################################################### Reformat single cause matrices to get VA analysis data
file <- getwd()

#### COMSA
InsilicoVA <- readRDS(file ="Data/single_insilicova_child_comsa.rds")
class(InsilicoVA)
dim(InsilicoVA)
head(InsilicoVA)
InsilicoVA <- melt(InsilicoVA, id.vars=c())
InsilicoVA <- subset(InsilicoVA, value==1)
names(InsilicoVA)[names(InsilicoVA) == 'Var1'] <- 'ID'
names(InsilicoVA)[names(InsilicoVA) == 'Var2'] <- 'InsilicoVA'
InsilicoVA$value <- NULL

InterVA <- readRDS(file ="Data/single_interva_child_comsa.rds")
class(InterVA)
InterVA <- as.matrix(InterVA)
dim(InterVA)
head(InterVA)
InterVA <- melt(InterVA, id.vars=c())
InterVA <- subset(InterVA, value==1)
names(InterVA)[names(InterVA) == 'Var1'] <- 'ID'
names(InterVA)[names(InterVA) == 'Var2'] <- 'InterVA'
InterVA$value <- NULL

EAVA <- readRDS(file ="Data/single_eava_child_comsa.rds")
class(EAVA)
EAVA$undecided <- NA
EAVA$undecided <- ifelse(EAVA$malaria %!in% c(0,1),1,0)
EAVA <- as.matrix(EAVA)
dim(EAVA)
head(EAVA)
EAVA <- melt(EAVA, id.vars=c())
EAVA <- subset(EAVA, value==1)
names(EAVA)[names(EAVA) == 'Var1'] <- 'ID'
names(EAVA)[names(EAVA) == 'Var2'] <- 'EAVA'
EAVA$value <- NULL

data <- merge(InterVA,InsilicoVA,by=c("ID"))
dim(data)
data <- merge(data,EAVA,by=c("ID"))

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
summary(data)  # age range approx: 28-1825
############

data <- data[,c("ID","InterVA","InsilicoVA","EAVA","sex","province","age_in_days")]

# write.csv(data, "Results/COD.1to59months.csv", row.names = FALSE)



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

InterVA.COD.ch1_59 <- data.frame(lapply(getTopCOD(InterVA5.ch1_59),as.character), stringsAsFactors = FALSE) 
Insilico.COD.ch1_59 <- data.frame(lapply(getTopCOD(codeVAInsilico.ch1_59),as.character), stringsAsFactors = FALSE) 
EAVA.COD.ch1_59 <- read.csv(file.path(file,"/Data/eava_child_comsa.csv"), stringsAsFactors = FALSE)

# Put InterVA and Insilico and EAVA together:
head(InterVA.COD.ch1_59)

head(Insilico.COD.ch1_59)

EAVA.COD.ch1_59 <- EAVA.COD.ch1_59[,c("ID","allexpertdxs")]
head(EAVA.COD.ch1_59)


################################## reformat original COD
names(InterVA.COD.ch1_59)[names(InterVA.COD.ch1_59) == 'cause'] <- 'InterVA_sm_cause'
InterVA.COD.ch1_59$ID <- trimws(InterVA.COD.ch1_59$ID)
names(Insilico.COD.ch1_59)[names(Insilico.COD.ch1_59) == 'cause'] <- 'InsilicoVA_sm_cause'
Insilico.COD.ch1_59$ID <- trimws(Insilico.COD.ch1_59$ID)
names(EAVA.COD.ch1_59)[names(EAVA.COD.ch1_59) == 'allexpertdxs'] <- 'EAVA_sm_cause'
EAVA.COD.ch1_59$ID <- trimws(EAVA.COD.ch1_59$ID)

################################## Compile and write out results
COD.ch1_59 <- read.csv(file.path(file,"/Results/COD.1to59months.csv"), stringsAsFactors = FALSE)

COD.ch1_59 <- merge(COD.ch1_59,InterVA.COD.ch1_59, by=c("ID"))
COD.ch1_59 <- merge(COD.ch1_59,Insilico.COD.ch1_59, by=c("ID"))
COD.ch1_59 <- merge(COD.ch1_59,EAVA.COD.ch1_59, by=c("ID"))

head(COD.ch1_59)

COD.ch1_59 <- COD.ch1_59[,c("ID","InterVA_sm_cause","InsilicoVA_sm_cause","EAVA_sm_cause","sex","province","age_in_days",
                            "InterVA","InsilicoVA","EAVA")]

head(COD.ch1_59)
############ check ages are in range
summary(COD.ch1_59)  # age range approx: 28-1782
############

write.csv(COD.ch1_59, "Results/COD.1to59months.specific.causes.csv", row.names = FALSE)






### 20201204 RE: Henry
data <- read.csv("Results/COD.1to59months.specific.causes.csv")
unique(data$InterVA_sm_cause)
unique(data$InsilicoVA_sm_cause)
unique(data$EAVA_sm_cause)

pertussis <- subset(data, InterVA_sm_cause %in% c("Pertussis") | InsilicoVA_sm_cause %in% c("Pertussis"))
View(pertussis)

write.csv(pertussis, "Results/COD.1to59months.pertussis.csv", row.names = FALSE)


### 20201204 RE: Hafiz
data <- read.csv("Results/COD.15to49.csv")

data <- subset(data, sex=="female")
table(data$InterVA_sm_cause)









