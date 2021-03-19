# Last edited: 8 Aug 2020
# Last run:    8 Aug 2020

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

write.csv(data, "Results/COD.1to59months.csv", row.names = FALSE)

