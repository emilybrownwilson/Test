# Last edited: 17 Feb 2021
# Last run:    17 Feb 2021
# Ojbective: merge neonate CODs with Henry's new VASA data and follow-up with Ashley's 2/11 and 2/16 emails

rm(list=ls())

################################### READ IN DATA - COD and raw VASA
start <- Sys.time()
file <- getwd()
file
COD.0to27days <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210126Li/Results/COD.0to27days.csv")
COD.stillbirths <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210126Li/Results/COD.stillbirths.csv")

names(COD.0to27days)[names(COD.0to27days) == 'ID'] <- 'comsa_id'
names(COD.stillbirths)[names(COD.stillbirths) == 'ID'] <- 'comsa_id'

VASAvars <- read.csv(file.path(file,"eng_ext_mat_SBNN_VASA_HKformat.csv"))  

neonates <- merge(COD.0to27days,VASAvars, by=c("comsa_id"))
dim(COD.0to27days)
dim(VASAvars)
dim(neonates)

stillbirths <- merge(COD.stillbirths,VASAvars, by=c("comsa_id"))
dim(COD.stillbirths)
dim(VASAvars)
dim(stillbirths)



#################################################################
# Retrieve variables age_neonate_minutes and age_neonate_hours
COMSAdata <- read.csv(file.path(file,"Data/all_WHO.csv"), stringsAsFactors = FALSE)
age_vars <- COMSAdata[,c("comsa_id","age_neonate_minutes","age_neonate_hours")]
head(age_vars)
# write.csv(age_vars,"share/additional_age_vars.csv", row.names = FALSE )

# Check province 
COMSAdata <- read.csv(file.path(file,"Data/all_WHO_wgt.csv"), stringsAsFactors = FALSE)
check_province <- subset(COMSAdata, comsa_id %in% c(6766, 4011)) # Tete

# Why does comsa_ID 4011 not have a COD?
load(file.path(file,"/Data/openVA_comsa.Rdata"))
rm(list = ls()[!ls() %in% c("codeVAInsilico.neonate")])
library(openVA)
test <- getTopCOD(codeVAInsilico.neonate)
case.4011 <- subset(test, ID %in% c(4011))
case.4011 # dropped by openVA but has a death in EAVA








# Check cases not merging
start <- Sys.time()
file <- getwd()
file

COD.0to27days <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210126Li/Results/COD.0to27days.csv")

#START HERE
original.COD.0to27days <- read.csv("~/Desktop/COMSA Analysis/VA Analysis/Results/COD.0to27days.csv")
COD.0to27days2 <- read.csv("/Users/EWilson/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210128Ashley/Data/eava_neonate_comsa.csv")
head(original.COD.0to27days)
head(COD.0to27days2)

test <- merge(original.COD.0to27days,COD.0to27days2,by="ID")
dim(original.COD.0to27days)
dim(COD.0to27days2)
dim(test)

setdiff(original.COD.0to27days$ID,COD.0to27days2$ID)
setdiff(COD.0to27days2$ID,original.COD.0to27days$ID)


all <- read.csv("/Users/EWilson/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210128Ashley/Data/all_WHO.csv")
names(all)[names(all) == 'comsa_id'] <- 'ID'
test <- merge(original.COD.0to27days,all,by="ID")
dim(original.COD.0to27days)
dim(all)
dim(test)

test <- merge(COD.0to27days2,all,by="ID")
dim(COD.0to27days2)
dim(all)
dim(test)

# COD.stillbirths <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210126Li/Results/COD.stillbirths.csv")
# test <- merge(COD.stillbirths,COD.0to27days2,by="ID")

VASAvars2 <- read.csv(file.path(file,"eng_ext_mat_sbNeonatal_VASA_newest_sorted.csv"))  
names(VASAvars2)[names(VASAvars2) == 'comsa_id'] <- 'ID'
VASAvars <- read.csv(file.path(file,"eng_ext_mat_SBNN_VASA_HKformat.csv"))  
names(VASAvars)[names(VASAvars) == 'comsa_id'] <- 'ID'

dim(VASAvars2)
dim(VASAvars)
# test <- merge(VASAvars2,all, by=c("ID"))
# dim(VASAvars2)
# dim(all)
# dim(test) # 5 VASA cases do not merge with all WHO

class(VASAvars2$ID)
class(COD.0to27days2$ID)
test <- merge(VASAvars2,COD.0to27days2, by=c("ID"))
dim(VASAvars2)
dim(COD.0to27days2)
dim(test)

# Previously...
test <- merge(original.COD.0to27days,VASAvars,by=c("ID"))
dim(original.COD.0to27days)
dim(VASAvars)
dim(test)
test <- merge(original.COD.0to27days,VASAvars2,by=c("ID"))
dim(original.COD.0to27days)
dim(VASAvars2)
dim(test)

test <- merge(COD.0to27days2,VASAvars,by=c("ID"))
dim(COD.0to27days2)
dim(VASAvars)
dim(test)
test <- merge(COD.0to27days2,VASAvars2,by=c("ID"))
dim(COD.0to27days2)
dim(VASAvars2)
dim(test)

test <- merge(original.COD.0to27days,COD.0to27days2,by=c("ID")) # There could be a lot of conflation with what InsilicoVA calls a stillbirth and 3 question criteria
dim(COD.0to27days)
dim(COD.0to27days2)
dim(test)

original.EAVA <- read.csv("/Users/EWilson/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/Data/eava_neonate_comsa.csv")
dim(original.EAVA)
dim(COD.0to27days)
dim(COD.0to27days2)
test <- merge(original.EAVA,COD.0to27days,by=c("ID")) # There could be a lot of conflation with what InsilicoVA calls a stillbirth and 3 question criteria
dim(original.EAVA)
dim(COD.0to27days)
dim(test)
test <- merge(original.EAVA,COD.0to27days2,by=c("ID")) # There could be a lot of conflation with what InsilicoVA calls a stillbirth and 3 question criteria
dim(original.EAVA)
dim(COD.0to27days2)
dim(test)


