# Last edited: 04 Mar 2021
# Last run:    04 Mar 2021
# Ojbective: merge neonate CODs with Henry's former VASA data until ID issue is resolved

rm(list=ls())

################################### READ IN DATA - COD and raw VASA
start <- Sys.time()
file <- getwd()
file
COD.0to27days <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210126Li/Results/COD.0to27days.csv")
COD.stillbirths <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210126Li/Results/COD.stillbirths.csv")
EAVA.0to27days <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210128Ashley/share/eava_neonate_comsa.csv")

names(COD.0to27days)[names(COD.0to27days) == 'ID'] <- 'comsa_id'
names(COD.stillbirths)[names(COD.stillbirths) == 'ID'] <- 'comsa_id'
names(EAVA.0to27days)[names(EAVA.0to27days) == 'ID'] <- 'comsa_id'

VASAvars <- read.csv("/Users/EWilson/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210128Ashley/eng_ext_mat_SBNN_VASA_HKformat.csv")

neonates <- merge(COD.0to27days,VASAvars, by=c("comsa_id"))
dim(COD.0to27days)
dim(VASAvars)
dim(neonates)

neonates2 <- merge(EAVA.0to27days,VASAvars, by=c("comsa_id"))
dim(EAVA.0to27days)
dim(VASAvars)
dim(neonates2)

setdiff(neonates$comsa_id,neonates2$comsa_id)
setdiff(neonates2$comsa_id,neonates$comsa_id)

stillbirths <- merge(COD.stillbirths,VASAvars, by=c("comsa_id"))
dim(COD.stillbirths)
dim(VASAvars)
dim(stillbirths)

# VASAvars2 <- read.csv(file.path(file,"eng_ext_mat_sbNeonatal_VASA_newest_sorted.csv"))  
# neonates2 <- merge(COD.0to27days,VASAvars2, by=c("comsa_id"))
# dim(COD.0to27days)
# dim(VASAvars2)
# dim(neonates)

# How many neonates merge with last run of COMSA
last.nn.run <- readRDS("/Users/EWilson/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/Results/20200808_comsa_data/single_eava_neonate_comsa.rds")
last.nn.run$comsa_id <- row.names(last.nn.run)
test <- merge(last.nn.run,VASAvars, by=c("comsa_id"))
head(test)

setdiff(neonates2$comsa_id,test$comsa_id)
setdiff(test$comsa_id,neonates2$comsa_id)

#################################################################
# Retrieve variables age_neonate_minutes and age_neonate_hours
COMSAdata <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210126Li/Data/all_WHO.csv", stringsAsFactors = FALSE)
age_vars <- COMSAdata[,c("comsa_id","age_neonate_minutes","age_neonate_hours")]
head(age_vars)
# write.csv(age_vars,"share/20210304additional_age_vars.csv", row.names = FALSE )

# Check province 
COMSAdata <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210126Li/Data/all_WHO_wgt.csv", stringsAsFactors = FALSE)
check_province <- subset(COMSAdata, comsa_id %in% c(6766, 4011)) # Tete

head(age_vars)
neonates2 <- merge(neonates,age_vars, by=c("comsa_id"))
dim(neonates2)
dim(age_vars)
dim(neonates)

stillbirths2 <- merge(stillbirths,age_vars, by=c("comsa_id"))
dim(stillbirths2)
dim(age_vars)
dim(stillbirths)



