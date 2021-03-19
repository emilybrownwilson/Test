# Last edited: 26 Jan 2021
# Last run:    26 Jan 2021
# Ojbective: merge neonate CODs with Ashley's VASA data

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


