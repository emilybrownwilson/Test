# Last edited: 2 Aug 2020
# Last run:    8 Aug 2020

# Objective: format multi cause va (interva and insilicova) comsa

library(openVA)

# FROM EMILY'S FILE
file <- getwd()
load(file.path(file,"/Data/openVA_comsa.Rdata"))

### keep only objects we're using
rm(list = ls()[!ls() %in% c("InterVA5.neonate", "InterVA5.ch1_59","codeVAInsilico.neonate","codeVAInsilico.ch1_59")])
file <- getwd()

# LOOK INTO CASE 1435 IN single_interva_neonate_comsa
test.nn1 <- getTopCOD(InterVA5.neonate)
test.nn2 <- getIndivProb(InterVA5.neonate)

test.1435.1 <- subset(test.nn1, ID==1435)
test.1435.2 <- subset(test.nn2, rownames(test.nn2)==1435)

test.1435.1 # prematurity
test.1435.2 # prematurity and birth asphyxia (equal probability)



# LOOK INTO CASE 4343 IN single_interva_child_comsa
test.ch1 <- getTopCOD(InterVA5.ch1_59)
test.ch2 <- getIndivProb(InterVA5.ch1_59)

test.4343.1 <- subset(test.ch1, ID==4343)
test.4343.2 <- subset(test.ch2, rownames(test.ch2)==4343)

test.4343.1 # HIV/AIDS related death
test.4343.2 # HIV/AIDS related death and severe malnutrition (equal probability)



# LOOK INTO CASE 4586 IN single_interva_child_comsa
test.4586.1 <- subset(test.ch1, ID==4586)
test.4586.2 <- subset(test.ch2, rownames(test.ch2)==4586)

test.4586.1 # severe malnutrition
test.4586.2 # diarhheal diseases and severe malnutrtion (equal probability)

data <- readRDS("/Users/EWilson/Desktop/Raw to CalibratedVA input pipeline/Results/20200802_comsa_data/single_interva_neonate_comsa.rds")
test <- subset(data, rownames(data) %in% c(1435))
test

data <- readRDS("/Users/EWilson/Desktop/Raw to CalibratedVA input pipeline/Results/20200802_comsa_data/single_interva_child_comsa.rds")
test <- subset(data, rownames(data) %in% c(4343,4586))
test






# FROM BRIAN'S FILE
load("/Users/EWilson/Desktop/Brian's Raw to CalibratedVA input pipeline/Data/openVA_comsa.Rdata")

### keep only objects we're using
rm(list = ls()[!ls() %in% c("InterVA5.neonate", "InterVA5.ch1_59","codeVAInsilico.neonate","codeVAInsilico.ch1_59")])

# LOOK INTO CASE 1435 IN single_interva_neonate_comsa
test.nn1 <- getTopCOD(InterVA5.neonate)
test.nn2 <- getIndivProb(InterVA5.neonate)

test.1435.1 <- subset(test.nn1, ID==1435)
test.1435.2 <- subset(test.nn2, rownames(test.nn2)==1435)

test.1435.1 # prematurity
test.1435.2 # prematurity and birth asphyxia (equal probability)



# LOOK INTO CASE 4343 IN single_interva_child_comsa
test.ch1 <- getTopCOD(InterVA5.ch1_59)
test.ch2 <- getIndivProb(InterVA5.ch1_59)

test.4343.1 <- subset(test.ch1, ID==4343)
test.4343.2 <- subset(test.ch2, rownames(test.ch2)==4343)

test.4343.1 # HIV
test.4343.2 # HIV and severe malnutrition (equal probability)



# LOOK INTO CASE 4586 IN single_interva_child_comsa
test.4586.1 <- subset(test.ch1, ID==4586)
test.4586.2 <- subset(test.ch2, rownames(test.ch2)==4586)

test.4586.1 # severe malnutrition
test.4586.2 # diarhheal diseases and severe malnutrtion (equal probability)

data <- readRDS("/Users/EWilson/Desktop/Brian's Raw to CalibratedVA input pipeline/Results/20200802_comsa_data/single_interva_neonate_comsa.rds")
test <- subset(data, rownames(data) %in% c(1435))
test

data <- readRDS("/Users/EWilson/Desktop/Brian's Raw to CalibratedVA input pipeline/Results/20200802_comsa_data/single_interva_child_comsa.rds")
test <- subset(data, rownames(data) %in% c(4343,4586))
test
