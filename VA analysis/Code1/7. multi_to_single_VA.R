# Last edited: 2 Aug 2020
# Last run:    2 Aug 2020

# Objective: convert multi to single cause VA, comsa and champs

library(openVA)
library(reshape2)
library(dplyr)

file <- getwd()

############################################################################################ EAVA single-cause, CHAMPS
model_broad_probs.EAVA.neonate.CHAMPS <- readRDS("Results/multi_eava_neonate_champs.rds")
model_broad_probs.EAVA.child.CHAMPS <- readRDS("Results/multi_eava_child_champs.rds")
head(model_broad_probs.EAVA.neonate.CHAMPS)
head(model_broad_probs.EAVA.child.CHAMPS)

single_cause_probs.EAVA.neonate.CHAMPS <- model_broad_probs.EAVA.neonate.CHAMPS
single_cause_probs.EAVA.neonate.CHAMPS[single_cause_probs.EAVA.neonate.CHAMPS==.25] <- 0.00
single_cause_probs.EAVA.neonate.CHAMPS[single_cause_probs.EAVA.neonate.CHAMPS==.75] <- 1.00

sum(rowSums(single_cause_probs.EAVA.neonate.CHAMPS))
dim(single_cause_probs.EAVA.neonate.CHAMPS)

single_cause_probs.EAVA.child.CHAMPS <- model_broad_probs.EAVA.child.CHAMPS
single_cause_probs.EAVA.child.CHAMPS[single_cause_probs.EAVA.child.CHAMPS==.25] <- 0.00
single_cause_probs.EAVA.child.CHAMPS[single_cause_probs.EAVA.child.CHAMPS==.75] <- 1.00

sum(rowSums(single_cause_probs.EAVA.child.CHAMPS))
dim(single_cause_probs.EAVA.child.CHAMPS)

saveRDS(single_cause_probs.EAVA.neonate.CHAMPS, file ="Results/single_eava_neonate_champs.rds")
saveRDS(single_cause_probs.EAVA.child.CHAMPS, file ="Results/single_eava_child_champs.rds")

############################################################################################ InterVA and InsilicoVA single-cause, CHAMPS
model_broad_probs.InterVA.neonates.CHAMPS <- readRDS("Results/multi_interva_neonate_champs.rds")
model_broad_probs.InterVA.child.CHAMPS <- readRDS("Results/multi_interva_child_champs.rds")
head(model_broad_probs.InterVA.neonates.CHAMPS)
head(model_broad_probs.InterVA.child.CHAMPS)
model_broad_probs.InsilicoVA.neonates.CHAMPS <- readRDS("Results/multi_insilicova_neonate_champs.rds")
model_broad_probs.InsilicoVA.child.CHAMPS <- readRDS("Results/multi_insilicova_child_champs.rds")
head(model_broad_probs.InsilicoVA.neonates.CHAMPS)
head(model_broad_probs.InsilicoVA.child.CHAMPS)

# InterVA
single_cause_probs.InterVA.neonates.CHAMPS <- model_broad_probs.InterVA.neonates.CHAMPS
single_cause_probs.InterVA.neonates.CHAMPS <- t(apply(model_broad_probs.InterVA.neonates.CHAMPS, 1, function(x) as.numeric(x == max(x))))
colnames(single_cause_probs.InterVA.neonates.CHAMPS) <- colnames(model_broad_probs.InterVA.neonates.CHAMPS)

head(model_broad_probs.InterVA.neonates.CHAMPS)
head(single_cause_probs.InterVA.neonates.CHAMPS)
tail(model_broad_probs.InterVA.neonates.CHAMPS)
tail(single_cause_probs.InterVA.neonates.CHAMPS)

# We are getting ipre=1 and prematurity=1 for case 32495080-FB1C-4408-ACA3-76D415C272DB? Because they're equally probable - should be prematurity, based on getTopCOD()
single_cause_probs.InterVA.neonates.CHAMPS <- as.data.frame(single_cause_probs.InterVA.neonates.CHAMPS)
single_cause_probs.InterVA.neonates.CHAMPS$ipre <- ifelse(rownames(single_cause_probs.InterVA.neonates.CHAMPS)=="32495080-FB1C-4408-ACA3-76D415C272DB", 0, single_cause_probs.InterVA.neonates.CHAMPS$ipre)
single_cause_probs.InterVA.neonates.CHAMPS <- as.matrix(single_cause_probs.InterVA.neonates.CHAMPS)
test <- subset(single_cause_probs.InterVA.neonates.CHAMPS, rownames(single_cause_probs.InterVA.neonates.CHAMPS)=="32495080-FB1C-4408-ACA3-76D415C272DB")
test

saveRDS(single_cause_probs.InterVA.neonates.CHAMPS, file ="Results/single_interva_neonate_champs.rds")


single_cause_probs.InterVA.child.CHAMPS <- model_broad_probs.InterVA.child.CHAMPS
single_cause_probs.InterVA.child.CHAMPS <- t(apply(model_broad_probs.InterVA.child.CHAMPS, 1, function(x) as.numeric(x == max(x))))
colnames(single_cause_probs.InterVA.child.CHAMPS) <- colnames(model_broad_probs.InterVA.child.CHAMPS)

head(model_broad_probs.InterVA.child.CHAMPS)
head(single_cause_probs.InterVA.child.CHAMPS)
tail(model_broad_probs.InterVA.child.CHAMPS)
tail(single_cause_probs.InterVA.child.CHAMPS)

saveRDS(single_cause_probs.InterVA.child.CHAMPS, file ="Results/single_interva_child_champs.rds")

# InsilicoVA
single_cause_probs.InsilicoVA.neonates.CHAMPS <- model_broad_probs.InsilicoVA.neonates.CHAMPS
single_cause_probs.InsilicoVA.neonates.CHAMPS <- t(apply(model_broad_probs.InsilicoVA.neonates.CHAMPS, 1, function(x) as.numeric(x == max(x))))
colnames(single_cause_probs.InsilicoVA.neonates.CHAMPS) <- colnames(model_broad_probs.InsilicoVA.neonates.CHAMPS)

head(model_broad_probs.InsilicoVA.neonates.CHAMPS)
head(single_cause_probs.InsilicoVA.neonates.CHAMPS)
tail(model_broad_probs.InsilicoVA.neonates.CHAMPS)
tail(single_cause_probs.InsilicoVA.neonates.CHAMPS)

saveRDS(single_cause_probs.InsilicoVA.neonates.CHAMPS, file ="Results/single_insilicova_neonate_champs.rds")


single_cause_probs.InsilicoVA.child.CHAMPS <- model_broad_probs.InsilicoVA.child.CHAMPS
single_cause_probs.InsilicoVA.child.CHAMPS <- t(apply(model_broad_probs.InsilicoVA.child.CHAMPS, 1, function(x) as.numeric(x == max(x))))
colnames(single_cause_probs.InsilicoVA.child.CHAMPS) <- colnames(model_broad_probs.InsilicoVA.child.CHAMPS)

head(model_broad_probs.InsilicoVA.child.CHAMPS)
head(single_cause_probs.InsilicoVA.child.CHAMPS)
tail(model_broad_probs.InsilicoVA.child.CHAMPS)
tail(single_cause_probs.InsilicoVA.child.CHAMPS)

saveRDS(single_cause_probs.InsilicoVA.child.CHAMPS, file ="Results/single_insilicova_child_champs.rds")











############################################################################################ EAVA single-cause, COMSA
model_broad_probs.EAVA.neonate.COMSA <- readRDS("Results/multi_eava_neonate_comsa.rds")
model_broad_probs.EAVA.child.COMSA <- readRDS("Results/multi_eava_child_comsa.rds")
head(model_broad_probs.EAVA.neonate.COMSA)
head(model_broad_probs.EAVA.child.COMSA)

single_cause_probs.EAVA.neonate.COMSA <- model_broad_probs.EAVA.neonate.COMSA
single_cause_probs.EAVA.neonate.COMSA[single_cause_probs.EAVA.neonate.COMSA==.25] <- 0.00
single_cause_probs.EAVA.neonate.COMSA[single_cause_probs.EAVA.neonate.COMSA==.75] <- 1.00

sum(rowSums(single_cause_probs.EAVA.neonate.COMSA))
dim(single_cause_probs.EAVA.neonate.COMSA)

single_cause_probs.EAVA.child.COMSA <- model_broad_probs.EAVA.child.COMSA
single_cause_probs.EAVA.child.COMSA[single_cause_probs.EAVA.child.COMSA==.25] <- 0.00
single_cause_probs.EAVA.child.COMSA[single_cause_probs.EAVA.child.COMSA==.75] <- 1.00

sum(rowSums(single_cause_probs.EAVA.child.COMSA))
dim(single_cause_probs.EAVA.child.COMSA)

saveRDS(single_cause_probs.EAVA.neonate.COMSA, file ="Results/single_eava_neonate_comsa.rds")
saveRDS(single_cause_probs.EAVA.child.COMSA, file ="Results/single_eava_child_comsa.rds")


############################################################################################ InterVA and InsilicoVA single-cause, COMSA
model_broad_probs.InterVA.neonates.COMSA <- readRDS("Results/multi_interva_neonate_comsa.rds")
model_broad_probs.InterVA.child.COMSA <- readRDS("Results/multi_interva_child_comsa.rds")
head(model_broad_probs.InterVA.neonates.COMSA)
head(model_broad_probs.InterVA.child.COMSA)
model_broad_probs.InsilicoVA.neonates.COMSA <- readRDS("Results/multi_insilicova_neonate_comsa.rds")
# model_broad_probs.InsilicoVA.neonates.COMSA <- readRDS("Outputs/20200612forJacob/multi_insilicova_neonate_comsa.rds")
model_broad_probs.InsilicoVA.child.COMSA <- readRDS("Results/multi_insilicova_child_comsa.rds")
head(model_broad_probs.InsilicoVA.neonates.COMSA)
head(model_broad_probs.InsilicoVA.child.COMSA)

# InterVA
single_cause_probs.InterVA.neonates.COMSA <- model_broad_probs.InterVA.neonates.COMSA
single_cause_probs.InterVA.neonates.COMSA <- t(apply(model_broad_probs.InterVA.neonates.COMSA, 1, function(x) as.numeric(x == max(x))))
colnames(single_cause_probs.InterVA.neonates.COMSA) <- colnames(model_broad_probs.InterVA.neonates.COMSA)

head(model_broad_probs.InterVA.neonates.COMSA)
head(single_cause_probs.InterVA.neonates.COMSA)
tail(model_broad_probs.InterVA.neonates.COMSA)
tail(single_cause_probs.InterVA.neonates.COMSA)

# We are getting ipre=1 and prematurity=1 for case 1437? Because they're equally probable - should be prematurity, based on getTopCOD()
single_cause_probs.InterVA.neonates.COMSA <- as.data.frame(single_cause_probs.InterVA.neonates.COMSA)
single_cause_probs.InterVA.neonates.COMSA$ipre <- ifelse(rownames(single_cause_probs.InterVA.neonates.COMSA)==1437, 0, single_cause_probs.InterVA.neonates.COMSA$ipre)
single_cause_probs.InterVA.neonates.COMSA <- as.matrix(single_cause_probs.InterVA.neonates.COMSA)
test <- subset(single_cause_probs.InterVA.neonates.COMSA, rownames(single_cause_probs.InterVA.neonates.COMSA)==1437)
test

saveRDS(single_cause_probs.InterVA.neonates.COMSA, file ="Results/single_interva_neonate_comsa.rds")

single_cause_probs.InterVA.child.COMSA <- model_broad_probs.InterVA.child.COMSA
single_cause_probs.InterVA.child.COMSA <- t(apply(model_broad_probs.InterVA.child.COMSA, 1, function(x) as.numeric(x == max(x))))
colnames(single_cause_probs.InterVA.child.COMSA) <- colnames(model_broad_probs.InterVA.child.COMSA)

head(model_broad_probs.InterVA.child.COMSA)
head(single_cause_probs.InterVA.child.COMSA)
tail(model_broad_probs.InterVA.child.COMSA)
tail(single_cause_probs.InterVA.child.COMSA)

# We are getting severe_malnutrition=1 and hiv=1 for case 4347? Because they're equally probable - should be hiv, based on getTopCOD()
single_cause_probs.InterVA.child.COMSA <- as.data.frame(single_cause_probs.InterVA.child.COMSA)
single_cause_probs.InterVA.child.COMSA$severe_malnutrition <- ifelse(rownames(single_cause_probs.InterVA.child.COMSA)==4347, 0, single_cause_probs.InterVA.child.COMSA$severe_malnutrition)
# We are getting diarrheal diseases=1 and severe malnutrition=1 for case 4590? Because they're equally probable - should be diarrhea, based on getTopCOD()
single_cause_probs.InterVA.child.COMSA$severe_malnutrition <- ifelse(rownames(single_cause_probs.InterVA.child.COMSA)==4590, 0, single_cause_probs.InterVA.child.COMSA$severe_malnutrition)
# We are getting diarrheal diseases=1 and severe malnutrition=1 for case 4586? Because they're equally probable - should be diarrhea, based on getTopCOD()
single_cause_probs.InterVA.child.COMSA$severe_malnutrition <- ifelse(rownames(single_cause_probs.InterVA.child.COMSA)==4586, 0, single_cause_probs.InterVA.child.COMSA$severe_malnutrition)

single_cause_probs.InterVA.child.COMSA <- as.matrix(single_cause_probs.InterVA.child.COMSA)
test <- subset(single_cause_probs.InterVA.child.COMSA, rownames(single_cause_probs.InterVA.child.COMSA)==4586)
test
saveRDS(single_cause_probs.InterVA.child.COMSA, file ="Results/single_interva_child_comsa.rds")

# InsilicoVA
single_cause_probs.InsilicoVA.neonates.COMSA <- model_broad_probs.InsilicoVA.neonates.COMSA
single_cause_probs.InsilicoVA.neonates.COMSA <- t(apply(model_broad_probs.InsilicoVA.neonates.COMSA, 1, function(x) as.numeric(x == max(x))))
colnames(single_cause_probs.InsilicoVA.neonates.COMSA) <- colnames(model_broad_probs.InsilicoVA.neonates.COMSA)

head(model_broad_probs.InsilicoVA.neonates.COMSA)
head(single_cause_probs.InsilicoVA.neonates.COMSA)
tail(model_broad_probs.InsilicoVA.neonates.COMSA)
tail(single_cause_probs.InsilicoVA.neonates.COMSA)

saveRDS(single_cause_probs.InsilicoVA.neonates.COMSA, file ="Results/single_insilicova_neonate_comsa.rds")

single_cause_probs.InsilicoVA.child.COMSA <- model_broad_probs.InsilicoVA.child.COMSA
single_cause_probs.InsilicoVA.child.COMSA <- t(apply(model_broad_probs.InsilicoVA.child.COMSA, 1, function(x) as.numeric(x == max(x))))
colnames(single_cause_probs.InsilicoVA.child.COMSA) <- colnames(model_broad_probs.InsilicoVA.child.COMSA)

head(model_broad_probs.InsilicoVA.child.COMSA)
head(single_cause_probs.InsilicoVA.child.COMSA)
tail(model_broad_probs.InsilicoVA.child.COMSA)
tail(single_cause_probs.InsilicoVA.child.COMSA)

saveRDS(single_cause_probs.InsilicoVA.child.COMSA, file ="Results/single_insilicova_child_comsa.rds")






