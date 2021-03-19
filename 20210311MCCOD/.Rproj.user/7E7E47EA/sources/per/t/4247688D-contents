# Last edited: 12 Mar 2021
# Last run:    12 Mar 2021

# Objective: convert multi to single cause VA, comsa and champs

library(openVA)
library(reshape2)
library(dplyr)

file <- getwd()





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







