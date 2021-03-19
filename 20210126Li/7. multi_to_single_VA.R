# Last edited: 26 Jan 2021
# Last run:    26 Jan 2021

# Objective: convert multi to single cause VA, comsa and champs

library(openVA)
library(reshape2)
library(dplyr)

file <- getwd()


############################################################################################ InsilicoVA single-cause, COMSA
model_broad_probs.InsilicoVA.neonates.COMSA <- readRDS("Results/multi_insilicova_neonate_comsa.rds")
# model_broad_probs.InsilicoVA.neonates.COMSA <- readRDS("Outputs/20200612forJacob/multi_insilicova_neonate_comsa.rds")
model_broad_probs.InsilicoVA.child.COMSA <- readRDS("Results/multi_insilicova_child_comsa.rds")
head(model_broad_probs.InsilicoVA.neonates.COMSA)
head(model_broad_probs.InsilicoVA.child.COMSA)

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






