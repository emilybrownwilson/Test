# Last edited: 2 Aug 2020
# Last run:    2 Aug 2020

# Objective: format single cause mits champs

library(openVA)
library(readxl)
library(reshape2)
library(dplyr)

'%!in%' <- function(x,y)!('%in%'(x,y))

file <- getwd()

CHAMPS.MITS.neonate <- read.csv("Data/mits_neonate_champs.csv")
CHAMPS.MITS.child <- read.csv("Data/mits_child_champs.csv")

CHAMPS.MITS.neonate$Name <- NULL
CHAMPS.MITS.child$Name <- NULL

table(CHAMPS.MITS.neonate$`Underlying_cat`, exclude = NULL)
table(CHAMPS.MITS.child$`Underlying_cat`, exclude = NULL)

### reshape
CHAMPS.MITS.neonate$Immediate_cat <- NULL
CHAMPS.MITS.neonate$X <- 1
mits_probs_neonate <- dcast(CHAMPS.MITS.neonate, ID ~ `Underlying_cat`, value.var = "X", fun.aggregate = mean)
mits_probs_neonate[ , 2:ncol(mits_probs_neonate) ][ mits_probs_neonate[ , 2:ncol(mits_probs_neonate) ] == "NaN" ] <- 0

CHAMPS.MITS.child$Immediate_cat <- NULL
CHAMPS.MITS.child$X <- 1
mits_probs_child <- dcast(CHAMPS.MITS.child, ID ~ `Underlying_cat`, value.var = "X", fun.aggregate = mean)
mits_probs_child[ , 2:ncol(mits_probs_child) ][ mits_probs_child[ , 2:ncol(mits_probs_child) ] == "NaN" ] <- 0 

rownames(mits_probs_neonate) <- mits_probs_neonate[,"ID"]
rownames(mits_probs_child) <- mits_probs_child[,"ID"]

test <- subset(mits_probs_neonate, None ==1) # 44F316AA-58A0-4E64-9F1A-6837588C3855
mits_probs_neonate <- subset(mits_probs_neonate, ID!="44F316AA-58A0-4E64-9F1A-6837588C3855")

mits_probs_neonate$ID <- NULL
mits_probs_neonate$None <- NULL
mits_probs_child$ID <- NULL

head(mits_probs_neonate)
head(mits_probs_child)

names(mits_probs_neonate) <- tolower(colnames(mits_probs_neonate))
mits_probs_neonate <- as.data.frame(mits_probs_neonate[,c("congenital_malformation","infection","ipre","other","prematurity")])

names(mits_probs_child) <- tolower(colnames(mits_probs_child))
names(mits_probs_child)[names(mits_probs_child) == 'severe malnutrition'] <- 'severe_malnutrition'
names(mits_probs_child)[names(mits_probs_child) == 'other infections'] <- 'other_infections'
mits_probs_child <- as.data.frame(mits_probs_child[,c("malaria","pneumonia","diarrhea","severe_malnutrition","hiv","other","other_infections")])

dim(mits_probs_neonate)
dim(mits_probs_child)


### save objects
saveRDS(mits_probs_neonate, file ="Results/single_mits_neonate_champs.rds")
saveRDS(mits_probs_child, file ="Results/single_mits_child_champs.rds")







