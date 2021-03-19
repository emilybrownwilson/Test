# Last edited: 10 Jun 2020
# Last run:    10 Jun 2020
# Objective: find mits that have a va pair that were not in the last run, and send to Henry for broad classification

library(openVA)
library(readxl)
library(reshape2)
library(dplyr)

'%!in%' <- function(x,y)!('%in%'(x,y))

file <- getwd()

### May 2020 MITS
# combine current VA and MITS, and subset pairs that were not included last run
VA <- read.csv(file.path(file,"Data/CHAMPS_MITS_VA_20200610/ASR-138_JHU_MITS_VA_V2/asr114_va2016.csv"))
MITS <- read.csv(file.path(file,"Data/CHAMPS_MITS_VA_20200610/ASR-138_JHU_MITS_VA_V2/asr_138_decode_results_classification.csv"))

head(VA)
head(MITS)

VA_MITS <- merge(VA,MITS, by=c("champs_deid"))
dim(VA)
dim(MITS)
dim(VA_MITS)

table(VA_MITS$case_type_desc) # 365 children, 517 neonates


# MITS FROM LAST RUN
MITS_original_neonate <- read.csv(file.path(file,"Data/mits_neonate_champs.csv"))
MITS_original_child <- read.csv(file.path(file,"Data/mits_child_champs.csv"))

MITS_original <- rbind(MITS_original_neonate, MITS_original_child)
dim(MITS_original)
names(MITS_original)[names(MITS_original) == 'ID'] <- 'champs_deid'

VA_MITS_new_vs_last_run <- merge(VA_MITS, MITS_original, by=c("champs_deid"))
dim(VA_MITS_new_vs_last_run)
table(VA_MITS_new_vs_last_run$case_type_desc) # 124+157=281 children from last run, 154+156+87=397 neonates from last run

table(VA_MITS$case_type_desc)
(171+194)-281 # 84 new children
(218+195+104)-397 #120 new neonates


# check against results from April 2020 run:
prev_run_child_mits <- readRDS("Results/20200514_comsa_data/single_mits_child_champs.rds")
prev_run_child_mits$champs_deid <- rownames(prev_run_child_mits)
head(prev_run_child_mits)
prev_run_neonate_mits <- readRDS("Results/20200514_comsa_data/single_mits_neonate_champs.rds")
prev_run_neonate_mits$champs_deid <- rownames(prev_run_neonate_mits)
head(prev_run_neonate_mits)

VA_MITS_neonate <- subset(VA_MITS, case_type_desc %in% c("Death in the first 24 hours","Early Neonate (1 to 6 days)","Late Neonate (7 to 27 days)"))
VA_MITS_child <- subset(VA_MITS, case_type_desc %in% c("Child (12 months to less than 60 Months)","Infant (28 days to less than 12 months)"))

VA_MITS_neonate_new <- anti_join(VA_MITS_neonate,prev_run_neonate_mits,by=c("champs_deid"))
dim(VA_MITS_neonate_new) # 124
VA_MITS_child_new <- anti_join(VA_MITS_child,prev_run_child_mits,by=c("champs_deid"))
dim(VA_MITS_child_new)   # 85


# Subset data from CHAMPS for Henry
VA_MITS_neonate_new <- VA_MITS_neonate_new[,c("champs_deid","case_type_desc")]
dim(VA_MITS_neonate_new)
table(VA_MITS_neonate_new$case_type_desc)
head(VA_MITS_neonate_new)

VA_MITS_child_new <- VA_MITS_child_new[,c("champs_deid","case_type_desc")]
dim(VA_MITS_child_new)
table(VA_MITS_child_new$case_type_desc)
head(VA_MITS_child_new)

VA_MITS_neonate_for_Henry <- merge(VA_MITS_neonate_new,MITS,by=c("champs_deid"))
VA_MITS_neonate_for_Henry$case_type_desc <- NULL
head(VA_MITS_neonate_for_Henry)
dim(VA_MITS_neonate_for_Henry)

VA_MITS_child_for_Henry <- merge(VA_MITS_child_new,MITS,by=c("champs_deid"))
VA_MITS_child_for_Henry$case_type_desc <- NULL
head(VA_MITS_child_for_Henry)
dim(VA_MITS_child_for_Henry)


write.csv(VA_MITS_neonate_for_Henry,"Data/CHAMPS_MITS_VA_20200610/CHAMPS-MITS-NEONATE - additional cases 2020MAY14 - RECLASSIFICATION TO VA CATEGORIES.csv", row.names = FALSE)
write.csv(VA_MITS_child_for_Henry,"Data/CHAMPS_MITS_VA_20200610/CHAMPS-MITS-CHILD - additional cases 2020MAY14 - RECLASSIFICATION TO VA CATEGORIES.csv", row.names = FALSE)















