# Last edited: 27 Jan 2021
# Last run:    27 Jan 2021
# Objective: Question of how InsilicoVA and EAVA compare
#            What is InsilicoVA calling cases that have no data?
#            What are the EAVA undecided cases called in InsilicoVA?

rm(list = ls())

# library(rJava)
library(openVA)
library(readr)
library(dplyr)
# library(CrossVA)
library(readxl)
library(xlsx)
library(data.table)
library(tidyverse)
library(haven)

############################### 1078 cases - 1. COMSA openVA results
file <- getwd()
file
load(file.path(file,"/Data/openVA_comsa.Rdata"))

rm(list = ls()[!ls() %in% c("IV5data.ch1_59","codeVAInsilico.ch1_59")])

### InSilicoVA Probabilities - 1-59mo
insilico_probs_child <- getIndivProb(codeVAInsilico.ch1_59)
insilico_probs_child[insilico_probs_child == 0] <- .0000001
insilico_probs_child <- t(apply(insilico_probs_child, 1, function(x) (x/sum(x))))
class(insilico_probs_child)

dim(IV5data.ch1_59)       
dim(insilico_probs_child) 


############################### 1074 cases - 2a. COMSA EAVA child results
EAVA_causes <- read.csv(file.path(file,"/Data/eava_child_comsa.csv"), stringsAsFactors = FALSE)
dim(EAVA_causes)


############################### 1078 cases - 6a. multi_va_comsa
library(openVA)

file <- getwd()
load(file.path(file,"/Data/openVA_comsa.Rdata"))

### keep only objects we're using
rm(list = ls()[!ls() %in% c("codeVAInsilico.ch1_59")])

file <- getwd()
test <- getIndivProb(codeVAInsilico.ch1_59)
sort(colnames(test))

### InSilicoVA Probabilities - 1-59mo
insilico_probs_child <- getIndivProb(codeVAInsilico.ch1_59)
insilico_probs_child[insilico_probs_child == 0] <- .0000001
insilico_probs_child <- t(apply(insilico_probs_child, 1, function(x) (x/sum(x))))
dim(insilico_probs_child) # 1078 cases

### Child causes
malaria <- c("Malaria")
pneumonia <- c("Acute resp infect incl pneumonia")
diarrhea <- c("Diarrhoeal diseases")
severe_malnutrition <- c("Severe malnutrition")
hiv <- c("HIV/AIDS related death")
other <- c("Abortion-related death","Accid expos to smoke fire & flame","Accid fall","Accid poisoning & noxious subs","Acute abdomen","Acute cardiac disease","Assault","Asthma", "Congenital malformation",
           "Anaemia of pregnancy",
           "Breast neoplasms",
           # "Bite of Venomous Animal",
           "Contact with venomous plant/animal",
           # "Childhood Cancer", "Childhood Cardiovascular Diseases","Digestive Diseases",
           "Diabetes mellitus","Epilepsy","Exposure to force of nature",
           "Ectopic pregnancy",
           # "Falls","Fires","Homicide","Injury",
           "Intentional self-harm","Liver cirrhosis",
           # "Other Defined Causes of Child Deaths",
           # "Other",
           "Other and unspecified cardiac dis","Other and unspecified external CoD","Other transport accident",
           "Renal failure",
           # "Road Traffic",
           "Road traffic accident",
           "Severe anaemia","Sickle cell with crisis","Stroke",
           # " Other and unspecified NCD",
           "Other and unspecified NCD",
           "Birth asphyxia",
           "Chronic obstructive pulmonary dis",
           "Digestive neoplasms",
           "Fresh stillbirth",
           "Obstetric haemorrhage",
           "Obstructed labour",
           "Oral neoplasms",
           "Other and unspecified neonatal CoD",
           "Other and unspecified neoplasms",
           "Pregnancy-induced hypertension",
           "Pregnancy-related sepsis",
           "Prematurity",
           "Reproductive neoplasms MF",
           "Respiratory neoplasms",
           "Ruptured uterus",
           "Accid drowning and submersion",
           "Macerated stillbirth",
           "Neonatal pneumonia",
           "Other and unspecified maternal CoD")

other_infections <- c("Dengue fever","Haemorrhagic fever (non-dengue)","Measles",
                      # "Meningitis/Encephalitis",
                      "Meningitis and encephalitis",
                      # "Meningitis",
                      "Other and unspecified infect dis",
                      # "Other infections",
                      # "Other Infectious Diseases",
                      "Pertussis","Sepsis (non-obstetric)","Tetanus",
                      "Neonatal sepsis",
                      "Pulmonary tuberculosis")

cause_map_child <- data.frame(causes = c(malaria, pneumonia, diarrhea,
                                         severe_malnutrition, hiv, other, other_infections),
                              broad_cause = rep(c("malaria", "pneumonia",
                                                  "diarrhea", "severe_malnutrition", "hiv", "other","other_infections"),
                                                c(length(malaria), length(pneumonia),
                                                  length(diarrhea), length(severe_malnutrition),
                                                  length(hiv), length(other), length(other_infections))))

causes_child <- c("malaria", "pneumonia","diarrhea", "severe_malnutrition", "hiv", "other",
                  "other_infections")
C <- length(causes_child)


### Check that I have included all the individual causes
test1 <- setdiff(cause_map_child$causes,colnames(insilico_probs_child))
test2 <- setdiff(colnames(insilico_probs_child),cause_map_child$causes)
test1
test2

### map individual causes into broad groups - InsilicoVA child
model_broad_probs.InsilicoVA.child <- insilico_probs_child
colnames(model_broad_probs.InsilicoVA.child) <- cause_map_child$broad_cause[match(colnames(model_broad_probs.InsilicoVA.child),
                                                                                  cause_map_child$causes)]

mn <- model.matrix(~ colnames(model_broad_probs.InsilicoVA.child) + 0)
model_broad_probs.InsilicoVA.child <- model_broad_probs.InsilicoVA.child %*% mn
colnames(model_broad_probs.InsilicoVA.child) <- gsub("colnames\\(model_broad_probs.InsilicoVA.child\\)", "",
                                                     colnames(model_broad_probs.InsilicoVA.child))
model_broad_probs.InsilicoVA.child <- model_broad_probs.InsilicoVA.child[,causes_child[!causes_child %in% "Unspecified"]]

head(model_broad_probs.InsilicoVA.child)
head(insilico_probs_child)

rownames(model_broad_probs.InsilicoVA.child)
colnames(model_broad_probs.InsilicoVA.child)

row.names(model_broad_probs.InsilicoVA.child) <- gsub(' ', '', row.names(model_broad_probs.InsilicoVA.child))

dim(model_broad_probs.InsilicoVA.child)



############################### 1074 cases - 6b. multi_eava_comsa
### load data with one cause
eava_probs_child <- read.csv(file.path(file,"/Data/eava_child_comsa.csv"), stringsAsFactors = FALSE)
eava_probs_child$source.1 <- NULL
eava_probs_child$allexpertdxs.alt <- NULL
names(eava_probs_child)[names(eava_probs_child)=="allexpertdxs"] <- "cause1"

### get second cause, for each case that meets clinical criteria
### CHILD
eava_probs_child$cause2 <- NA
# eava_probs_child$cause2[is.na(eava_probs_child$cause2) & (eava_probs_child$ba5==1 | eava_probs_child$bi5==1)] <- "Intrapartum"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Malformation" & eava_probs_child$congmalf2==1] <- "Malformation"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Injury" & eava_probs_child$injury3_slide15_4==1] <- "Injury"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="AIDS" & eava_probs_child$AIDS4==1] <- "AIDS"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Malnutrition" & eava_probs_child$malnutrition2==1] <- "Malnutrition"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Measles" & eava_probs_child$measles4==1] <- "Measles"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Meningitis/Encephalitis" & eava_probs_child$meningitis==1] <- "Meningitis/Encephalitis"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Diarrhea/Dysentery" & eava_probs_child$diardysn8==1] <- "Diarrhea/Dysentery"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Other infections" & eava_probs_child$pertussis==1] <- "Other infections"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Pneumonia" & eava_probs_child$pneumoniafb2daysgr==1] <- "Pneumonia"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Malaria" & eava_probs_child$malaria251==1] <- "Malaria"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Diarrhea/Dysentery" & eava_probs_child$possdiardysn8_4==1] <- "Diarrhea/Dysentery"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Pneumonia" & eava_probs_child$possibleari3==1] <- "Pneumonia"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Other infections" & eava_probs_child$hemfever==1] <- "Other infections"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Other infections" & eava_probs_child$sepsis_nomal251==1] <- "Other infections"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Malaria" & eava_probs_child$malaria_possible==1] <- "Malaria"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Other infections" & eava_probs_child$residual_infect_slide15_4==1] <- "Other infections"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Malnutrition" & eava_probs_child$malnutrition1==1] <- "Malnutrition"
eava_probs_child$cause2[is.na(eava_probs_child$cause2) & eava_probs_child$cause1!="Preterm" & eava_probs_child$preterm_all_mo==1] <- "Preterm"
eava_probs_child$cause2[is.na(eava_probs_child$cause2)] <- "."

table(eava_probs_child$cause1, exclude = NULL)
table(eava_probs_child$cause2, exclude = NULL)
table(eava_probs_child$cause1, eava_probs_child$cause2, exclude = NULL)
eava_probs_child$cause1_cat <- "."
eava_probs_child$cause2_cat <- "."

eava_probs_child <- eava_probs_child[,c("ID","cause1","cause1_cat","cause2","cause2_cat")]

head(eava_probs_neonate)
head(eava_probs_child)

### Create cause map
### Child causes
unique(eava_probs_child$cause1)
eava_probs_child$cause1_cat[eava_probs_child$cause1 == "Malaria"] <- "malaria"
eava_probs_child$cause1_cat[eava_probs_child$cause1 %in% c("Pneumonia")] <- "pneumonia"
eava_probs_child$cause1_cat[eava_probs_child$cause1 %in% c("Diarrhea/Dysentery")] <- "diarrhea"
eava_probs_child$cause1_cat[eava_probs_child$cause1 %in% c("Malnutrition")] <- "severe_malnutrition"
eava_probs_child$cause1_cat[eava_probs_child$cause1 %in% c("AIDS")] <- "hiv"
eava_probs_child$cause1_cat[eava_probs_child$cause1 %in% c("Intrapartum","Malformation","Preterm","Injury")] <- "other"
eava_probs_child$cause1_cat[eava_probs_child$cause1 %in% c("Other infections","Meningitis/Encephalitis","Measles")] <- "other_infections"
table(eava_probs_child$cause1, eava_probs_child$cause1_cat, exclude = NULL)

eava_probs_child$cause2_cat[eava_probs_child$cause2 == "Malaria"] <- "malaria"
eava_probs_child$cause2_cat[eava_probs_child$cause2 %in% c("Pneumonia")] <- "pneumonia"
eava_probs_child$cause2_cat[eava_probs_child$cause2 %in% c("Diarrhea/Dysentery")] <- "diarrhea"
eava_probs_child$cause2_cat[eava_probs_child$cause2 %in% c("Malnutrition")] <- "severe_malnutrition"
eava_probs_child$cause2_cat[eava_probs_child$cause2 %in% c("AIDS")] <- "hiv"
eava_probs_child$cause2_cat[eava_probs_child$cause2 %in% c("Intrapartum","Malformation","Preterm","Injury")] <- "other"
eava_probs_child$cause2_cat[eava_probs_child$cause2 %in% c("Other infections","Meningitis/Encephalitis","Measles")] <- "other_infections"
table(eava_probs_child$cause2, eava_probs_child$cause2_cat, exclude = NULL)

head(eava_probs_child)

multi_cause_probs.EAVA.child.COMSA <- eava_probs_child


### COMSA - Child
multi_cause_probs.EAVA.child.COMSA <- multi_cause_probs.EAVA.child.COMSA[,c("ID","cause1_cat","cause2_cat")]
multi_cause_probs.EAVA.child.COMSA$X[multi_cause_probs.EAVA.child.COMSA$cause1_cat!="." & multi_cause_probs.EAVA.child.COMSA$cause2_cat=="."] <- 1
multi_cause_probs.EAVA.child.COMSA$X[multi_cause_probs.EAVA.child.COMSA$cause1_cat!="." & multi_cause_probs.EAVA.child.COMSA$cause2_cat!="."] <- .75
multi_cause_probs.EAVA.child.COMSA.1 <- dcast(multi_cause_probs.EAVA.child.COMSA, ID ~ cause1_cat, value.var = "X", fun.aggregate = mean)

multi_cause_probs.EAVA.child.COMSA$X[multi_cause_probs.EAVA.child.COMSA$cause2_cat!="."] <- .25
multi_cause_probs.EAVA.child.COMSA.2 <- dcast(multi_cause_probs.EAVA.child.COMSA, ID ~ cause2_cat, value.var = "X", fun.aggregate = mean)

multi_cause_probs.EAVA.child.COMSA.1$`.` <- NULL
multi_cause_probs.EAVA.child.COMSA.2$`.` <- NULL

multi_cause_probs.EAVA.child.COMSA_original <- multi_cause_probs.EAVA.child.COMSA
head(multi_cause_probs.EAVA.child.COMSA_original)
head(multi_cause_probs.EAVA.child.COMSA.1)
head(multi_cause_probs.EAVA.child.COMSA.2)

multi_cause_probs.EAVA.child.COMSA <- merge(multi_cause_probs.EAVA.child.COMSA.1,multi_cause_probs.EAVA.child.COMSA.2, by=c("ID"))
multi_cause_probs.EAVA.child.COMSA <- multi_cause_probs.EAVA.child.COMSA %>% rowwise() %>% mutate(diarrhea = sum(diarrhea.x,diarrhea.y, na.rm=TRUE))
multi_cause_probs.EAVA.child.COMSA <- multi_cause_probs.EAVA.child.COMSA %>% rowwise() %>% mutate(malaria = sum(malaria.x,malaria.y, na.rm=TRUE))
multi_cause_probs.EAVA.child.COMSA <- multi_cause_probs.EAVA.child.COMSA %>% rowwise() %>% mutate(other = sum(other.x,other.y, na.rm=TRUE))
multi_cause_probs.EAVA.child.COMSA <- multi_cause_probs.EAVA.child.COMSA %>% rowwise() %>% mutate(other_infections = sum(other_infections.x,other_infections.y, na.rm=TRUE))
multi_cause_probs.EAVA.child.COMSA <- multi_cause_probs.EAVA.child.COMSA %>% rowwise() %>% mutate(pneumonia = sum(pneumonia.x,pneumonia.y, na.rm=TRUE))
multi_cause_probs.EAVA.child.COMSA <- multi_cause_probs.EAVA.child.COMSA %>% rowwise() %>% mutate(severe_malnutrition = sum(severe_malnutrition.x,severe_malnutrition.y, na.rm=TRUE))

multi_cause_probs.EAVA.child.COMSA <- as.data.frame(multi_cause_probs.EAVA.child.COMSA[,c("ID","malaria","pneumonia","diarrhea","severe_malnutrition","hiv","other","other_infections")])

rownames(multi_cause_probs.EAVA.child.COMSA) <- multi_cause_probs.EAVA.child.COMSA[,"ID"]
multi_cause_probs.EAVA.child.COMSA$ID <- NULL
multi_cause_probs.EAVA.child.COMSA[is.na(multi_cause_probs.EAVA.child.COMSA)] <- 0

head(multi_cause_probs.EAVA.child.COMSA_original)
head(multi_cause_probs.EAVA.child.COMSA.1)
head(multi_cause_probs.EAVA.child.COMSA.2)
head(multi_cause_probs.EAVA.child.COMSA)

undecided.EAVA.child.COMSA <- multi_cause_probs.EAVA.child.COMSA[rowSums(multi_cause_probs.EAVA.child.COMSA) == 0, ]
undecided.EAVA.child.COMSA[,1:ncol(undecided.EAVA.child.COMSA)] <- 1/(ncol(undecided.EAVA.child.COMSA))
not.undecided.EAVA.child.COMSA <- multi_cause_probs.EAVA.child.COMSA[rowSums(multi_cause_probs.EAVA.child.COMSA) == 1, ]

multi_cause_probs.EAVA.child.COMSA <- rbind(not.undecided.EAVA.child.COMSA,undecided.EAVA.child.COMSA)
head(multi_cause_probs.EAVA.child.COMSA)

row.names(multi_cause_probs.EAVA.child.COMSA) <- gsub(' ', '', row.names(multi_cause_probs.EAVA.child.COMSA))

dim(multi_cause_probs.EAVA.child.COMSA)








############################################################################################ EAVA single-cause, COMSA
model_broad_probs.EAVA.child.COMSA <- readRDS("Results/multi_eava_child_comsa.rds")
head(model_broad_probs.EAVA.child.COMSA)

single_cause_probs.EAVA.child.COMSA <- model_broad_probs.EAVA.child.COMSA
single_cause_probs.EAVA.child.COMSA[single_cause_probs.EAVA.child.COMSA==.25] <- 0.00
single_cause_probs.EAVA.child.COMSA[single_cause_probs.EAVA.child.COMSA==.75] <- 1.00

sum(rowSums(single_cause_probs.EAVA.child.COMSA))
dim(single_cause_probs.EAVA.child.COMSA)



############################################################################################ InterVA and InsilicoVA single-cause, COMSA
model_broad_probs.InsilicoVA.child.COMSA <- readRDS("Results/multi_insilicova_child_comsa.rds")
head(model_broad_probs.InsilicoVA.child.COMSA)

# InsilicoVA
single_cause_probs.InsilicoVA.child.COMSA <- model_broad_probs.InsilicoVA.child.COMSA
single_cause_probs.InsilicoVA.child.COMSA <- t(apply(model_broad_probs.InsilicoVA.child.COMSA, 1, function(x) as.numeric(x == max(x))))
colnames(single_cause_probs.InsilicoVA.child.COMSA) <- colnames(model_broad_probs.InsilicoVA.child.COMSA)

head(model_broad_probs.InsilicoVA.child.COMSA)
head(single_cause_probs.InsilicoVA.child.COMSA)
tail(model_broad_probs.InsilicoVA.child.COMSA)
tail(single_cause_probs.InsilicoVA.child.COMSA)
dim(single_cause_probs.InsilicoVA.child.COMSA)









############################### cases - 8. match mits-va and format



file <- getwd()
date <- "20200808_comsa_data"

# 1. Single cause VA COMSA matrix : This is a matrix where each row represents a subject in the COMSA set, and each column represents a cause. 
#     Then for entry i,j (i-th individual, jth cause), there is a 1 is that is the single VA COD for that individual, and 0 if not (using the condensed final cause list). 
#     For EAVA, if individual i has an undecided COD, then put 1/C in each entry for that individual, where C is the number of causes

single_interva_child_comsa <- readRDS("Results/single_interva_child_comsa.rds")
single_insilicova_child_comsa <- readRDS("Results/single_insilicova_child_comsa.rds")
single_eava_child_comsa <- readRDS("Results/single_eava_child_comsa.rds")

single_interva_child_comsa <- single_interva_child_comsa[ rownames(single_interva_child_comsa) %in% rownames(single_eava_child_comsa), ]
single_insilicova_child_comsa <- single_insilicova_child_comsa[ rownames(single_insilicova_child_comsa) %in% rownames(single_eava_child_comsa), ]

head(single_interva_child_comsa)
head(single_insilicova_child_comsa)
head(single_eava_child_comsa)

single_insilicova_child_comsa <- single_insilicova_child_comsa[match(rownames(single_interva_child_comsa), rownames(single_insilicova_child_comsa)), ]
single_eava_child_comsa <- single_eava_child_comsa[match(rownames(single_interva_child_comsa), rownames(single_eava_child_comsa)), ]

identical(rownames(single_interva_child_comsa), rownames(single_insilicova_child_comsa))
identical(rownames(single_interva_child_comsa), rownames(single_eava_child_comsa))

identical(colnames(single_interva_child_comsa), colnames(single_insilicova_child_comsa))
identical(colnames(single_interva_child_comsa), colnames(single_eava_child_comsa))

dim(single_interva_child_comsa)
dim(single_insilicova_child_comsa)
dim(single_eava_child_comsa)

# 2. Multi-cause VA COMSA matrix: For the same individuals above, entry i,j is now the VA algorithm probability for cause j for individual i. 
#    For EAVA if there are two different causes, put .75 for the top cause and .25 for the second cause. Again put 1/C for each entry if the cause is undecided 

multi_interva_child_comsa <- readRDS("Results/multi_interva_child_comsa.rds")
multi_insilicova_child_comsa <- readRDS("Results/multi_insilicova_child_comsa.rds")
multi_eava_child_comsa <- readRDS("Results/multi_eava_child_comsa.rds")

multi_interva_child_comsa <- multi_interva_child_comsa[ rownames(multi_interva_child_comsa) %in% rownames(single_eava_child_comsa), ]
multi_insilicova_child_comsa <- multi_insilicova_child_comsa[ rownames(multi_insilicova_child_comsa) %in% rownames(single_eava_child_comsa), ]

head(multi_interva_child_comsa)
head(multi_insilicova_child_comsa)
head(multi_eava_child_comsa)

multi_insilicova_child_comsa <- multi_insilicova_child_comsa[match(rownames(multi_interva_child_comsa), rownames(multi_insilicova_child_comsa)), ]
multi_eava_child_comsa <- multi_eava_child_comsa[match(rownames(multi_interva_child_comsa), rownames(multi_eava_child_comsa)), ]

identical(rownames(multi_interva_child_comsa), rownames(multi_insilicova_child_comsa))
identical(rownames(multi_interva_child_comsa), rownames(multi_eava_child_comsa))

identical(colnames(multi_interva_child_comsa), colnames(multi_insilicova_child_comsa))
identical(colnames(multi_interva_child_comsa), colnames(multi_eava_child_comsa))

dim(multi_interva_child_comsa)
dim(multi_insilicova_child_comsa)
dim(multi_eava_child_comsa)


# stored and handed to Abhi and Brian


multi_insilicova_child_comsa <- readRDS("Results/20200802/multi_insilicova_child_comsa.rds")


