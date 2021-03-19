# Last edited: 2 Aug 2020
# Last run:    2 Aug 2020

# Objective: format multi cause va (eava) comsa

library(openVA)
library(reshape2)
library(dplyr)

file <- getwd()

### load data with one cause
eava_probs_neonate <- read.csv(file.path(file,"/Data/eava_neonate_comsa.csv"), stringsAsFactors = FALSE)
eava_probs_neonate$allexpertdxs2 <- NULL
names(eava_probs_neonate)[names(eava_probs_neonate)=="allexpertdxs1"] <- "cause1"

eava_probs_child <- read.csv(file.path(file,"/Data/eava_child_comsa.csv"), stringsAsFactors = FALSE)
eava_probs_child$source.1 <- NULL
eava_probs_child$allexpertdxs.alt <- NULL
names(eava_probs_child)[names(eava_probs_child)=="allexpertdxs"] <- "cause1"

### get second cause, for each case that meets clinical criteria
### NEONATE
eava_probs_neonate$cause2 <- NA
# eava_probs_neonate$cause2[is.na(eava_probs_neonate$cause2) & eava_probs_neonate$nnt1==1] <- "NNT"
eava_probs_neonate$cause2[is.na(eava_probs_neonate$cause2) & eava_probs_neonate$cause1!="Malformation" & eava_probs_neonate$congmalf2==1] <- "Malformation"
eava_probs_neonate$cause2[is.na(eava_probs_neonate$cause2) & eava_probs_neonate$cause1!="Intrapartum" & (eava_probs_neonate$ba5==1 | eava_probs_neonate$bi5==1)] <- "Intrapartum"
eava_probs_neonate$cause2[is.na(eava_probs_neonate$cause2) & eava_probs_neonate$cause1!="Preterm" & eava_probs_neonate$preterm_all_mo==1] <- "Preterm"
eava_probs_neonate$cause2[is.na(eava_probs_neonate$cause2) & eava_probs_neonate$cause1!="Meningitis" & eava_probs_neonate$meningitis451_nonnt1==1] <- "Meningitis"
eava_probs_neonate$cause2[is.na(eava_probs_neonate$cause2) & eava_probs_neonate$cause1!="Diarrhea" & eava_probs_neonate$diarrhea8==1] <- "Diarrhea"
eava_probs_neonate$cause2[is.na(eava_probs_neonate$cause2) & eava_probs_neonate$cause1!="Pneumonia" & eava_probs_neonate$pneumonia157==1] <- "Pneumonia"
eava_probs_neonate$cause2[is.na(eava_probs_neonate$cause2) & eava_probs_neonate$cause1!="Diarrhea" & eava_probs_neonate$possiblediar8_8==1] <- "Diarrhea"
eava_probs_neonate$cause2[is.na(eava_probs_neonate$cause2) & eava_probs_neonate$cause1!="Pneumonia" & eava_probs_neonate$possiblepneumonia9==1] <- "Pneumonia"
eava_probs_neonate$cause2[is.na(eava_probs_neonate$cause2) & eava_probs_neonate$cause1!="Sepsis" & eava_probs_neonate$sepsisfvr2_2_nonnt1==1] <- "Sepsis"
eava_probs_neonate$cause2[is.na(eava_probs_neonate$cause2) & eava_probs_neonate$cause1!="Other" & eava_probs_neonate$jaundice2==1] <- "Other"
eava_probs_neonate$cause2[is.na(eava_probs_neonate$cause2) & eava_probs_neonate$cause1!="Other" & eava_probs_neonate$hemorrhageNN==1] <- "Other"
eava_probs_neonate$cause2[is.na(eava_probs_neonate$cause2) & eava_probs_neonate$cause1!="Other" & eava_probs_neonate$suid==1] <- "Other"
eava_probs_neonate$cause2[is.na(eava_probs_neonate$cause2)] <- "."

table(eava_probs_neonate$cause1, exclude = NULL)
table(eava_probs_neonate$cause2, exclude = NULL)
table(eava_probs_neonate$cause1, eava_probs_neonate$cause2, exclude = NULL)
eava_probs_neonate$cause1_cat <- "."
eava_probs_neonate$cause2_cat <- "."

eava_probs_neonate <- eava_probs_neonate[,c("ID","cause1","cause1_cat","cause2","cause2_cat")]

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
### Neonatal causes
unique(eava_probs_neonate$cause1)
eava_probs_neonate$cause1_cat[eava_probs_neonate$cause1 == "Malformation"] <- "congenital_malformation"
eava_probs_neonate$cause1_cat[eava_probs_neonate$cause1 %in% c("Diarrhea","Pneumonia","Sepsis","NNT","Meningitis")] <- "infection"
eava_probs_neonate$cause1_cat[eava_probs_neonate$cause1 %in% c("Intrapartum")] <- "ipre"
eava_probs_neonate$cause1_cat[eava_probs_neonate$cause1 %in% c("Other")] <- "other"
eava_probs_neonate$cause1_cat[eava_probs_neonate$cause1 %in% c("Preterm")] <- "prematurity"
table(eava_probs_neonate$cause1, eava_probs_neonate$cause1_cat, exclude = NULL)

unique(eava_probs_neonate$cause2)
eava_probs_neonate$cause2_cat[eava_probs_neonate$cause2 == "Malformation"] <- "congenital_malformation"
eava_probs_neonate$cause2_cat[eava_probs_neonate$cause2 %in% c("Diarrhea","Pneumonia","Sepsis","NNT","Meningitis")] <- "infection"
eava_probs_neonate$cause2_cat[eava_probs_neonate$cause2 %in% c("Intrapartum")] <- "ipre"
eava_probs_neonate$cause2_cat[eava_probs_neonate$cause2 %in% c("Other")] <- "other"
eava_probs_neonate$cause2_cat[eava_probs_neonate$cause2 %in% c("Preterm")] <- "prematurity"
table(eava_probs_neonate$cause2, eava_probs_neonate$cause2_cat, exclude = NULL)


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

head(eava_probs_neonate)
head(eava_probs_child)


multi_cause_probs.EAVA.neonate.COMSA <- eava_probs_neonate
multi_cause_probs.EAVA.child.COMSA <- eava_probs_child



### COMSA - Neonates
multi_cause_probs.EAVA.neonate.COMSA <- multi_cause_probs.EAVA.neonate.COMSA[,c("ID","cause1_cat","cause2_cat")]
multi_cause_probs.EAVA.neonate.COMSA$X[multi_cause_probs.EAVA.neonate.COMSA$cause1_cat!="." & multi_cause_probs.EAVA.neonate.COMSA$cause2_cat=="."] <- 1
multi_cause_probs.EAVA.neonate.COMSA$X[multi_cause_probs.EAVA.neonate.COMSA$cause1_cat!="." & multi_cause_probs.EAVA.neonate.COMSA$cause2_cat!="."] <- .75
multi_cause_probs.EAVA.neonate.COMSA.1 <- dcast(multi_cause_probs.EAVA.neonate.COMSA, ID ~ cause1_cat, value.var = "X", fun.aggregate = mean)

multi_cause_probs.EAVA.neonate.COMSA$X[multi_cause_probs.EAVA.neonate.COMSA$cause2_cat!="."] <- .25
multi_cause_probs.EAVA.neonate.COMSA.2 <- dcast(multi_cause_probs.EAVA.neonate.COMSA, ID ~ cause2_cat, value.var = "X", fun.aggregate = mean)

multi_cause_probs.EAVA.neonate.COMSA.2$congenital_malformation <- NaN
multi_cause_probs.EAVA.neonate.COMSA.1$`.` <- NULL
multi_cause_probs.EAVA.neonate.COMSA.2$`.` <- NULL

multi_cause_probs.EAVA.neonate.COMSA_original <- multi_cause_probs.EAVA.neonate.COMSA
head(multi_cause_probs.EAVA.neonate.COMSA_original)
head(multi_cause_probs.EAVA.neonate.COMSA.1)
head(multi_cause_probs.EAVA.neonate.COMSA.2)

multi_cause_probs.EAVA.neonate.COMSA <- merge(multi_cause_probs.EAVA.neonate.COMSA.1,multi_cause_probs.EAVA.neonate.COMSA.2, by=c("ID"))
head(multi_cause_probs.EAVA.neonate.COMSA.1)
head(multi_cause_probs.EAVA.neonate.COMSA.2)

multi_cause_probs.EAVA.neonate.COMSA <- multi_cause_probs.EAVA.neonate.COMSA %>% rowwise() %>% mutate(infection = sum(infection.x,infection.y, na.rm=TRUE))
multi_cause_probs.EAVA.neonate.COMSA <- multi_cause_probs.EAVA.neonate.COMSA %>% rowwise() %>% mutate(ipre = sum(ipre.x,ipre.y, na.rm=TRUE))
multi_cause_probs.EAVA.neonate.COMSA <- multi_cause_probs.EAVA.neonate.COMSA %>% rowwise() %>% mutate(congenital_malformation = sum(congenital_malformation.x,congenital_malformation.y, na.rm=TRUE))
multi_cause_probs.EAVA.neonate.COMSA <- multi_cause_probs.EAVA.neonate.COMSA %>% rowwise() %>% mutate(other = sum(other.x,other.y, na.rm=TRUE))
multi_cause_probs.EAVA.neonate.COMSA <- multi_cause_probs.EAVA.neonate.COMSA %>% rowwise() %>% mutate(prematurity = sum(prematurity.x,prematurity.y, na.rm=TRUE))

multi_cause_probs.EAVA.neonate.COMSA <- as.data.frame(multi_cause_probs.EAVA.neonate.COMSA[,c("ID","congenital_malformation","infection","ipre","other","prematurity")])

rownames(multi_cause_probs.EAVA.neonate.COMSA) <- multi_cause_probs.EAVA.neonate.COMSA[,"ID"]
multi_cause_probs.EAVA.neonate.COMSA$ID <- NULL

head(multi_cause_probs.EAVA.neonate.COMSA_original)
head(multi_cause_probs.EAVA.neonate.COMSA.1)
head(multi_cause_probs.EAVA.neonate.COMSA.2)

undecided.EAVA.neonate.COMSA <- multi_cause_probs.EAVA.neonate.COMSA[rowSums(multi_cause_probs.EAVA.neonate.COMSA) == 0, ]
undecided.EAVA.neonate.COMSA[,1:ncol(undecided.EAVA.neonate.COMSA)] <- 1/(ncol(undecided.EAVA.neonate.COMSA))
not.undecided.EAVA.neonate.COMSA <- multi_cause_probs.EAVA.neonate.COMSA[rowSums(multi_cause_probs.EAVA.neonate.COMSA) == 1, ]

multi_cause_probs.EAVA.neonate.COMSA <- rbind(not.undecided.EAVA.neonate.COMSA,undecided.EAVA.neonate.COMSA)
head(multi_cause_probs.EAVA.neonate.COMSA)


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

saveRDS(multi_cause_probs.EAVA.neonate.COMSA, file ="Results/multi_eava_neonate_comsa.rds")
saveRDS(multi_cause_probs.EAVA.child.COMSA, file ="Results/multi_eava_child_comsa.rds")

