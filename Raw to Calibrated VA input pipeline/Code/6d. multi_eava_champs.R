# Last edited: 2 Aug 2020
# Last run:    8 Aug 2020

# Objective: format multi cause va (eava) champs

library(openVA)

file <- getwd()

### load data with one cause
eava_probs_neonate <- read.csv(file.path(file,"Data/eava_neonate_champs.csv"), stringsAsFactors = FALSE)
eava_probs_neonate$allexpertdxs2 <- NULL
names(eava_probs_neonate)[names(eava_probs_neonate)=="allexpertdxs1"] <- "cause1"

eava_probs_child <- read.csv(file.path(file,"Data/eava_child_champs.csv"), stringsAsFactors = FALSE)
names(eava_probs_child)[names(eava_probs_child)=="allexpertdxs"] <- "cause1"

#########
class(eava_probs_neonate$ID)
dim(eava_probs_child)
dim(eava_probs_neonate)


#########

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

rownames(eava_probs_neonate) <- eava_probs_neonate[,"ID"]
eava_probs_neonate$ID <- NULL
rownames(eava_probs_neonate)
colnames(eava_probs_neonate)

rownames(eava_probs_child) <- eava_probs_child[,"ID"]
eava_probs_child$ID <- NULL
rownames(eava_probs_child)
colnames(eava_probs_child)

head(eava_probs_neonate)
head(eava_probs_child)

model_broad_probs.EAVA.neonate.CHAMPS <- eava_probs_neonate
model_broad_probs.EAVA.child.CHAMPS <- eava_probs_child
head(model_broad_probs.EAVA.neonate.CHAMPS)
head(model_broad_probs.EAVA.child.CHAMPS)

### CHAMPS - Neonates
multi_cause_probs.EAVA.neonate.CHAMPS <- model_broad_probs.EAVA.neonate.CHAMPS[,c("cause1_cat","cause2_cat")]
multi_cause_probs.EAVA.neonate.CHAMPS$ID <- rownames(multi_cause_probs.EAVA.neonate.CHAMPS)
multi_cause_probs.EAVA.neonate.CHAMPS$X[multi_cause_probs.EAVA.neonate.CHAMPS$cause1_cat!="." & multi_cause_probs.EAVA.neonate.CHAMPS$cause2_cat=="."] <- 1
multi_cause_probs.EAVA.neonate.CHAMPS$X[multi_cause_probs.EAVA.neonate.CHAMPS$cause1_cat!="." & multi_cause_probs.EAVA.neonate.CHAMPS$cause2_cat!="."] <- .75
multi_cause_probs.EAVA.neonate.CHAMPS.1 <- dcast(multi_cause_probs.EAVA.neonate.CHAMPS, ID ~ cause1_cat, value.var = "X", fun.aggregate = mean)

multi_cause_probs.EAVA.neonate.CHAMPS$X[multi_cause_probs.EAVA.neonate.CHAMPS$cause2_cat!="."] <- .25
multi_cause_probs.EAVA.neonate.CHAMPS.2 <- dcast(multi_cause_probs.EAVA.neonate.CHAMPS, ID ~ cause2_cat, value.var = "X", fun.aggregate = mean)

multi_cause_probs.EAVA.neonate.CHAMPS.2$congenital_malformation <- NaN
multi_cause_probs.EAVA.neonate.CHAMPS.1$`.` <- NULL
multi_cause_probs.EAVA.neonate.CHAMPS.2$`.` <- NULL

multi_cause_probs.EAVA.neonate.CHAMPS_original <- multi_cause_probs.EAVA.neonate.CHAMPS
head(multi_cause_probs.EAVA.neonate.CHAMPS_original)
head(multi_cause_probs.EAVA.neonate.CHAMPS.1)
head(multi_cause_probs.EAVA.neonate.CHAMPS.2)

multi_cause_probs.EAVA.neonate.CHAMPS <- merge(multi_cause_probs.EAVA.neonate.CHAMPS.1,multi_cause_probs.EAVA.neonate.CHAMPS.2, by=c("ID"))
multi_cause_probs.EAVA.neonate.CHAMPS <- multi_cause_probs.EAVA.neonate.CHAMPS %>% rowwise() %>% mutate(infection = sum(infection.x,infection.y, na.rm=TRUE))
multi_cause_probs.EAVA.neonate.CHAMPS <- multi_cause_probs.EAVA.neonate.CHAMPS %>% rowwise() %>% mutate(ipre = sum(ipre.x,ipre.y, na.rm=TRUE))
multi_cause_probs.EAVA.neonate.CHAMPS <- multi_cause_probs.EAVA.neonate.CHAMPS %>% rowwise() %>% mutate(congenital_malformation = sum(congenital_malformation.x,congenital_malformation.y, na.rm=TRUE))
multi_cause_probs.EAVA.neonate.CHAMPS <- multi_cause_probs.EAVA.neonate.CHAMPS %>% rowwise() %>% mutate(other = sum(other.x,other.y, na.rm=TRUE))
multi_cause_probs.EAVA.neonate.CHAMPS <- multi_cause_probs.EAVA.neonate.CHAMPS %>% rowwise() %>% mutate(prematurity = sum(prematurity.x,prematurity.y, na.rm=TRUE))

multi_cause_probs.EAVA.neonate.CHAMPS <- as.data.frame(multi_cause_probs.EAVA.neonate.CHAMPS[,c("ID","congenital_malformation","infection","ipre","other","prematurity")])

rownames(multi_cause_probs.EAVA.neonate.CHAMPS) <- multi_cause_probs.EAVA.neonate.CHAMPS[,"ID"]
multi_cause_probs.EAVA.neonate.CHAMPS$ID <- NULL

head(multi_cause_probs.EAVA.neonate.CHAMPS_original)
head(multi_cause_probs.EAVA.neonate.CHAMPS.1)
head(multi_cause_probs.EAVA.neonate.CHAMPS.2)
head(multi_cause_probs.EAVA.neonate.CHAMPS)

undecided.EAVA.neonate.CHAMPS <- multi_cause_probs.EAVA.neonate.CHAMPS[rowSums(multi_cause_probs.EAVA.neonate.CHAMPS) == 0, ]
undecided.EAVA.neonate.CHAMPS[,1:ncol(undecided.EAVA.neonate.CHAMPS)] <- 1/(ncol(undecided.EAVA.neonate.CHAMPS))
not.undecided.EAVA.neonate.CHAMPS <- multi_cause_probs.EAVA.neonate.CHAMPS[rowSums(multi_cause_probs.EAVA.neonate.CHAMPS) == 1, ]

multi_cause_probs.EAVA.neonate.CHAMPS <- rbind(not.undecided.EAVA.neonate.CHAMPS,undecided.EAVA.neonate.CHAMPS)
head(multi_cause_probs.EAVA.neonate.CHAMPS)
dim(multi_cause_probs.EAVA.neonate.CHAMPS)

### CHAMPS - Child
multi_cause_probs.EAVA.child.CHAMPS <- model_broad_probs.EAVA.child.CHAMPS[,c("cause1_cat","cause2_cat")]
multi_cause_probs.EAVA.child.CHAMPS$ID <- rownames(multi_cause_probs.EAVA.child.CHAMPS)
multi_cause_probs.EAVA.child.CHAMPS$X[multi_cause_probs.EAVA.child.CHAMPS$cause1_cat!="." & multi_cause_probs.EAVA.child.CHAMPS$cause2_cat=="."] <- 1
multi_cause_probs.EAVA.child.CHAMPS$X[multi_cause_probs.EAVA.child.CHAMPS$cause1_cat!="." & multi_cause_probs.EAVA.child.CHAMPS$cause2_cat!="."] <- .75
multi_cause_probs.EAVA.child.CHAMPS.1 <- dcast(multi_cause_probs.EAVA.child.CHAMPS, ID ~ cause1_cat, value.var = "X", fun.aggregate = mean)

multi_cause_probs.EAVA.child.CHAMPS$X[multi_cause_probs.EAVA.child.CHAMPS$cause2_cat!="."] <- .25
multi_cause_probs.EAVA.child.CHAMPS.2 <- dcast(multi_cause_probs.EAVA.child.CHAMPS, ID ~ cause2_cat, value.var = "X", fun.aggregate = mean)

multi_cause_probs.EAVA.child.CHAMPS.1$`.` <- NULL
multi_cause_probs.EAVA.child.CHAMPS.2$`.` <- NULL

multi_cause_probs.EAVA.child.CHAMPS_original <- multi_cause_probs.EAVA.child.CHAMPS
head(multi_cause_probs.EAVA.child.CHAMPS_original)
head(multi_cause_probs.EAVA.child.CHAMPS.1)
head(multi_cause_probs.EAVA.child.CHAMPS.2)


multi_cause_probs.EAVA.child.CHAMPS <- merge(multi_cause_probs.EAVA.child.CHAMPS.1,multi_cause_probs.EAVA.child.CHAMPS.2, by=c("ID"))
multi_cause_probs.EAVA.child.CHAMPS <- multi_cause_probs.EAVA.child.CHAMPS %>% rowwise() %>% mutate(hiv = sum(hiv.x,hiv.y, na.rm=TRUE))
multi_cause_probs.EAVA.child.CHAMPS <- multi_cause_probs.EAVA.child.CHAMPS %>% rowwise() %>% mutate(diarrhea = sum(diarrhea.x,diarrhea.y, na.rm=TRUE))
multi_cause_probs.EAVA.child.CHAMPS <- multi_cause_probs.EAVA.child.CHAMPS %>% rowwise() %>% mutate(malaria = sum(malaria.x,malaria.y, na.rm=TRUE))
multi_cause_probs.EAVA.child.CHAMPS <- multi_cause_probs.EAVA.child.CHAMPS %>% rowwise() %>% mutate(other = sum(other.x,other.y, na.rm=TRUE))
multi_cause_probs.EAVA.child.CHAMPS <- multi_cause_probs.EAVA.child.CHAMPS %>% rowwise() %>% mutate(other_infections = sum(other_infections.x,other_infections.y, na.rm=TRUE))
multi_cause_probs.EAVA.child.CHAMPS <- multi_cause_probs.EAVA.child.CHAMPS %>% rowwise() %>% mutate(pneumonia = sum(pneumonia.x,pneumonia.y, na.rm=TRUE))
multi_cause_probs.EAVA.child.CHAMPS <- multi_cause_probs.EAVA.child.CHAMPS %>% rowwise() %>% mutate(severe_malnutrition = sum(severe_malnutrition.x,severe_malnutrition.y, na.rm=TRUE))

multi_cause_probs.EAVA.child.CHAMPS <- as.data.frame(multi_cause_probs.EAVA.child.CHAMPS[,c("ID","malaria","pneumonia","diarrhea","severe_malnutrition","hiv","other","other_infections")])


rownames(multi_cause_probs.EAVA.child.CHAMPS) <- multi_cause_probs.EAVA.child.CHAMPS[,"ID"]
multi_cause_probs.EAVA.child.CHAMPS$ID <- NULL
multi_cause_probs.EAVA.child.CHAMPS[is.na(multi_cause_probs.EAVA.child.CHAMPS)] <- 0

head(multi_cause_probs.EAVA.child.CHAMPS_original)
head(multi_cause_probs.EAVA.child.CHAMPS.1)
head(multi_cause_probs.EAVA.child.CHAMPS.2)
head(multi_cause_probs.EAVA.child.CHAMPS)

undecided.EAVA.child.CHAMPS <- multi_cause_probs.EAVA.child.CHAMPS[rowSums(multi_cause_probs.EAVA.child.CHAMPS) == 0, ]
undecided.EAVA.child.CHAMPS[,1:ncol(undecided.EAVA.child.CHAMPS)] <- 1/(ncol(undecided.EAVA.child.CHAMPS))
not.undecided.EAVA.child.CHAMPS <- multi_cause_probs.EAVA.child.CHAMPS[rowSums(multi_cause_probs.EAVA.child.CHAMPS) == 1, ]

multi_cause_probs.EAVA.child.CHAMPS <- rbind(not.undecided.EAVA.child.CHAMPS,undecided.EAVA.child.CHAMPS)
head(multi_cause_probs.EAVA.child.CHAMPS)

saveRDS(multi_cause_probs.EAVA.neonate.CHAMPS, file ="Results/multi_eava_neonate_champs.rds")
saveRDS(multi_cause_probs.EAVA.child.CHAMPS, file ="Results/multi_eava_child_champs.rds")

