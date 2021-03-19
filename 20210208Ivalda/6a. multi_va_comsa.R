# Last edited: 8 Feb 2021
# Last run:    8 Feb 2021

# Objective: format multi cause insilicova comsa

library(openVA)

file <- getwd()
file
load(file.path(file,"/Data/openVA_comsa.Rdata"))

### keep only objects we're using
rm(list = ls()[!ls() %in% c("codeVAInsilico.neonate","codeVAInsilico.ch1_59m")])

file <- getwd()

test <- getTopCOD(codeVAInsilico.neonate)
sort(unique(as.character(test$cause)))

test <- getTopCOD(codeVAInsilico.ch1_59m)
sort(unique(as.character(test$cause)))

test <- getIndivProb(codeVAInsilico.neonate)
sort(colnames(test))

test <- getIndivProb(codeVAInsilico.ch1_59m)
sort(colnames(test))


### InterVA Probabilities - Neonate
# interva_probs_neonate <- getIndivProb(InterVA5.neonate)
# interva_probs_neonate[interva_probs_neonate == 0] <- .0000001
# interva_probs_neonate <- t(apply(interva_probs_neonate, 1, function(x) (x/sum(x))))

### InSilicoVA Probabilities - Neonate
insilico_probs_neonate <- getIndivProb(codeVAInsilico.neonate)
insilico_probs_neonate[insilico_probs_neonate == 0] <- .0000001
insilico_probs_neonate <- t(apply(insilico_probs_neonate, 1, function(x) (x/sum(x))))

### InterVA Probabilities - 1-59mo
# interva_probs_child <- getIndivProb(InterVA5.ch1_59)
# interva_probs_child[interva_probs_child == 0] <- .0000001
# interva_probs_child <- t(apply(interva_probs_child, 1, function(x) (x/sum(x))))

### InSilicoVA Probabilities - 1-59mo
insilico_probs_child <- getIndivProb(codeVAInsilico.ch1_59m)
insilico_probs_child[insilico_probs_child == 0] <- .0000001
insilico_probs_child <- t(apply(insilico_probs_child, 1, function(x) (x/sum(x))))

### Create cause map
### Neonatal causes
congenital_malformation <- c("Congenital malformation")

infection <- c("Acute resp infect incl pneumonia",
               "Dengue fever",
               "Diarrhoeal diseases",
               "Haemorrhagic fever (non-dengue)",
               "HIV/AIDS related death",
               "Malaria",
               "Measles",
               "Meningitis and encephalitis",
               "Neonatal pneumonia",
               "Neonatal sepsis",
               "Other and unspecified infect dis",
               "Pertussis",
               "Pulmonary tuberculosis",
               "Sepsis (non-obstetric)",
               "Tetanus")

ipre <- c("Birth asphyxia")

other <- c("Abortion-related death",
           "Accid poisoning & noxious subs",
           "Anaemia of pregnancy",
           "Assault",
           "Asthma",
           "Accid drowning and submersion",
           "Accid expos to smoke fire & flame",
           "Accid fall",
           "Acute abdomen",
           "Acute cardiac disease",
           "Breast neoplasms",
           "Chronic obstructive pulmonary dis",
           "Contact with venomous plant/animal",
           "Diabetes mellitus",
           "Digestive neoplasms",
           "Ectopic pregnancy",
           "Epilepsy",
           "Exposure to force of nature",
           "Fresh stillbirth",
           "Intentional self-harm",
           "Liver cirrhosis",
           "Macerated stillbirth",
           "Obstetric haemorrhage",
           "Obstructed labour",
           "Oral neoplasms",
           "Other and unspecified cardiac dis",
           "Other and unspecified external CoD",
           "Other and unspecified maternal CoD",
           "Other and unspecified neonatal CoD",
           "Other and unspecified neoplasms",
           "Other transport accident",
           # " Other and unspecified NCD",
           "Other and unspecified NCD",
           "Pregnancy-induced hypertension",
           "Pregnancy-related sepsis",
           "Renal failure",
           "Reproductive neoplasms MF",
           "Respiratory neoplasms",
           "Road traffic accident",
           "Ruptured uterus",
           "Severe anaemia",
           "Severe malnutrition",
           "Sickle cell with crisis",
           "Stroke")

prematurity <- c("Prematurity")
# Unspecified <- c("Unspecified")

cause_map_neonate <- data.frame(causes = c(congenital_malformation, infection, ipre,
                                           other, prematurity),
                           broad_cause = rep(c("congenital_malformation", "infection",
                                               "ipre", "other", "prematurity"),
                                             c(length(congenital_malformation), length(infection),
                                               length(ipre), length(other),
                                               length(prematurity))))

causes_neonate <- c("congenital_malformation", "infection","ipre", "other", "prematurity")
C <- length(causes_neonate)

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
# test1 <- setdiff(cause_map_neonate$causes,colnames(interva_probs_neonate))
# test2 <- setdiff(colnames(interva_probs_neonate),cause_map_neonate$causes)
# test1
# test2

test1 <- setdiff(cause_map_neonate$causes,colnames(insilico_probs_neonate))
test2 <- setdiff(colnames(insilico_probs_neonate),cause_map_neonate$causes)
test1
test2

# test1 <- setdiff(cause_map_child$causes,colnames(interva_probs_child))
# test2 <- setdiff(colnames(interva_probs_child),cause_map_child$causes)
# test1
# test2

test1 <- setdiff(cause_map_child$causes,colnames(insilico_probs_child))
test2 <- setdiff(colnames(insilico_probs_child),cause_map_child$causes)
test1
test2

### map individual causes into broad groups - InterVA Neonate
# model_broad_probs.InterVA.neonates <- interva_probs_neonate
# colnames(model_broad_probs.InterVA.neonates) <- cause_map_neonate$broad_cause[match(colnames(model_broad_probs.InterVA.neonates),
#                                                                                        cause_map_neonate$causes)]
# 
# mn <- model.matrix(~ colnames(model_broad_probs.InterVA.neonates) + 0)
# model_broad_probs.InterVA.neonates <- model_broad_probs.InterVA.neonates %*% mn
# colnames(model_broad_probs.InterVA.neonates) <- gsub("colnames\\(model_broad_probs.InterVA.neonates\\)", "",
#                                                         colnames(model_broad_probs.InterVA.neonates))
# model_broad_probs.InterVA.neonates <- model_broad_probs.InterVA.neonates[,causes_neonate]
# 
# head(model_broad_probs.InterVA.neonates)
# head(interva_probs_neonate)

### map individual causes into broad groups - InsilicoVA Neonate
model_broad_probs.InsilicoVA.neonates <- insilico_probs_neonate
colnames(model_broad_probs.InsilicoVA.neonates) <- cause_map_neonate$broad_cause[match(colnames(model_broad_probs.InsilicoVA.neonates),
                                                           cause_map_neonate$causes)]

mn <- model.matrix(~ colnames(model_broad_probs.InsilicoVA.neonates) + 0)
model_broad_probs.InsilicoVA.neonates <- model_broad_probs.InsilicoVA.neonates %*% mn
colnames(model_broad_probs.InsilicoVA.neonates) <- gsub("colnames\\(model_broad_probs.InsilicoVA.neonates\\)", "",
                                    colnames(model_broad_probs.InsilicoVA.neonates))
model_broad_probs.InsilicoVA.neonates <- model_broad_probs.InsilicoVA.neonates[,causes_neonate[!causes_neonate %in% "Unspecified"]]

head(model_broad_probs.InsilicoVA.neonates)
head(insilico_probs_neonate)

### map individual causes into broad groups - InterVA child
# model_broad_probs.InterVA.child <- interva_probs_child
# colnames(model_broad_probs.InterVA.child) <- cause_map_child$broad_cause[match(colnames(model_broad_probs.InterVA.child),
#                                                                                     cause_map_child$causes)]
# 
# mn <- model.matrix(~ colnames(model_broad_probs.InterVA.child) + 0)
# model_broad_probs.InterVA.child <- model_broad_probs.InterVA.child %*% mn
# colnames(model_broad_probs.InterVA.child) <- gsub("colnames\\(model_broad_probs.InterVA.child\\)", "",
#                                                      colnames(model_broad_probs.InterVA.child))
# model_broad_probs.InterVA.child <- model_broad_probs.InterVA.child[,causes_child]
# 
# head(model_broad_probs.InterVA.child)
# head(interva_probs_child)

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

# rownames(model_broad_probs.InterVA.neonates)
# colnames(model_broad_probs.InterVA.neonates)
rownames(model_broad_probs.InsilicoVA.neonates)
colnames(model_broad_probs.InsilicoVA.neonates)
# rownames(model_broad_probs.InterVA.child)
# colnames(model_broad_probs.InterVA.child)
rownames(model_broad_probs.InsilicoVA.child)
colnames(model_broad_probs.InsilicoVA.child)

# row.names(model_broad_probs.InterVA.neonates) <- gsub(' ', '', row.names(model_broad_probs.InterVA.neonates))
row.names(model_broad_probs.InsilicoVA.neonates) <- gsub(' ', '', row.names(model_broad_probs.InsilicoVA.neonates))
# row.names(model_broad_probs.InterVA.child) <- gsub(' ', '', row.names(model_broad_probs.InterVA.child))
row.names(model_broad_probs.InsilicoVA.child) <- gsub(' ', '', row.names(model_broad_probs.InsilicoVA.child))

### save objects
# saveRDS(model_broad_probs.InterVA.neonates, file ="Results/multi_interva_neonate_comsa.rds")
saveRDS(model_broad_probs.InsilicoVA.neonates, file ="Results/multi_insilicova_neonate_comsa.rds")

# saveRDS(model_broad_probs.InterVA.child, file ="Results/multi_interva_child_comsa.rds")
saveRDS(model_broad_probs.InsilicoVA.child, file ="Results/multi_insilicova_child_comsa.rds")
 





