# Last edited: 2 Aug 2020
# Last run:    8 Aug 2020

# Objective: format multi cause mits champs

library(openVA)
library(readxl)
library(reshape2)
library(dplyr)

'%!in%' <- function(x,y)!('%in%'(x,y))

file <- getwd()

multi_cause_probs.MITS.neonate.CHAMPS <- read.csv("Data/mits_neonate_champs.csv")
multi_cause_probs.MITS.child.CHAMPS <- read.csv("Data/mits_child_champs.csv")

head(multi_cause_probs.MITS.neonate.CHAMPS)
dim(multi_cause_probs.MITS.neonate.CHAMPS)
head(multi_cause_probs.MITS.child.CHAMPS)
dim(multi_cause_probs.MITS.child.CHAMPS)

### CHAMPS - Neonates
multi_cause_probs.MITS.neonate.CHAMPS <- multi_cause_probs.MITS.neonate.CHAMPS[,c("ID","Underlying_cat","Immediate_cat")]
table(multi_cause_probs.MITS.neonate.CHAMPS$Underlying_cat, multi_cause_probs.MITS.neonate.CHAMPS$Immediate_cat, exclude=NULL)
multi_cause_probs.MITS.neonate.CHAMPS$X[multi_cause_probs.MITS.neonate.CHAMPS$Immediate_cat=="None"] <- 1
multi_cause_probs.MITS.neonate.CHAMPS$X[multi_cause_probs.MITS.neonate.CHAMPS$Underlying_cat!="." & multi_cause_probs.MITS.neonate.CHAMPS$Immediate_cat!="None"] <- .5
multi_cause_probs.MITS.neonate.CHAMPS.1 <- dcast(multi_cause_probs.MITS.neonate.CHAMPS, ID ~ Underlying_cat, value.var = "X", fun.aggregate = mean)

multi_cause_probs.MITS.neonate.CHAMPS$X[multi_cause_probs.MITS.neonate.CHAMPS$Immediate_cat!="."] <- .5
multi_cause_probs.MITS.neonate.CHAMPS.2 <- dcast(multi_cause_probs.MITS.neonate.CHAMPS, ID ~ Immediate_cat, value.var = "X", fun.aggregate = mean)

multi_cause_probs.MITS.neonate.CHAMPS.1$`.` <- NULL
multi_cause_probs.MITS.neonate.CHAMPS.2$`.` <- NULL

multi_cause_probs.MITS.neonate.CHAMPS_original <- multi_cause_probs.MITS.neonate.CHAMPS
head(multi_cause_probs.MITS.neonate.CHAMPS_original)
head(multi_cause_probs.MITS.neonate.CHAMPS.1)
head(multi_cause_probs.MITS.neonate.CHAMPS.2)
head(multi_cause_probs.MITS.neonate.CHAMPS)
class(multi_cause_probs.MITS.neonate.CHAMPS)

multi_cause_probs.MITS.neonate.CHAMPS <- merge(multi_cause_probs.MITS.neonate.CHAMPS.1,multi_cause_probs.MITS.neonate.CHAMPS.2, by=c("ID"))
head(multi_cause_probs.MITS.neonate.CHAMPS)

multi_cause_probs.MITS.neonate.CHAMPS <- multi_cause_probs.MITS.neonate.CHAMPS %>% rowwise() %>% mutate(infection = sum(Infection.x,Infection.y, na.rm=TRUE))
multi_cause_probs.MITS.neonate.CHAMPS <- multi_cause_probs.MITS.neonate.CHAMPS %>% rowwise() %>% mutate(ipre = sum(IPRE.x,IPRE.y, na.rm=TRUE))
multi_cause_probs.MITS.neonate.CHAMPS <- multi_cause_probs.MITS.neonate.CHAMPS %>% rowwise() %>% mutate(congenital_malformation = sum(Congenital_malformation.x,Congenital_malformation.y, na.rm=TRUE))
multi_cause_probs.MITS.neonate.CHAMPS <- multi_cause_probs.MITS.neonate.CHAMPS %>% rowwise() %>% mutate(other = sum(Other.x,Other.y, na.rm=TRUE))
multi_cause_probs.MITS.neonate.CHAMPS <- multi_cause_probs.MITS.neonate.CHAMPS %>% rowwise() %>% mutate(prematurity = sum(Prematurity.x,Prematurity.y, na.rm=TRUE))

multi_cause_probs.MITS.neonate.CHAMPS <- as.data.frame(multi_cause_probs.MITS.neonate.CHAMPS[,c("ID","congenital_malformation","infection","ipre","other","prematurity")])

multi_cause_probs.MITS.neonate.CHAMPS <- subset(multi_cause_probs.MITS.neonate.CHAMPS, ID!="44F316AA-58A0-4E64-9F1A-6837588C3855")

rownames(multi_cause_probs.MITS.neonate.CHAMPS) <- multi_cause_probs.MITS.neonate.CHAMPS[,"ID"]
multi_cause_probs.MITS.neonate.CHAMPS$ID <- NULL

head(multi_cause_probs.MITS.neonate.CHAMPS_original)
head(multi_cause_probs.MITS.neonate.CHAMPS.1)
head(multi_cause_probs.MITS.neonate.CHAMPS.2)
head(multi_cause_probs.MITS.neonate.CHAMPS)
dim(multi_cause_probs.MITS.neonate.CHAMPS)

# undecided.MITS.neonate.CHAMPS <- multi_cause_probs.MITS.neonate.CHAMPS[rowSums(multi_cause_probs.MITS.neonate.CHAMPS) == 0, ]
# undecided.MITS.neonate.CHAMPS[,1:ncol(undecided.MITS.neonate.CHAMPS)] <- 1/(ncol(undecided.MITS.neonate.CHAMPS))
# not.undecided.MITS.neonate.CHAMPS <- multi_cause_probs.MITS.neonate.CHAMPS[rowSums(multi_cause_probs.MITS.neonate.CHAMPS) == 1, ]
# multi_cause_probs.MITS.neonate.CHAMPS <- rbind(not.undecided.MITS.neonate.CHAMPS,undecided.MITS.neonate.CHAMPS)



### CHAMPS - Child
multi_cause_probs.MITS.child.CHAMPS <- multi_cause_probs.MITS.child.CHAMPS[,c("ID","Underlying_cat","Immediate_cat")]
table(multi_cause_probs.MITS.child.CHAMPS$Underlying_cat, multi_cause_probs.MITS.child.CHAMPS$Immediate_cat, exclude=NULL)
multi_cause_probs.MITS.child.CHAMPS$X[multi_cause_probs.MITS.child.CHAMPS$Immediate_cat=="None"] <- 1
multi_cause_probs.MITS.child.CHAMPS$X[multi_cause_probs.MITS.child.CHAMPS$Underlying_cat!="." & multi_cause_probs.MITS.child.CHAMPS$Immediate_cat!="None"] <- .5
multi_cause_probs.MITS.child.CHAMPS.1 <- dcast(multi_cause_probs.MITS.child.CHAMPS, ID ~ Underlying_cat, value.var = "X", fun.aggregate = mean)

multi_cause_probs.MITS.child.CHAMPS$X[multi_cause_probs.MITS.child.CHAMPS$Immediate_cat!="."] <- .5
multi_cause_probs.MITS.child.CHAMPS.2 <- dcast(multi_cause_probs.MITS.child.CHAMPS, ID ~ Immediate_cat, value.var = "X", fun.aggregate = mean)

# names(multi_cause_probs.MITS.child.CHAMPS.1)[names(multi_cause_probs.MITS.child.CHAMPS.1) == 'Congenital'] <- 'Malformation'
# multi_cause_probs.MITS.child.CHAMPS.2$Malformation <- NaN
multi_cause_probs.MITS.child.CHAMPS.1$`.` <- NULL
multi_cause_probs.MITS.child.CHAMPS.2$`.` <- NULL

multi_cause_probs.MITS.child.CHAMPS_original <- multi_cause_probs.MITS.child.CHAMPS
head(multi_cause_probs.MITS.child.CHAMPS_original)
head(multi_cause_probs.MITS.child.CHAMPS.1)
head(multi_cause_probs.MITS.child.CHAMPS.2)
head(multi_cause_probs.MITS.child.CHAMPS)

multi_cause_probs.MITS.child.CHAMPS <- merge(multi_cause_probs.MITS.child.CHAMPS.1,multi_cause_probs.MITS.child.CHAMPS.2, by=c("ID"))

multi_cause_probs.MITS.child.CHAMPS <- multi_cause_probs.MITS.child.CHAMPS %>% rowwise() %>% mutate(diarrhea = sum(Diarrhea.x,Diarrhea.y, na.rm=TRUE))
multi_cause_probs.MITS.child.CHAMPS <- multi_cause_probs.MITS.child.CHAMPS %>% rowwise() %>% mutate(malaria = sum(Malaria.x,Malaria.y, na.rm=TRUE))
multi_cause_probs.MITS.child.CHAMPS <- multi_cause_probs.MITS.child.CHAMPS %>% rowwise() %>% mutate(other = sum(Other.x,Other.y, na.rm=TRUE))
multi_cause_probs.MITS.child.CHAMPS <- multi_cause_probs.MITS.child.CHAMPS %>% rowwise() %>% mutate(other_infections = sum(`Other infections.x`,`Other infections.y`, na.rm=TRUE))
multi_cause_probs.MITS.child.CHAMPS <- multi_cause_probs.MITS.child.CHAMPS %>% rowwise() %>% mutate(pneumonia = sum(Pneumonia.x,Pneumonia.y, na.rm=TRUE))
multi_cause_probs.MITS.child.CHAMPS <- multi_cause_probs.MITS.child.CHAMPS %>% rowwise() %>% mutate(severe_malnutrition = sum(`Severe malnutrition.x`,`Severe malnutrition.y`, na.rm=TRUE))
names(multi_cause_probs.MITS.child.CHAMPS)[names(multi_cause_probs.MITS.child.CHAMPS) == 'HIV'] <- 'hiv'

multi_cause_probs.MITS.child.CHAMPS <- as.data.frame(multi_cause_probs.MITS.child.CHAMPS[,c("ID","malaria","pneumonia","diarrhea","severe_malnutrition","hiv","other","other_infections")])

rownames(multi_cause_probs.MITS.child.CHAMPS) <- multi_cause_probs.MITS.child.CHAMPS[,"ID"]
multi_cause_probs.MITS.child.CHAMPS$ID <- NULL

head(multi_cause_probs.MITS.child.CHAMPS_original)
head(multi_cause_probs.MITS.child.CHAMPS.1)
head(multi_cause_probs.MITS.child.CHAMPS.2)
head(multi_cause_probs.MITS.child.CHAMPS)


### save objects
saveRDS(multi_cause_probs.MITS.neonate.CHAMPS, file ="Results/multi_mits_neonate_champs.rds")
saveRDS(multi_cause_probs.MITS.child.CHAMPS, file ="Results/multi_mits_child_champs.rds")



