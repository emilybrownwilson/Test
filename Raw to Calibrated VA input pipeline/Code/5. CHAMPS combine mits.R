# Last edited: 25 Jan 2021
# Last run:    25 Jan 2021

# Objective: find mits that have a va pair

library(openVA)
library(readxl)
library(reshape2)
library(dplyr)

'%!in%' <- function(x,y)!('%in%'(x,y))

file <- getwd()

### Original CHAMPS MITS - NEONATES
CHAMPS.MITS.neonate <- read_excel(file.path(file,"Data/CHAMPS-MITS - OCT2019 - FOR RECLASSIFICATION TO VA CATEGORIES(4-revised .._.xlsx"), sheet = "Neonates")
colnames(CHAMPS.MITS.neonate)

# Initial analysis
CHAMPS.MITS.neonate_initial <- CHAMPS.MITS.neonate[,c("champs_deid_2","VA Und COD (Initial analysis, revised 022020)","VA multiple COD (Initial analysis, revised 022020)","Name")]
CHAMPS.MITS.neonate <- CHAMPS.MITS.neonate_initial
names(CHAMPS.MITS.neonate)[names(CHAMPS.MITS.neonate) == 'champs_deid_2'] <- 'ID'
names(CHAMPS.MITS.neonate)[names(CHAMPS.MITS.neonate) == 'VA Und COD (Initial analysis, revised 022020)'] <- 'Underlying_cat'
names(CHAMPS.MITS.neonate)[names(CHAMPS.MITS.neonate) == 'VA multiple COD (Initial analysis, revised 022020)'] <- 'Immediate_cat'

CHAMPS.MITS.neonate <- subset(CHAMPS.MITS.neonate, !is.na(CHAMPS.MITS.neonate$Underlying_cat) & CHAMPS.MITS.neonate$Underlying_cat!="Unspecified")

table(CHAMPS.MITS.neonate$Underlying_cat, exclude = NULL)
table(CHAMPS.MITS.neonate$Immediate_cat, exclude = NULL)

CHAMPS.MITS.neonate_original <- CHAMPS.MITS.neonate


### Original CHAMPS MITS - CHILD
CHAMPS.MITS.child <- read_excel(file.path(file,"Data/CHAMPS-MITS - OCT2019 - FOR RECLASSIFICATION TO VA CATEGORIES(4-revised .._.xlsx"), sheet = "Child")
colnames(CHAMPS.MITS.child)

# Initial analysis
CHAMPS.MITS.child_initial <- CHAMPS.MITS.child[,c("champs_deid_2","VA Und COD (Initial analysis, revised 022020)","VA multiple COD (Initial analysis, revised 022020)","Name")]
CHAMPS.MITS.child <- CHAMPS.MITS.child_initial
names(CHAMPS.MITS.child)[names(CHAMPS.MITS.child) == 'champs_deid_2'] <- 'ID'
names(CHAMPS.MITS.child)[names(CHAMPS.MITS.child) == 'VA Und COD (Initial analysis, revised 022020)'] <- 'Underlying_cat'
names(CHAMPS.MITS.child)[names(CHAMPS.MITS.child) == 'VA multiple COD (Initial analysis, revised 022020)'] <- 'Immediate_cat'

CHAMPS.MITS.child <- subset(CHAMPS.MITS.child, CHAMPS.MITS.child$Underlying_cat!="Unspecified")
CHAMPS.MITS.child$Underlying_cat[CHAMPS.MITS.child$Underlying_cat=="Congenital"] <- "Other"
CHAMPS.MITS.child$Underlying_cat[CHAMPS.MITS.child$Underlying_cat=="Injury"] <- "Other"
CHAMPS.MITS.child$Underlying_cat[CHAMPS.MITS.child$Underlying_cat=="Meningits"] <- "Other infections"

table(CHAMPS.MITS.child$Underlying_cat, exclude = NULL)
table(CHAMPS.MITS.child$Immediate_cat, exclude = NULL)

CHAMPS.MITS.child_original <- CHAMPS.MITS.child





### Additional Nov 2019 CHAMPS MITS - NEONATES
CHAMPS.MITS.neonate <- read_excel(file.path(file,"Data/CHAMPS_MITS_VA_20191115/categories/CHAMPS-MITS-NEONATE - addtional cases 2019NOV15 - FOR RECLASSIFICATION T.._.xlsx"))

# Initial analysis
CHAMPS.MITS.neonate_initial <- CHAMPS.MITS.neonate[,c("champs_deid","VA Und COD (Initial)","VA multiple COD (Initial)","Name")]
CHAMPS.MITS.neonate <- CHAMPS.MITS.neonate_initial
names(CHAMPS.MITS.neonate)[names(CHAMPS.MITS.neonate) == 'champs_deid'] <- 'ID'
names(CHAMPS.MITS.neonate)[names(CHAMPS.MITS.neonate) == 'VA Und COD (Initial)'] <- 'Underlying_cat'
names(CHAMPS.MITS.neonate)[names(CHAMPS.MITS.neonate) == 'VA multiple COD (Initial)'] <- 'Immediate_cat'

CHAMPS.MITS.neonate <- subset(CHAMPS.MITS.neonate, !is.na(CHAMPS.MITS.neonate$Underlying_cat) & CHAMPS.MITS.neonate$Underlying_cat!="Unspecified" & CHAMPS.MITS.neonate$Underlying_cat!="??")

table(CHAMPS.MITS.neonate$Underlying_cat, exclude = NULL)
table(CHAMPS.MITS.neonate$Immediate_cat, exclude = NULL)

CHAMPS.MITS.neonate_nov2019 <- CHAMPS.MITS.neonate


### Additional NOV 2019 CHAMPS MITS - CHILD
CHAMPS.MITS.child <- read_excel(file.path(file,"Data/CHAMPS_MITS_VA_20191115/categories/CHAMPS-MITS-CHILD - addtional cases 2019NOV15 - FOR RECLASSIFICATION TO .._.xlsx"))

# Initial analysis
CHAMPS.MITS.child_initial <- CHAMPS.MITS.child[,c("champs_deid","VA Und COD (Initial)","VA multiple COD (Initial)","Name")]
CHAMPS.MITS.child <- CHAMPS.MITS.child_initial
names(CHAMPS.MITS.child)[names(CHAMPS.MITS.child) == 'champs_deid'] <- 'ID'
names(CHAMPS.MITS.child)[names(CHAMPS.MITS.child) == 'VA Und COD (Initial)'] <- 'Underlying_cat'
names(CHAMPS.MITS.child)[names(CHAMPS.MITS.child) == 'VA multiple COD (Initial)'] <- 'Immediate_cat'

CHAMPS.MITS.child <- subset(CHAMPS.MITS.child, CHAMPS.MITS.child$Underlying_cat!="Unspecified")
CHAMPS.MITS.child$Underlying_cat[CHAMPS.MITS.child$Underlying_cat=="Congenital"] <- "Other"
CHAMPS.MITS.child$Underlying_cat[CHAMPS.MITS.child$Underlying_cat=="Injury"] <- "Other"
CHAMPS.MITS.child$Underlying_cat[CHAMPS.MITS.child$Underlying_cat=="Meningits"] <- "Other infections"

table(CHAMPS.MITS.child$Underlying_cat, exclude = NULL)
table(CHAMPS.MITS.child$Immediate_cat, exclude = NULL)

CHAMPS.MITS.child_nov2019 <- CHAMPS.MITS.child







### Additional May 2020 CHAMPS MITS - NEONATES
CHAMPS.MITS.neonate <- read_excel(file.path(file,"Data/CHAMPS_MITS_VA_20200610/categories/CHAMPS-MITS-NEONATE - additional cases 2020MAY14 - RECLASSIFICATION TO VA CATEGORIES.xlsx"))

# Initial analysis
CHAMPS.MITS.neonate_initial <- CHAMPS.MITS.neonate[,c("champs_deid","VA Und COD (Initial analysis, 062020)","VA multiple COD (Initial analysis, 062020)","Name")]
CHAMPS.MITS.neonate <- CHAMPS.MITS.neonate_initial
names(CHAMPS.MITS.neonate)[names(CHAMPS.MITS.neonate) == 'champs_deid'] <- 'ID'
names(CHAMPS.MITS.neonate)[names(CHAMPS.MITS.neonate) == 'VA Und COD (Initial analysis, 062020)'] <- 'Underlying_cat'
names(CHAMPS.MITS.neonate)[names(CHAMPS.MITS.neonate) == 'VA multiple COD (Initial analysis, 062020)'] <- 'Immediate_cat'

CHAMPS.MITS.neonate <- subset(CHAMPS.MITS.neonate, CHAMPS.MITS.neonate$Underlying_cat!="Unspecified")

table(CHAMPS.MITS.neonate$Underlying_cat, exclude = NULL)
table(CHAMPS.MITS.neonate$Immediate_cat, exclude = NULL)

CHAMPS.MITS.neonate_may2020 <- CHAMPS.MITS.neonate


### Additional May 2020 CHAMPS MITS - CHILD
CHAMPS.MITS.child <- read_excel(file.path(file,"Data/CHAMPS_MITS_VA_20200610/categories/CHAMPS-MITS-CHILD - additional cases 2020MAY14 - RECLASSIFICATION TO VA CATEGORIES.xlsx"))

# Initial analysis
CHAMPS.MITS.child_initial <- CHAMPS.MITS.child[,c("champs_deid","VA Und COD (Initial analysis, 062020)","VA multiple COD (Initial analysis, 062020)","Name")]
CHAMPS.MITS.child <- CHAMPS.MITS.child_initial
names(CHAMPS.MITS.child)[names(CHAMPS.MITS.child) == 'champs_deid'] <- 'ID'
names(CHAMPS.MITS.child)[names(CHAMPS.MITS.child) == 'VA Und COD (Initial analysis, 062020)'] <- 'Underlying_cat'
names(CHAMPS.MITS.child)[names(CHAMPS.MITS.child) == 'VA multiple COD (Initial analysis, 062020)'] <- 'Immediate_cat'

CHAMPS.MITS.child <- subset(CHAMPS.MITS.child, CHAMPS.MITS.child$Underlying_cat!="Unspecified")
CHAMPS.MITS.child$Immediate_cat[CHAMPS.MITS.child$Immediate_cat=="Prematurity"] <- "Other"

table(CHAMPS.MITS.child$Underlying_cat, exclude = NULL)
table(CHAMPS.MITS.child$Immediate_cat, exclude = NULL)

CHAMPS.MITS.child_may2020 <- CHAMPS.MITS.child









### Combine Original, November 2019, and May 2020 MITS - NEONATE
head(CHAMPS.MITS.neonate_original)
head(CHAMPS.MITS.neonate_nov2019)
head(CHAMPS.MITS.neonate_may2020)
dim(CHAMPS.MITS.neonate_original)
dim(CHAMPS.MITS.neonate_nov2019)
dim(CHAMPS.MITS.neonate_may2020)

# Drop one case that is in both sets (suspect that these did not have VAs before)
test <- merge(CHAMPS.MITS.neonate_original,CHAMPS.MITS.neonate_nov2019,by=c("ID"))
CHAMPS.MITS.neonate_original <- subset(CHAMPS.MITS.neonate_original, ID %!in% c("F6FE3DEF-2A73-498D-B9B8-C04ECA8C66DD"))
dim(CHAMPS.MITS.neonate_original)

test <- merge(CHAMPS.MITS.neonate_original,CHAMPS.MITS.neonate_may2020,by=c("ID"))
CHAMPS.MITS.neonate_original <- subset(CHAMPS.MITS.neonate_original, ID %!in% c("28388379-021A-425E-AA00-6CA541B89DB1","99DDEC89-A944-45BE-B57F-8C9A60CACE51"))

test <- merge(CHAMPS.MITS.neonate_nov2019,CHAMPS.MITS.neonate_may2020,by=c("ID"))

CHAMPS.MITS.neonate <- rbind(CHAMPS.MITS.neonate_original,CHAMPS.MITS.neonate_nov2019,CHAMPS.MITS.neonate_may2020)
CHAMPS.MITS.neonate$Underlying_cat[CHAMPS.MITS.neonate$Underlying_cat=="Congenital malformation"] <- "Congenital_malformation"
CHAMPS.MITS.neonate$Underlying_cat[CHAMPS.MITS.neonate$Underlying_cat=="Congenital"] <- "Congenital_malformation"
unique(CHAMPS.MITS.neonate$Underlying_cat, exclude=NULL)
CHAMPS.MITS.neonate$Immediate_cat[CHAMPS.MITS.neonate$Immediate_cat=="Congenital malformation"] <- "Congenital_malformation"
CHAMPS.MITS.neonate$Immediate_cat[CHAMPS.MITS.neonate$Immediate_cat=="Congenital"] <- "Congenital_malformation"
unique(CHAMPS.MITS.neonate$Immediate_cat, exclude=NULL)
dim(CHAMPS.MITS.neonate)


### Combine Original, November 2019, and May 2020 MITS - CHILD
head(CHAMPS.MITS.child_original)
head(CHAMPS.MITS.child_nov2019)
head(CHAMPS.MITS.child_may2020)
dim(CHAMPS.MITS.child_original)
dim(CHAMPS.MITS.child_nov2019)
dim(CHAMPS.MITS.child_may2020)

# Drop two cases that are in both sets (suspect that these did not have VAs last time)
test <- merge(CHAMPS.MITS.child_original,CHAMPS.MITS.child_nov2019, by=c("ID"))
CHAMPS.MITS.child_original <- subset(CHAMPS.MITS.child_original, ID %!in% c("86D6D280-0CA0-43F5-8EC5-F4F1454166CD","DD360EBA-5C37-4A6B-841F-95E45ACCC650"))
dim(CHAMPS.MITS.child_original)

test <- merge(CHAMPS.MITS.child_original,CHAMPS.MITS.child_may2020,by=c("ID"))

test <- merge(CHAMPS.MITS.child_nov2019,CHAMPS.MITS.child_may2020,by=c("ID"))

CHAMPS.MITS.child <- rbind(CHAMPS.MITS.child_original,CHAMPS.MITS.child_nov2019,CHAMPS.MITS.child_may2020)
unique(CHAMPS.MITS.child$Underlying_cat)
unique(CHAMPS.MITS.child$Immediate_cat)
dim(CHAMPS.MITS.child)

write.csv(CHAMPS.MITS.neonate, "Data/mits_neonate_champs.csv", row.names = FALSE)
write.csv(CHAMPS.MITS.child, "Data/mits_child_champs.csv", row.names = FALSE)


















