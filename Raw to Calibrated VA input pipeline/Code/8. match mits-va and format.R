# Last edited: 2 Aug 2020
# Last run:    8 Aug 2020

# Objective: format matrices so that row names match across methods

library(openVA)
library(reshape2)
library(dplyr)
library(compare)

file <- getwd()
date <- "20200808_comsa_data"

# 1. Single cause VA COMSA matrix : This is a matrix where each row represents a subject in the COMSA set, and each column represents a cause. 
#     Then for entry i,j (i-th individual, jth cause), there is a 1 is that is the single VA COD for that individual, and 0 if not (using the condensed final cause list). 
#     For EAVA, if individual i has an undecided COD, then put 1/C in each entry for that individual, where C is the number of causes
single_interva_neonate_comsa <- readRDS("Results/single_interva_neonate_comsa.rds")
single_insilicova_neonate_comsa <- readRDS("Results/single_insilicova_neonate_comsa.rds")
single_eava_neonate_comsa <- readRDS("Results/single_eava_neonate_comsa.rds")

head(single_interva_neonate_comsa)
head(single_insilicova_neonate_comsa)
head(single_eava_neonate_comsa)

single_insilicova_neonate_comsa <- single_insilicova_neonate_comsa[match(rownames(single_interva_neonate_comsa), rownames(single_insilicova_neonate_comsa)), ]
single_eava_neonate_comsa <- single_eava_neonate_comsa[match(rownames(single_interva_neonate_comsa), rownames(single_eava_neonate_comsa)), ]

identical(rownames(single_interva_neonate_comsa), rownames(single_insilicova_neonate_comsa))
identical(rownames(single_interva_neonate_comsa), rownames(single_eava_neonate_comsa))

identical(colnames(single_interva_neonate_comsa), colnames(single_insilicova_neonate_comsa))
identical(colnames(single_interva_neonate_comsa), colnames(single_eava_neonate_comsa))

dim(single_interva_neonate_comsa)
dim(single_insilicova_neonate_comsa)
dim(single_eava_neonate_comsa)

saveRDS(single_interva_neonate_comsa, file =paste0("Results/",date,"/single_interva_neonate_comsa.rds"))
saveRDS(single_insilicova_neonate_comsa, file =paste0("Results/",date,"/single_insilicova_neonate_comsa.rds"))
saveRDS(single_eava_neonate_comsa, file =paste0("Results/",date,"/single_eava_neonate_comsa.rds"))



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

saveRDS(single_interva_child_comsa, file =paste0("Results/",date,"/single_interva_child_comsa.rds"))
saveRDS(single_insilicova_child_comsa, file =paste0("Results/",date,"/single_insilicova_child_comsa.rds"))
saveRDS(single_eava_child_comsa, file =paste0("Results/",date,"/single_eava_child_comsa.rds"))


# 2. Multi-cause VA COMSA matrix: For the same individuals above, entry i,j is now the VA algorithm probability for cause j for individual i. 
#    For EAVA if there are two different causes, put .75 for the top cause and .25 for the second cause. Again put 1/C for each entry if the cause is undecided 
multi_interva_neonate_comsa <- readRDS("Results/multi_interva_neonate_comsa.rds")
multi_insilicova_neonate_comsa <- readRDS("Results/multi_insilicova_neonate_comsa.rds")
multi_eava_neonate_comsa <- readRDS("Results/multi_eava_neonate_comsa.rds")

head(multi_interva_neonate_comsa)
head(multi_insilicova_neonate_comsa)
head(multi_eava_neonate_comsa)

multi_insilicova_neonate_comsa <- multi_insilicova_neonate_comsa[match(rownames(multi_interva_neonate_comsa), rownames(multi_insilicova_neonate_comsa)), ]
multi_eava_neonate_comsa <- multi_eava_neonate_comsa[match(rownames(multi_interva_neonate_comsa), rownames(multi_eava_neonate_comsa)), ]

identical(rownames(multi_interva_neonate_comsa), rownames(multi_insilicova_neonate_comsa))
identical(rownames(multi_interva_neonate_comsa), rownames(multi_eava_neonate_comsa))

identical(colnames(multi_interva_neonate_comsa), colnames(multi_insilicova_neonate_comsa))
identical(colnames(multi_interva_neonate_comsa), colnames(multi_eava_neonate_comsa))

dim(multi_interva_neonate_comsa)
dim(multi_insilicova_neonate_comsa)
dim(multi_eava_neonate_comsa)

saveRDS(multi_interva_neonate_comsa, file =paste0("Results/",date,"/multi_interva_neonate_comsa.rds"))
saveRDS(multi_insilicova_neonate_comsa, file =paste0("Results/",date,"/multi_insilicova_neonate_comsa.rds"))
saveRDS(multi_eava_neonate_comsa, file =paste0("Results/",date,"/multi_eava_neonate_comsa.rds"))


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

saveRDS(multi_interva_child_comsa, file =paste0("Results/",date,"/multi_interva_child_comsa.rds"))
saveRDS(multi_insilicova_child_comsa, file =paste0("Results/",date,"/multi_insilicova_child_comsa.rds"))
saveRDS(multi_eava_child_comsa, file =paste0("Results/",date,"/multi_eava_child_comsa.rds"))









# 3. Single-cause MITS CHAMPS matrix. Again, for entry i,j put a 1 if cause j is the underlying MITS cause, and 0 otherwise
single_mits_neonate_champs <- readRDS("Results/single_mits_neonate_champs.rds")
head(single_mits_neonate_champs)
dim(single_mits_neonate_champs)

single_mits_child_champs <- readRDS("Results/single_mits_child_champs.rds")
head(single_mits_child_champs)
dim(single_mits_child_champs)


# 4. Multi-cause MITS CHAMPS matrix. For entry i,j, put a 1 if cause j is the underlying MITS cause and there are no immediate causes 
#    (or the immediate cause is the same as the underlying). Otherwise, put a .5 if cause j is either an underlying or immediate cause
multi_mits_neonate_champs <- readRDS("Results/multi_mits_neonate_champs.rds")
head(multi_mits_neonate_champs)
dim(multi_mits_neonate_champs)

multi_mits_child_champs <- readRDS("Results/multi_mits_child_champs.rds")
head(multi_mits_child_champs)
dim(multi_mits_child_champs)

multi_mits_child_champs[is.na(multi_mits_child_champs)]<-0

identical(rownames(single_mits_neonate_champs), rownames(multi_mits_neonate_champs))
identical(rownames(single_mits_child_champs), rownames(multi_mits_child_champs))

identical(colnames(single_mits_neonate_champs), colnames(multi_mits_neonate_champs))
identical(colnames(single_mits_child_champs), colnames(multi_mits_child_champs))









# 5. Single cause and multi-cause VA CHAMPS matrices, defined the same way as 1 and 2, except now for the CHAMPS data. 
#    Be careful with the getIndivProb function, because we’ve seen that it can screw up the ordering of the subjects, 
#    so make sure that the rows in these matrices align with the MITS CHAMPS matrices (i.e. the ID should be the same for each row of the CHAMPS matrices
### SINGLE
single_interva_neonate_champs <- readRDS("Results/single_interva_neonate_champs.rds")
single_insilicova_neonate_champs <- readRDS("Results/single_insilicova_neonate_champs.rds")
single_eava_neonate_champs <- readRDS("Results/single_eava_neonate_champs.rds")

head(single_interva_neonate_champs)
head(single_insilicova_neonate_champs)
head(single_eava_neonate_champs)

dim(single_interva_neonate_champs)
dim(single_insilicova_neonate_champs)
dim(single_eava_neonate_champs)

### MATCH MITS AND VA
interva_mits <- merge(single_interva_neonate_champs,single_mits_neonate_champs, by="row.names")
head(interva_mits)
### SEPARATE INTERVA
single_interva_neonate_champs <- interva_mits[,c("Row.names","congenital_malformation.x","infection.x","ipre.x","other.x","prematurity.x")]
names(single_interva_neonate_champs) = sub(".x","",names(single_interva_neonate_champs))
rownames(single_interva_neonate_champs) <- single_interva_neonate_champs[,"Row.names"]
single_interva_neonate_champs$Row.names <- NULL
### SEPARATE MITS
single_mits_neonate_champs <- interva_mits[,c("Row.names","congenital_malformation.y","infection.y","ipre.y","other.y","prematurity.y")]
names(single_mits_neonate_champs) = sub(".y","",names(single_mits_neonate_champs))
names(single_mits_neonate_champs)[names(single_mits_neonate_champs) == 'prematuri.y'] <- 'prematurity'
rownames(single_mits_neonate_champs) <- single_mits_neonate_champs[,"Row.names"]
single_mits_neonate_champs$Row.names <- NULL

### SEPARATE INSILICOVA
single_insilicova_neonate_champs <- single_insilicova_neonate_champs[match(rownames(single_interva_neonate_champs), rownames(single_insilicova_neonate_champs)), ]

### SEPARATE EAVA
single_eava_neonate_champs <- single_eava_neonate_champs[match(rownames(single_interva_neonate_champs), rownames(single_eava_neonate_champs)), ]

setdiff(rownames(single_interva_neonate_champs), rownames(single_insilicova_neonate_champs))
setdiff(rownames(single_interva_neonate_champs), rownames(single_mits_neonate_champs))

setdiff(rownames(single_interva_neonate_champs), rownames(single_eava_neonate_champs))
setdiff(rownames(single_eava_neonate_champs), rownames(single_interva_neonate_champs))
###

identical(rownames(single_interva_neonate_champs), rownames(single_insilicova_neonate_champs))
identical(rownames(single_interva_neonate_champs), rownames(single_eava_neonate_champs))
compare(rownames(single_interva_neonate_champs), rownames(single_eava_neonate_champs))

identical(colnames(single_interva_neonate_champs), colnames(single_insilicova_neonate_champs))
identical(colnames(single_interva_neonate_champs), colnames(single_eava_neonate_champs))

dim(single_interva_neonate_champs)
dim(single_insilicova_neonate_champs)
dim(single_eava_neonate_champs)

saveRDS(single_mits_neonate_champs, file =paste0("Results/",date,"/single_mits_neonate_champs.rds"))

saveRDS(single_interva_neonate_champs, file =paste0("Results/",date,"/single_interva_neonate_champs.rds"))
saveRDS(single_insilicova_neonate_champs, file =paste0("Results/",date,"/single_insilicova_neonate_champs.rds"))
saveRDS(single_eava_neonate_champs, file =paste0("Results/",date,"/single_eava_neonate_champs.rds"))

### CHILD

single_interva_child_champs <- readRDS("Results/single_interva_child_champs.rds")
single_insilicova_child_champs <- readRDS("Results/single_insilicova_child_champs.rds")
single_eava_child_champs <- readRDS("Results/single_eava_child_champs.rds")

head(single_interva_child_champs)
head(single_insilicova_child_champs)
head(single_eava_child_champs)

interva_mits <- merge(single_interva_child_champs,single_mits_child_champs, by="row.names")
head(interva_mits)
### SEPARATE VA
single_interva_child_champs <- interva_mits[,c("Row.names","malaria.x","pneumonia.x","diarrhea.x","severe_malnutrition.x","hiv.x","other.x","other_infections.x")]
names(single_interva_child_champs) = sub(".x","",names(single_interva_child_champs))
rownames(single_interva_child_champs) <- single_interva_child_champs[,"Row.names"]
single_interva_child_champs$Row.names <- NULL
### SEPARATE MITS
single_mits_child_champs <- interva_mits[,c("Row.names","malaria.y","pneumonia.y","diarrhea.y","severe_malnutrition.y","hiv.y","other.y","other_infections.y")]
names(single_mits_child_champs) = sub(".y","",names(single_mits_child_champs))
rownames(single_mits_child_champs) <- single_mits_child_champs[,"Row.names"]
single_mits_child_champs$Row.names <- NULL

single_insilicova_child_champs <- single_insilicova_child_champs[match(rownames(single_interva_child_champs), rownames(single_insilicova_child_champs)), ]
single_eava_child_champs <- single_eava_child_champs[match(rownames(single_interva_child_champs), rownames(single_eava_child_champs)), ]

identical(rownames(single_interva_child_champs), rownames(single_insilicova_child_champs))
identical(rownames(single_interva_child_champs), rownames(single_eava_child_champs))

identical(colnames(single_interva_child_champs), colnames(single_insilicova_child_champs))
identical(colnames(single_interva_child_champs), colnames(single_eava_child_champs))

dim(single_interva_child_champs)

dim(single_insilicova_child_champs)
dim(single_eava_child_champs)

saveRDS(single_mits_child_champs, file =paste0("Results/",date,"/single_mits_child_champs.rds"))

saveRDS(single_interva_child_champs, file =paste0("Results/",date,"/single_interva_child_champs.rds"))
saveRDS(single_insilicova_child_champs, file =paste0("Results/",date,"/single_insilicova_child_champs.rds"))
saveRDS(single_eava_child_champs, file =paste0("Results/",date,"/single_eava_child_champs.rds"))





### MULTI
multi_interva_neonate_champs <- readRDS("Results/multi_interva_neonate_champs.rds")
multi_insilicova_neonate_champs <- readRDS("Results/multi_insilicova_neonate_champs.rds")
multi_eava_neonate_champs <- readRDS("Results/multi_eava_neonate_champs.rds")

head(multi_interva_neonate_champs)
head(multi_insilicova_neonate_champs)
head(multi_eava_neonate_champs)

### MATCH MITS AND VA
interva_mits <- merge(multi_interva_neonate_champs,multi_mits_neonate_champs, by="row.names")
head(interva_mits)
### SEPARATE VA
multi_interva_neonate_champs <- interva_mits[,c("Row.names","congenital_malformation.x","infection.x","ipre.x","other.x","prematurity.x")]
names(multi_interva_neonate_champs) = sub(".x","",names(multi_interva_neonate_champs))
rownames(multi_interva_neonate_champs) <- multi_interva_neonate_champs[,"Row.names"]
multi_interva_neonate_champs$Row.names <- NULL
### SEPARATE MITS
multi_mits_neonate_champs <- interva_mits[,c("Row.names","congenital_malformation.y","infection.y","ipre.y","other.y","prematurity.y")]
names(multi_mits_neonate_champs) = sub(".y","",names(multi_mits_neonate_champs))
names(multi_mits_neonate_champs)[names(multi_mits_neonate_champs) == 'prematuri.y'] <- 'prematurity'
rownames(multi_mits_neonate_champs) <- multi_mits_neonate_champs[,"Row.names"]
multi_mits_neonate_champs$Row.names <- NULL

multi_insilicova_neonate_champs <- multi_insilicova_neonate_champs[match(rownames(multi_interva_neonate_champs), rownames(multi_insilicova_neonate_champs)), ]
multi_eava_neonate_champs <- multi_eava_neonate_champs[match(rownames(multi_interva_neonate_champs), rownames(multi_eava_neonate_champs)), ]

identical(rownames(multi_interva_neonate_champs), rownames(multi_insilicova_neonate_champs))
identical(rownames(multi_interva_neonate_champs), rownames(multi_eava_neonate_champs))

identical(colnames(multi_interva_neonate_champs), colnames(multi_insilicova_neonate_champs))
identical(colnames(multi_interva_neonate_champs), colnames(multi_eava_neonate_champs))

dim(multi_interva_neonate_champs)
dim(multi_insilicova_neonate_champs)
dim(multi_eava_neonate_champs)

saveRDS(multi_mits_neonate_champs, file =paste0("Results/",date,"/multi_mits_neonate_champs.rds"))

saveRDS(multi_interva_neonate_champs, file =paste0("Results/",date,"/multi_interva_neonate_champs.rds"))
saveRDS(multi_insilicova_neonate_champs, file =paste0("Results/",date,"/multi_insilicova_neonate_champs.rds"))
saveRDS(multi_eava_neonate_champs, file =paste0("Results/",date,"/multi_eava_neonate_champs.rds"))





multi_interva_child_champs <- readRDS("Results/multi_interva_child_champs.rds")
multi_insilicova_child_champs <- readRDS("Results/multi_insilicova_child_champs.rds")
multi_eava_child_champs <- readRDS("Results/multi_eava_child_champs.rds")

head(multi_interva_child_champs)
head(multi_insilicova_child_champs)
head(multi_eava_child_champs)

### MATCH MITS AND VA
interva_mits <- merge(multi_interva_child_champs,multi_mits_child_champs, by="row.names")
head(interva_mits)
### SEPARATE VA
multi_interva_child_champs <- interva_mits[,c("Row.names","malaria.x","pneumonia.x","diarrhea.x","severe_malnutrition.x","hiv.x","other.x","other_infections.x")]
names(multi_interva_child_champs) = sub(".x","",names(multi_interva_child_champs))
rownames(multi_interva_child_champs) <- multi_interva_child_champs[,"Row.names"]
multi_interva_child_champs$Row.names <- NULL
### SEPARATE MITS
multi_mits_child_champs <- interva_mits[,c("Row.names","malaria.y","pneumonia.y","diarrhea.y","severe_malnutrition.y","hiv.y","other.y","other_infections.y")]
names(multi_mits_child_champs) = sub(".y","",names(multi_mits_child_champs))
rownames(multi_mits_child_champs) <- multi_mits_child_champs[,"Row.names"]
multi_mits_child_champs$Row.names <- NULL

multi_insilicova_child_champs <- multi_insilicova_child_champs[match(rownames(multi_interva_child_champs), rownames(multi_insilicova_child_champs)), ]
multi_eava_child_champs <- multi_eava_child_champs[match(rownames(multi_interva_child_champs), rownames(multi_eava_child_champs)), ]

identical(rownames(multi_interva_child_champs), rownames(multi_insilicova_child_champs))
identical(rownames(multi_interva_child_champs), rownames(multi_eava_child_champs))

identical(colnames(multi_interva_child_champs), colnames(multi_insilicova_child_champs))
identical(colnames(multi_interva_child_champs), colnames(multi_eava_child_champs))

dim(multi_interva_child_champs)
dim(multi_insilicova_child_champs)
dim(multi_eava_child_champs)

saveRDS(multi_mits_child_champs, file =paste0("Results/",date,"/multi_mits_child_champs.rds"))

saveRDS(multi_interva_child_champs, file =paste0("Results/",date,"/multi_interva_child_champs.rds"))
saveRDS(multi_insilicova_child_champs, file =paste0("Results/",date,"/multi_insilicova_child_champs.rds"))
saveRDS(multi_eava_child_champs, file =paste0("Results/",date,"/multi_eava_child_champs.rds"))




identical(rownames(single_interva_child_champs), rownames(multi_interva_child_champs))
identical(rownames(single_interva_child_champs), rownames(multi_interva_child_champs))

identical(colnames(single_interva_child_champs), colnames(multi_interva_child_champs))
identical(colnames(single_interva_child_champs), colnames(multi_interva_child_champs))






######## produce spreadsheet with MITS IDs and countries
single_mits_neonate <- readRDS(file =paste0("Results/",date,"/single_mits_neonate_champs.rds"))
single_mits_child <- readRDS(file =paste0("Results/",date,"/single_mits_child_champs.rds"))
multi_mits_neonate <- readRDS(file =paste0("Results/",date,"/multi_mits_neonate_champs.rds"))
multi_mits_child <- readRDS(file =paste0("Results/",date,"/multi_mits_child_champs.rds"))

dim(single_mits_neonate)
dim(single_mits_child)
dim(multi_mits_neonate)
dim(multi_mits_child)

single_mits_neonate$ID <- rownames(single_mits_neonate) 
single_mits_child$ID <- rownames(single_mits_child)
multi_mits_neonate$ID <- rownames(multi_mits_neonate)
multi_mits_child$ID <- rownames(multi_mits_child)

head(single_mits_neonate)
head(single_mits_child)
head(multi_mits_neonate)
head(multi_mits_child)

NeonateMITS <- read.csv("Data/mits_neonate_champs.csv")
dim(NeonateMITS)

ChildMITS <- read.csv("Data/mits_child_champs.csv")
dim(ChildMITS)

country_mits_neonate_champs <- merge(single_mits_neonate,NeonateMITS, by=c("ID"))
country_mits_neonate_champs <- country_mits_neonate_champs[,c("ID","Name")]

country_mits_child_champs <- merge(single_mits_child,ChildMITS, by=c("ID"))
country_mits_child_champs <- country_mits_child_champs[,c("ID","Name")]

dim(country_mits_neonate_champs)
dim(country_mits_child_champs)
head(country_mits_neonate_champs)
head(country_mits_child_champs)

write.csv(country_mits_neonate_champs,"Data/country_mits_neonate_champs.csv", row.names = FALSE)
write.csv(country_mits_child_champs,"Data/country_mits_child_champs.csv", row.names = FALSE)


