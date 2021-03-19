library(openVA)
library(reshape2)
library(dplyr)

file <- getwd()
file

# 1. Single cause VA COMSA matrix : This is a matrix where each row represents a subject in the COMSA set, and each column represents a cause. 
#     Then for entry i,j (i-th individual, jth cause), there is a 1 is that is the single VA COD for that individual, and 0 if not (using the condensed final cause list). 
#     For EAVA, if individual i has an undecided COD, then put 1/C in each entry for that individual, where C is the number of causes
single_interva_neonate_comsa <- readRDS("single_interva_neonate_comsa.rds")
single_insilicova_neonate_comsa <- readRDS("single_insilicova_neonate_comsa.rds")
single_eava_neonate_comsa <- readRDS("single_eava_neonate_comsa.rds")

head(single_interva_neonate_comsa)
head(single_insilicova_neonate_comsa)
head(single_eava_neonate_comsa)

identical(rownames(single_interva_neonate_comsa), rownames(single_insilicova_neonate_comsa))
identical(rownames(single_interva_neonate_comsa), rownames(single_eava_neonate_comsa))

identical(colnames(single_interva_neonate_comsa), colnames(single_insilicova_neonate_comsa))
identical(colnames(single_interva_neonate_comsa), colnames(single_eava_neonate_comsa))

dim(single_interva_neonate_comsa)
dim(single_insilicova_neonate_comsa)
dim(single_eava_neonate_comsa)

unique(rowSums(single_interva_neonate_comsa))
# which(rowSums(single_interva_neonate_comsa)==2)
# x <- subset(single_interva_neonate_comsa, rownames(single_interva_neonate_comsa) %in% c(1437))
unique(rowSums(single_insilicova_neonate_comsa))
unique(rowSums(single_eava_neonate_comsa))



single_interva_child_comsa <- readRDS("single_interva_child_comsa.rds")
single_insilicova_child_comsa <- readRDS("single_insilicova_child_comsa.rds")
single_eava_child_comsa <- readRDS("single_eava_child_comsa.rds")

head(single_interva_child_comsa)
head(single_insilicova_child_comsa)
head(single_eava_child_comsa)

identical(rownames(single_interva_child_comsa), rownames(single_insilicova_child_comsa))
identical(rownames(single_interva_child_comsa), rownames(single_eava_child_comsa))

identical(colnames(single_interva_child_comsa), colnames(single_insilicova_child_comsa))
identical(colnames(single_interva_child_comsa), colnames(single_eava_child_comsa))

dim(single_interva_child_comsa)
dim(single_insilicova_child_comsa)
dim(single_eava_child_comsa)

unique(rowSums(single_interva_child_comsa))
which(rowSums(single_interva_child_comsa)==2)
# x <- subset(single_interva_child_comsa, rownames(single_interva_child_comsa) %in% c(4347))
unique(rowSums(single_insilicova_child_comsa))
unique(rowSums(single_eava_child_comsa))
# which(is.na(rowSums(single_interva_child_comsa)))





# # 2. Multi-cause VA COMSA matrix: For the same individuals above, entry i,j is now the VA algorithm probability for cause j for individual i. 
# #    For EAVA if there are two different causes, put .75 for the top cause and .25 for the second cause. Again put 1/C for each entry if the cause is undecided 
multi_interva_neonate_comsa <- readRDS("multi_interva_neonate_comsa.rds")
multi_insilicova_neonate_comsa <- readRDS("multi_insilicova_neonate_comsa.rds")
multi_eava_neonate_comsa <- readRDS("multi_eava_neonate_comsa.rds")

head(multi_interva_neonate_comsa)
head(multi_insilicova_neonate_comsa)
head(multi_eava_neonate_comsa)

identical(rownames(multi_interva_neonate_comsa), rownames(multi_insilicova_neonate_comsa))
identical(rownames(multi_interva_neonate_comsa), rownames(multi_eava_neonate_comsa))

identical(colnames(multi_interva_neonate_comsa), colnames(multi_insilicova_neonate_comsa))
identical(colnames(multi_interva_neonate_comsa), colnames(multi_eava_neonate_comsa))

dim(multi_interva_neonate_comsa)
dim(multi_insilicova_neonate_comsa)
dim(multi_eava_neonate_comsa)

unique(rowSums(multi_interva_neonate_comsa))
# write.csv(multi_interva_neonate_comsa, "multi_interva_neonate_comsa.csv")
unique(rowSums(multi_insilicova_neonate_comsa))
unique(rowSums(multi_eava_neonate_comsa))





multi_interva_child_comsa <- readRDS("multi_interva_child_comsa.rds")
multi_insilicova_child_comsa <- readRDS("multi_insilicova_child_comsa.rds")
multi_eava_child_comsa <- readRDS("multi_eava_child_comsa.rds")

head(multi_interva_child_comsa)
head(multi_insilicova_child_comsa)
head(multi_eava_child_comsa)

identical(rownames(multi_interva_child_comsa), rownames(multi_insilicova_child_comsa))
identical(rownames(multi_interva_child_comsa), rownames(multi_eava_child_comsa))

identical(colnames(multi_interva_child_comsa), colnames(multi_insilicova_child_comsa))
identical(colnames(multi_interva_child_comsa), colnames(multi_eava_child_comsa))

dim(multi_interva_child_comsa)
dim(multi_insilicova_child_comsa)
dim(multi_eava_child_comsa)

unique(rowSums(multi_interva_child_comsa))
unique(rowSums(multi_insilicova_neonate_comsa))
unique(rowSums(multi_eava_neonate_comsa))











# 3. Single-cause MITS CHAMPS matrix. Again, for entry i,j put a 1 if cause j is the underlying MITS cause, and 0 otherwise
single_mits_neonate_champs <- readRDS("single_mits_neonate_champs.rds")
head(single_mits_neonate_champs)

single_mits_child_champs <- readRDS("single_mits_child_champs.rds")
head(single_mits_child_champs)



# 4. Multi-cause MITS CHAMPS matrix. For entry i,j, put a 1 if cause j is the underlying MITS cause and there are no immediate causes 
#    (or the immediate cause is the same as the underlying). Otherwise, put a .5 if cause j is either an underlying or immediate cause
multi_mits_neonate_champs <- readRDS("multi_mits_neonate_champs.rds")
head(multi_mits_neonate_champs)

multi_mits_child_champs <- readRDS("multi_mits_child_champs.rds")
head(multi_mits_child_champs)

identical(rownames(single_mits_neonate_champs), rownames(multi_mits_neonate_champs))
identical(rownames(single_mits_child_champs), rownames(multi_mits_child_champs))

identical(colnames(single_mits_neonate_champs), colnames(multi_mits_neonate_champs))
identical(colnames(single_mits_child_champs), colnames(multi_mits_child_champs))

dim(single_mits_neonate_champs)
dim(multi_mits_neonate_champs)

dim(single_mits_child_champs)
dim(multi_mits_child_champs)

unique(rowSums(single_mits_neonate_champs))
# which(rowSums(single_mits_neonate_champs)==0)
unique(rowSums(multi_mits_neonate_champs))

unique(rowSums(single_mits_child_champs))
unique(rowSums(multi_mits_child_champs))
# which(rowSums(multi_mits_child_champs)==.5)





# 5. Single cause and multi-cause VA CHAMPS matrices, defined the same way as 1 and 2, except now for the CHAMPS data. 
#    Be careful with the getIndivProb function, because we’ve seen that it can screw up the ordering of the subjects, 
#    so make sure that the rows in these matrices align with the MITS CHAMPS matrices (i.e. the ID should be the same for each row of the CHAMPS matrices
### SINGLE
single_interva_neonate_champs <- readRDS("single_interva_neonate_champs.rds")
single_insilicova_neonate_champs <- readRDS("single_insilicova_neonate_champs.rds")
single_eava_neonate_champs <- readRDS("single_eava_neonate_champs.rds")

head(single_interva_neonate_champs)
head(single_insilicova_neonate_champs)
head(single_eava_neonate_champs)

identical(rownames(single_interva_neonate_champs), rownames(single_insilicova_neonate_champs))
identical(rownames(single_interva_neonate_champs), rownames(single_eava_neonate_champs))

identical(colnames(single_interva_neonate_champs), colnames(single_insilicova_neonate_champs))
identical(colnames(single_interva_neonate_champs), colnames(single_eava_neonate_champs))

dim(single_interva_neonate_champs)
dim(single_insilicova_neonate_champs)
dim(single_eava_neonate_champs)

unique(rowSums(single_interva_neonate_champs))
# which(rowSums(single_interva_neonate_champs)==2)
unique(rowSums(single_insilicova_neonate_champs))
unique(rowSums(single_eava_neonate_champs))
# which(rowSums(single_eava_neonate_champs)!=1)





single_interva_child_champs <- readRDS("single_interva_child_champs.rds")
single_insilicova_child_champs <- readRDS("single_insilicova_child_champs.rds")
single_eava_child_champs <- readRDS("single_eava_child_champs.rds")

head(single_interva_child_champs)
head(single_insilicova_child_champs)
head(single_eava_child_champs)

identical(rownames(single_interva_child_champs), rownames(single_insilicova_child_champs))
identical(rownames(single_interva_child_champs), rownames(single_eava_child_champs))

identical(colnames(single_interva_child_champs), colnames(single_insilicova_child_champs))
identical(colnames(single_interva_child_champs), colnames(single_eava_child_champs))

dim(single_interva_child_champs)
dim(single_insilicova_child_champs)
dim(single_eava_child_champs)

unique(rowSums(single_interva_child_champs))
unique(rowSums(single_insilicova_child_champs))
unique(rowSums(single_eava_child_champs))



### MULTI
multi_interva_neonate_champs <- readRDS("multi_interva_neonate_champs.rds")
multi_insilicova_neonate_champs <- readRDS("multi_insilicova_neonate_champs.rds")
multi_eava_neonate_champs <- readRDS("multi_eava_neonate_champs.rds")

head(multi_interva_neonate_champs)
head(multi_insilicova_neonate_champs)
head(multi_eava_neonate_champs)

multi_insilicova_neonate_champs <- multi_insilicova_neonate_champs[match(rownames(multi_interva_neonate_champs), rownames(multi_insilicova_neonate_champs)), ]
multi_eava_neonate_champs <- multi_eava_neonate_champs[match(rownames(multi_interva_neonate_champs), rownames(multi_eava_neonate_champs)), ]

identical(rownames(multi_interva_neonate_champs), rownames(multi_insilicova_neonate_champs))
identical(rownames(multi_interva_neonate_champs), rownames(multi_eava_neonate_champs))

identical(colnames(multi_interva_neonate_champs), colnames(multi_insilicova_neonate_champs))
identical(colnames(multi_interva_neonate_champs), colnames(multi_eava_neonate_champs))

dim(multi_interva_neonate_champs)
dim(multi_insilicova_neonate_champs)
dim(multi_eava_neonate_champs)

unique(rowSums(multi_interva_neonate_champs))
unique(rowSums(multi_insilicova_neonate_champs))
unique(rowSums(multi_eava_neonate_champs))




multi_interva_child_champs <- readRDS("multi_interva_child_champs.rds")
multi_insilicova_child_champs <- readRDS("multi_insilicova_child_champs.rds")
multi_eava_child_champs <- readRDS("multi_eava_child_champs.rds")

head(multi_interva_child_champs)
head(multi_insilicova_child_champs)
head(multi_eava_child_champs)

multi_insilicova_child_champs <- multi_insilicova_child_champs[match(rownames(multi_interva_child_champs), rownames(multi_insilicova_child_champs)), ]
multi_eava_child_champs <- multi_eava_child_champs[match(rownames(multi_interva_child_champs), rownames(multi_eava_child_champs)), ]

identical(rownames(multi_interva_child_champs), rownames(multi_insilicova_child_champs))
identical(rownames(multi_interva_child_champs), rownames(multi_eava_child_champs))

identical(colnames(multi_interva_child_champs), colnames(multi_insilicova_child_champs))
identical(colnames(multi_interva_child_champs), colnames(multi_eava_child_champs))

dim(multi_interva_child_champs)
dim(multi_insilicova_child_champs)
dim(multi_eava_child_champs)

unique(rowSums(multi_interva_child_champs))
unique(rowSums(multi_insilicova_child_champs))
unique(rowSums(multi_eava_child_champs))



identical(rownames(single_interva_child_champs), rownames(multi_interva_child_champs))
identical(rownames(single_interva_child_champs), rownames(multi_interva_child_champs))

identical(colnames(single_interva_child_champs), colnames(multi_interva_child_champs))
identical(colnames(single_interva_child_champs), colnames(multi_interva_child_champs))






