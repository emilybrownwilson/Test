# Last edited: 4 Dec 2020
# Last run:    4 Dec 2020

# Objective: Produce broad cause graphs from individual COD, for each method

rm(list = ls())

library(openVA)

file <- getwd()
load(file.path(file,"/Data/openVA_comsa.Rdata"))

'%!in%' <- function(x,y)!('%in%'(x,y))

### keep only objects we're using
# rm(list = ls()[!ls() %in% c("InterVA5.ch5_14", "InterVA5.ad15_49","InterVA5.ad50",
#                             "codeVAInsilico.ch5_14", "codeVAInsilico.ad15_49", "codeVAInsilico.ad50")])

file <- getwd()
file
########################################################### Cut the data differently

data5to14 <- read.csv(file.path(file,"Results/COD.5to14.csv"), stringsAsFactors = FALSE)
data15to49 <- read.csv(file.path(file,"Results/COD.15to49.csv"), stringsAsFactors = FALSE)

head(data5to14)
dim(data5to14)
head(data15to49)
dim(data15to49)

data <- rbind(data5to14,data15to49)
dim(data)

data5to11 <- subset(data, age_in_days < 12*365.25)
dim(data5to11)

data12to19 <- subset(data, age_in_days >= 12*365.25 & age_in_days < 20*365.25)
dim(data12to19)

############ check ages are in range
summary(data5to11)  # age range approx: 1826-4378
summary(data12to19) # age range approx: 4383-7296
############

write.csv(data5to11, file.path(file,"/Results/COD.5to11.csv"), row.names = FALSE)
write.csv(data12to19, file.path(file,"/Results/COD.12to19.csv"), row.names = FALSE)
