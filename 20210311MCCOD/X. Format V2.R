# Last edited: 11 Feb 2021
# Last run:    11 Feb 2021

# Objective: get input for VA analysis

rm(list = ls())

# library(reshape2)
library(dplyr)
# library(stringr)

'%!in%' <- function(x,y)!('%in%'(x,y))

########################################################### Read in results and write out for Li
file <- getwd()

COD.neonate <- read.csv(file.path(file,"Results/COD.0to27days.csv"), stringsAsFactors = FALSE)
head(COD.neonate)
dim(COD.neonate)

unique(COD.neonate$place_of_death)
COD.neonate$place_of_death[COD.neonate$place_of_death %in% c("hospital","other_health_facility")] <- "facility"
COD.neonate$place_of_death[COD.neonate$place_of_death %in% c("home","other","on_route_to_hospital_or_facility")] <- "community"
unique(COD.neonate$place_of_death)
# COD.neonate$strat <- paste0(COD.neonate$InsilicoVA," ",COD.neonate$place_of_death)

COD.neonate.cause.InsilicoVA <- COD.neonate %>%
  group_by(place_of_death, InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
COD.neonate.cause.InsilicoVA

names(COD.neonate.cause.InsilicoVA)[names(COD.neonate.cause.InsilicoVA) == 'InsilicoVA'] <- 'cause'
COD.neonate.cause.InsilicoVA$method <- "InsilicoVA"

COD.neonate.cause.EAVA <- COD.neonate %>%
  group_by(place_of_death, EAVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
names(COD.neonate.cause.EAVA)[names(COD.neonate.cause.EAVA) == 'EAVA'] <- 'cause'
COD.neonate.cause.EAVA$method <- "EAVA"

COD.neonate.cause <- rbind(COD.neonate.cause.InsilicoVA,COD.neonate.cause.EAVA)
write.csv(COD.neonate.cause, "Results/COD.0to27days.CSMF.csv", row.names = FALSE)







COD.1to59months <- read.csv(file.path(file,"Results/COD.1to59months.csv"), stringsAsFactors = FALSE)
head(COD.1to59months)
dim(COD.1to59months)

unique(COD.1to59months$place_of_death)
COD.1to59months$place_of_death[COD.1to59months$place_of_death %in% c("hospital","other_health_facility")] <- "facility"
COD.1to59months$place_of_death[COD.1to59months$place_of_death %in% c("home","other","on_route_to_hospital_or_facility")] <- "community"
unique(COD.1to59months$place_of_death)

COD.1to59months.cause.InsilicoVA <- COD.1to59months %>%
  group_by(place_of_death, InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
names(COD.1to59months.cause.InsilicoVA)[names(COD.1to59months.cause.InsilicoVA) == 'InsilicoVA'] <- 'cause'
COD.1to59months.cause.InsilicoVA$method <- "InsilicoVA"

COD.1to59months.cause.EAVA <- COD.1to59months %>%
  group_by(place_of_death, EAVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
names(COD.1to59months.cause.EAVA)[names(COD.1to59months.cause.EAVA) == 'EAVA'] <- 'cause'
COD.1to59months.cause.EAVA$method <- "EAVA"

COD.1to59months.cause <- rbind(COD.1to59months.cause.InsilicoVA,COD.1to59months.cause.EAVA)
write.csv(COD.1to59months.cause, "Results/COD.1to59months.CSMF.csv", row.names = FALSE)


