# Last edited: 11 Feb 2021
# Last run:    11 Feb 2021

# Objective: get input for VA analysis

rm(list = ls())

library(reshape2)
library(dplyr)
library(stringr)

'%!in%' <- function(x,y)!('%in%'(x,y))

########################################################### Read in results and write out for Li
file <- getwd()

COD.neonate <- read.csv(file.path(file,"Results/COD.0to27days.csv"), stringsAsFactors = FALSE)
head(COD.neonate)
table(COD.neonate$InsilicoVA_sm_cause,COD.neonate$InsilicoVA, exclude=NULL)

COD.neonate.2019 <- COD.neonate %>%filter(str_detect(date_of_death, "2019"))
COD.neonate.2020 <- COD.neonate %>%filter(str_detect(date_of_death, "2020"))
dim(COD.neonate.2019)
dim(COD.neonate.2020)
dim(COD.neonates)

COD.neonate.cause <- COD.neonate %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
# COD.neonate.cause <- subset(COD.neonate.cause, province %!in% "")
write.csv(COD.neonate.cause, "Results/COD.0to27days.CSMF.csv", row.names = FALSE)

COD.neonate.cause.2019 <- COD.neonate.2019 %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.neonate.cause.2019, "Results/COD.neonate.2019.CSMF.csv", row.names = FALSE)
write.csv(COD.neonate.2019, "Results/COD.neonate.2019.csv", row.names = FALSE)

COD.neonate.cause.2020 <- COD.neonate.2020 %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.neonate.cause.2020, "Results/COD.neonate.2020.CSMF.csv", row.names = FALSE)
write.csv(COD.neonate.2020, "Results/COD.neonate.2020.csv", row.names = FALSE)





COD.1to59months <- read.csv(file.path(file,"Results/COD.1to59months.csv"), stringsAsFactors = FALSE)
head(COD.1to59months)
table(COD.1to59months$InsilicoVA_sm_cause,COD.1to59months$InsilicoVA, exclude=NULL)

COD.1to59months.2019 <- COD.1to59months %>%filter(str_detect(date_of_death, "2019"))
COD.1to59months.2020 <- COD.1to59months %>%filter(str_detect(date_of_death, "2020"))
dim(COD.1to59months.2019)
dim(COD.1to59months.2020)
dim(COD.1to59months)

COD.1to59months.cause.2019 <- COD.1to59months.2019 %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.1to59months.cause.2019, "Results/COD.1to59months.2019.CSMF.csv", row.names = FALSE)
write.csv(COD.1to59months.2019, "Results/COD.1to59months.2019.csv", row.names = FALSE)

COD.1to59months.cause.2020 <- COD.1to59months.2020 %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.1to59months.cause.2020, "Results/COD.1to59months.2020.CSMF.csv", row.names = FALSE)
write.csv(COD.1to59months.2020, "Results/COD.1to59months.2020.csv", row.names = FALSE)




COD.5to14 <- read.csv(file.path(file,"Results/COD.5to14.csv"), stringsAsFactors = FALSE)
head(COD.5to14)
table(COD.5to14$InsilicoVA_sm_cause,COD.5to14$InsilicoVA, exclude=NULL)

COD.5to14.2019 <- COD.5to14 %>%filter(str_detect(date_of_death, "2019"))
COD.5to14.2020 <- COD.5to14 %>%filter(str_detect(date_of_death, "2020"))
dim(COD.5to14.2019)
dim(COD.5to14.2020)
dim(COD.5to14)

COD.5to14.cause.2019 <- COD.5to14.2019 %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.5to14.cause.2019, "Results/COD.5to14.2019.CSMF.csv", row.names = FALSE)
write.csv(COD.5to14.2019, "Results/COD.5to14.2019.csv", row.names = FALSE)

COD.5to14.cause.2020 <- COD.5to14.2020 %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.5to14.cause.2020, "Results/COD.5to14.2020.CSMF.csv", row.names = FALSE)
write.csv(COD.5to14.2020, "Results/COD.5to14.2020.csv", row.names = FALSE)





COD.15to49 <- read.csv(file.path(file,"Results/COD.15to49.csv"), stringsAsFactors = FALSE)
head(COD.15to49)
table(COD.15to49$InsilicoVA_sm_cause,COD.15to49$InsilicoVA, exclude=NULL)

COD.15to49.2019 <- COD.15to49 %>%filter(str_detect(date_of_death, "2019"))
COD.15to49.2020 <- COD.15to49 %>%filter(str_detect(date_of_death, "2020"))
dim(COD.15to49.2019)
dim(COD.15to49.2020)
dim(COD.15to49)

COD.15to49.cause.2019 <- COD.15to49.2019 %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.15to49.cause.2019, "Results/COD.15to49.2019.CSMF.csv", row.names = FALSE)
write.csv(COD.15to49.2019, "Results/COD.15to49.2019.csv", row.names = FALSE)

COD.15to49.cause.2020 <- COD.15to49.2020 %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.15to49.cause.2020, "Results/COD.15to49.2020.CSMF.csv", row.names = FALSE)
write.csv(COD.15to49.2020, "Results/COD.15to49.2020.csv", row.names = FALSE)




COD.50plus <- read.csv(file.path(file,"Results/COD.50plus.csv"), stringsAsFactors = FALSE)
head(COD.50plus)
table(COD.50plus$InsilicoVA_sm_cause,COD.50plus$InsilicoVA, exclude=NULL)

COD.50plus.2019 <- COD.50plus %>%filter(str_detect(date_of_death, "2019"))
COD.50plus.2020 <- COD.50plus %>%filter(str_detect(date_of_death, "2020"))
dim(COD.50plus.2019)
dim(COD.50plus.2020)
dim(COD.50plus)

COD.50plus.cause.2019 <- COD.50plus.2019 %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.50plus.cause.2019, "Results/COD.50plus.2019.CSMF.csv", row.names = FALSE)
write.csv(COD.50plus.2019, "Results/COD.50plus.2019.csv", row.names = FALSE)

COD.50plus.cause.2020 <- COD.50plus.2020 %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.50plus.cause.2020, "Results/COD.50plus.2020.CSMF.csv", row.names = FALSE)
write.csv(COD.50plus.2020, "Results/COD.50plus.2020.csv", row.names = FALSE)

