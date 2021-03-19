# Last edited: 8 Feb 2021
# Last run:    8 Feb 2021

# Objective: get input for VA analysis

rm(list = ls())

library(reshape2)
library(dplyr)

'%!in%' <- function(x,y)!('%in%'(x,y))

########################################################### Read in results and write out for Li
file <- getwd()
# COD.stillbirths <- read.csv(file.path(file,"Results/COD.stillbirths.csv"), stringsAsFactors = FALSE)
# COD.stillbirths.causebyprovince <- COD.stillbirths %>%
#   group_by(province, InsilicoVA_sm_cause) %>%
#   summarise (n = n()) %>%
#   mutate(freq = round(n / sum(n),digits = 3))
# names(COD.stillbirths.causebyprovince)[names(COD.stillbirths.causebyprovince) == 'InsilicoVA_sm_cause'] <- 'InsilicoVA'
# write.csv(COD.stillbirths.causebyprovince, "Results/COD.stillbirths.causeXprovince.csv", row.names = FALSE)

COD.neonate <- read.csv(file.path(file,"Results/COD.0to27days.csv"), stringsAsFactors = FALSE)
head(COD.neonate)
table(COD.neonate$InsilicoVA_sm_cause,COD.neonate$InsilicoVA, exclude=NULL)
COD.neonate.cause <- COD.neonate %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
# COD.neonate.cause <- subset(COD.neonate.cause, province %!in% "")
write.csv(COD.neonate.cause, "Results/COD.0to27days.CSMF.csv", row.names = FALSE)




COD.1to59months <- read.csv(file.path(file,"Results/COD.1to59months.csv"), stringsAsFactors = FALSE)
head(COD.1to59months)
table(COD.1to59months$InsilicoVA_sm_cause,COD.1to59months$InsilicoVA, exclude=NULL)
COD.1to59months.cause <- COD.1to59months %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.1to59months.cause, "Results/COD.1to59months.CSMF.csv", row.names = FALSE)




COD.5to14 <- read.csv(file.path(file,"Results/COD.5to14.csv"), stringsAsFactors = FALSE)
head(COD.5to14)
table(COD.5to14$InsilicoVA_sm_cause,COD.5to14$InsilicoVA, exclude=NULL)
COD.5to14.cause <- COD.5to14 %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.5to14.cause, "Results/COD.5to14.CSMF.csv", row.names = FALSE)




COD.15to49 <- read.csv(file.path(file,"Results/COD.15to49.csv"), stringsAsFactors = FALSE)
head(COD.15to49)
table(COD.15to49$InsilicoVA_sm_cause,COD.15to49$InsilicoVA, exclude=NULL)
COD.15to49.cause <- COD.15to49 %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.15to49.cause, "Results/COD.15to49.CSMF.csv", row.names = FALSE)




COD.50plus <- read.csv(file.path(file,"Results/COD.50plus.csv"), stringsAsFactors = FALSE)
head(COD.50plus)
table(COD.50plus$InsilicoVA_sm_cause,COD.50plus$InsilicoVA, exclude=NULL)
COD.50plus.cause<- COD.50plus %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.50plus.cause, "Results/COD.50plus.CSMF.csv", row.names = FALSE)
