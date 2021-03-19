# Last edited: 26 Jan 2021
# Last run:    26 Jan 2021

# Objective: get input for VA analysis

rm(list = ls())

library(reshape2)
library(dplyr)

'%!in%' <- function(x,y)!('%in%'(x,y))

########################################################### Read in results and write out for Li
file <- getwd()
COD.stillbirths <- read.csv(file.path(file,"Results/COD.stillbirths.csv"), stringsAsFactors = FALSE)
COD.stillbirths.causebyprovince <- COD.stillbirths %>%
  group_by(province, InsilicoVA_sm_cause) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
names(COD.stillbirths.causebyprovince)[names(COD.stillbirths.causebyprovince) == 'InsilicoVA_sm_cause'] <- 'InsilicoVA'
write.csv(COD.stillbirths.causebyprovince, "Results/COD.stillbirths.causeXprovince.csv", row.names = FALSE)

COD.neonate <- read.csv(file.path(file,"Results/COD.0to27days.csv"), stringsAsFactors = FALSE)
COD.neonate.causebyprovince <- COD.neonate %>%
  group_by(province, InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
COD.neonate.causebyprovince <- subset(COD.neonate.causebyprovince, province %!in% "")
write.csv(COD.neonate.causebyprovince, "Results/COD.0to27days.causeXprovince.csv", row.names = FALSE)



COD.1to59months <- read.csv(file.path(file,"Results/COD.1to59months.csv"), stringsAsFactors = FALSE)
COD.1to59months.causebyprovince <- COD.1to59months %>%
  group_by(province, InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.1to59months.causebyprovince, "Results/COD.1to59months.causeXprovince.csv", row.names = FALSE)


COD.1to11months <- read.csv(file.path(file,"Results/COD.1to11months.csv"), stringsAsFactors = FALSE)
COD.1to11months.causebyprovince <- COD.1to11months %>%
  group_by(province, InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.1to11months.causebyprovince, "Results/COD.1to11months.causeXprovince.csv", row.names = FALSE)

COD.12to59months <- read.csv(file.path(file,"Results/COD.12to59months.csv"), stringsAsFactors = FALSE)
COD.12to59months.causebyprovince <- COD.12to59months %>%
  group_by(province, InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.ch12to59months.causebyprovince, "Results/COD.12to59months.causeXprovince.csv", row.names = FALSE)




COD.5to19years <- read.csv(file.path(file,"Results/COD.5to19years.csv"), stringsAsFactors = FALSE)
COD.5to19years.causebyprovince <- COD.5to19years %>%
  group_by(province, InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(COD.5to19years.causebyprovince, "Results/COD.5to19years.causebyprovince.csv", row.names = FALSE)


