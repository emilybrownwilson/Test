# Last edited: 29 Jan 2021
# Last run:    29 Jan 2021

# Objective: RE:Agbessi's request

rm(list = ls())

library(reshape2)
library(dplyr)
library(stringr)

'%!in%' <- function(x,y)!('%in%'(x,y))

########################################################### Reformat single cause matrices to get VA analysis data
file <- getwd()
file
#### COMSA
COD.5to14 <- read.csv("~/Desktop/COMSA Analysis/VA analysis/Results/COD.5to14.csv")
head(COD.5to14)
other <- subset(COD.5to14, InsilicoVA %in% c("Other"))
summary(other$age_in_days)
min <- 1826/365.25
max <- 5390/365.25

twelvetofourteen <- subset(COD.5to14, age_in_days>=12*365 & age_in_days<15*365)

twelvetofourteenCOD <- twelvetofourteen %>%
  group_by(InsilicoVA) %>%
  summarise (n = n()) %>%
  mutate(freq = round(n / sum(n),digits = 3))
write.csv(twelvetofourteenCOD, "COD.12to14.csv", row.names = FALSE)

sm.vs.lg.cause <- table(twelvetofourteen$InsilicoVA_sm_cause, twelvetofourteen$InsilicoVA)
write.csv(sm.vs.lg.cause, "COD.12to14.table.csv")

under12 <- subset(COD.5to14, age_in_days<12*365)
sm.vs.lg.cause <- table(under12$InsilicoVA_sm_cause, under12$InsilicoVA)
write.csv(sm.vs.lg.cause, "COD.5to11.table.csv")

