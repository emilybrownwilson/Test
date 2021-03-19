# Last edited: 3 Mar 2021
# Last run:    3 Mar 2021

# Objective: get input for COMSA website

rm(list = ls())

library(reshape2)
library(dplyr)

'%!in%' <- function(x,y)!('%in%'(x,y))

########################################################### Get tables for website
file <- getwd()
file
neonate <- read.csv("Results/COD.0to27days.csv")
child <- read.csv("Results/COD.1to59months.csv")
COD.5to14 <- read.csv("Results/COD.5to14.csv")
COD.15to49 <- read.csv("Results/COD.15to49.csv")
COD.50plus <- read.csv("Results/COD.50plus.csv")

########## neonate
raw.data <- neonate %>% group_by(InsilicoVA) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

names(raw.data)[names(raw.data) == 'InsilicoVA'] <- 'cause'
names(raw.data)[names(raw.data) == 'freq'] <- 'value'

raw.data$method <- "InsilicoVA"
raw.data$variable <- "raw"
raw.data <- raw.data %>% select(cause, variable, value, n, method)
raw.data 
write.csv(raw.data, file.path(file,"/Results/forSafia/CSMF.neonate.website.csv"), row.names = FALSE)

########## male neonate
raw.data <- subset(neonate, sex == "male")
raw.data <- raw.data %>% group_by(InsilicoVA) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

names(raw.data)[names(raw.data) == 'InsilicoVA'] <- 'cause'
names(raw.data)[names(raw.data) == 'freq'] <- 'value'

raw.data$method <- "InsilicoVA"
raw.data$variable <- "raw"
raw.data <- raw.data %>% select(cause, variable, value, n, method)
raw.data$sex <- "male"
raw.data 
write.csv(raw.data, file.path(file,"/Results/forSafia/CSMF.neonate.male.website.csv"), row.names = FALSE)

########## female neonate
raw.data <- subset(neonate, sex == "female")
raw.data <- raw.data %>% group_by(InsilicoVA) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

names(raw.data)[names(raw.data) == 'InsilicoVA'] <- 'cause'
names(raw.data)[names(raw.data) == 'freq'] <- 'value'

raw.data$method <- "InsilicoVA"
raw.data$variable <- "raw"
raw.data <- raw.data %>% select(cause, variable, value, n, method)
raw.data$sex <- "female"
raw.data 
write.csv(raw.data, file.path(file,"/Results/forSafia/CSMF.neonate.female.website.csv"), row.names = FALSE)








########## child
raw.data <- child %>% group_by(InsilicoVA) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

names(raw.data)[names(raw.data) == 'InsilicoVA'] <- 'cause'
names(raw.data)[names(raw.data) == 'freq'] <- 'value'

raw.data$method <- "InsilicoVA"
raw.data$variable <- "raw"
raw.data <- raw.data %>% select(cause, variable, value, n, method)
raw.data 
write.csv(raw.data, file.path(file,"/Results/forSafia/CSMF.child.website.csv"), row.names = FALSE)

########## male child
raw.data <- subset(child, sex == "male")
raw.data <- raw.data %>% group_by(InsilicoVA) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

names(raw.data)[names(raw.data) == 'InsilicoVA'] <- 'cause'
names(raw.data)[names(raw.data) == 'freq'] <- 'value'

raw.data$method <- "InsilicoVA"
raw.data$variable <- "raw"
raw.data <- raw.data %>% select(cause, variable, value, n, method)
raw.data$sex <- "male"
raw.data 
write.csv(raw.data, file.path(file,"/Results/forSafia/CSMF.child.male.website.csv"), row.names = FALSE)

########## female child
raw.data <- subset(child, sex == "female")
raw.data <- raw.data %>% group_by(InsilicoVA) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

names(raw.data)[names(raw.data) == 'InsilicoVA'] <- 'cause'
names(raw.data)[names(raw.data) == 'freq'] <- 'value'

raw.data$method <- "InsilicoVA"
raw.data$variable <- "raw"
raw.data <- raw.data %>% select(cause, variable, value, n, method)
raw.data$sex <- "female"
raw.data 
write.csv(raw.data, file.path(file,"/Results/forSafia/CSMF.child.female.website.csv"), row.names = FALSE)











########## COD.5to14 
raw.data <- COD.5to14  %>% group_by(InsilicoVA) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

names(raw.data)[names(raw.data) == 'InsilicoVA'] <- 'cause'
names(raw.data)[names(raw.data) == 'freq'] <- 'value'

raw.data$method <- "InsilicoVA"
raw.data$variable <- "raw"
raw.data <- raw.data %>% select(cause, variable, value, n, method)
raw.data 
write.csv(raw.data, file.path(file,"/Results/forSafia/CSMF.COD.5to14.website.csv"), row.names = FALSE)

########## male COD.5to14 
raw.data <- subset(COD.5to14 , sex == "male")
raw.data <- raw.data %>% group_by(InsilicoVA) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

names(raw.data)[names(raw.data) == 'InsilicoVA'] <- 'cause'
names(raw.data)[names(raw.data) == 'freq'] <- 'value'

raw.data$method <- "InsilicoVA"
raw.data$variable <- "raw"
raw.data <- raw.data %>% select(cause, variable, value, n, method)
raw.data$sex <- "male"
raw.data 
write.csv(raw.data, file.path(file,"/Results/forSafia/CSMF.COD.5to14.male.website.csv"), row.names = FALSE)

########## female COD.5to14 
raw.data <- subset(COD.5to14 , sex == "female")
raw.data <- raw.data %>% group_by(InsilicoVA) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

names(raw.data)[names(raw.data) == 'InsilicoVA'] <- 'cause'
names(raw.data)[names(raw.data) == 'freq'] <- 'value'

raw.data$method <- "InsilicoVA"
raw.data$variable <- "raw"
raw.data <- raw.data %>% select(cause, variable, value, n, method)
raw.data$sex <- "female"
raw.data 
write.csv(raw.data, file.path(file,"/Results/forSafia/CSMF.COD.5to14.female.website.csv"), row.names = FALSE)










########## COD.15to49 
raw.data <- COD.15to49  %>% group_by(InsilicoVA) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

names(raw.data)[names(raw.data) == 'InsilicoVA'] <- 'cause'
names(raw.data)[names(raw.data) == 'freq'] <- 'value'

raw.data$method <- "InsilicoVA"
raw.data$variable <- "raw"
raw.data <- raw.data %>% select(cause, variable, value, n, method)
raw.data 
write.csv(raw.data, file.path(file,"/Results/forSafia/CSMF.COD.15to49.website.csv"), row.names = FALSE)

########## male COD.15to49 
raw.data <- subset(COD.15to49 , sex == "male")
raw.data <- raw.data %>% group_by(InsilicoVA) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

names(raw.data)[names(raw.data) == 'InsilicoVA'] <- 'cause'
names(raw.data)[names(raw.data) == 'freq'] <- 'value'

raw.data$method <- "InsilicoVA"
raw.data$variable <- "raw"
raw.data <- raw.data %>% select(cause, variable, value, n, method)
raw.data$sex <- "male"
raw.data 
write.csv(raw.data, file.path(file,"/Results/forSafia/CSMF.COD.15to49.male.website.csv"), row.names = FALSE)

########## female COD.15to49 
raw.data <- subset(COD.15to49 , sex == "female")
raw.data <- raw.data %>% group_by(InsilicoVA) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

names(raw.data)[names(raw.data) == 'InsilicoVA'] <- 'cause'
names(raw.data)[names(raw.data) == 'freq'] <- 'value'

raw.data$method <- "InsilicoVA"
raw.data$variable <- "raw"
raw.data <- raw.data %>% select(cause, variable, value, n, method)
raw.data$sex <- "female"
raw.data 
write.csv(raw.data, file.path(file,"/Results/forSafia/CSMF.COD.15to49.female.website.csv"), row.names = FALSE)









########## COD.50plus 
raw.data <- COD.50plus  %>% group_by(InsilicoVA) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

names(raw.data)[names(raw.data) == 'InsilicoVA'] <- 'cause'
names(raw.data)[names(raw.data) == 'freq'] <- 'value'

raw.data$method <- "InsilicoVA"
raw.data$variable <- "raw"
raw.data <- raw.data %>% select(cause, variable, value, n, method)
raw.data 
write.csv(raw.data, file.path(file,"/Results/forSafia/CSMF.COD.50plus.website.csv"), row.names = FALSE)

########## male COD.50plus 
raw.data <- subset(COD.50plus , sex == "male")
raw.data <- raw.data %>% group_by(InsilicoVA) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

names(raw.data)[names(raw.data) == 'InsilicoVA'] <- 'cause'
names(raw.data)[names(raw.data) == 'freq'] <- 'value'

raw.data$method <- "InsilicoVA"
raw.data$variable <- "raw"
raw.data <- raw.data %>% select(cause, variable, value, n, method)
raw.data$sex <- "male"
raw.data 
write.csv(raw.data, file.path(file,"/Results/forSafia/CSMF.COD.50plus.male.website.csv"), row.names = FALSE)

########## female COD.50plus 
raw.data <- subset(COD.50plus , sex == "female")
raw.data <- raw.data %>% group_by(InsilicoVA) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

names(raw.data)[names(raw.data) == 'InsilicoVA'] <- 'cause'
names(raw.data)[names(raw.data) == 'freq'] <- 'value'

raw.data$method <- "InsilicoVA"
raw.data$variable <- "raw"
raw.data <- raw.data %>% select(cause, variable, value, n, method)
raw.data$sex <- "female"
raw.data 
write.csv(raw.data, file.path(file,"/Results/forSafia/CSMF.COD.50plus.female.website.csv"), row.names = FALSE)

