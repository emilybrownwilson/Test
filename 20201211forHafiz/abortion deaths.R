getwd()

library(haven)

# combine causes with raw data
raw <- read.csv("/Users/EWilson/Desktop/COMSA Analysis/VA analysis/Data/all_WHO.csv")
results <- read.csv("/Users/EWilson/Desktop/COMSA Analysis/VA analysis/Results/COD.15to49.csv")
dim(raw)
dim(results)
names(raw)[names(raw) == 'comsa_id'] <- 'ID'

merged <- merge(results,raw,by=c("ID"))
dim(merged)

abortion_deaths <- subset(merged, InsilicoVA_sm_cause == "Abortion-related death")

table(abortion_deaths$InsilicoVA_sm_cause, abortion_deaths$id10022)
table(abortion_deaths$InsilicoVA_sm_cause, abortion_deaths$id10194)
table(abortion_deaths$InsilicoVA_sm_cause, abortion_deaths$id10195)
table(abortion_deaths$InsilicoVA_sm_cause, abortion_deaths$id10333)
table(abortion_deaths$InsilicoVA_sm_cause, abortion_deaths$id10335)

table(abortion_deaths$InsilicoVA_sm_cause, abortion_deaths$id10019)
table(abortion_deaths$InsilicoVA_sm_cause, abortion_deaths$id10334)
table(abortion_deaths$InsilicoVA_sm_cause, abortion_deaths$id10336)


## LOOK AT FILE THAT GOES INTO openVA algorithms
## A+
# table(IV5data.ad15_49$i022c)
# table(IV5data.ad15_49$i022l)
# table(IV5data.ad15_49$i194o)
# table(IV5data.ad15_49$i195o)
# table(IV5data.ad15_49$i333o)
# table(IV5data.ad15_49$i135o)
# ## I
# table(IV5data.ad15_49$i019b)
# table(IV5data.ad15_49$i334o)
# table(IV5data.ad15_49$i336o)

## Merge with multi-cause
### keep only objects we're using
library(openVA)
load("/Users/EWilson/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/Data/openVA_comsa.Rdata")
rm(list = ls()[!ls() %in% c("codeVAInsilico.ad15_49")])

file <- getwd()

test <- getIndivProb(codeVAInsilico.ad15_49)
class(test)
# combine causes with raw data
raw <- read.csv("/Users/EWilson/Desktop/COMSA Analysis/VA analysis/Data/all_WHO.csv")
results <- read.csv("/Users/EWilson/Desktop/COMSA Analysis/VA analysis/Results/COD.15to49.csv")
dim(raw)
dim(results)
names(raw)[names(raw) == 'comsa_id'] <- 'ID'

merged <- merge(results,raw,by=c("ID"))
dim(merged)

abortion_deaths <- subset(merged, InsilicoVA_sm_cause == "Abortion-related death")
dim(abortion_deaths)
table(abortion_deaths$id10306)
# abortion_deaths <- abortion_deaths[,c("ID","id10019","id10022","id10194","id10195","id10333","id10334","id10335","id10336")]
# RE: Henry's 20210301 email
abortion_deaths <- abortion_deaths[,c("ID","id10019","id10022","id10194","id10195","id10302","id10305","id10312","id10333","id10334","id10335","id10336")]

testdf <- data.frame(ID = row.names(test), test)
abortion_cases <- as.data.frame(abortion_deaths$ID)
names(abortion_cases)[names(abortion_cases) == 'abortion_deaths$ID'] <- 'ID'
testdf <- merge(abortion_cases,testdf,by=c("ID"))
test2 <- merge(abortion_deaths,testdf,by=c("ID"))

head(test2)
abortion_deaths_output <- test2[,c("ID","Abortion.related.death","id10019","id10022","id10194","id10195","id10302","id10305","id10312","id10333","id10334","id10335","id10336")]
View(abortion_deaths_output)

write.csv(abortion_deaths_output, "/Users/EWilson/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20201211forHafiz/abortion_deaths_symptoms.csv", row.names = FALSE)
# write.csv(testdf,"/Users/EWilson/Desktop/Admin/FOR/Hafiz/20201211forHafiz/abortion_deaths_multicause.csv", row.names=FALSE)
