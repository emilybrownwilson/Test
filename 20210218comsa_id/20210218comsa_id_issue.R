# Last edited: 18 Feb 2021
# Last run:    18 Feb 2021
# Ojbective:look into comsa_id issue to report to Agbessi, Malick, Akum

rm(list=ls())

################################### READ IN DATA - COD and raw VASA
start <- Sys.time()
file <- getwd()
file

################################### Checking back to see if the ID issue is resolved. No, 384 of the 518 neonates from August merge
# subset neontaes, by running eava - merge with last VA run (Aug 2020)
Aug.COD.0to27days <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210218comsa_id/Data/Aug.COD.0to27days.csv")
Mar4.eava_neonate_comsa <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210218comsa_id/Data/Mar4.eava_neonate_comsa.csv")
head(Aug.COD.0to27days)
head(Mar4.eava_neonate_comsa)

test <- merge(Aug.COD.0to27days,Mar4.eava_neonate_comsa,by="ID")
dim(Aug.COD.0to27days)
dim(Mar4.eava_neonate_comsa)
dim(test)

setdiff(Aug.COD.0to27days$ID,Feb11.COD.0to27days$ID)
setdiff(Feb11.COD.0to27days$ID,Aug.COD.0to27days$ID)











# Was it a problem with what we gave Ivalda? - YES (only 183 of the 518 neonates from August merge)
Aug.COD.0to27days <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210218comsa_id/Data/Aug.COD.0to27days.csv")
Feb11.COD.0to27days <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210218comsa_id/Data/Feb11.COD.0to27days.csv")
head(Aug.COD.0to27days)
head(Feb11.COD.0to27days)

test <- merge(Aug.COD.0to27days,Feb11.COD.0to27days,by="ID")
dim(Aug.COD.0to27days)
dim(Feb11.COD.0to27days)
dim(test)

setdiff(Aug.COD.0to27days$ID,Feb11.COD.0to27days$ID)
setdiff(Feb11.COD.0to27days$ID,Aug.COD.0to27days$ID)

# Was it a problem with what we gave Li? - NO (all 518 of the neonates form August merge)
Jan.COD.0to27days <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210218comsa_id/Data/Jan.COD.0to27days.csv")
head(Aug.COD.0to27days)
head(Jan.COD.0to27days)
test <- merge(Aug.COD.0to27days,Jan.COD.0to27days,by="ID")
dim(Aug.COD.0to27days)
dim(Jan.COD.0to27days)
dim(test)

setdiff(Aug.COD.0to27days$ID,Feb11.COD.0to27days$ID)
setdiff(Feb11.COD.0to27days$ID,Aug.COD.0to27days$ID)

# Yesterday and today
Feb17.eava_neonate_comsa <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210218comsa_id/Data/Feb17.eava_neonate_comsa.csv")
Feb18.eava_neonate_comsa <- read.csv("~/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210218comsa_id/Data/Feb18.eava_neonate_comsa.csv")
head(Feb17.eava_neonate_comsa)
head(Feb18.eava_neonate_comsa)

test <- merge(Feb17.eava_neonate_comsa,Feb18.eava_neonate_comsa,by="ID")
dim(Feb17.eava_neonate_comsa)
dim(Feb18.eava_neonate_comsa)
dim(test)

# Does Yesterday merge with August? No - only 384 of the 518.
test <- merge(Aug.COD.0to27days,Feb18.eava_neonate_comsa,by="ID")
dim(Aug.COD.0to27days)
dim(Feb18.eava_neonate_comsa)
dim(test)


# Does what we gave Ivalda merge with yesterday and today? Yes
test <- merge(Feb11.COD.0to27days,Feb17.eava_neonate_comsa,by="ID")
dim(Feb11.COD.0to27days)
dim(Feb17.eava_neonate_comsa)
dim(test)



# Are all of the cases there?
all <- read.csv("/Users/EWilson/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210218comsa_id/Data/all_WHO.csv")
names(all)[names(all) == 'comsa_id'] <- 'ID'
dim(all)
Aug <- merge(Aug.COD.0to27days,all, by=c("ID"))
Jan <- merge(Jan.COD.0to27days,all, by=c("ID"))
Feb11 <- merge(Feb11.COD.0to27days,all, by=c("ID"))
Feb17 <- merge(Feb17.eava_neonate_comsa,all, by=c("ID"))
Feb18 <- merge(Feb18.eava_neonate_comsa,all, by=c("ID"))
dim(Aug.COD.0to27days)
dim(Aug)
dim(Jan.COD.0to27days)
dim(Jan)
dim(Feb11.COD.0to27days)
dim(Feb11)
dim(Feb17.eava_neonate_comsa)
dim(Feb17)
dim(Feb18.eava_neonate_comsa)
dim(Feb18)




all <- read.csv("/Users/EWilson/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210218comsa_id/Data/all_WHO_with_age.csv")
names(all)[names(all) == 'comsa_id'] <- 'ID'
dim(all)
all <- all[,c("ID","ageatdeath")]
test <- merge(Aug.COD.0to27days,all, by=c("ID"))
dim(Aug.COD.0to27days)
dim(all)
dim(test)
summary(test$ageatdeath)    
    
head(test)

write.csv(test,"/Users/EWilson/Desktop/COMSA Analysis/Raw to CalibratedVA input pipeline/For/20210218comsa_id/comsa_id_issue_example.csv", row.names = FALSE)






