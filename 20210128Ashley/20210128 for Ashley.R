# Last edited: 26 Jan 2021
# Last run:    26 Jan 2021
# Ojbective: provide Ashley with example of COMSA age variables

rm(list=ls())

################################### READ IN DATA AND WHO VARIABLES 
start <- Sys.time()
file <- getwd()
file
COMSAdata <- read.csv(file.path(file,"Data/all_WHO.csv"), stringsAsFactors = FALSE)

names(COMSAdata) <- tolower(colnames(COMSAdata))
COMSAvariables <- as.vector(colnames(COMSAdata))
dim(COMSAdata)

# Remove Stillbirths
table(COMSAdata$id10104, exclude = NULL)
table(COMSAdata$id10109, exclude = NULL)
table(COMSAdata$id10110, exclude = NULL)
table(COMSAdata$id10114, exclude = NULL)
COMSAdata$Stillbirth <- ifelse(COMSAdata$id10104 %in% c("no","dk") & COMSAdata$id10109 %in% c("no","dk") & COMSAdata$id10110 %in% c("no","dk"),1,0)


dim(COMSAdata)
COMSAdata <- COMSAdata[COMSAdata$Stillbirth!=1,]
dim(COMSAdata)
COMSAdata <- as.data.frame(COMSAdata)


                            ###### Age
table(COMSAdata$id10022, exclude=NULL)
table(COMSAdata$ageindays, exclude=NULL)
table(COMSAdata$ageinmonths, exclude = NULL)
table(COMSAdata$ageinyears)
table(COMSAdata$isneonatal, COMSAdata$age_group, exclude=NULL)
table(COMSAdata$age_neonate_minutes, exclude=NULL)
table(COMSAdata$age_neonate_hours, exclude=NULL)
table(COMSAdata$ageindaysneonate, exclude=NULL) 
table(COMSAdata$ageinmonths, exclude=NULL)
table(COMSAdata$age_neonate_hours, exclude=NULL)
table(COMSAdata$age_child_months, exclude=NULL)
table(COMSAdata$age_child_years, exclude=NULL)
table(COMSAdata$age_adult, exclude=NULL)
table(COMSAdata$age_adult, COMSAdata$id10022)

# revise age group into one "ageatdeath" variable based on WHO format
COMSAdata$ageatdeath <- NULL
COMSAdata$ageatdeath <- COMSAdata$ageindays
table(is.na(COMSAdata$ageatdeath))

COMSAdata$ageatdeath <- ifelse(is.na(COMSAdata$ageatdeath) & !is.na(COMSAdata$age_neonate_minutes) & COMSAdata$isneonatal==1, COMSAdata$age_neonate_minutes/(60*24), COMSAdata$ageatdeath)
table(is.na(COMSAdata$ageatdeath))
COMSAdata$ageatdeath <- ifelse(is.na(COMSAdata$ageatdeath) & !is.na(COMSAdata$age_neonate_hours) & COMSAdata$isneonatal==1, COMSAdata$age_neonate_hours/(24), COMSAdata$ageatdeath)
table(is.na(COMSAdata$ageatdeath))
COMSAdata$ageatdeath <- ifelse(is.na(COMSAdata$ageatdeath) & !is.na(COMSAdata$ageindaysneonate) & COMSAdata$isneonatal==1, COMSAdata$ageindaysneonate, COMSAdata$ageatdeath)
table(is.na(COMSAdata$ageatdeath))
COMSAdata$ageatdeath <- ifelse(is.na(COMSAdata$ageatdeath) & !is.na(COMSAdata$age_child_months) & COMSAdata$ischild==1, COMSAdata$age_child_months*30.4, COMSAdata$ageatdeath)
table(is.na(COMSAdata$ageatdeath))
COMSAdata$ageatdeath <- ifelse(is.na(COMSAdata$ageatdeath) & !is.na(COMSAdata$age_child_years) & COMSAdata$ischild==1, COMSAdata$age_child_years*365.25, COMSAdata$ageatdeath)
table(is.na(COMSAdata$ageatdeath))
COMSAdata$ageatdeath <- ifelse(is.na(COMSAdata$ageatdeath) & !is.na(COMSAdata$age_adult) & COMSAdata$isadult==1, COMSAdata$age_adult*365.25, COMSAdata$ageatdeath)
table(is.na(COMSAdata$ageatdeath))
COMSAdata$ageatdeath <- ifelse(is.na(COMSAdata$ageatdeath) & !is.na(COMSAdata$ageinyears) & COMSAdata$isadult==1, COMSAdata$ageinyears*365.25, COMSAdata$ageatdeath)
table(is.na(COMSAdata$ageatdeath))
COMSAdata$ageatdeath <- ifelse(is.na(COMSAdata$ageatdeath) & !is.na(COMSAdata$ageinmonths) & COMSAdata$isadult==1, COMSAdata$ageinmonths*30.4, COMSAdata$ageatdeath)
table(is.na(COMSAdata$ageatdeath))


######## ADD AGE GROUPS
table(COMSAdata$ageatdeath, exclude=NULL)
COMSAdata$ageatdeath <- as.numeric(as.character(COMSAdata$ageatdeath))
COMSAdata$group <- NULL
COMSAdata$group[COMSAdata$ageatdeath < 28] <- "nn"
COMSAdata$group[COMSAdata$ageatdeath >= 28 & COMSAdata$ageatdeath < 60*30.4] <- "ch1_59"
COMSAdata$group[COMSAdata$ageatdeath >= 60*30.4 & COMSAdata$ageatdeath < 15*365.25] <- "ch5_14"
COMSAdata$group[COMSAdata$ageatdeath >= 15*365.25 & COMSAdata$ageatdeath < 50*365.25] <- "ad15_49"
COMSAdata$group[COMSAdata$ageatdeath >= 50*365.25] <- "ad50"

table(COMSAdata$group, exclude=NULL)
table(is.na(COMSAdata$ageatdeath))
