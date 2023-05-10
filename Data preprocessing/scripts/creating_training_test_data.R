#Sets wd to Group 6 folder
setwd("/rds/general/project/hda-22-23/live/TDS/Group6")



#loads control dataset
controls_raw <- readRDS("outcome_definition/Outputs/Controls/output_final.rds")
#Loads variable data
variable_data <- readRDS("variable_data.rds")

#Selects possible eid controls, excluding incident_case=1 and prevalent_case=1

controls <- controls_raw %>%
  group_by(eid) %>%
  filter(case==0)
  select(c(eid, date_recr, date_diagnosis, time_to_diagnosis, date_death))

#merges variable_data with each eid.
controls_final <- merge(controls,variable_data,by="eid")


#in case we want to use treatments in future (unlikely)
#controls_treatments <- select(controls_final, starts_with(c("eid", "treatment")))


# corrects the name of columns in the dataframe

controls_final <- rename(controls_final, date_recr.0.0 = date_recr, 
                         date_diagnosis.0.0 = date_diagnosis,
       date_death.0.0 = date_death,time_to_diagnosis.0.0 = time_to_diagnosis)


#selects eid column and columns ending in 0.0
controls_final <- select(controls_final,ends_with(c("eid","0.0")))

#convert year of recruitment to age of recruitment 
controls_final$date_recr.0.0 <- as.Date(controls_final$date_recr.0.0)
#controls$date_diagnosis.0.0 <- as.Date(controls$date_diagnosis.0.0)
#controls$date_death.0.0 <- as.Date(controls$date_death.0.0)
controls_final$year_recr.0.0 <- as.numeric(format(controls_final$date_recr.0.0, format = "%Y"))
#controls$year_diagnosis.0.0 <- as.numeric(format(controls$date_diagnosis.0.0, format = "%Y"))


controls_final <- mutate(controls_final, Age_recr.0.0 = year_recr.0.0-Year_of_birth.0.0)

#counts NAs in each column
colSums(is.na(controls_final))

#remove .0.0s from column names cuz it's annoying
colnames(controls_final) <- gsub("\\.0\\.0$", "", colnames(controls_final))


controls_original <- saveRDS(controls_final,file="Data/controls_original.rds")

#######################################################################################

#splits controls data into training and test

#make the splitting reproducible
set.seed(1)

#use 80% of dataset as training set and 20% as test set
sample <- sample(c(TRUE, FALSE), nrow(controls_final), replace=TRUE, prob=c(0.8,0.2))
controls_train  <- controls_final[sample, ]
controls_test   <- controls_final[!sample, ]

#save files
saveRDS(controls_train,file="Data/controls_train.rds")
saveRDS(controls_test,file="Data/controls_test.rds")

##################################CASES###################################################

#loads cases file
cases_final <- readRDS("cases.rds")

#make the splitting reproducible
set.seed(1)

#use 80% of dataset as training set and 20% as test set
sample <- sample(c(TRUE, FALSE), nrow(cases_final), replace=TRUE, prob=c(0.8,0.2))
cases_train  <- cases_final[sample, ]
cases_test   <- cases_final[!sample, ]

#save files
saveRDS(cases_train,file="Data/cases_train.rds")
saveRDS(cases_test,file="Data/cases_test.rds")

############################ MAKING TRAINING AND TEST DATA ##################################

cases_train <- readRDS("Data/Original_data/cases_train.rds")
controls_train <- readRDS("Data/Original_data/controls_train.rds")

cases_test <- readRDS("Data/Original_data/cases_test.rds")
controls_test <- readRDS("Data/Original_data/controls_test.rds")


#assign case control status (needed for matching)
cases_train$case_status <- 1
controls_train$case_status <- 0
cases_test$case_status <- 1
controls_test$case_status <- 0


#checks no case eids are in controls eids
table(cases_train$eid %in% controls_train$eid)

#removes country dep scores as have merged dep_score
controls_train = subset(controls_train, select = -c(deprivation_wales,deprivation_eng,deprivation_scot) )

controls_test = subset(controls_test, select = -c(deprivation_wales,deprivation_eng,deprivation_scot) )

#merges both dataframes together
test <- merge(controls_test,cases_test,all=T)

saveRDS(test,file="Data/Original_data/test_original.rds")

#counts NAs for test set
colSums(is.na(test))

#Smoke_pack_years, Smoking_status have lots of NAs

library(dplyr)

#removes test samples with NAs in blood cell count or covariates data only
test <- test %>%
  filter(complete.cases(test[,c(1,5:7,9:10,12:26)]))

#remove Smoking_status as we have Smoke_status
test <- subset(test, select = -c(Smoking_status))

saveRDS(test,file="Data/test_clean.rds")
