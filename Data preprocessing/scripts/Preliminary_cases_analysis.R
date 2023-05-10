#Sets wd to Group 6 folder
setwd("/rds/general/project/hda-22-23/live/TDS/Group6")

#Loads blood cancer cases datasets
Le_output_hes <- readRDS("outcome_definition/Outputs/Leukaemia/output_final.rds")
Ly_output_hes <- readRDS("outcome_definition/Outputs/Lymphoma/output_final.rds")
My_output_hes <- readRDS("outcome_definition/Outputs/Myeloma/output_final.rds")
Mds_output_hes <- readRDS("outcome_definition/Outputs/Myelodysplastic_syndromes/output_final.rds")
Pcv_output_hes <- readRDS("outcome_definition/Outputs/Polycythaemia_vera/output_final.rds")
Thr_output_hes <- readRDS("outcome_definition/Outputs/Thrombocythaemia/output_final.rds")
Myf_output_hes <- readRDS("outcome_definition/Outputs/Myelofibrosis/output_final.rds")
Chm_output_hes <- readRDS("outcome_definition/Outputs/Chronic_myeloprolif/output_final.rds")

#Loads variable data
variable_data <- readRDS("extraction_and_recoding/outputs/ukb_extracted.rds")

# prevalent cases
table(Le_output_hes$prevalent_case)
table(Ly_output_hes$prevalent_case)
table(My_output_hes$prevalent_case)
table(Mds_output_hes$prevalent_case)
table(Pcv_output_hes$prevalent_case)
table(Thr_output_hes$prevalent_case)
table(Myf_output_hes$incident_case)
table(Chm_output_hes$prevalent_case)

# incident cases
table(Le_output_hes$incident_case)
table(Ly_output_hes$incident_case)
table(My_output_hes$incident_case)
table(Mds_output_hes$incident_case)
table(Pcv_output_hes$incident_case)
table(Thr_output_hes$incident_case)
table(Myf_output_hes$incident_case)
table(Chm_output_hes$incident_case)

#There are 136 incident cases for Myeloma, 1755 incident cases for Leukemia, 
#2887 incident cases for Lymphoma

# the number of total cases
table(Le_output_hes$incident_case) + table(Ly_output_hes$incident_case) + table(My_output_hes$incident_case) + table(Mds_output_hes$incident_case) + table(Pcv_output_hes$incident_case) + table(Thr_output_hes$incident_case) + table(Myf_output_hes$incident_case) + table(Chm_output_hes$incident_case)

#For now, the row names in the variable_data are the eids, 
#we want to create a column with the eids
variable_data$eid <- rownames(variable_data)

#changing the ethnicity groups
variable_data$Ethnicity.0.0[variable_data$Ethnicity.0.0 == 1001] <- 'British'
variable_data$Ethnicity.0.0[variable_data$Ethnicity.0.0 == 1002] <- 'Irish'
variable_data$Ethnicity.0.0[variable_data$Ethnicity.0.0 == 1003] <- 'Other White'
variable_data$Ethnicity.0.0[variable_data$Ethnicity.0.0 == -1 | variable_data$Ethnicity.0.0 == -3] <- NA
variable_data$Ethnicity.0.0[variable_data$Ethnicity.0.0 != 'British' & variable_data$Ethnicity.0.0 != 'Irish' & variable_data$Ethnicity.0.0 != 'Other White' & !is.na(variable_data$Ethnicity.0.0)] <- 'Other'

table(variable_data$Ethnicity.0.0)


#changing the smoking groups
variable_data$Smoking_status.0.0[variable_data$Smoking_status.0.0 == 111] <- 'Regular'
variable_data$Smoking_status.0.0[variable_data$Smoking_status.0.0 == 112] <- 'Occasional'
variable_data$Smoking_status.0.0[variable_data$Smoking_status.0.0 == 113] <- 'Previous'
variable_data$Smoking_status.0.0[variable_data$Smoking_status.0.0 == 114] <- 'Never'
variable_data$Smoking_status.0.0[variable_data$Smoking_status.0.0 == -818] <- NA

table(variable_data$Smoking_status.0.0)

#changing the smoke status groups (have less missing values than smoking groups)
variable_data$Smoke_status.0.0[variable_data$Smoke_status.0.0 == -3] <- NA
variable_data$Smoke_status.0.0[variable_data$Smoke_status.0.0 == 0] <- 'Never'
variable_data$Smoke_status.0.0[variable_data$Smoke_status.0.0 == 1] <- 'Previous'
variable_data$Smoke_status.0.0[variable_data$Smoke_status.0.0 == 2] <- 'Current'


#changing the alcohol groups
variable_data$Alcohol_status.0.0[variable_data$Alcohol_status.0.0 == 2] <- 'Current'
variable_data$Alcohol_status.0.0[variable_data$Alcohol_status.0.0 == 1] <- 'Previous'
variable_data$Alcohol_status.0.0[variable_data$Alcohol_status.0.0 == 0] <- 'Never'
variable_data$Alcohol_status.0.0[variable_data$Alcohol_status.0.0 == -3] <- NA

table(variable_data$Alcohol_status.0.0)

#deprivation score (merging the scores from the different countries)
#variable_data <- mutate(variable_data, deprivation_score = sum(deprivation_eng.0.0, deprivation_wales.0.0, deprivation_scot.0.0, na.rm=TRUE))
variable_data$deprivation_score.0.0 <- rowSums(variable_data[,c("deprivation_eng.0.0", "deprivation_wales.0.0", "deprivation_scot.0.0")], na.rm=TRUE)

summary(variable_data$deprivation_score.0.0)

saveRDS(variable_data, file = "variable_data.rds")


#Selects eid cases for each cancer (this was selecting both incident and prevalent but we only want incident because cohort study)
library(dplyr)

Le_cases <- Le_output_hes %>%
  group_by(eid) %>%
  filter(any(incident_case==1)) %>%
  select(c(eid, date_recr, date_diagnosis, time_to_diagnosis, date_death))

Ly_cases <- Ly_output_hes %>%
  group_by(eid) %>%
  filter(any(incident_case==1)) %>%
  select(c(eid, date_recr, date_diagnosis, time_to_diagnosis, date_death))

My_cases <- My_output_hes %>%
  group_by(eid) %>%
  filter(any(incident_case==1)) %>%
  select(c(eid, date_recr, date_diagnosis, time_to_diagnosis, date_death))

Mds_cases <- Mds_output_hes %>%
  group_by(eid) %>%
  filter(any(incident_case==1)) %>%
  select(c(eid, date_recr, date_diagnosis, time_to_diagnosis, date_death))

Pcv_cases <- Pcv_output_hes %>%
  group_by(eid) %>%
  filter(any(incident_case==1)) %>%
  select(c(eid, date_recr, date_diagnosis, time_to_diagnosis, date_death))

Thr_cases <- Thr_output_hes %>%
  group_by(eid) %>%
  filter(any(incident_case==1)) %>%
  select(c(eid, date_recr, date_diagnosis, time_to_diagnosis, date_death))

Myf_cases <- Myf_output_hes %>%
  group_by(eid) %>%
  filter(any(incident_case==1)) %>%
  select(c(eid, date_recr, date_diagnosis, time_to_diagnosis, date_death))

Chm_cases <- Chm_output_hes %>%
  group_by(eid) %>%
  filter(any(incident_case==1)) %>%
  select(c(eid, date_recr, date_diagnosis, time_to_diagnosis, date_death))








#merges variable_data for patients with lymphoma, myeloma or leukaemia, by eid.
Le_cases_final <- merge(Le_cases,variable_data,by="eid")
Ly_cases_final <- merge(Ly_cases,variable_data,by="eid")
My_cases_final <- merge(My_cases,variable_data,by="eid")
Mds_cases_final <- merge(Mds_cases,variable_data,by="eid")
Pcv_cases_final <- merge(Pcv_cases,variable_data,by="eid")
Thr_cases_final <- merge(Thr_cases,variable_data,by="eid")
Myf_cases_final <- merge(Myf_cases,variable_data,by="eid")
Chm_cases_final <- merge(Chm_cases,variable_data,by="eid")

#find no. of NA's in each column
colSums(is.na(Le_cases_final))
colSums(is.na(Ly_cases_final))
colSums(is.na(My_cases_final))
colSums(is.na(Mds_cases_final))
colSums(is.na(Pcv_cases_final))
colSums(is.na(Thr_cases_final))
colSums(is.na(Myf_cases_final))
colSums(is.na(Chm_cases_final))


#There seems to be a lot of NA's for instances 1,2,3 etc, but not for instance 0.
#Let's remove these variables then. To do this, we can select only the columns that 
#end with 0.0, however, this will get rid of all the treatment columns. So we will
#need to put those columns back in, let's first save these columns separately for
#each dataframe.

# extracts treatment columns from each dataframe
Le_cases_treatments <- select(Le_cases_final, starts_with(c("eid", "treatment")))
Ly_cases_treatments <- select(Ly_cases_final, starts_with(c("eid", "treatment")))
My_cases_treatments <- select(My_cases_final, starts_with(c("eid", "treatment")))
Mds_cases_treatments <- select(Mds_cases_final, starts_with(c("eid", "treatment")))
Pcv_cases_treatments <- select(Pcv_cases_final, starts_with(c("eid", "treatment")))
Thr_cases_treatments <- select(Thr_cases_final, starts_with(c("eid", "treatment")))
Myf_cases_treatments <- select(Myf_cases_final, starts_with(c("eid", "treatment")))
Chm_cases_treatments <- select(Chm_cases_final, starts_with(c("eid", "treatment")))

# corrects the name of columns in the dataframe
Le_cases_final$date_recr.0.0 <- Le_cases_final$date_recr
Le_cases_final$date_diagnosis.0.0 <- Le_cases_final$date_diagnosis
Le_cases_final$date_death.0.0 <- Le_cases_final$date_death
Le_cases_final$time_to_diagnosis.0.0 <- Le_cases_final$time_to_diagnosis

Ly_cases_final$date_recr.0.0 <- Ly_cases_final$date_recr
Ly_cases_final$date_diagnosis.0.0 <- Ly_cases_final$date_diagnosis
Ly_cases_final$date_death.0.0 <- Ly_cases_final$date_death
Ly_cases_final$time_to_diagnosis.0.0 <- Ly_cases_final$time_to_diagnosis

My_cases_final$date_recr.0.0 <- My_cases_final$date_recr
My_cases_final$date_diagnosis.0.0 <- My_cases_final$date_diagnosis
My_cases_final$date_death.0.0 <- My_cases_final$date_death
My_cases_final$time_to_diagnosis.0.0 <- My_cases_final$time_to_diagnosis

Mds_cases_final$date_recr.0.0 <- Mds_cases_final$date_recr
Mds_cases_final$date_diagnosis.0.0 <- Mds_cases_final$date_diagnosis
Mds_cases_final$date_death.0.0 <- Mds_cases_final$date_death
Mds_cases_final$time_to_diagnosis.0.0 <- Mds_cases_final$time_to_diagnosis

Pcv_cases_final$date_recr.0.0 <- Pcv_cases_final$date_recr
Pcv_cases_final$date_diagnosis.0.0 <- Pcv_cases_final$date_diagnosis
Pcv_cases_final$date_death.0.0 <- Pcv_cases_final$date_death
Pcv_cases_final$time_to_diagnosis.0.0 <- Pcv_cases_final$time_to_diagnosis

Thr_cases_final$date_recr.0.0 <- Thr_cases_final$date_recr
Thr_cases_final$date_diagnosis.0.0 <- Thr_cases_final$date_diagnosis
Thr_cases_final$date_death.0.0 <- Thr_cases_final$date_death
Thr_cases_final$time_to_diagnosis.0.0 <- Thr_cases_final$time_to_diagnosis

Myf_cases_final$date_recr.0.0 <- Myf_cases_final$date_recr
Myf_cases_final$date_diagnosis.0.0 <- Myf_cases_final$date_diagnosis
Myf_cases_final$date_death.0.0 <- Myf_cases_final$date_death
Myf_cases_final$time_to_diagnosis.0.0 <- Myf_cases_final$time_to_diagnosis

Chm_cases_final$date_recr.0.0 <- Chm_cases_final$date_recr
Chm_cases_final$date_diagnosis.0.0 <- Chm_cases_final$date_diagnosis
Chm_cases_final$date_death.0.0 <- Chm_cases_final$date_death
Chm_cases_final$time_to_diagnosis.0.0 <- Chm_cases_final$time_to_diagnosis

#selects eid column and columns ending in 0.0
Le_cases_FINAL <- select(Le_cases_final,ends_with(c("eid","0.0")))
Ly_cases_FINAL <- select(Ly_cases_final,ends_with(c("eid","0.0")))
My_cases_FINAL <- select(My_cases_final,ends_with(c("eid","0.0")))
Mds_cases_FINAL <- select(Mds_cases_final,ends_with(c("eid","0.0")))
Pcv_cases_FINAL <- select(Pcv_cases_final,ends_with(c("eid","0.0")))
Thr_cases_FINAL <- select(Thr_cases_final,ends_with(c("eid","0.0")))
Myf_cases_FINAL <- select(Myf_cases_final,ends_with(c("eid","0.0")))
Chm_cases_FINAL <- select(Chm_cases_final,ends_with(c("eid","0.0")))

#Merges eid and 0.0 data with treatment data
# Le_cases_FINAL <- cbind(Le_cases_clean,Le_cases_treatments)
# Ly_cases_FINAL <- cbind(Ly_cases_clean,Ly_cases_treatments)
# My_cases_FINAL <- cbind(My_cases_clean,My_cases_treatments)
# Mds_cases_FINAL <- cbind(Mds_cases_clean,Mds_cases_treatments)
# Pcv_cases_FINAL <- cbind(Pcv_cases_clean,Pcv_cases_treatments)
# Thr_cases_FINAL <- cbind(Thr_cases_clean,Thr_cases_treatments)
# Myf_cases_FINAL <- cbind(Myf_cases_clean,Myf_cases_treatments)
# Chm_cases_FINAL <- cbind(Chm_cases_clean,Chm_cases_treatments)


colSums(is.na(Ly_cases_FINAL))

#convert relevant variables to factors
#Le_cases_FINAL$Sex.0.0 <- factor(Le_cases_FINAL$Sex.0.0)
#Le_cases_FINAL$Smoke_status.0.0 <- factor(Le_cases_FINAL$Smoke_status.0.0)
#Le_cases_FINAL$Alcohol_status.0.0 <- factor(Le_cases_FINAL$Alcohol_status.0.0)
#Le_cases_FINAL$Ethnicity.0.0 <- factor(Le_cases_FINAL$Ethnicity.0.0)

#My_cases_FINAL$Sex.0.0 <- factor(My_cases_FINAL$Sex.0.0)
#My_cases_FINAL$Smoke_status.0.0 <- factor(My_cases_FINAL$Smoke_status.0.0)
#My_cases_FINAL$Alcohol_status.0.0 <- factor(My_cases_FINAL$Alcohol_status.0.0)
#My_cases_FINAL$Ethnicity.0.0 <- factor(My_cases_FINAL$Ethnicity.0.0)

#Ly_cases_FINAL$Sex.0.0 <- factor(Ly_cases_FINAL$Sex.0.0)
#Ly_cases_FINAL$Smoke_status.0.0 <- factor(Ly_cases_FINAL$Smoke_status.0.0)
#Ly_cases_FINAL$Alcohol_status.0.0 <- factor(Ly_cases_FINAL$Alcohol_status.0.0)
#Ly_cases_FINAL$Ethnicity.0.0 <- factor(Ly_cases_FINAL$Ethnicity.0.0)





Le_cases_FINAL$type <- rep("Leukaemia", times=nrow(Le_cases_FINAL))
Ly_cases_FINAL$type <- rep("Lymphoma", times=nrow(Ly_cases_FINAL))
My_cases_FINAL$type <- rep("Myeloma", times=nrow(My_cases_FINAL))
Mds_cases_FINAL$type <- rep("Myelodysplastic_syndromes", times=nrow(Mds_cases_FINAL))
Pcv_cases_FINAL$type <- rep("Polycythaemia_vera", times=nrow(Pcv_cases_FINAL))
Thr_cases_FINAL$type <- rep("Thrombocythaemia", times=nrow(Thr_cases_FINAL))
Myf_cases_FINAL$type <- rep("Myelofibrosis", times=nrow(Myf_cases_FINAL))
Chm_cases_FINAL$type <- rep("Chronic_myeloprolif", times=nrow(Chm_cases_FINAL))

#merged dataset of the 3 cancers with the indicator of the blood cancer
cases <- bind_rows(Le_cases_FINAL, Ly_cases_FINAL, My_cases_FINAL, Mds_cases_FINAL, Pcv_cases_FINAL, Thr_cases_FINAL, Myf_cases_FINAL, Chm_cases_FINAL)
cases$row_index <- 1:nrow(cases)

#removing marginal deprivation scores (because they are summarised in the deprivation_score variable)
cases <- cases[,!names(cases) %in% c("deprivation_eng.0.0", "deprivation_wales.0.0", "deprivation_scot.0.0")]


#convert year of recruitment to age of recruitment 
cases$date_recr.0.0 <- as.Date(cases$date_recr.0.0)
cases$date_diagnosis.0.0 <- as.Date(cases$date_diagnosis.0.0)
cases$date_death.0.0 <- as.Date(cases$date_death.0.0)
cases$year_recr.0.0 <- as.numeric(format(cases$date_recr.0.0, format = "%Y"))
cases$year_diagnosis.0.0 <- as.numeric(format(cases$date_diagnosis.0.0, format = "%Y"))

# check if all study participants were born before 2000 : OK
sum(cases$Year_of_birth.0.0 > 2000)

#add a Year of recruitment 
cases <- mutate(cases, Age_recr.0.0 = year_recr.0.0-Year_of_birth.0.0)


#buffer period: 6 months (check that participants developed disease at least 6 months after recruitment)
cases$days_since_diagnosis.0.0 <- as.numeric(cases$date_diagnosis.0.0-cases$date_recr.0.0)
cases <- filter(cases, (cases$date_diagnosis.0.0-cases$date_recr.0.0)/30.437 > 6)
str(cases)

saveRDS(cases, file = "cases_with_duplicates.rds")


cases <- readRDS("cases_with_duplicates.rds")


#removing duplicate cancer diagnosis and only keeping the first diagnosis
#if several cancers are diagnosed simultaneously as a first diagnosis, we remove them from the dataset and store them in the "other" dataset

duplicates <- cases[duplicated(cases$eid),]
nrow(duplicates)
unique(duplicates$type)
#all types except Leukaemia, 1471 rows

duplicates_ALL <- cases[cases$eid %in% duplicates$eid,]
unique(duplicates_ALL$type)
nrow(duplicates_ALL)
#all types, 2683 rows, contains (?) all rows that are at least duplicated once in terms of eid

#removing all duplicates from cases
cases <- cases[!(cases$eid %in% duplicates_ALL$eid),]


duplicates_ALL <- duplicates_ALL %>% group_by(eid)

split_dup <- group_split(duplicates_ALL)

other <- data.frame()
real_type <- data.frame()


for (i in 1:length(split_dup)) {
  #i=3
  max_days <- split_dup[[i]][which.max(split_dup[[i]]$days_since_diagnosis.0.0),]$days_since_diagnosis.0.0
  # if there is another value of days_since_diagnosis that is the same as the max value 
  #(= 2 diff cancer diagnosis for the earliest date of diagnosis)
  if (any(split_dup[[i]][-which.max(split_dup[[i]]$days_since_diagnosis.0.0),]$days_since_diagnosis.0.0==max_days)) {
    other <- rbind(other, split_dup[[i]])
  }
  else {
    real_type <- rbind(real_type, split_dup[[i]][which.max(split_dup[[i]]$days_since_diagnosis.0.0),])
  }
}

#binding the rows with the true types to cases
cases <- rbind(cases, real_type)

#checking there aren't any duplicates left
nrow(cases[duplicated(cases$eid),])==0


#removing the 0.0 from the column names that have it
colnames(cases) <- gsub('.0.0', '', colnames(cases))




## KNN Imputation`
library(caret)
library(RANN)

# checking the number of missing values for each column
colSums(is.na(cases))

# knn imputation - rule of thumb for k: the square root of the number of observations
preProcValues <- preProcess(cases %>% 
                              select(Smoke_pack_years, BMI, WBC_count, red_blood_cell_count, haemoglobin_conc, platelet_count, lymphocyte_count, monocyte_count, neutrophil_count, eosinophil_count, basophil_count, nucleated_rbc_count, reticulocyte_count, mean_cell_volume),
                            method = c("knnImpute"),
                            k = 80,
                            knnSummary = mean)

# check the number of missing columns for each observation (for the selected columns that we will impute on)
idx <- apply(cases[,c("Smoke_pack_years", "BMI", "WBC_count", "red_blood_cell_count", "haemoglobin_conc", 
                      "platelet_count", "lymphocyte_count", "monocyte_count", "neutrophil_count", 
                      "eosinophil_count", "basophil_count", "nucleated_rbc_count", "reticulocyte_count", "mean_cell_volume")], 1, function(x) sum(is.na(x)))

# check the number of observations that have missing data for more than 10 columns
length(as.vector(which(idx >= 10)))
# remove from cases those observations with missing columns more than 10 NA columns (we think that knn imputation will not be accurate for the observations having fewer than 4 columns)
cases <- cases[-which(idx > 10), ]

# impute
cases_imputed <- predict(preProcValues, cases,na.action = na.pass)

# check the number of missing values for each column
colSums(is.na(cases_imputed))

#saving cases and cases_imputed as rds files
saveRDS(cases, file = "cases.rds")
saveRDS(cases_imputed, file = "cases_imputed.rds")


#matching controls to the cases
library(ccoptimalmatch)





#creating table ones

library("table1")

cases_imputed <- readRDS("cases_imputed.rds")

cases_imputed$Sex <- factor(cases_imputed$Sex, labels=c("Male", "Female"))

label(cases_imputed$Smoke_status)<-"Smoke Status"
label(cases_imputed$Alcohol_status)<-"Alcohol Status"
label(cases_imputed$Ethnicity)<-"Ethnicity"
label(cases_imputed$Sex)<-"Sex"
label(cases_imputed$Age_recr)<-"Age at recruitment"
label(cases_imputed$BMI)<-"BMI"
label(cases_imputed$deprivation_score)<-"Deprivation score"
label(cases_imputed$red_blood_cell_count)<-"RBC count"
label(cases_imputed$nucleated_rbc_count)<-"Nucleated RBC count"
label(cases_imputed$reticulocyte_count)<-"Reticulocyte count"
label(cases_imputed$WBC_count)<-"WBC count"
label(cases_imputed$neutrophil_count)<-"Neutrophil count"
label(cases_imputed$eosinophil_count)<-"Eosinophil count"
label(cases_imputed$basophil_count)<-"Basophil count"
label(cases_imputed$platelet_count)<-"Platelet count"
label(cases_imputed$lymphocyte_count)<-"Lymphocyte count"
label(cases_imputed$monocyte_count)<-"Monocyte count"
label(cases_imputed$mean_cell_volume)<-"Mean cell volume"
label(cases_imputed$haemoglobin_conc)<-"Haemoglobin conc"

general_caption <- "Summary statistics of covariates of the dataset"
rbc_caption <- "Summary statistics of red blood counts in the dataset"
wbc_caption <- "Summary statistics of white blood counts in the dataset"
obc_caption <- "Summary statistics of blood cell counts (other)"

general_table <- table1(~ Sex + Age_recr+ BMI + Ethnicity + Smoke_status + Alcohol_status + deprivation_score| type, data=cases_imputed,
                        overall=FALSE, caption=general_caption)

rbc_table <- table1(~ red_blood_cell_count + nucleated_rbc_count + reticulocyte_count | type, data=cases_imputed,
                    overall=FALSE, caption=rbc_caption)

wbc_table <- table1(~ WBC_count + neutrophil_count + eosinophil_count + basophil_count | type, data=cases_imputed,
                    overall=FALSE, caption=wbc_caption)

obc_table <- table1(~ haemoglobin_conc + platelet_count + lymphocyte_count + monocyte_count + mean_cell_volume | type, data=cases_imputed,
                    overall=FALSE, caption=obc_caption)

