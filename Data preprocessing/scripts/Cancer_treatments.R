#Sets wd to Group 6 folder
setwd("/rds/general/project/hda-22-23/live/TDS/Group6")
rm(list=ls())
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




#For now, the row names in the variable_data are the eids, 
#we want to create a column with the eids
variable_data$eid <- rownames(variable_data)



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

# extracts treatment columns from each dataframe
Le_cases_treatments <- select(Le_cases_final, starts_with(c("eid", "treatment")))
Ly_cases_treatments <- select(Ly_cases_final, starts_with(c("eid", "treatment")))
My_cases_treatments <- select(My_cases_final, starts_with(c("eid", "treatment")))
Mds_cases_treatments <- select(Mds_cases_final, starts_with(c("eid", "treatment")))
Pcv_cases_treatments <- select(Pcv_cases_final, starts_with(c("eid", "treatment")))
Thr_cases_treatments <- select(Thr_cases_final, starts_with(c("eid", "treatment")))
Myf_cases_treatments <- select(Myf_cases_final, starts_with(c("eid", "treatment")))
Chm_cases_treatments <- select(Chm_cases_final, starts_with(c("eid", "treatment")))


#keep cancer treatment data only (X30-X35? X37-X38, X65, X67, X70-X74, X89, X90, X96)

library(dplyr)

treatment_codes <- c("X30","X31","X32","X33","X34","X35","X37","X38","X65","X67",
                     "X70","X71","X72","X73","X74","X89","X90","X96")


# Loop through each column of the data frame for Leukaemia
for (i in seq_along(Le_cases_treatments)) {
  # Use grep to find values that start with code
  match_idx <- grep(paste(treatment_codes, collapse = "|"), Le_cases_treatments[,i])
  # Replace non-matching values with NA
  Le_cases_treatments[-match_idx,i] <- NA
  Le_cases_treatments[Le_cases_treatments == ""] <- NA
}

#repeat the above code but for all other cancers

for (i in seq_along(Ly_cases_treatments)) {
  # Use grep to find values that start with code
  match_idx <- grep(paste(treatment_codes, collapse = "|"), Ly_cases_treatments[,i])
  # Replace non-matching values with NA
  Ly_cases_treatments[-match_idx,i] <- NA
  Ly_cases_treatments[Ly_cases_treatments == ""] <- NA
  
}

for (i in seq_along(My_cases_treatments)) {
  # Use grep to find values that start with code
  match_idx <- grep(paste(treatment_codes, collapse = "|"), My_cases_treatments[,i])
  # Replace non-matching values with NA
  My_cases_treatments[-match_idx,i] <- NA
  My_cases_treatments[My_cases_treatments == ""] <- NA
  
}

for (i in seq_along(Mds_cases_treatments)) {
  # Use grep to find values that start with code
  match_idx <- grep(paste(treatment_codes, collapse = "|"), Mds_cases_treatments[,i])
  # Replace non-matching values with NA
  Mds_cases_treatments[-match_idx,i] <- NA
  Mds_cases_treatments[Mds_cases_treatments == ""] <- NA
  
}

for (i in seq_along(Pcv_cases_treatments)) {
  # Use grep to find values that start with code
  match_idx <- grep(paste(treatment_codes, collapse = "|"), Pcv_cases_treatments[,i])
  # Replace non-matching values with NA
  Pcv_cases_treatments[-match_idx,i] <- NA
  Pcv_cases_treatments[Pcv_cases_treatments == ""] <- NA
  
}

for (i in seq_along(Thr_cases_treatments)) {
  # Use grep to find values that start with code
  match_idx <- grep(paste(treatment_codes, collapse = "|"), Thr_cases_treatments[,i])
  # Replace non-matching values with NA
  Thr_cases_treatments[-match_idx,i] <- NA
  Thr_cases_treatments[Thr_cases_treatments == ""] <- NA
  
}

for (i in seq_along(Myf_cases_treatments)) {
  # Use grep to find values that start with code
  match_idx <- grep(paste(treatment_codes, collapse = "|"), Myf_cases_treatments[,i])
  # Replace non-matching values with NA
  Myf_cases_treatments[-match_idx,i] <- NA
  Myf_cases_treatments[Myf_cases_treatments == ""] <- NA
  
}
### the code doesn't work well for Chm for some reason (ignores a column)
for (i in seq_along(Chm_cases_treatments)) {
  # Use grep to find values that start with code
  match_idx <- grep(paste(treatment_codes, collapse = "|"), Chm_cases_treatments[,i])
  # Replace non-matching values with NA
  Chm_cases_treatments[-match_idx,i] <- NA
  Chm_cases_treatments[Chm_cases_treatments == ""] <- NA
  
}

#proportion of rows with at least 1 non-NA value in each dataframe
#(they have >= 1 treatment)

sum(rowSums(!is.na(Le_cases_treatments[,-1])) > 0) / nrow(Le_cases_treatments)
sum(rowSums(!is.na(Ly_cases_treatments[,-1])) > 0) / nrow(Ly_cases_treatments)
sum(rowSums(!is.na(My_cases_treatments[,-1])) > 0) / nrow(My_cases_treatments)
sum(rowSums(!is.na(Mds_cases_treatments[,-1])) > 0) / nrow(Mds_cases_treatments)
sum(rowSums(!is.na(Chm_cases_treatments[,-1])) > 0) / nrow(Chm_cases_treatments)
sum(rowSums(!is.na(Myf_cases_treatments[,-1])) > 0) / nrow(Myf_cases_treatments)
sum(rowSums(!is.na(Pcv_cases_treatments[,-1])) > 0) / nrow(Pcv_cases_treatments)
sum(rowSums(!is.na(Thr_cases_treatments[,-1])) > 0) / nrow(Thr_cases_treatments)





