setwd("/rds/general/project/hda-22-23/live/TDS/Group6")

#install.packages("missForest")
library(missForest)


training <- readRDS("Data/training_matched_cc.rds")
training$Smoke_status <- as.factor(training$Smoke_status)
training$Ethnicity <- as.factor(training$Ethnicity)
training$Alcohol_status <- as.factor(training$Alcohol_status)
training$eid <- as.numeric(training$eid)


#loads data to impute


cols_to_impute <- c("eid", "WBC_count", "red_blood_cell_count", "haemoglobin_conc", 
                      "platelet_count", "lymphocyte_count", "monocyte_count", 
                      "neutrophil_count", "eosinophil_count", "basophil_count", 
                      "nucleated_rbc_count", "reticulocyte_count", "mean_cell_volume",
                      "Smoke_status","Ethnicity", "Alcohol_status", "BMI", "Smoke_pack_years")

imputed_data.imp <- missForest(training[, cols_to_impute], verbose=TRUE)
imputed_data <- imputed_data.imp$ximp

# Check if there are no NA values
colSums(is.na(imputed_data))

training_other <- training[,c("eid", "case_status", "date_recr", "Year_of_birth", 
                              "treatment", "year_recr", "type",
                              "year_diagnosis", "days_since_diagnosis")]
imputed_data$eid <- as.character(imputed_data$eid)
training_other$eid <- as.character(training_other$eid)

#str(imputed_data)
#str(training_other)

imputed_data <- merge(imputed_data, training_other, by="eid")
str(imputed_data)
saveRDS(imputed_data, file="Data/training_matched_imputed.rds")

