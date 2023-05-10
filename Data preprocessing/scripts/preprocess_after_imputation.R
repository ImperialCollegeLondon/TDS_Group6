training_imputed_final <- readRDS("/rds/general/project/hda-22-23/live/TDS/Group6/Data/training_imputed_final.rds")

df <- training_imputed_final
colnames <- c("WBC_count", "red_blood_cell_count", "haemoglobin_conc", "platelet_count", 
              "lymphocyte_count", "monocyte_count", "neutrophil_count", 
              "eosinophil_count", "basophil_count", "nucleated_rbc_count", 
            "reticulocyte_count", "mean_cell_volume", "BMI", "Smoke_pack_years")
  #colname = "BMI"
# iterate over each numeric column and 
# identify values above or below 3 standard deviations

for (col in names(df)) {
  if (is.numeric(df[[col]])) {
    col_mean <- mean(df[[col]], na.rm = TRUE)
    col_sd <- sd(df[[col]], na.rm = TRUE)
    lower_threshold <- col_mean - 3*col_sd
    upper_threshold <- col_mean + 3*col_sd
    
    below_threshold <- df[[col]] < lower_threshold & !is.na(df[[col]])
    above_threshold <- df[[col]] > upper_threshold & !is.na(df[[col]])
    
    # add new column with TRUE for values below or above threshold
    df <- cbind(df, below_threshold | above_threshold)
    colnames(df)[ncol(df)] <- paste0(col, "_outlier")
  } else {
    print(paste("Skipping non-numeric column:", col))
  }
}

#remove columns that aren't useful

df_2 <- df[,!names(df) %in% 
             c("row_index","eid_outlier","time_to_diagnosis_outlier","Sex_outlier",
               "Age_recr_outlier","deprivation_score_outlier","case_status_outlier",
               "Year_of_birth_outlier","year_recr_outlier","row_index_outlier",
               "year_diagnosis_outlier","days_since_diagnosis_outlier")]

# remove rows with outliers

df_sum <- apply(df_2[,33:46], 1, sum)
df_3 <- df_2[df_sum == 0,]

#checking it worked
summary(df$BMI)
summary(df_3$BMI)

#remove columns used to detect outliers
df_3 <- df_3[,1:32]

#save file - hoorayyyyy
saveRDS(df_3, file="Data/training_preprocessed.rds")
