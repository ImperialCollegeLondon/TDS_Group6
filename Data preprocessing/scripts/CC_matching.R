#clears environment
rm(list = ls())

#Sets wd to Group 6 folder
setwd("/rds/general/project/hda-22-23/live/TDS/Group6")

training <- readRDS("Data/training_preprocessed.rds")

training$Sex <- as.factor(training$Sex)
training$Ethnicity <- as.factor(training$Ethnicity)
training$Alcohol_status <- as.factor(training$Alcohol_status)
training$Smoke_status <- as.factor(training$Smoke_status)


#load matching package

library(MatchIt)


test <- matchit(case_status ~Age_recr + Sex + Smoke_status + 
                  Alcohol_status + Ethnicity + BMI + deprivation_score, method="nearest",distance= "glm",
              data=training,ratio=3)

matched_training <- match.data(test)
saveRDS(matched_training, file="Data/training_matched_final.rds")

