# GMM and Kmeans Cox Regression
rm(list = ls())
library(survival)
library(lubridate) #for extracting year
library(ggplot2)
library(survminer)

setwd("/rds/general/project/hda-22-23/live/TDS/Group6/Data/")

data <- read.csv("dataset_for_cox_5_1_23.csv")
#reformat to make survival variabless
data$date_death <- as.Date(data$date_death, format = "%m/%d/%Y")
data$year_death <- year(data$date_death)
# convert KMeans_cluster to a factor
data$KMeans_cluster <- factor(data$KMeans_cluster)
# convert GMM cluster to a factor
data$cluster <- factor(data$cluster)

#making hazard years
data$hazard_years <- ifelse(data$case_status == 1, data$year_diagnosis - data$year_of_birth - data$Age_recr, 2023 - data$year_of_birth -data$Age_recr)

#view data
View(data)

#making the surv object
surv_obj <- Surv(time = data$hazard_years, event = data$case_status)

#model 1: covariates only
{
  cox_covs <- coxph(surv_obj ~ 
                    Age_recr +
                    Smoke_status +	
                    Ethnicity	+ 
                    Alcohol_status +	
                    BMI	+ 
                    Smoke_pack_years	+ 
                    Sex	+ 
                    deprivation_score
                  , data = data)
}
summary(cox_covs)
m1p_vals <- summary(cox_covs)$coefficients[, "Pr(>|z|)"]
View(m1p_vals)
cox_covs_graph <- cox.zph(cox_covs)

# Plot the residuals
plot(cox_covs_graph, var = 1)

#model 2: KMeans
{
  cox_kmeans <- coxph(surv_obj ~ 
                      Age_recr +
                      Smoke_status +	
                      Ethnicity	+ 
                      Alcohol_status +	
                      BMI	+ 
                      Smoke_pack_years	+ 
                      Sex	+ 
                      deprivation_score +
                      KMeans_cluster
                    , data = data)
}
summary(cox_kmeans)
m2p_vals <- summary(cox_kmeans)$coefficients[, "Pr(>|z|)"]
View(m2p_vals)
cox_kmeans_graph <- cox.zph(cox_kmeans)

# Plot the residuals
plot(cox_kmeans, var = 1)

#model 3: GMM
{
  cox_gmm <- coxph(surv_obj ~ 
                        Age_recr +
                        Smoke_status +	
                        Ethnicity	+ 
                        Alcohol_status +	
                        BMI	+ 
                        Smoke_pack_years	+ 
                        Sex	+ 
                        deprivation_score +
                        cluster
                      , data = data)
}
summary(cox_gmm)
m3p_vals <- summary(cox_gmm)$coefficients[, "Pr(>|z|)"]
View(m3p_vals)
cox_gmm_graph <- cox.zph(cox_gmm)

#model 4: blood counts
{
  cox_blood <- coxph(surv_obj ~ 
                     Age_recr +
                     Smoke_status +	
                     Ethnicity	+ 
                     Alcohol_status +	
                     BMI	+ 
                     Smoke_pack_years	+ 
                     Sex	+ 
                     deprivation_score +
                       WBC_count +
                       red_blood_cell_count	+
                       haemoglobin_conc	+
                       platelet_count	+
                       lymphocyte_count	+
                       monocyte_count	+
                       neutrophil_count	+
                       eosinophil_count	+
                       basophil_count	+
                       nucleated_rbc_count	+
                       reticulocyte_count
                   , data = data)
}
summary(cox_blood)
m4p_vals <- summary(cox_blood)$coefficients[, "Pr(>|z|)"]
View(m4p_vals)
cox_blood_graph <- cox.zph(cox_blood)

# Plot the residuals
plot(cox_gmm, var = 1)

#plot the survival curve
# Create a data frame for the survival object
surv_df <- data.frame(time = surv_obj$time, status = surv_obj$event)

# Create the Kaplan-Meier curve
km_curve <- survfit(surv_obj ~ 1)

# Create a data frame for the curve
km_df <- data.frame(time = km_curve$time, surv = km_curve$surv)

# Plot the curve
ggplot(km_df, aes(x = time, y = surv)) +
  geom_step() +
  xlab("Hazard Years") +
  ylab("Survival Probability") +
  ggtitle("Kaplan-Meier Curve") +
  theme_minimal()
