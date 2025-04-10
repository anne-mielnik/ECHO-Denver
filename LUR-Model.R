# ECHO LUR Model for Metals in PM2.5 across Denver, CO
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Anne Mielnik (anne.mielnik@colostate.edu)
# Department of Environmental & Radiological Health Sciences
# Colorado State University, Fort Collins, CO 80523
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# This file provides a template of R script(s) used to model metal air pollution in 
# fine particulate matter (PM2.5) across Denver, CO using land use regression (LUR) as part of the 
# Environmental influences on Child Health Outcomes (ECHO) study.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Load packages.
library(ggplot2)
library(glmnet)
library(maps)
library(mapview)
library(sf)
library(tidyverse)

# Define coordinate reference system.
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# Prior to running LUR models: Import data and create new column(s) for metal concentrations in PM2.5. 
# Note: Concentration data converted to units of ng/m3, log-transformed, and subset by sampling campaign and meteorological season.

# Define y (i.e., specify the metal species in PM2.5).
y <- df$metal_species

# Define x based on columns in data frame that are predictor variables (e.g., 6-97).
x <- subset(df, select = c(6:97))

# Determine what columns/predictor variables contain NA values to remove for LASSO
# variable selection. In our model, elevation, population count, and population density 
# with certain buffers contain too many NA values (n > 1) to run LASSO regression.
names(which(colSums(is.na(x)) > 0))
x <- subset(x, select = -c(elevation_50, pop_count_50, pop_count_100, pop_count_250,
                           pop_count_500, pop_density_50, pop_density_100, pop_density_250,
                           pop_density_500))
x <- data.matrix(x)

# Parameterize model for 10-fold CV.
cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model$lambda.min

plot(cv_model) 

best_model <- glmnet(x, y, nfolds = 10, alpha = 1, lambda = best_lambda)
coef(best_model)

# Define gridded observation matrix (i.e., x-values for un-monitored locations).
# Predictors are in columns 3-94.
new <- subset(df_grid, select = c(3:94))

# Remove columns/predictor variables from grid with too many NA values, as determined by LASSO.
# See lines 31-33.
new <- subset(new, select = -c(elevation_50, pop_count_50, pop_count_100, pop_count_250,
                               pop_count_500, pop_density_50, pop_density_100, pop_density_250,
                               pop_density_500))
new <- data.matrix(new)

# Estimate metal concentrations for un-monitored locations.
y_grid <- predict(best_model, s = best_lambda, newx = new)
y_predict <- predict(best_model, s = best_lambda, newx = x)

# Evaluate model performance for log-transformed data via RMSE and MAE.
actual_original <- exp(y)
predicted_original <- exp(y_predict)
rmse_original <- sqrt(mean((actual_original - predicted_original)^2))
mae <- mean(abs(predicted_original - actual_original))
