## Create a training set and a 20% testing set of our energy efficiency data set

# the data set in xlsx format. 

#Let's first load the required library 

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(randomForest)
library(GGally)
library(Metrics)
library(readxl)

## url of our energy efficiency public dataset- https://archive.ics.uci.edu/ml/machine-learning-databases/00242/ENB2012_data.xlsx

#Download our public dataset on energy parameters of buildings 

tmp_energy <- tempfile()
download.file( "https://archive.ics.uci.edu/ml/machine-learning-databases/00242/ENB2012_data.xlsx", tmp_energy,
               quiet = TRUE, mode = "wb", method = "auto", cacheOK = TRUE, 
               options(timeout = max(1000, getOption("timeout"))) )

energy_efficiency <- read_excel(tmp_energy)

#energy_test will be used as a final validation set. For testing our models, we will only use energy_training dataset

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y =energy_efficiency$Y1, times = 1, p = 0.2, list = FALSE)
energy_training <- energy_efficiency[-test_index,]
energy_test <- energy_efficiency[test_index,]

## Data exploration 

# Structure of our energy buildings public dataset

str(energy_efficiency)

# Summary statistics of energy_training

summary(energy_training)

## Correlation between variables 
cor_matrix <- round(cor(energy_training), 2)


## As, it is a symmetrical matrix we can only show the half 
as.dist(cor_matrix)

#Use Gcally library for a visual representation of our variables correlation

ggcorr(energy_training)

# Behavior of variables by histogram visualization 

## small number of building have a high cooling load

par(mfrow = c(1,2))
hist(energy_training$Y1, col = 'yellow', xlab = 'Heating load (kWh/m^2)', main = 'Heating load of buildings')
hist(energy_training$Y2, col = 'cadetblue1', xlab = 'Cooling load (kWh/m^2)', main = 'Cooling load of buildings')

# Behavior and characteristics of predictors 

par(mfrow = c(2,4))

hist(energy_training$X1, main = "", xlab = 'Relative compactness')
hist(energy_training$X2, main = "", xlab = 'Surface Area(m^2)')
hist(energy_training$X3, main = "", xlab = 'Wall Area (m^2)')
hist(energy_training$X4, main = "", xlab = 'Roof Area (m^2)')
hist(energy_training$X5, main = "", xlab = 'Height (m)')
hist(energy_training$X6, main = "", xlab = 'Orientation')
hist(energy_training$X7, main = "", xlab = 'Glazing Area')
hist(energy_training$X8, main = "", xlab = 'Glazing variations')

#Heating load and cooling load forecasting are crucial for estimating energy consumption and 
#improvement of energy performance during the design phase of buildings

######################################################################################################
#Models testing #############


###### linear regression models with original data #####

# Fit the linear model of Y1 with all predictors

# Original data 

original_model_lm_1 <- train(Y1~X1+X2+X3+X4+X5+X6+X7+X8, data = energy_training, method= "lm")

summary(original_model_lm_1)

varImp(original_model_lm_1)

# Predicting the trained model on test data
original_lm_Y1_hat <- original_model_lm_1 %>% predict(energy_test)

#Calculate RMSE

rmse(original_lm_Y1_hat, energy_test$Y1)
mse(original_lm_Y1_hat, energy_test$Y1)

# Fit the linear model of Y1 with X4 predictor dropped down
# Original data 

original_model_lm_1 <- train(Y1~X1+X2+X3+X5+X6+X7+X8, data = energy_training, method= "lm")

summary(original_model_lm_1)

varImp(original_model_lm_1)

# Predicting the trained model on test data
original_lm_Y1_hat <- original_model_lm_1 %>% predict(energy_test)

#Calculate RMSE and MSE

RMSE_Y1_lm<- rmse(original_lm_Y1_hat, energy_test$Y1)
MSE_Y1_lm<- mse(original_lm_Y1_hat, energy_test$Y1)

# Fit the linear model of Y2 with X4 predictor dropped down
# Original data 

original_model_lm_2 <- train(Y2~X1+X2+X3+X5+X6+X7+X8, data = energy_training, method= "lm")

summary(original_model_lm_2)

varImp(original_model_lm_2)

# Predicting the trained model on test data
original_lm_Y2_hat <- original_model_lm_2 %>% predict(energy_test)

#Calculate RMSE and MSE

RMSE_Y2_lm<- rmse(original_lm_Y2_hat, energy_test$Y2)
MSE_Y2_lm<- mse(original_lm_Y2_hat, energy_test$Y2)

###### RandomForest models with original data #####
# Original data Random Forest for Y1

original_model_rf_1 <- train(Y1~X1+X2+X3+X4+X5+X6+X7+X8, data = energy_training, method= "rf")

original_model_rf_1

# Variables importance 
varImp(original_model_rf_1)


# Predicting the trained model on test data
original_rf_Y1_hat <- original_model_rf_1 %>% predict(energy_test)

#Calculate RMSE and MSE
RMSE_Y1_rf<-rmse(original_rf_Y1_hat, energy_test$Y1)
MSE_Y1_rf<- mse(original_rf_Y1_hat, energy_test$Y1)

# Original data Random Forest for Y2

original_model_rf_2 <- train(Y2~X1+X2+X3+X4+X5+X6+X7+X8, data = energy_training, method= "rf")

original_model_rf_2

# Variables importance 
varImp(original_model_rf_2)


# Predicting the trained model on test data
original_rf_Y2_hat <- original_model_rf_2 %>% predict(energy_test)

#Calculate RMSE and MSE
RMSE_Y2_rf<-rmse(original_rf_Y2_hat, energy_test$Y2)
MSE_Y2_rf<- mse(original_rf_Y2_hat, energy_test$Y2)

#Standardization of the data 
energy_efficiency_S <- as.data.frame(scale(energy_efficiency, center = TRUE, scale = TRUE))

#Split standardize data into train and test set
set.seed(1)
test_index_S <- createDataPartition(y =energy_efficiency_S$Y1, times = 1, p = 0.2, list = FALSE)
energy_training_S <- energy_efficiency_S[-test_index,]
energy_test_S <- energy_efficiency_S[test_index,]

#Standardized linear model for Y1 without X4
standard_model_lm_1 <- train(Y1~X1+X2+X3+X5+X6+X7+X8, data = energy_training_S, method= "lm")

summary(standard_model_lm_1)

varImp(standard_model_lm_1)

#Predicting with standardization model on test standardized set 

standard_lm_Y1_hat <- standard_model_lm_1 %>% predict(energy_test_S)

#Calculate RMSE and MSE
RMSE_Y1_lm_S<- rmse(standard_lm_Y1_hat, energy_test_S$Y1)
MSE_Y1_lm_S<- mse(standard_lm_Y1_hat, energy_test_S$Y1)

#Standardized linear model for Y2 without X4
standard_model_lm_2 <- train(Y2~X1+X2+X3+X5+X6+X7+X8, data = energy_training_S, method= "lm")

summary(standard_model_lm_2)

varImp(standard_model_lm_2)

#Predicting with standardization model on test standardized set 

standard_lm_Y2_hat <- standard_model_lm_2 %>% predict(energy_test_S)

#Calculate RMSE and MSE
RMSE_Y2_lm_S<- rmse(standard_lm_Y2_hat, energy_test_S$Y2)
MSE_Y2_lm_S<- mse(standard_lm_Y2_hat, energy_test_S$Y2)

#Standardized randomForest for Y1 

standard_model_rf_1 <- train(Y1~X1+X2+X3+X4+X5+X6+X7+X8, data = energy_training_S, method= "rf")

standard_model_rf_1

varImp(standard_model_rf_1)

#Predicting with standardization model on test standardized set 

standard_rf_Y1_hat <- standard_model_rf_1 %>% predict(energy_test_S)

#Calculate RMSE and MSE
RMSE_Y1_rf_S<- rmse(standard_rf_Y1_hat, energy_test_S$Y1)
MSE_Y1_rf_S<- mse(standard_rf_Y1_hat, energy_test_S$Y1)

#Standardized randomForest for Y2 

standard_model_rf_2 <- train(Y2~X1+X2+X3+X4+X5+X6+X7+X8, data = energy_training_S, method= "rf")

standard_model_rf_2

varImp(standard_model_rf_2)

#Predicting with standardization model on test standardized set 

standard_rf_Y2_hat <- standard_model_rf_2 %>% predict(energy_test_S)

#Calculate RMSE and MSE
RMSE_Y2_rf_S<- rmse(standard_rf_Y2_hat, energy_test_S$Y2)
MSE_Y2_rf_S<- mse(standard_rf_Y2_hat, energy_test_S$Y2)