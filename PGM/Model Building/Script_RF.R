#!/bin/bash
#SBATCH --job-name=RandomForest
#SBATCH --partition=common
#SBATCH --nodes=1  # num. nodes
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1  # num. cores
#SBATCH --output=RandomForest.out  # stdout
#SBATCH --error=RandomForest .e  # stderr
#SBATCH --mem-per-cpu=1gb
#SBATCH --time=0-04:00 # time limit (D-HH:MM)


cd ~/demo


########################################################################
#            Building Random Forest on Entire Data                     # 
########################################################################

# Reading in data 
load("/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Cluster\ Data/ClusterTrain.RData")
load("/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Cluster\ Data/ClusterTest.RData")

# Initializing libraries 
library(ggplot2)
library(gridExtra)
library(dplyr)
library(caret)
library(rpart)         
library(rpart.plot)    
library(class)         
library(randomForest)  
library(infer)
names(ClusterTrain)
# Initializing Random Forest
forest_model <- train(
  SelfPerceivedHealth ~ .,
  data = ClusterTrain,
  method = "rf",
  tuneGrid = data.frame(mtry = c(2, 12, 64)),
  trControl = trainControl(method = "oob"),
  metric = "Accuracy",
  na.action = na.omit)

# Plot Forest Model 
plot(forest_model)

# Final model Forest 
forest_model$finalModel

# Predictions 
table(dat_RF$SelfPerceivedHealth, predict(forest_model, dat_RF))

# Spit out variables of importance
variable_importance <- data.frame(randomForest::importance(forest_model$finalModel)) %>%  mutate(predictor = rownames(.))

## Arrange predictors by importance (most to least)
variable_importance %>% 
  arrange(desc(MeanDecreaseGini)) 

########################################################################
#            Splitting Data into HS and Middle School                  # 
########################################################################

# Middle School 
train_middle <- dat_RF %>% dplyr::filter(P1 == 8) %>% select(-P1)

# Initializing Random Forest Middle 
forest_model_middle <- train(
  SelfPerceivedHealth ~ .,
  data = train_middle,
  method = "rf",
  tuneGrid = data.frame(mtry = c(2, 12, 64)),
  trControl = trainControl(method = "oob"),
  metric = "Accuracy",
  na.action = na.omit)

# Plot Forest Model 
plot(forest_model_middle)

# Final model Forest 
forest_model_middle$finalModel

# Predictions 
table(train_middle$SelfPerceivedHealth, predict(forest_model_middle, train_middle))

# Spit out variables of importance
variable_importance_middle <- data.frame(randomForest::importance(forest_model_middle$finalModel)) %>%  mutate(predictor = rownames(.))

## Arrange predictors by importance (most to least)
variable_importance_middle %>% 
  arrange(desc(MeanDecreaseGini)) 

# --------------------------------
# High School 
train_high <- dat_RF %>% filter(P1 != 8) %>% select(-P1)

# Initializing Random Forest High School 
forest_model_HS <- train(
  SelfPerceivedHealth ~ .,
  data = train_high,
  method = "rf",
  tuneGrid = data.frame(mtry = c(2, 12, 64)),
  trControl = trainControl(method = "oob"),
  metric = "Accuracy",
  na.action = na.omit)

# Plot Forest Model 
plot(forest_model_HS)

# Final model Forest 
forest_model_HS$finalModel

# Predictions 
table(train_high$SelfPerceivedHealth, predict(forest_model_HS, train_high))

# Spit out variables of importance
variable_importance_HS <- data.frame(randomForest::importance(forest_model_HS$finalModel)) %>%  mutate(predictor = rownames(.))

## Arrange predictors by importance (most to least)
variable_importance_HS %>% 
  arrange(desc(MeanDecreaseGini)) 

