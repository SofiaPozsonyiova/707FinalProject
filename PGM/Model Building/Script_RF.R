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
# Cluster
# load("/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Cluster\ Data/ClusterTrain.RData")
# load("/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Cluster\ Data/ClusterTest.RData")

load("/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/ModelDev_Train.RData")
load("/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/ModelDev_Test.RData")

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


Train_sub <- Train %>% select(-P28) %>% dplyr::select(c(P49f,P49l,P26a,P50b,P49a,P37,P49h,P45,P49i,P49e,P34,P47e,P50c,P41g,P47a,P50d,P47b,P49p,P49d,P50a,SelfPerceivedHealth))
Train_sub5 <- Train %>% select(-P28) %>% dplyr::select(c(P37,P49f,P45,P26a,P49l,SelfPerceivedHealth))
Train_sub2 <- Train %>% select(-P28) %>% dplyr::select(c(P49f,P49l,SelfPerceivedHealth))
Train_sub1 <- Train %>% select(-P28) %>% dplyr::select(c(P49f,SelfPerceivedHealth))

# Initializing Random Forest
forest_model <- train(
  SelfPerceivedHealth ~ ., 
  data = Train_sub1,
  method = "rf",
  tuneGrid = data.frame(mtry = c(1,2)),
  trControl = trainControl(method = "oob"),
  metric = "Accuracy",
  na.action = na.omit)

# Plot Forest Model 
plot(forest_model)

# Final model Forest 
forest_model$finalModel

# Spit out variables of importance
variable_importance_sub1 <- data.frame(randomForest::importance(forest_model$finalModel)) %>%  mutate(predictor = rownames(.))
# save(variable_importance_sub20,file = "/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Cluster\ Data/Var_Imp20.RData")

## Arrange predictors by importance (most to least)
variable_importance_sub1 %>% 
  arrange(desc(MeanDecreaseGini)) 

## Get variable importance, and turn into a data frame
var_imp <- varImp(forest_model, scale=FALSE)$importance
var_imp <- data.frame(variables=row.names(var_imp), importance=var_imp$Overall)


## Create a plot of variable importance
var_imp %>%
  
  ## Sort the data by importance
  arrange(importance) %>%
  
  ## Create a ggplot object for aesthetic
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  
  ## Plot the bar graph
  geom_bar(stat='identity', fill = "cornflowerblue") + 
  
  ## Flip the graph to make a horizontal bar plot
  coord_flip() + 
  
  ## Add x-axis label
  xlab('Variables') +
  
  ## Add a title
  labs(title='Random forest Variable Importance') + ylab("Importance")
  
  ## Some layout for the plot
  theme_minimal() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20),)

  
  
  
Test_sub <- Test %>% dplyr::select(c(P49f,P49l,P26a,P50b,P49a,P37,P49h,P45,P49i,P49e,P34,P47e,P50c,P41g,P47a,P50d,P47b,P49p,P49d,P50a,SelfPerceivedHealth))
Test_sub5 <- Test %>% dplyr::select(c(P37,P49f,P45,P26a,P49l,SelfPerceivedHealth))
Test_sub2 <- Test %>% dplyr::select(c(P49f,P49l,SelfPerceivedHealth))
Test_sub1 <- Test %>% dplyr::select(c(P49f,SelfPerceivedHealth))


## Generate predictions
y_hats <- predict(
  
  ## Random forest object
  object=forest_model, 
  
  ## Data to use for predictions; remove the Species
  newdata= Test_sub1 %>% select(-SelfPerceivedHealth))

## Print the accuracy
accuracy <- mean(y_hats == Test_sub1$SelfPerceivedHealth)*100
cat('Accuracy on testing data: ', round(accuracy, 2), '%',  sep='')


# Including all variables: 75.09%
# Including 20 variables: 73.92%
# Including 5 variables: 72.14%
# Including 2 variables: 71.1%
# Including 1 variable: 70.03%








# Define forest_plot
forest_plot <- function(x1, x2, y, lab_1, lab_2){
  y <- as.factor(y)
  model <- randomForest(y ~ x1 + x2, ntree = 500)
  x1s <- seq(min(x1), max(x1), len = 100)
  x2s <- seq(min(x2), max(x2), len = 100) 
  testdata <- expand.grid(x1s,x2s) %>% 
    mutate(class = predict(model, newdata = data.frame(x1 = Var1, x2 = Var2), type = "class"))
  ggplot(testdata, aes(x = Var1, y = Var2, color = class)) + 
    geom_point() + 
    labs(x = paste(lab_1), y = paste(lab_2), title = "forest classification boundaries") + 
    theme(legend.position = "bottom")
}

forest_plot(land$Mean_G, land$NDVI, land$class, lab_1 = "Mean_G", lab_2 = "NDVI")

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

