# Load libraries
library(tidyverse)
library(InformationValue)

# Load data
load("/Users/maireaddillon/Documents/Duke/BIOS-707/Project/Private707/data/ModelDev_Train.RData")
load("/Users/maireaddillon/Documents/Duke/BIOS-707/Project/Private707/data/ModelDev_Test.RData")

# Get rid of duplicate variable in dataset
Train1 <- Train %>% subset(select = -c(P28))
Test1 <- Test %>% subset(select = -c(P28))

# Run logistic regression model
logistic_reg <- glm(SelfPerceivedHealth ~ ., data=Train1, family="binomial")

# View model summary
summary(logistic_reg)

# Predict on test data
predicted <- predict(logistic_reg, Test1, type="response")

# Find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(Test1$SelfPerceivedHealth, predicted)[1]
optimal

# Run confusion matrix
confusionMatrix(Test1$SelfPerceivedHealth, predicted)

# Calculate sensitivity
sensitivity(Test1$SelfPerceivedHealth, predicted)

# Calculate specificity
specificity(Test1$SelfPerceivedHealth, predicted)

# Calculate total missclassification error rate
misClassError(Test1$SelfPerceivedHealth, predicted, threshold=optimal)

# Plot ROC Curve
plotROC(Test1$SelfPerceivedHealth, predicted)


