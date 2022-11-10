# Load libraries
library(tidyverse)
library(MASS)
library(ROCR)

# Load data
load("/Users/maireaddillon/Documents/Duke/BIOS-707/Project/Private707/data/ModelDev_Train.RData")

# Make dataset with independent variables
X_Train <- Train %>% subset(select = -c(SelfPerceivedHealth))

# Make data numeric
X_Train <- sapply(X_Train, as.numeric)

# Make dataframe
X_Train <- as.data.frame(X_Train)

# Make vector with dependent variable
Y_Train <- Train %>% subset(select = SelfPerceivedHealth)

# Standardize data
X_Train_Standardized <- scale(X_Train, center=TRUE, scale=TRUE)

# Remerge data
Train_Standardized <- cbind(X_Train_Standardized, Y_Train)

# Train linear discriminant analysis model
lda_model <- lda(SelfPerceivedHealth~., data=Train_Standardized)
lda_model

# Predict on the test set
predicted <- predict(lda_model)
predicted$class

mean(predicted$class==Train_Standardized$SelfPerceivedHealth)

# Run prediction function
pred <- prediction(as.numeric(predicted$class), Train_Standardized$SelfPerceivedHealth)

# Run performance function
perf <- performance(pred, "tpr", "fpr")

# Plot ROC curve
plot(perf)

# Calculate area under the curve
auc.tmp <- performance(pred, "auc")
auc <- as.numeric(auc.tmp@y.values)
print(auc)

# Run logistic regression model
glm(SelfPerceivedHealth ~ ., data=Train, family="binomial")
