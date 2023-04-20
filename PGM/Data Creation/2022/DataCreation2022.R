##########################
# Data Processing (SP)   # 
##########################

# ----------------------- Reading in Data -----------------------

# Libraries 
library(dplyr)
library(DataExplorer)
library(ggplot2)

# Reading in Provided Data and Saving Out
# dat <- read.table("/Users/sofiapozsonyiova/Downloads/MSS22-3CountyNoRaceCodebook/MSS22-3CountyNoRace.dat", header = TRUE,sep = "\t")
# save(dat, file ="/Users/sofiapozsonyiova/Downloads/FullDataSet2022.RData")

# Loading Data as Rdat for speed and efficiency 
setwd("/Users/sofiapozsonyiova/Downloads/")
load("FullDataSet2022.RData")


#----------------------- Subsetting Data -----------------------

# Extracting variables of interest based off of domain knowledge 
# A39 = Self Perceived Health
dat_sub_named <- dat %>% dplyr::select(A39,A58F,A58A,A59B,A58L,A49) %>% mutate(SelfPerceivedHealth = as.factor(A39),P49f = A58F, P49a = A58A,P50b = A59B, P49l = A58L, P37 = A49) %>% select(-c(A39,A58F,A58A,A59B,A58L,A49))

# Visualizing Missing 
# plot_missing(dat_sub_named)

# Remove missingness:
na_removed_dat <- na.omit(dat_sub_named)

# Categorizing Health 
na_removed_dat <- na_removed_dat %>% mutate(SelfPerceivedHealth = ifelse(SelfPerceivedHealth == 1 |SelfPerceivedHealth == 2, 0,ifelse(SelfPerceivedHealth == 4 | SelfPerceivedHealth == 5,1, NA)))  %>% mutate(SelfPerceivedHealth = as.factor(SelfPerceivedHealth)) 

plot_bar(na_removed_dat$SelfPerceivedHealth)
na_removed_dat %>%
  group_by(SelfPerceivedHealth) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(freq))

na_removed_dat <- na_removed_dat[!is.na(na_removed_dat$SelfPerceivedHealth), ]
table(na_removed_dat$SelfPerceivedHealth,na_removed_dat$Biosex)

#----------------------- Splitting Data -----------------------
# Splitting into final testing and validation set
set.seed(707)
sample <- sample(c(TRUE, FALSE), nrow(na_removed_dat), replace=TRUE, prob=c(0.8,0.2))

# Final Test 
final_test <- na_removed_dat[!sample, ]
final_test <- final_test[!is.na(final_test$SelfPerceivedHealth), ]

# Training 
ModelDev  <- na_removed_dat[sample, ]

#----------------------- Balancing Data -----------------------
# 1 0                   31359 0.624
# 2 NA                  13506 0.269
# 3 1                    5412 0.108

grouped_data <- ModelDev %>%
  group_by(SelfPerceivedHealth) %>%
  mutate(size=n())

grouped_data2 <-grouped_data  %>%
  sample_n(size= 8572,replace = TRUE) %>%
  ungroup() 

Train_Model_Dev_Balanced2022 <- grouped_data2[!is.na(grouped_data2$SelfPerceivedHealth), ]
Train_Model_Dev_Balanced2022 <- Train_Model_Dev_Balanced2022 %>% select(-size)
Train_Model_Dev_Balanced2022 %>%
  group_by(SelfPerceivedHealth) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(freq))


# Logistic Regression 
library(tidyverse)
library(caret)
library(InformationValue)
library(pander)
library(lmtest)

load("/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Balanced/ModelDev_Train_BalancedUSE.RData")


# Set seed
set.seed(2023)
Train <- Train_Model_Dev_Balanced
Train_sub5 <- Train %>% select(-P28) %>% dplyr::select(c(P49f,P49a,P50b,P37,P49l,SelfPerceivedHealth))

# Create model with all variables as predictors
trControl <- trainControl(method = "cv",
                          number = 5)
log_reg_5 <- train(as.factor(SelfPerceivedHealth) ~ .,
                   method = "glm",
                   family = binomial(),
                   trControl = trControl,
                   metric = "Accuracy",
                   data = Train_sub5)

summary(log_reg_5) %>% pander()

# Get odds ratios for all coefficients
exp(coef(log_reg_5$finalModel))

# Get confidence intervals
exp(confint(log_reg_5$finalModel))

# Predict on train data
predicted_5 <- stats::predict(log_reg_5, na_removed_dat)
predicted_5

# Run confusion matrix
caret::confusionMatrix(predicted_5, as.factor(na_removed_dat$SelfPerceivedHealth))
