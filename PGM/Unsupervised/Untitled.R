# Load libraries
library(tidyverse)
library(MASS)

# Load data
load("/Users/maireaddillon/Documents/Duke/BIOS-707/Project/Private707/data/ModelDev_Train.RData")

# Make dataset with independent variables
X_Train <- Train %>% subset(select = -c("SelfPerceivedHealth"))

# Make vector with dependent variable

# Standardize data
Train_Standardized <- scale(Train[-c("SelfPerceivedHealth")], center=TRUE, scale=TRUE)

Train$SelfPerceivedHealth
