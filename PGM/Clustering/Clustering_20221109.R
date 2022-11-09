library(tidyverse)


setwd("/Users/sdm98/Documents/GitHub/707FinalProject")
load("/Users/sdm98/Documents/GitHub/Private707/data/ModelDev_Train.RData")

#change back to numeric
col_names <- names(Train)
Train[,col_names] <- lapply(Train[,col_names] , as.numeric)

#Standardize
#remove outcome (SelfPerceivedHealth) and standardize all other variables
Train_standardized <- Train %>% subset(select = -c(SelfPerceivedHealth)) %>% mutate_all(~(scale(.) %>% as.vector))
summary(Train_standardized)

#Perform hierarchical clustering on entire data set - not informative of self-perceived health (no surprise)
dis <- dist(Train_standardized, method = "euclidean")
clus1 <- hclust(dis, method = "complete")

plot(clus1)
table(cutree(clus1, k=2), Train$SelfPerceivedHealth)

#Perform clustering on specific groups of variables and label clusters by self-perceived health

#Alcohol/Drug Related
