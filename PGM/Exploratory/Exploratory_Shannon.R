
# Loading Data
load("C:/Users/sdm98/Documents/GitHub/Private707/SubsetDat.RData")

# Libraries 
library(dplyr)
library(tidyverse)
library(maps)
library(gridExtra)
library(ggplot2)
library(VIM)

# Visualizing Population Spread 
# Sex 
sexplot <- dat_sub %>% 
  ggplot(aes(x = as.factor(Biosex), fill = as.factor(Biosex))) + 
  geom_bar() + 
  ggtitle("Distribution of Sex") +
  scale_fill_brewer(palette="Dark2")

sexplot

# Grade 
grade <- dat_sub %>% 
  ggplot(aes(x = as.factor(P1), fill = as.factor(P1))) + 
  geom_bar() + 
  ggtitle("Distribution of Grade") +
  scale_fill_brewer(palette="Dark2")

grade 

# Outcome 
selfhealth <- dat_sub %>% 
  ggplot(aes(x = as.factor(P28), fill = as.factor(P28))) + 
  geom_bar() + 
  ggtitle("Distribution of Self-Perceived Health") +
  scale_fill_brewer(palette="Dark2")

selfhealth 


#delete variables and subjects with high missingness
keepcol <- colMeans(is.na(dat_sub)) <= 0.3
keeprow <- rowMeans(is.na(dat_sub)) <= 0.3

dat_sub2 <- dat_sub[,keepcol]
dat_sub2 <- dat_sub2[!is.na(dat_sub2$P28),]
dat_sub2 <- dat_sub2[keeprow,]

#plot missingness - can only do for a subset of variables for this to be informative
dat_sub3 <- dat_sub2 %>% select(P1, P2, Biosex, P5, P8, P11, P12,P13,P15,P18b, P18c, P18d)
aggr_plot <- aggr(dat_sub3, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(dat_sub3), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
aggr_plot
