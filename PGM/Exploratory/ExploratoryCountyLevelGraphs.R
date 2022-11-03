

# Loading Data
load("~/Desktop/Duke/BIOSTAT 707 - Stat Methods/Assignments/Final Project/untitled folder/CombinedSet.RData")
load("~/Desktop/Duke/BIOSTAT 707 - Stat Methods/Assignments/Final Project/untitled folder/CombinedSetGEE.RData")
load("~/Desktop/Duke/BIOSTAT 707 - Stat Methods/Assignments/Final Project/untitled folder/yesnosmoking.RData")

# Libraries 
library(dplyr)
library(tidyverse)
library(maps)
library(gridExtra)
library(ggplot2)

# Visualizing Population Spread 
# Sex 
sexplotF <- CombinedSet %>% 
  ggplot() + 
  geom_sf(aes(fill = Female)) + 
  ggtitle("Proportion of Female Students") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_gradient2(low = "#CC79A7",  high = "#D55E00", midpoint = 0.5)

# ADD TEXT SIZE 
sexplotM <- CombinedSet %>% 
  ggplot() + 
  geom_sf(aes(fill = Male)) + 
  ggtitle("Proportion of Male Students") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_gradient2(low = "#CC79A7",  high = "#D55E00", midpoint = 0.5)

grid.arrange(sexplotF, sexplotM, ncol = 2)

# Grade 
middle <- CombinedSet %>% 
  ggplot() + 
  geom_sf(aes(fill = grade.MiddleSchool)) + 
  ggtitle("Proportion of Students Grades 6 - 8") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5)

lowerhs <- CombinedSet %>% 
  ggplot() + 
  geom_sf(aes(fill = grade.LowerHighSchool)) + 
  ggtitle("Proportion of Students Grades 9 & 10") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5)

upperhs <- CombinedSet %>% 
  ggplot() + 
  geom_sf(aes(fill = grade.UpperHighSchool)) + 
  ggtitle("Proportion of Students Grades 11 & 12") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5)

middle
lowerhs
upperhs