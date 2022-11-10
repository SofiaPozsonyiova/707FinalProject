library(tidyverse)
library(ggplot2)
library(lubridate)
library(patchwork)
library(gridExtra)
library(psych)
library(corrplot)
library(ggfortify)
library(factoextra)
library(readr)
library(gmodels)
library(DataExplorer)

setwd("/Users/sdm98/Documents/GitHub/707FinalProject")
load("/Users/sdm98/Documents/GitHub/Private707/data/ModelDev_Train.RData")

#change back to numeric
col_names <- names(Train)
Train[,col_names] <- lapply(Train[,col_names] , as.numeric)

#Standardize
#remove outcome (SelfPerceivedHealth) and standardize all other variables
Train_standardized <- Train %>% subset(select = -c(SelfPerceivedHealth, P28)) %>% mutate_all(~(scale(.) %>% as.vector))
summary(Train_standardized)

#Perform hierarchical clustering on entire data set - not informative of self-perceived health (no surprise)
#dis <- dist(Train_standardized, method = "euclidean")
#clus1 <- hclust(dis, method = "complete")

#plot(clus1)
#table(cutree(clus1, k=2), Train$SelfPerceivedHealth)

#Perform clustering on specific groups of variables and label clusters by self-perceived health

#Physical Health Related
phys_health <- Train_standardized %>% 
  subset(select = c(#miss school for sickness
                    P14ab1, 
                    
                    #doctor's appt
                    #P14ab2, 
                    
                    #not enough sleep
                    P14ab5, 
                    
                    #sports team/physical activity
                    P26a, P26f, 
                    
                    #dentist appt
                    #P30, 
                    
                    #physical disability/long term health problem
                    P33, 
                    
                    #physical activity
                    P37, 
                    
                    #skipping lunch
                    P39a,
                    
                    #fast food, soda, sports drinks, energy drinks,
                    #P40d, P41b, P41c, P41d, 
                    
                    #sweetened coffee/tea, water
                    #P41e, P41g,
                    
                    #asthma, allergy w/ epi pen, hours of sleep/nigh
                    P42c, P42d, P45, 
                    
                    #vape, alcohol, alcohol quantity, prescription drugs
                    P71d, P74, P76, P81,
                    
                    FiveFV, Binge, Diab_Prediab,
                    DrugFreq, NicFreq, DentalIssues))


phys_health_clus <- kmeans(phys_health, centers = 2, nstart = 10)

#Compare with fruit/vegetable consumption
table(phys_health_clus$cluster, Train$FiveFV)

#Compare with Diab/Prediab
table(phys_health_clus$cluster, Train$Diab_Prediab)

#Compare with long term disability
table(phys_health_clus$cluster, Train$P33)

#No clear cluster relationship for good/bad physical health


#compare with self-perceived health 1 = good 2 = bad
table(phys_health_clus2$cluster, Train$SelfPerceivedHealth)
fviz_cluster(phys_health_clus2, phys_health, geom = 'point', main = 'Clusters with physical health')

phys_crosstab <- CrossTable(phys_health_clus$cluster, Train$SelfPerceivedHealth, prop.chisq = FALSE)
phys_prop <- 100*phys_crosstab$prop.col


      phys_binary <- ggplot(phys_prop, aes(x = y, y = Freq, fill = x))+
        geom_bar(stat = "identity")+
        coord_flip()+
        labs(x = "Self-Perceived Health", y = "Percentage",fill = "Cluster")+
        ggtitle(label = "Proportion of Each Physical Health Cluster in Self-Perceived Health Groups")+ 
        scale_fill_brewer(labels = c('Good Health','Bad Health'),palette = 'Set2', limits = c(2, 1))
      
      phys_crosstab_5 <- CrossTable(phys_health_clus$cluster, Train$P28, prop.chisq = FALSE)
      phys_prop_5 <- 100*phys_crosstab_5$prop.col
      
      
      phys_5 <- ggplot(phys_prop_5, aes(x = y, y = Freq, fill = x))+
        geom_bar(stat = "identity")+
        coord_flip()+
        labs(x = "Self-Perceived Health", y = "Percentage",fill = "Cluster")+
        ggtitle(label = "Proportion of Each Physical Health Cluster in Self-Perceived Health Groups")+ 
        scale_fill_brewer(labels = c('Good Health','Bad Health'),palette = 'Set2', limits = c(2, 1))
      
      
      grid.arrange(phys_binary, phys_5, nrow = 1, ncol = 2)



#Psychological Health Related
psych_health <- Train_standardized %>%
  subset(select = c(P14ab4, P14ab11, P27f, P34, 
                    
                    #not sure if 49 should be included
                    #P49a, P49c, P49e,
                    #P49f, P49g, P49h, P49i, P49j, P49k, P49l, 
                    #P49n, 
                    
                    P50a, P50b, P50c, P50d, 
                    
                    #not sure if 70 should be included
                    #P70d, 
                    
                    P86, AlcDrug_Residence, ImprisGaurd, 
                    MentalHealthSelf, AlcDrugSelf, Suicide))

psych_health_clus <- kmeans(psych_health, centers = 2, nstart = 10)
#Compare with suicide attempt/consider 
table(psych_health_clus$cluster, Train$Suicide)

#Compare with feeling nervous/anxious/on edge 1 = not at all, 4 = nearly every day
table(psych_health_clus$cluster, Train$P50c)
  #this suggests cluster 1 = good psychological health, cluster 2 = bad psychological health

#compare cluster membership to self-perceived health
table(psych_health_clus$cluster, Train$SelfPerceivedHealth)
  #2 = good, 1 = bad?

#create visualization
#add better labels ######################### remove obs numbers, want to see boundary
fviz_cluster(psych_health_clus, psych_health, geom = 'point', main = 'Clusters with psychological health')

      psych_crosstab <- CrossTable(psych_health_clus$cluster, Train$SelfPerceivedHealth, prop.chisq = FALSE)
      psych_prop <- 100*psych_crosstab$prop.col
      
      
      psych_binary <- ggplot(psych_prop, aes(x = y, y = Freq, fill = x))+
        geom_bar(stat = "identity")+
        coord_flip()+
        labs(x = "Self-Perceived Health", y = "Percentage",fill = "Cluster")+
        ggtitle(label = "Proportion of Each Psychological Health Cluster in Self-Perceived Health Groups")+ 
        scale_fill_brewer(labels = c('Good Health','Bad Health'),palette = 'Set2', limits = c(2, 1))
      
      psych_crosstab_5 <- CrossTable(psych_health_clus$cluster, Train$P28, prop.chisq = FALSE)
      psych_prop_5 <- 100*psych_crosstab_5$prop.col
      
      
      psych_5 <- ggplot(psych_prop_5, aes(x = y, y = Freq, fill = x))+
        geom_bar(stat = "identity")+
        coord_flip()+
        labs(x = "Self-Perceived Health", y = "Percentage",fill = "Cluster")+
        ggtitle(label = "Proportion of Each Psychological Health Cluster in Self-Perceived Health Groups")+ 
        scale_fill_brewer(labels = c('Good Health','Bad Health'),palette = 'Set2', limits = c(2, 1))
      
      grid.arrange(psych_binary, psych_5, nrow = 1, ncol = 2)

#Social Support Related
social <- Train_standardized %>%
  subset(select = c(P14ab6, P14ab7, P14ab9, P18b, 
                    P18c, P18d, 
                    
                    #bullying
                    P20a, P20b, P20c, P20f, P20g, P20h,
                    P21, P22a, P22e, P23c, P23e, 
                    
                    #school/community programs
                    #P25, P26b, P26d, P26e, P26g, P26h,
                    
                    #leadership, trusting relationships, 
                    #decision making, learning useful skills
                    #P27b, P27c, P27e, P27g, 
                    
                    #feel cared about
                    P47a, P47b, P47c, P47d, P47e, P49d,
                    
                    #not sure about these
                    #P49m, P49o, P49p, P49q,
                    
                    #unhealthy/abusive home life
                    P61, P62, P63, P64, P65, P66,
                    
                    #run away from home, hit others
                    P70a, P70c,
                    
                    #romantic relationship abuse
                    AbuseRelationship
                
                   ))

social_clus <- kmeans(social, centers = 2, nstart = 10)
table(social_clus$cluster, Train$SelfPerceivedHealth)
#compare cluster to 'I feel safe at home' 1 = strongly agree, 4 = strongly disagree
table(social_clus$cluster, Train$P18d)
#compare cluster to 'How much do you feel your parents care about you' 1 = not at all, 5 = very much
table(social_clus$cluster, Train$P47a)

#this may suggest cluster 2 = bad social support and cluster 1 = good social support

fviz_cluster(social_clus, social, geom = 'point', main = 'Clusters with social support')

      
      social_crosstab <- CrossTable(social_clus$cluster, Train$SelfPerceivedHealth, prop.chisq = FALSE)
      social_prop <- 100*social_crosstab$prop.col
      
      
      soc_binary <- ggplot(social_prop, aes(x = y, y = Freq, fill = x))+
        geom_bar(stat = "identity")+
        coord_flip()+
        labs(x = "Self-Perceived Health", y = "Percentage",fill = "Cluster")+
        ggtitle(label = "Proportion of Each Social Support Cluster in Self-Perceived Health Groups")+ 
        scale_fill_brewer(labels = c('Good Health','Bad Health'),palette = 'Set2')
      
      social_crosstab_5 <- CrossTable(social_clus$cluster, Train$P28, prop.chisq = FALSE)
      social_prop_5 <- 100*social_crosstab_5$prop.col
      
      
      soc_5 <- ggplot(social_prop_5, aes(x = y, y = Freq, fill = x))+
        geom_bar(stat = "identity")+
        coord_flip()+
        labs(x = "Self-Perceived Health", y = "Percentage",fill = "Cluster")+
        ggtitle(label = "Proportion of Each Social Support Cluster in Self-Perceived Health Groups") + 
        scale_fill_brewer(labels = c('Good Health','Bad Health'), palette = 'Set2')
      
      
      grid.arrange(soc_binary, soc_5, nrow = 1, ncol = 2)

#run all together

subset <- as.data.frame(c(social, psych_health, phys_health))
subset_clus <- kmeans(subset, centers = 2, nstart = 10)
table(subset_clus$cluster, Train$SelfPerceivedHealth)
fviz_cluster(subset_clus, psych_health, geom = 'point', main = 'Clusters with combination of categories')

      subset_crosstab <- CrossTable(subset_clus$cluster, Train$SelfPerceivedHealth, prop.chisq = FALSE)
      propcol <- 100*subset_crosstab$prop.col
      
      
      subset_binary <- ggplot(propcol, aes(x = y, y = Freq, fill = x))+
        geom_bar(stat = "identity")+
        coord_flip()+
        labs(x = "Self-Perceived Health", y = "Percentage",fill = "Cluster")+
        ggtitle(label = "Proportion of Each Subset Cluster in Self-Perceived Health Groups")+ 
        scale_fill_brewer(labels = c('Good Health','Bad Health'),palette = 'Set2')
      
      
      subset_crosstab_5 <- CrossTable(subset_clus$cluster, Train$P28, prop.chisq = FALSE)
      subset_prop_5 <- 100*subset_crosstab_5$prop.col
      
      
      subset_5 <- ggplot(subset_prop_5, aes(x = y, y = Freq, fill = x))+
        geom_bar(stat = "identity")+
        coord_flip()+
        labs(x = "Self-Perceived Health", y = "Percentage",fill = "Cluster")+
        ggtitle(label = "Proportion of Each Subset Cluster in Self-Perceived Health Groups")+ 
        scale_fill_brewer(labels = c('Good Health','Bad Health'),palette = 'Set2')
      
      
      grid.arrange(subset_binary, subset_5, nrow = 1, ncol = 2)



#Find misclassified subjects

miss <- subset[Train$SelfPerceivedHealth != subset_clus$cluster,]
plot_bar(miss)
same <- subset[Train$SelfPerceivedHealth == subset_clus$cluster,]
plot_bar(same)



# pca
pco <- princomp(subset)
summary(pco)

sd <- pco$sdev

screeplot(pco, type = c("lines"), main = "Scree Plot")
