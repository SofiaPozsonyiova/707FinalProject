################################ 
#    Creating Cluster Data     #
################################ 

library(dplyr)

# Physical Cluster  
load("/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Cluster\ Data/Individual\ Clusters/physical_clusters.RData")
physical <- physical_clusters %>% mutate(Clus1_physical = `Clus 1`,
                                 Clus2_physical = `Clus 2`)%>% dplyr::select(-c(`Clus 1`,`Clus 2`))

# Psychological Cluster 
load("/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Cluster\ Data/Individual\ Clusters/psychological_clusters.RData")
psychological <- psychological_clusters %>% mutate(Clus1_psychological = `Clus 1`,
                                         Clus2_psychological = `Clus 2`)%>% dplyr::select(-c(`Clus 1`,`Clus 2`))

# Social Cluster 
load("/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Cluster\ Data/Individual\ Clusters/social_clusters.RData")
social <- social_clusters %>% mutate(Clus1_social = `Clus 1`,
                                                   Clus2_social = `Clus 2`)%>% dplyr::select(-c(`Clus 1`,`Clus 2`))

# Demo Cluster 
load("/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Cluster\ Data/Individual\ Clusters/demo_clusters.RData")
demo <- demo_clusters %>% mutate(Clus1_demo = `Clus 1`,
                                     Clus2_demo = `Clus 2`)%>% dplyr::select(-c(`Clus 1`,`Clus 2`))

# School Cluster 
load("/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Cluster\ Data/Individual\ Clusters/school_clusters.RData")
school <- school_clusters %>% mutate(Clus1_school = `Clus 1`,
                                 Clus2_school = `Clus 2`)%>% dplyr::select(-c(`Clus 1`,`Clus 2`))

##########################
#    Merging Cluster     #
##########################

dat1 <- merge(physical,social, by = "subject")
dat2 <- merge(dat1,psychological, by = "subject")
dat3 <- merge(dat2,demo, by = "subject")
ClusterDat <- merge(dat3,school, by = "subject")


##################################
#   Merging with Original Data   #
##################################

load("/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/SubsetDat.RData")
ClusterSubsetData <- cbind(na_removed_dat, ClusterDat)

save(ClusterDat,file = "/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Cluster\ Data/Individual\ Clusters/ClusterData.RData")

##################################
#        Splitting Data          #
##################################

## Splitting Data 
set.seed(707)
sample <- sample(c(TRUE, FALSE), nrow(ClusterSubsetData), replace=TRUE, prob=c(0.8,0.2))

### Validation 
Clustervalidation_test <- ClusterSubsetData[!sample, ]

### Training 
ClusterModelDev  <- ClusterSubsetData[sample, ]
sample2 <- sample(c(TRUE, FALSE), nrow(ClusterModelDev), replace=TRUE, prob=c(0.70,0.30))
ClusterTrain <- ClusterModelDev[sample2, ]
ClusterTest <- ClusterModelDev[!sample2, ]


##########################
#    Save Data           #
##########################
save(ClusterDat,file = "/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Cluster\ Data/ClusterData.RData")
save(Clustervalidation_test,file = "/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Cluster\ Data/Clustervalidation_test.RData")
save(ClusterModelDev,file = "/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Cluster\ Data/ClusterModelDev.RData")
save(ClusterTrain,file = "/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Cluster\ Data/ClusterTrain.RData")
save(ClusterTest,file = "/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/Cluster\ Data/ClusterTest.RData")
