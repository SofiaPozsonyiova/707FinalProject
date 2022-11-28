##########################
# Data Processing (SP)   # 
##########################

# ----------------------- Reading in Data -----------------------

# Libraries 
library(dplyr)
library(DataExplorer)
library(ggplot2)

# Reading in Provided Data and Saving Out
# dat <- read.csv("/Users/sofiapozsonyiova/Desktop/PubR2019-race\ no\ county\ csv/PubR2019-race\ no\ county.csv")
# save(dat, file ="/Users/sofiapozsonyiova/Downloads/FullDataSet.RData")

# Loading Data as Rdat for speed and efficiency 
setwd("/Users/sofiapozsonyiova/Downloads/")
load("SubsetDat.RData")


#----------------------- Subsetting Data -----------------------

# Extracting variables of interest based off of domain knowledge 
dat_sub_named <- dat_sub %>% dplyr::select(P1,P2,Biosex,P5,P8,P11:P14a,P14ab1:P14ab14,P15:P17c,P18b:P18d,P20a:P20c,P20f:P22a,P22e,P23c,P23e,P25:P26h,P27b,P27c,P27e:P29e,P30:P39a,P40d,P41b:P41e,P41g:P42d,P45,P47a:P47e,P49d,P49a,P49c:P50d,P52a:P52c,P54a:P54b,P56a:P56c,P58a:P58c,P59:P66,P70a,P70c:P70d,P71a:P71d,P74,P76,P80c:P80g,P80i:P81,P86,FiveFV,Binge,Region) %>% mutate(SelfPerceivedHealth = as.factor(P28))

# Setting variables to be factors
col_names <- names(dat_sub_named)
dat_sub_named[,col_names] <- lapply(dat_sub_named[,col_names] , factor)

# QC:
# str(dat_sub_named)

#----------------------- Exploratory Analysis -----------------------

# Visualizing Missing 
plot_missing(dat_sub_named)

# Visualizing Each Distribution
plot_bar(dat_sub_named)

# Visualizing Outcome 
ggplot(data=na_removed_dat, aes(x=SelfPerceivedHealth)) +
  geom_bar(fill = "cornflowerblue") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)+ scale_x_discrete(labels=c('Excellent', 'Very good', 'Good', 'Fair','Poor'))+ xlab("Self-Perceived Health")+ylab("Count") + ggtitle("Counts of Self-Perceived Health")
# ggsave("/Users/sofiapozsonyiova/Documents/GitHub/707FinalProject/selfperceivedhealth.png")

#----------------------- Removing Missing  -----------------------

# Comparing outcome variable if all na removed
na_removed_dat <- na.omit(dat_sub_named)
full_dat <- dat_sub_named

# Comparing counts visually 
plot_bar(full_dat$SelfPerceivedHealth)
plot_bar(na_removed_dat$SelfPerceivedHealth)

# Checking Proportions within outcome variable numerically 
na_removed_dat %>%
  group_by(SelfPerceivedHealth) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(freq))
full_dat %>%
  group_by(SelfPerceivedHealth) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(freq))

# QC:
# Proportions: 
#    n     prop.full prop.red
# 1	44576	  0.262	      0.233	
# 2 61968	  0.364	      0.391	
# 3	42768	  0.251	      0.269	
# 4	12547	  0.074	      0.090		
# 5	2431  	0.014       0.018	


#----------------------- Combining and Redefining Variables  -----------------------

# SelfPerceivedHealth (P28)    
# 0 = Great Health 
# 1 = Poor or Fair Health

na_removed_dat <- na_removed_dat %>% mutate(SelfPerceivedHealth = ifelse(SelfPerceivedHealth == 1 |SelfPerceivedHealth == 2, 0,ifelse(SelfPerceivedHealth == 3 |SelfPerceivedHealth == 4 | SelfPerceivedHealth == 5,1,NA))) 

# QC:
# na_removed_dat %>%
#   group_by(SelfPerceivedHealth) %>%
#   summarise(cnt = n()) %>%
#   mutate(freq = round(cnt / sum(cnt), 3)) %>% 
#   arrange(desc(freq))


# Diabetes (P42a) or Pre-Diabetes (P42b)
# 0 = No
# 1 = Yes 

na_removed_dat <- na_removed_dat %>% mutate(Diab_Prediab = ifelse(P42a == 1 | P42b == 1,1,
                                                                  ifelse(P42a == 2 | P42b == 2,0,NA))) %>% dplyr::select(-c(P42a,P42b))

# QC:
# na_removed_dat %>%
#   group_by(Diab_Prediab) %>%
#   summarise(cnt = n()) %>%
#   mutate(freq = round(cnt / sum(cnt), 3)) %>% 
#   arrange(desc(freq))

# Abusive Relationship 
# P54a:Have you been in a casual or serious relationship where your partner ever physically hurt you on purpose?
# P54b: Have you been in a casual or serious relationship where your partner ever verbally hurt or controlled you? 
# 0 = No
# 1 = Yes 

na_removed_dat <- na_removed_dat %>% mutate(AbuseRelationship = ifelse(P54a == 1 | P54b == 1,1,
                                                                       ifelse(P54a == 2 | P54b == 2,0,NA))) %>% dplyr::select(-c(P54a,P54b))

# QC:
# na_removed_dat %>%
#   group_by(AbuseRelationship) %>%
#   summarise(cnt = n()) %>%
#   mutate(freq = round(cnt / sum(cnt), 3)) %>% 
#   arrange(desc(freq))

# Foster Care 
# P56a, Have you ever been in foster care? No
# P56b,Have you ever been in foster care? Yes, during the last year
# P56c,Have you ever been in foster care? Yes, more than a year ago
# 0 = No
# 1 = Yes 

na_removed_dat <- na_removed_dat %>% mutate(FosterCare = ifelse(P56a == 1 & (P56b == 0 | P56c == 0),0,
                                                                ifelse(P56a == 0 & (P56b == 1 | P56c == 1),1,NA))) %>% dplyr::select(-c(P56a,P56b,P56c))

# QC:
# na_removed_dat %>%
#   group_by(FosterCare) %>%
#   summarise(cnt = n()) %>%
#   mutate(freq = round(cnt / sum(cnt), 3)) %>% 
#   arrange(desc(freq))

# Drug Frequency 
# P80c	263	During the last 12 months, on how many occasions (if any) have you used MDMA (E, X, ecstasy, Molly), GHB (G, Liquid E, Liquid X, roofies) or Ketamine (Special K)?
# P80d	264	During the last 12 months, on how many occasions (if any) have you used crack, coke or cocaine in any form?
# P80e	265	During the last 12 months, on how many occasions (if any) have you used heroin (smack, junk, China White)?
# P80f	266	During the last 12 months, on how many occasions (if any) have you used methamphetamine (meth, glass, crank, crystal meth, ice)?
# P80g	267	During the last 12 months, on how many occasions (if any) have you used over-the-counter drugs such as cough syrup, cold medicine or diet pills that you took only to get high?
# P80i	269	During the last 12 months, on how many occasions (if any) have you used any other synthetic drugs such as bath salts (Ivory Wave, White Lightning) that you took only to get high?
# 1	= 0 days
# 2	= 1 to 2 days
# 3	= 3 to 5 days
# 4	= 6 to 9 days
# 5	= 10 to 19 days
# 6	= 20 or more
# 
# In the last 30 days have you utilized any hard drugs? 
# 0 = No
# 1 = Yes

na_removed_dat <- na_removed_dat %>% mutate(DrugFreq = ifelse(P80c == 1 | P80d == 1 | P80e == 1 | P80f == 1 |P80g == 1 |P80i == 1,0,
                                                              ifelse(P80c != 1 | P80d != 1 | P80e != 1 | P80f != 1 |P80g != 1 |P80i != 1,1,NA))) %>% dplyr::select(-c(P80c,P80d,P80e,P80f,P80g,P80i))

# QC:
# check <- dat_sub_named%>% na.omit() %>% dplyr::select(P80c,P80d,P80e,P80f,P80g,P80i) 
# check %>% group_by(P80c) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq))

# na_removed_dat %>%
#   group_by(DrugFreq) %>%
#   summarise(cnt = n()) %>%
#   mutate(freq = round(cnt / sum(cnt), 3)) %>%
#   arrange(desc(freq))

# AlcDrug_Residence
# P59		Do you live with anyone who drinks too much alcohol?
# P60		Do you live with anyone who uses illegal drugs or abuses prescription drugs?
# 0 = No
# 1 = Yes

na_removed_dat <- na_removed_dat %>% mutate(AlcDrug_Residence = ifelse(P59 == 1 | P60 == 1,1,
                                                                       ifelse(P59 == 2 | P60 == 2,0,NA))) %>% dplyr::select(-c(P59,P60))

# QC:
# check <- dat_sub_named %>% na.omit() %>% dplyr::select(P59,P60)
# check %>% group_by(P59) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq)) 
# check %>% group_by(P60) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq)) 
# 
# na_removed_dat %>%
#   group_by(AlcDrug_Residence) %>%
#   summarise(cnt = n()) %>%
#   mutate(freq = round(cnt / sum(cnt), 3)) %>%
#   arrange(desc(freq))


# Nicotine Freq
# P71a	During the last 30 days, on how many days did you smoke a cigarette?
# P71b 	During the last 30 days, on how many days did you smoke cigars, cigarillos or little cigars?
# P71c	During the last 30 days, on how many days did you use chewing tobacco, snuff or dip?
# 1	= 0 days
# 2	= 1 to 2 days
# 3	= 3 to 9 days
# 4	= 10 to 19 days
# 5	= 20 to 29 days
# 6	= All 30 days
# 
# In the last 30 days have you used any nicotine (not including vape)? 
# 0 = No
# 1 = Yes

na_removed_dat <- na_removed_dat %>% mutate(NicFreq = ifelse(P71a == 1| P71b == 1| P71c == 1,0,
                                                             ifelse(P71a != 1| P71b != 1| P71c != 1,1,NA))) %>% dplyr::select(-c(P71a,P71b,P71c))

# QC:
# check <- dat_sub_named %>% na.omit() %>% dplyr::select(P71a,P71b,P71c)
# check %>% group_by(P71a) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq)) 
# check %>% group_by(P71b) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq)) 
# check %>% group_by(P71c) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq)) 

# na_removed_dat %>%
#   group_by(NicFreq) %>%
#   summarise(cnt = n()) %>%
#   mutate(freq = round(cnt / sum(cnt), 3)) %>%
#   arrange(desc(freq))


# Imprisoned Parent/Guardian 
# P58a	Have any of your parents or guardians ever been in jail or prison? None of my parents or guardians has ever been in jail or prison
# P58b	Have any of your parents or guardians ever been in jail or prison? Yes, I have a parent or guardian in jail or prison right now
# P58c	Have any of your parents or guardians ever been in jail or prison? Yes, I have had a parent or guardian in jail or prison in the past
# 0 = No
# 1 = Yes


na_removed_dat <- na_removed_dat %>% mutate(ImprisGaurd = ifelse(P58a == 1 & (P58b == 0 | P58c == 0),0,
                                                                 ifelse(P58a == 0 & (P58b == 1 | P58c == 1),1,NA))) %>% dplyr::select(-c(P58a,P58b,P58c))

# QC:
# check <- dat_sub_named %>% na.omit() %>% dplyr::select(P58a,P58b,P58c)
# check %>% group_by(P58a) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq))
# check %>% group_by(P58b) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq))
# check %>% group_by(P58c) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq))
# 
# na_removed_dat %>%
#   group_by(ImprisGaurd) %>%
#   summarise(cnt = n()) %>%
#   mutate(freq = round(cnt / sum(cnt), 3)) %>%
#   arrange(desc(freq))


# Dental Issues: 
# " Do you have any dental problems?"
# P29a	Have you had any of the following dental problems during the past 12 months? Toothaches or pain
# P29b	Have you had any of the following dental problems during the past 12 months? Decayed teeth or cavities
# P29c	Have you had any of the following dental problems during the past 12 months? Swollen, painful or bleeding gums
# P29d	Have you had any of the following dental problems during the past 12 months? Could not eat certain foods because of a dental problem
# 
# 0 = No  
# 1 = Yes 
na_removed_dat <- na_removed_dat %>% mutate(DentalIssues = ifelse(P29a == 0 | P29b == 0|P29c == 0| P29d == 0,0,
                                                                  ifelse(P29a == 1| P29b == 1|P29c == 1| P29d == 1,1,NA))) %>% dplyr::select(-c(P29a:P29e))

# QC:
# check <- dat_sub_named %>% na.omit() %>% dplyr::select(P29a:P29f)
# check %>% group_by(P29a) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq))
# check %>% group_by(P29b) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq))
# check %>% group_by(P29b) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq))
# check %>% group_by(P29d) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq))
# check %>% group_by(P29e) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq))
# 
# na_removed_dat %>%
#   group_by(DentalIssues) %>%
#   summarise(cnt = n()) %>%
#   mutate(freq = round(cnt / sum(cnt), 3)) %>%
#   arrange(desc(freq))

# Mental Health Self 
# Have you ever been treated for a mental health, emotional or behavioral problem? 
# P35a	Have you ever been treated for a mental health, emotional or behavioral problem? No
# P35b	Have you ever been treated for a mental health, emotional or behavioral problem? Yes, during the last year
# P35c	Have you ever been treated for a mental health, emotional or behavioral problem? Yes, more than a year ago
# 0 = No
# 1 = Yes 

na_removed_dat <- na_removed_dat %>% mutate(MentalHealthSelf = ifelse(P35a == 1 & (P35b == 0 | P35c == 0),0,
                                                                      ifelse(P35a == 0 & (P35b == 1 | P35c == 1),1,NA))) %>% dplyr::select(-c(P35a,P35b,P35c))

# QC:
# check <- dat_sub_named %>% na.omit() %>% dplyr::select(P35a:P35c)
# check %>% group_by(P35a) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq))
# check %>% group_by(P35b) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq))
# check %>% group_by(P35c) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq))
# 
# na_removed_dat %>%
#   group_by(MentalHealthSelf) %>%
#   summarise(cnt = n()) %>%
#   mutate(freq = round(cnt / sum(cnt), 3)) %>% 
#   arrange(desc(freq))

# Drug/Alcohol Self 
# Have you ever been treated for a drug or alcohol problem? 
# P36a Have you ever been treated for an alcohol or drug problem? No
# P36b Have you ever been treated for an alcohol or drug problem? Yes, during the last year
# P36c Have you ever been treated for an alcohol or drug problem? Yes, more than a year ago
# 0 = No
# 1 = Yes

na_removed_dat <- na_removed_dat %>% mutate(AlcDrugSelf = ifelse(P36a == 1 & (P36b == 0 | P36c == 0),0,
                                                                 ifelse(P36a == 0 & (P36b == 1 | P36c == 1),1,NA))) %>% dplyr::select(-c(P36a,P36b,P36c))

# QC:
# check <- dat_sub_named %>% na.omit() %>% dplyr::select(P36a:P36c)
# check %>% group_by(P36a) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq))
# check %>% group_by(P36b) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq))
# check %>% group_by(P36c) %>% summarise(cnt = n()) %>% mutate(freq = round(cnt / sum(cnt), 3)) %>%  arrange(desc(freq))
# 
# na_removed_dat %>%
#   group_by(AlcDrugSelf) %>%
#   summarise(cnt = n()) %>%
#   mutate(freq = round(cnt / sum(cnt), 3)) %>% 
#   arrange(desc(freq))

# Suicide
# P52a	Have you ever seriously considered attempting suicide? No
# P52b	Have you ever seriously considered attempting suicide? Yes, during the last year
# P52c	Have you ever seriously considered attempting suicide? Yes, more than a year ago

na_removed_dat <- na_removed_dat %>% mutate(Suicide = ifelse(P52a == 1 & (P52b == 0 | P52c == 0),0, ifelse(P52a == 0 & (P52b == 1 | P52c == 1),1,NA))) %>% dplyr::select(-c(P52a,P52b,P52c))

# QC:
# na_removed_dat %>%
#   group_by(Suicide) %>%
#   summarise(cnt = n()) %>%
#   mutate(freq = round(cnt / sum(cnt), 3)) %>% 
#   arrange(desc(freq))

#----------------------- Variable Transformations -----------------------

# Setting variables to their proper form (i.e factor, numeric, ordinal, etc.)

factor_vars <- na_removed_dat %>% dplyr::select(c(P1,P2,Biosex,P5,P8,P11,P12,P13,P14ab1,P14ab2,P14ab3,P14ab4,P14ab5,P14ab6,P14ab7,P14ab8,P14ab9,P14ab10,P14ab11,P14ab12,P14ab13,P14ab14,P25,P34,P38,P39a,P42c,P42d,P61:P66,FiveFV,Region,AbuseRelationship,SelfPerceivedHealth,Diab_Prediab,FosterCare,DrugFreq,AlcDrug_Residence,NicFreq,ImprisGaurd,DentalIssues,MentalHealthSelf,AlcDrugSelf,Suicide))
numeric_vars <- na_removed_dat %>% dplyr::select(c(P14a,P15,P16a:P18d,P20a:P23e,P26a:P28,P30,P37,P40d:P41g,P45:P50d,P70a:P86,Binge))

fac_col_names <- names(factor_vars)
num_col_names <- names(numeric_vars)

na_removed_dat[,fac_col_names] <- lapply(na_removed_dat[,fac_col_names] , as.factor)

na_removed_dat[,num_col_names] <- lapply(na_removed_dat[,num_col_names] , as.numeric)


#----------------------- Splitting Data -----------------------
# Splitting into final testing and validation set
set.seed(707)
sample <- sample(c(TRUE, FALSE), nrow(na_removed_dat), replace=TRUE, prob=c(0.8,0.2))

# Final Test 
final_test <- na_removed_dat[!sample, ]

# Training 
ModelDev  <- na_removed_dat[sample, ]
sample2 <- sample(c(TRUE, FALSE), nrow(ModelDev), replace=TRUE, prob=c(0.70,0.30))

## Splitting training further 
Train <- ModelDev[sample2, ]
Test <- ModelDev[!sample2, ]

#----------------------- Saving out data  -----------------------

# save(na_removed_dat,file = "/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/SubsetDat.RData")
# save(final_test,file = "/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/FinalTest.RData")
# save(Train,file ="/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/ModelDev_Train.RData")
# save(Test,file ="/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/ModelDev_Test.RData")
# save(ModelDev, file = "/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/ModelDev.RData")

