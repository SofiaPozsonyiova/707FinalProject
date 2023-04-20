###############################
# Data Processing (SP) FULL   # 
###############################

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
dat_sub <- dat %>% dplyr::select(A1,A2,biosex,A13,A15,A18,A21_inperson,A23A,A23C,A23D,A23E,A23F,A23G_inperson,A23H_inperson,A23i,A23J,A23K,A23L,A23M_inperson,A24,A25A,A25B,A25C,A26A,A26B,A26C,A27B,A27C,A27D,A30A,A30B,A30C,A30F,A30G,A30H,A31,A32A,A32E,A33C,A33E,A35,A36A,A36B,A36C,A36D,A36E,A36G,A36H,A36I,A38B,A38C,A38E,A38F,A38G,A42,A45,A46,A49,A50,A51A,A52D,A53B,A53C,A53D,A53E,A53H,A54C,A54D,A55,A56A,A56B,A56C,A56D,A56F,A58A,A58C,A58D,A58E,A58F,A58G,A58H,A58I,A58J,A58K,A58L,A58N,A58O,A58P,A58Q,A59A,A59B,A59C,A59D,A71,A72,A73,A74,A75,A76,A79A,A80C,A80D,A81D,A85,A88,A94,A99,FiveFV,Binge,A54A,A54B,A63A,A63B,A65A,A65B,A65C,A93C,A93D,A93E,A93F,A93G,A93I,A69,A70,A81A,A81B,A81C,A67A,A67B,A67C,A40A,A40B,A40C,A40D,A47A,A47B,A47C,A48A,A48B,A48C,A61A,A61B,A61C,A39,biosex) %>% rename(P1 = A1 ,
                                    P2 = A2,
                                    P8 = A13 ,
                                    P11 = A15,
                                    P13 = A18,
                                    P14a = A21_inperson,
                                    P14ab1 = A23A,
                                    P14ab2 = A23C,
                                    P14ab3 = A23D,
                                    P14ab4 = A23E,
                                    P14ab5 = A23F,
                                    P14ab6 = A23G_inperson,
                                    P14ab7 = A23H_inperson,
                                    P14ab8 = A23i,
                                    P14ab9 = A23J,
                                    P14ab12 = A23K,
                                    P14ab13 = A23L,
                                    P14ab14 = A23M_inperson,
                                    P15 = A24,
                                    P16a = A25A,
                                    P16b = A25B,
                                    P16c = A25C,
                                    P17a = A26A,
                                    P17b = A26B,
                                    P17c = A26C,
                                    P18b = A27B,
                                    P18c = A27C,
                                    P18d = A27D,
                                    P20a = A30A,
                                    P20b = A30B,
                                    P20c = A30C,
                                    P20f = A30F,
                                    P20g = A30G,
                                    P20h = A30H,
                                    P21 = A31,
                                    P22a = A32A,
                                    P22e = A32E,
                                    P23c = A33C,
                                    P23e = A33E,
                                    P25 = A35,
                                    P26a = A36A,
                                    P26b = A36B,
                                    P26c = A36C,
                                    P26d = A36D,
                                    P26e = A36E,
                                    P26f = A36G,
                                    P26g = A36H,
                                    P26h = A36I,
                                    P27b = A38B,
                                    P27c = A38C,
                                    P27e = A38E,
                                    P27f = A38F,
                                    P27g = A38G,
                                    P30 = A42,
                                    P33 = A45,
                                    P34 = A46,
                                    P37 = A49,
                                    P38 = A50,
                                    P39a = A51A,
                                    P40d = A52D,
                                    P41b = A53B,
                                    P41c = A53C,
                                    P41d = A53D,
                                    P41e = A53E,
                                    P41g = A53H,
                                    P42c = A54C,
                                    P42d = A54D,
                                    P45 = A55,
                                    P47a = A56A,
                                    P47b = A56B,
                                    P47c = A56C,
                                    P47d = A56D,
                                    P47e = A56F,
                                    P49a = A58A,
                                    P49c = A58C,
                                    P49d = A58D,
                                    P49e = A58E,
                                    P49f = A58F,
                                    P49g = A58G,
                                    P49h = A58H,
                                    P49i = A58I,
                                    P49j = A58J,
                                    P49k = A58K,
                                    P49l = A58L,
                                    P49n = A58N,
                                    P49o = A58O,
                                    P49p = A58P,
                                    P49q = A58Q,
                                    P50a = A59A,
                                    P50b = A59B,
                                    P50c = A59C,
                                    P50d = A59D,
                                    P61 = A71,
                                    P62 = A72,
                                    P63 = A73,
                                    P64 = A74,
                                    P65 = A75,
                                    P66 = A76,
                                    P70a = A79A,
                                    P70c = A80C,
                                    P70d = A80D,
                                    P71d = A81D,
                                    P74 = A85,
                                    P76 = A88,
                                    P81 = A94,
                                    P86 = A99,
                                    P42a = A54A,
                                    P42b = A54B,
                                    P54a = A63A,
                                    P54b = A63B,
                                    P56a = A65A,
                                    P56b = A65B,
                                    P56c = A65C,
                                    P80c = A93C,
                                    P80d = A93D,
                                    P80e = A93E,
                                    P80f = A93F,
                                    P80g = A93G,
                                    P80i = A93I,
                                    P59 = A69,
                                    P60 = A70,
                                    P71a = A81A,
                                    P71b = A81B,
                                    P71c = A81C,
                                    P58a = A67A,
                                    P58b = A67B,
                                    P58c = A67C,
                                    P29a = A40A,
                                    P29b = A40B,
                                    P29c = A40C,
                                    P29d = A40D,
                                    P35a = A47A,
                                    P35b = A47B,
                                    P35c = A47C,
                                    P36a = A48A,
                                    P36b = A48B,
                                    P36c = A48C,
                                    P52a = A61A,
                                    P52b = A61B,
                                    P52c = A61C,
                                    SelfPerceivedHealth = A39)

# Visualizing Missing 
# P76 
# P70a
dat_sub_named <- dat_sub %>% select(-c(P76,P70a)) 
plot_missing(dat_sub_named)

# per <- dat_sub %>%
#   summarise(across(everything(), ~ mean(is.na(.)) * 100, .names = "perc_missing_{.col}"))

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
table(na_removed_dat$SelfPerceivedHealth)

#----------------------- Combining and Redefining Variables  -----------------------
# REDEFINING SELF PERCEIVED HEALTH
# SelfPerceivedHealth (P28)    
# 0 = Great Health 
# 1 = Poor or Fair Health

# 0 = '1 = Excellent', '2 = Very good' = GOOD HEALTH  
# 1 = '4 = Fair','5 = Poor'= POOR HEALTH 
# NA = '3 = Good'

ggplot(data=na_removed_dat, aes(x=SelfPerceivedHealth)) +
  geom_bar(fill = "cornflowerblue") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)+ scale_x_discrete(labels=c('Good','Poor'))+ xlab("Self-Perceived Health")+ylab("Count") + ggtitle("Counts of Self-Perceived Health")
ggsave("/Users/sofiapozsonyiova/Documents/GitHub/707FinalProject/selfperceivedhealthDic.png", height = 6.5, width = 4)


# ggplot(data=na_removed_dat, aes(x=factor(Biosex, labels = c("Male", "Female")), fill = factor(SelfPerceivedHealth, labels = c("Good", "Poor")))) +
#   geom_bar() +
#   geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1) +
#   scale_x_discrete(labels=c('Male','Female')) +
#   xlab("Biological Sex") +
#   ylab("Count") +
#   ggtitle("Self-Perceived Health by Bio Sex") +
#   facet_wrap(~factor(SelfPerceivedHealth, labels = c("Good", "Poor"))) +
#   guides(fill = "none")
# 
# ggsave("/Users/sofiapozsonyiova/Documents/GitHub/707FinalProject/SexBio.png", height = 6, width = 5)


# ggplot(data=na_removed_dat, aes(x=P1, fill = SelfPerceivedHealth)) +
#   geom_bar() +
#   geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1) +
#   scale_x_discrete(labels=c('8','9','11')) +
#   xlab("Grade") +
#   ylab("Count") +
#   ggtitle("Self-Perceived Health by Grade")  +
#   guides(fill = "none")+facet_wrap(~factor(SelfPerceivedHealth, labels = c("Good", "Poor")))

# ggsave("/Users/sofiapozsonyiova/Documents/GitHub/707FinalProject/Grade.png", height = 6.2, width = 5.5)


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
                                                                  ifelse(P29a == 1| P29b == 1|P29c == 1| P29d == 1,1,NA))) %>% dplyr::select(-c(P29a:P29d))

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

factor_vars <- na_removed_dat %>% dplyr::select(c(P1,P2,P8,P11,P13,P14ab1,P14ab2,P14ab3,P14ab4,P14ab5,P14ab6,P14ab7,P14ab8,P14ab9,P14ab12,P14ab13,P14ab14,P25,P34,P38,P39a,P42c,P42d,P61:P66,FiveFV,AbuseRelationship,SelfPerceivedHealth,Diab_Prediab,FosterCare,DrugFreq,AlcDrug_Residence,NicFreq,ImprisGaurd,DentalIssues,MentalHealthSelf,AlcDrugSelf,Suicide,biosex))
numeric_vars <- na_removed_dat %>% dplyr::select(c(P14a,P15,P16a:P18d,P20a:P23e,P26a:P27g,P30,P37,P40d:P41g,P45:P50d,P70c:P86,Binge)) 

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
  sample_n(size=5079,replace = TRUE) %>%
  ungroup() 

Train_Model_Dev_Balanced <- grouped_data2[!is.na(grouped_data2$SelfPerceivedHealth), ]
Train_Model_Dev_Balanced <- Train_Model_Dev_Balanced %>% select(-size)
Train_Model_Dev_Balanced %>%
  group_by(SelfPerceivedHealth) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(freq))

#----------------------- Saving out data  -----------------------

save(final_test,file = "/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/2022/FinalValidation_UnbalancedUSE.RData")
save(Train_Model_Dev_Balanced,file ="/Users/sofiapozsonyiova/Documents/GitHub/Private707/data/2022/ModelDev_Train_BalancedUSE.RData")

