################################################################
# PROJECT: Advanced Data Analysis Final Project
# PURPOSE: Code used to run analyses for Final Project
# DIR:     "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/natality/Nat2017ps.zip"
# DATA:    CDC Vital Statistics File - Birth Records 2017
# AUTHOR:  Mackenzie Robinson and Clive Musonza
# CREATED: 3/27/19 
# LATEST:  5/1/19 
################################################################

#Load libraries and read in the data.
```{r setup, include=FALSE}
library(readr) #for read txt file
library(knitr) #for creating nicer tables
library(tidyverse) # for various packages
library(nnet) #Multinomial logistic regresison
library(MASS) #Ordinal logistic regression
library(plyr)
library(ggplot2)
library(odds.n.ends)
```

##Look at the data and the main variables sof interest.
```{r}
#setwd("~/Desktop/Spring 2019/Advanced Data Analysis")
#getwd()
#Load dataset
birthrecord <- read.csv(file="natl2017.csv",head=TRUE,sep=",")

#Get all variable column names that are in dataset
colnames(birthrecord)

#Get frequencies of those who responded to the question on fertility treatment use at all, 1=yes and 0=no
table(birthrecord$f_rf_inf_drg) #3,717,512 reported this variable
table(birthrecord$f_rf_inf_art) #3,717,512 reported on this variable

#Get frequencies of those using fertility treatment at all (both types)
table(birthrecord$rf_inf) #Yes = 72,331

#See what kind of variable "Infertility treatment" is
class(birthrecord$rf_inftr) #factor

#Check # of missing values for each variable of interest
#Outcome variables - Overall infertility treatment
summary(birthrecord$rf_inftr)
table(birthrecord$rf_inftr)
"There are 3,786 Unknowns/Unstated to combine with 3,788,637 No's"

#Outcome variables - use of drugs 
summary(birthrecord$rf_fedrg)
table(birthrecord$rf_fedrg)
"U = 8,563, X (not applicable) = 3,788,637"
class(birthrecord$rf_fedrg) #check class of variable

#Outcome variables - use of ART (how do we treat Unknowns/unstated? Might need to combine them with "No" category)
summary(birthrecord$rf_artec)
table(birthrecord$rf_artec)
"U = 8,563, X (not applicable) = 3,788,637"

#Recode Independent variable
table(birthrecord$rf_phyp)

birthrecord$pre_hyp[birthrecord$rf_phyp=="Y"]<-1
birthrecord$pre_hyp[birthrecord$rf_phyp=="N"]<-0
birthrecord$pre_hyp[birthrecord$rf_phyp=="U"]<-0
birthrecord$pre_hyp <-factor(birthrecord$pre_hyp, levels=c(0,1), labels=c("No/Unknown", "Yes"))

table(birthrecord$pre_hyp) #check recoding

#Recode Outcome Variables
#Recode Use of Fertility Treatment to combine "Unknowns/unstated"" with "No" category
birthrecord$inftr_bin[birthrecord$rf_inftr=="Y"]<-1
birthrecord$inftr_bin[birthrecord$rf_inftr=="N"]<-0
birthrecord$inftr_bin[birthrecord$rf_inftr=="U"]<-0
birthrecord$inftr_bin <-factor(birthrecord$inftr_bin, levels=c(0,1), labels=c("No/Unknown", "Yes"))

table(birthrecord$inftr_bin) #Yes = 72,331, No/Uknown = 3,792,423
summary(birthrecord$inftr_bin)
head(birthrecord$inftr_bin)

#Recode 'Use of Fertility Treatment - Drugs' to combine 'Unknown/unstated' with 'No category
birthrecord$inf_drg[birthrecord$rf_fedrg=="Y"]<-1
birthrecord$inf_drg[birthrecord$rf_fedrg=="N" | birthrecord$rf_fedrg=="U"]<-0
birthrecord$inf_drg[birthrecord$rf_fedrg=="X"]<-NA

#Relevel 'Use of Fertility Treatment - Drugs'  to have 'No/uknown' as reference
birthrecord$inf_drg <-factor(birthrecord$inf_drg, levels=c(0,1), labels=c("No/Unknown", "Yes"))
table(birthrecord$inf_drg) #check to make sure it recoded properly

#Recode 'Use of Fertility Treatment - ART' to combine "Unknown/unstated"" with "No" category
birthrecord$inf_art2[birthrecord$rf_artec=="Y"]<-1
birthrecord$inf_art2[birthrecord$rf_artec=="N" | birthrecord$rf_artec=="U"]<-0
birthrecord$inf_art2[birthrecord$rf_artec=="X"]<-NA

#Relevel 'Use of Fertility Treatment - ART' to have 'No/uknown' as reference
birthrecord$inf_art2 <-factor(birthrecord$inf_art2, levels=c(0,1), labels=c("No/Unknown", "Yes"))
table(birthrecord$inf_art2)
```

##Data Management - recoding independent variables
```{r}
"We will adjust the models for mother's education (meduc), mother's age (mager9), race (mbrace), smoking (keep the categories as is, cig0_r)"

#Get classes of all variables to adjust for
class(birthrecord$meduc) #integer
class(birthrecord$cig0_r) #integer
class(birthrecord$mager9) #integer
class(birthrecord$mrace6) #integer

#Relevel adjusting variables
#Recode Cigarette smoking before pregenancy variable to exclude "unknown or not stated" (Value is 6 for unknown/unstated) and re-level to make non-smokers the reference group.
birthrecord$cig_cat[birthrecord$cig0_r==0]<-0
birthrecord$cig_cat[birthrecord$cig0_r==1]<-1
birthrecord$cig_cat[birthrecord$cig0_r==2]<-1
birthrecord$cig_cat[birthrecord$cig0_r==3]<-1
birthrecord$cig_cat[birthrecord$cig0_r ==4]<-1
birthrecord$cig_cat[birthrecord$cig0_r==5]<-1
birthrecord$cig_cat[birthrecord$cig0_r==6]<-NA
birthrecord$cig_cat=factor(cig_cat, 0,1, c("Non-smoker", "Smoker"))
table(birthrecord$cig_cat)

#convert to factor
birthrecord$cig_cat <-as.factor(birthrecord$cig_cat)
class(birthrecord$cig_cat)
birthrecord$cig_cat <- relevel(birthrecord$cig_cat, ref = "0") #making non-smokers as reference category

#Recode Mother's education variable to exclude "unknown or not stated" (Value is 9 for unknown/unstated).
table(birthrecord$meduc)

#Recode Mother's age variable to exclude "unknown or not stated" (Value is 9 for unknown/unstated).
table(birthrecord$mager9)

#Recode Mother's race variable to exclude "unknown or not stated" (Value is 9 for unknown/unstated).
table(birthrecord$mbrace)

##Re-level the dependent variable to have "No Use of Infertility treatment" as the reference category for modeling.
```

##Setting up data to conduct a complete case analysis. 
```{r}
#defining variables to include in the complete data set
myvars <- c("inftr_bin", "rf_inftr", "inf_drg", "inf_art2", "pre_hyp", "meduc", "cig0_r", "mager9", "mbrace", "bmi")

#subsetting by those variables
birthrecord_cc <- birthrecord[myvars]

#omitting NA's in the data set
birthrecord_cc <- na.omit(birthrecord_cc)

#checking to make sure there are no NA's
summary(birthrecord_cc)
```

#Test the linearity assumption
```{r}
#linearity
ifbmi.log <- birthrecord_cc$bmi * log(BRFSS_cc$bmi) #create term to test linearity

boxTidwellBMI <- glm(diabetes_binary ~ bmi + bmi.times.logbmi, data=BRFSS_cc, family="binomial") #Box Tidwell technique, test the assumption of linearity

summary(boxTidwellBMI)
```

##Model 1: Logistic regression of pre-pregnancy hypertension on treatment use overall, adjusting for mother's education, race, age, and cigarette smoking before pregnancy.
```{r}
#Re-level pre-pregnancy hypertension to make "no/unknown" status as the reference category
birthrecord_cc$pre_hyp <- relevel(birthrecord_cc$pre_hyp, ref = "No/Unknown")

#logistic model
infuse_logit <- glm(inftr_bin ~ pre_hyp + meduc + cig0_r + mager9 + mbrace + bmi, data=birthrecord_cc, family="binomial")
  summary(infuse_logit)

ORinf <-exp(cbind(OR = coef(infuse_logit), confint(infuse_logit))) #calculate ORs and 95% CIs
  ORinf #print ORs and 95% CIs ##doesn't work
```

##Multinomial regression model to examine the effect of pre-pregnancy hyptertension on use of infertility treatment - overall, adjusting for mother's education, age, race, and cigarette smoking before pregnancy. We ran a multinomial regression to determine if there would be any differences between the two models.
```{r}
#Attach the data to avoid repeatly typing the dataset name
attach(birthrecord_cc)

##Re-leveling data, choose those who did not use treatment "No" as reference
treatment <- relevel(rf_inftr, ref = "N")

##Multinomial regression for infertility treatment overall
mod_Rx <- multinom(rf_inftr ~ pre_hyp + meduc + cig0_r + mager9 + mbrace + bmi)
summary(mod_Rx)
exp(coef(mod_Rx)) #get the OR
exp(confint(mod_Rx, level = 0.95)) # get the confidence intervals
```

##Model 2 - Logistic regression of pre-pregnancy hyptertension on use of infertility treatment - drugs, adjusting for mother's education, race, age, and cigarette smoking before pregnancy.
```{r}
#logistic model
drugs_logit <- glm(inf_drg ~ pre_hyp + meduc + cig0_r + mager9 + mbrace + bmi, data=birthrecord_cc, family="binomial")
  summary(drugs_logit)

ORinf <-exp(cbind(OR = coef(drugs_logit), confint(drugs_logit))) #calculate ORs and 95% CIs
  ORinf #print ORs and 95% CIs
```

##Model 3 - Logistic regression of pre-pregnancy hypertension on infertility treatment - ART, adjusting for mother's education, race, age, and cigarette smoking before pregnancy.
```{r}
#logistic model
ARTRx_logit <- glm(inf_art2 ~ pre_hyp + meduc + cig0_r + mager9 + mbrace + bmi, data=birthrecord_cc, family="binomial")
  summary(ARTRx_logit)

ORinf <-exp(cbind(OR = coef(ARTRx_logit), confint(ARTRx_logit))) #calculate ORs and 95% CIs
  ORinf #print ORs and 95% CIs ##doesn't work
```
