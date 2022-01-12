#Meredith Palmore
#Biostat 624 Project 

#open foreign package to convert from XPT to CSV
library(foreign)

#set working directory
setwd("C:/Users/softb/OneDrive - Johns Hopkins/624/_Palmore_Meredith_project/")

################################################################
#read in alcohol data
ALQ <- read.xport("ALQ_J.XPT")

#select variables of interest
myvars <- c("SEQN", "ALQ111", "ALQ121", "ALQ130")
ALQ_filt <- ALQ[myvars]

#export to excel

write.csv(ALQ_filt,file="Alcohol_NHANES_2017.csv")

###############################################################
#read in outcome (sleep hours) data 

sleep <- read.xport("SLQ_J.XPT")

#select variables of interest
myvars2 <- c("SEQN", "SLD012", "SLD013", "SLQ120")
SLQ_filt <- sleep[myvars2]


## dichotomize daytime sleepiness 
SQL_filt = mutate(SQL_filt, day_sleep = ifelse(SQL_filt$SLQ120 == '3' | SQL_filt$SLQ120 == '4',1,0))

write.csv(SQL_filt,file="Sleep_NHANES_2017.csv")

##############################################################

#read in demographic and weighting data
demog <- read.xport("DEMO_J.XPT")

#select variables of interest
myvars3 <- c("SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH3", 
             "WTINT2YR", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "INDFMPIR")
Demo_filt <- demog[myvars3]

#export to excel

write.csv(Demo_filt,file="Demographic_NHANES_2017.csv")

##############################################################

#read in covariate (smoking) data
SMO <- read.xport("SMQRTU_J.XPT")

#select variables of interest
myvars <- c("SEQN", "SMQ681")
SMO_filt <- SMO[myvars]

#export to excel

write.csv(SMO_filt,file="SMOKING_NHANES_2017.csv")

#############################################################
#read in covariate (BMI) data
BMI <- read.xport("BMX_J.XPT")

#select variables of interest
myvars <- c("SEQN", "BMXBMI")
BMI_filt <- BMI[myvars]

#export to excel

write.csv(BMI_filt,file="BMI_NHANES_2017.csv")

############################################################

#read in covariate (caffeine) data
Caffeine_d1 <- read.xport("DR1TOT_J.XPT")
Caffeine_d2 <- read.xport("DR2TOT_J.XPT")

#select variables of interest
myvars <- c("SEQN", "DR1TCAFF")
Caff_filt <- Caffeine_d1[myvars]

myvars <- c("SEQN", "DR2TCAFF")
Caff2_filt <- Caffeine_d2[myvars]

#export to excel

write.csv(Caff_filt,file="Caffeineday1_NHANES_2017.csv")

write.csv(Caff2_filt,file="Caffeineday2_NHANES_2017.csv")

## average the two days of caffeine measurements
caffday1 <- read_csv("Caffeineday1_NHANES_2017.csv")

caffday2 <- read_csv("Caffeineday2_NHANES_2017.csv")

caffday1 <- append(caffday1, caffday2[,2:3])

caffday1 <- as.data.frame(caffday1)

caffday1 <- mutate(caffday1, avg_mg_caffeine=(DR1TCAFF+DR2TCAFF)/2)

caffday1$SEQN.1 <- NULL

write.csv(caffday1, file="Caffeine_NHANES_2017.csv")

###############################################################

#read in covariate (Physical Activity) data
PA <- read.xport("PAQ_J.XPT")

#select variables of interest
myvars <- c("SEQN", "PAQ605","PAQ620","PAQ650","PAQ665")
PA_filt <- PA[myvars]

#export to excel

write.csv(PA_filt,file="Activity_NHANES_2017.csv")

################################################################

#read in covariate (Depression) data
Depress <- read.xport("DPQ_J.XPT")

#All variables are of interest

#export to excel

write.csv(Depress,file="Depression_NHANES_2017.csv")

#################################################################

library(dplyr)
library(tidyverse)

setwd("C:/Users/softb/OneDrive - Johns Hopkins/624/_Palmore_Meredith_project/")

##read in all data 
demog <- read_csv("Demographic_NHANES_2017.csv")

sleep <- read_csv("Sleep_NHANES_2017.csv")

alc <- read_csv("Alcohol_NHANES_2017.csv")

#change to missing if "Refused" or "Don't Know"

alc$ALQ121 <- na_if(alc$ALQ121,77)
alc$ALQ121 <- na_if(alc$ALQ121,99)

alc$ALQ130 <- na_if(alc$ALQ130,777)
alc$ALQ130 <- na_if(alc$ALQ130,999)

activity <- read_csv("Activity_NHANES_2017.csv")

#change to missing if "Refused" or "Don't Know"
names(activity)
activity$PAQ605 <- na_if(activity$PAQ605,9)
activity$PAQ620 <- na_if(activity$PAQ620,9)


bmi <- read_csv("BMI_NHANES_2017.csv")

caffeine <- read_csv("Caffeine_NHANES_2017.csv")

Depress <- read_csv("Depression_NHANES_2017.csv")

#change to missing if "Refused" or "Don't Know"
names(Depress)

Depress$DPQ010 = Depress$DPQ010 %>% na_if(9) %>% na_if(7)

Depress$DPQ020 = Depress$DPQ020 %>% na_if(9) %>% na_if(7)

Depress$DPQ030 = Depress$DPQ030 %>% na_if(9) %>% na_if(7)

Depress$DPQ040 = Depress$DPQ040 %>% na_if(9) %>% na_if(7)

Depress$DPQ050 = Depress$DPQ050 %>% na_if(9) %>% na_if(7)

Depress$DPQ050 = Depress$DPQ050 %>% na_if(9) %>% na_if(7)

Depress$DPQ060 = Depress$DPQ060 %>% na_if(9) %>% na_if(7)

Depress$DPQ070 = Depress$DPQ070 %>% na_if(9) %>% na_if(7)

Depress$DPQ080 = Depress$DPQ080 %>% na_if(9) %>% na_if(7)

Depress$DPQ090 = Depress$DPQ090 %>% na_if(9) %>% na_if(7)

smoking <- read_csv("SMOKING_NHANES_2017.csv")

# Merge component files
# Keep all records in DEMO, even if that SEQN does not match to other files

one_tmp <- left_join(demog, sleep, by="SEQN") %>%
  left_join(., alc , by="SEQN") %>%
  left_join(., activity, by="SEQN") %>%
  left_join(., bmi, by="SEQN") %>%
  left_join(., caffeine, by="SEQN") %>%
  left_join(., Depress, by="SEQN") %>%
  left_join(., smoking, by="SEQN")

############################################################################

##create a depression score summing all question responses except for DPQ100

one_tmp <- mutate(one_tmp, depress_score=rowSums(select(one_tmp, starts_with("DPQ")& !ends_with("100"))))

## Turn depression into a categorical variable

#Depression is categorized (dichotomized) based on guidelines
# No depression: no_dep = 1 if total score <5; 0 if total score >= 5.
# Mild depression: mild_dep = 1 if total score <10 and >4; 0 otherwise.
# Moderate depression: mod_dep = 1 if total score >9 and <15; 0 therwise
# Moderate to severe: mod_sev_dep = 1 if total score >14 and <20; 0 otherwise
# Severe: sev_dep = 1 if total score >19; 0 otherwise

one_tmp = one_tmp %>% mutate(depress_cat = cut(depress_score, breaks = c(-Inf,4.5,9.5,14.5,19.5,Inf),
                                         labels = c('nodep','mild_dep','mod_dep','mod_sev_dep','sev_dep'))
)

##########################################################################

# Create race and Hispanic ethnicity categories 
# combined Non-Hispanic white and Non-Hispanic other and multiple races, to 
 # approximate the sampling domains
one_tmp = one_tmp %>% mutate(
  race1 = factor(c(3, 3, 4, 1, NA, 2, 4)[RIDRETH3],
                 labels = c('NH Black','NH Asian', 'Hispanic', 'NH White and Other')),
  # Create race and Hispanic ethnicity categories for hypertension analysis 
  raceEthCat= factor(c(4, 4, 1, 2, NA, 3, 5)[RIDRETH3],
                     labels = c('NH White', 'NH Black', 'NH Asian', 'Hispanic', 'NH Other/Multiple')),
  # Create age categories for adults aged 18 and over: ages 18-39, 40-59, 60 and over
  ageCat_18 = cut(RIDAGEYR, breaks = c(17, 39, 59, Inf),
                  labels = c('18-39','40-59','60+'))
)

###########################################################################

# Regular moderate or vigorous activity reported? 1 = yes; 0 = no

one_tmp <-  mutate(one_tmp, activity=ifelse(PAQ605==1|PAQ620==1|PAQ650==1|PAQ665==1, 1, 0))

##########################################################################

# Recommended caffeine dose is 400 mg. Dichotomize to above and below that threshold:

one_tmp <- mutate(one_tmp, hi_caffeine=ifelse(avg_mg_caffeine>400.0,1,0))

###########################################################################

# There are standards for categorizing BMI. Let's carry them out here:

one_tmp = one_tmp %>% mutate(BMI_cat = cut(BMXBMI, breaks = c(-Inf,18.49,24.9, 29.9,Inf),
                                           labels = c('underweight','normal','overweight','obese'))
)

###########################################################################

# Categorize alcohol use

# Above 2 drinks a day for men or above 1 drink a day for women are considered 
# in excess of "moderate drinking".
# https://www.niaaa.nih.gov/alcohol-health/overview-alcohol-consumption/moderate-binge-drinking

one_tmp <-  mutate(one_tmp, 
                   heavy_drink = ifelse((ALQ130>2 & RIAGENDR == 1) 
                                        | (ALQ130>1 & RIAGENDR == 2),1, 0))

table(one_tmp$ALQ130, one_tmp$heavy_drink)


# Write everything to CSV file

write.csv(one_tmp, "fulldata_2017_NHANES.csv")

##########################################################################
## Correcting for missingness


# read in dataset
full_dat <- read_csv("fulldata_2017_NHANES.csv")

unweighted_data <- filter(full_dat, RIDAGEYR >=18)

## How many are missing? 
## conventional 10% cutoff

missing <- colSums(is.na(unweighted_data))# Number of missing per variable
length(unweighted_data$SEQN)

missing <- missing/5856

missing

#Re-code alcohol consumption accounting for those who don't drink to correct missingness
unweighted_data = unweighted_data %>%  mutate(heavy_drink2 = ifelse(ALQ111==2 | ALQ121==0, 0, heavy_drink))


## How many are missing? 
## conventional 10% cutoff

missing <- colSums(is.na(unweighted_data))# Number of missing per variable
length(unweighted_data$SEQN)

missing <- missing/5856

missing

# looks better now. Re-write full dataset to csv

write.csv(full_dat, "fulldata_2017_NHANES.csv")

