## Meredith Palmore
## NHANES analysis 
## Term 4 2021

# Part 0:  Setup 
# Part a:  Scatterplot matrices
# Part b:  Fit weighted and unweighted SLRs
# Part c:  Fit initial weighted and unweighted MLR model using all predictors
# Part d:  Best subset model selection and human-selected model
# Part e.  Compare model AICs and goodness of fit
# Part f:  Check model:  Analysis of Residuals
# Part f-1:  Plot person residuals -vs- predicted values
# Part f-2:  Boxplot and list outlier studentized residuals
# Part g: Check model:  leverage boxplots
# Part h: Check model:  influence
# Part h-1:  Make boxplot and list outliers for influence
# Part h-2:  Leverage -vs- residual squared (L-R) plot
# Part : Sensitivity analysis:  Re-fit without influential points
# Part i: Check for multi-collinearity using variance inflation factors


## Part 0: Setup
##################################################################################
#set working directory
setwd("C:/Users/softb/OneDrive - Johns Hopkins/624/_Palmore_Meredith_project/")

# Load libraries needed
# May need to install these with install.packages() if you don't have them already
library(car)         # for added variable plots, VIF
library(sandwich)    # for robust linear regression
library(GGally)      # for fancier scatterplot matrices
library(broom)       # for tidy model output using tidy() function
library(tidyverse)   # general functions for working with data
library(lmtest)      # coefficient test for robust variance

# Load the tidyverse last because both the dplyr package from the tidyverse and the MASS package have a select() 
# function and the one loaded last is the one that will be used.


# read in dataset
data <- read_csv("fulldata_2017_NHANES.csv")

data <- mutate(data,inAnalysis=(RIDAGEYR >=18))

# explore data 
glimpse(data)
names(data)

# Specify categorical variables as ordered or unordered

data$BMI_cat <- factor(data$BMI_cat, ordered = FALSE )
data$raceEthCat <- factor(data$raceEthCat, ordered = FALSE )
data$ageCat_18 <- factor(data$ageCat_18, ordered = FALSE)
data$depress_cat <- ordered(data$depress_cat, levels = c("nodep","mild_dep","mod_dep","mod_sev_dep","sev_dep"))


## make sure all variables are leveled properly 
data <- within(data, raceEthCat <- relevel(raceEthCat, ref = "NH White"))
data <- within(data, BMI_cat <- relevel(BMI_cat, ref = "normal"))
data <- within(data, ageCat_18 <- relevel(ageCat_18, ref = "18-39"))


#### Part a: Scatterplot Matrix
################################################################################

# Using the ggpairs() function
# Nice for getting a quick scatterplot matrix plot

#first created an unweighted dataset of those >=18

unweighted_data <- filter(data, RIDAGEYR >=18)

pdf("scatterplot_matrices_person.pdf")
unweighted_data %>%
  dplyr::select(RIAGENDR, RIDAGEYR, INDFMPIR,depress_score,BMXBMI) %>%
  pairs(upper.panel=NULL)
dev.off()


pdf("scatterplot_matrices_behavior.pdf")
unweighted_data %>%
  dplyr::select(avg_mg_caffeine,SMQ681,ALQ130,activity) %>%
  pairs(upper.panel=NULL)
dev.off()

## There is no clear correlation between X's

# Part b: Fit SLRs
#################################################################################
# load survey package 
library(survey)

# Create a survey weight object
NHANES_all <- svydesign(data=data, id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTINT2YR, nest=TRUE)
NHANES_all_MEC <- svydesign(data=data, id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC2YR, nest=TRUE)

NHANES <- subset(NHANES_all, inAnalysis==1)
NHANES_MEC <- subset(NHANES_all_MEC, inAnalysis==1)


# fit the weighted models and store the results
model_1w <- svyglm(day_sleep~RIAGENDR, design=NHANES, family=quasibinomial(link="log"))
model_2w <- svyglm(day_sleep~ageCat_18, design=NHANES, family=quasibinomial(link="log"))
#center income to federal poverty line ratio at 1
model_3w <- svyglm(day_sleep~INDFMPIR, design=NHANES, family=quasibinomial(link="log"))
model_4w <- svyglm(day_sleep~SMQ681, design=NHANES, family=quasibinomial(link="log"))
model_5w <- svyglm(day_sleep~raceEthCat, design=NHANES, family=quasibinomial(link="log"))
model_6w <- svyglm(day_sleep~activity, design=NHANES, family=quasibinomial(link="log"))
model_7w <- svyglm(day_sleep~hi_caffeine, design=NHANES_MEC, family=quasibinomial(link="log"))
model_8w <- svyglm(day_sleep~BMI_cat, design=NHANES_MEC, family=quasibinomial(link="log"))
model_9w <- svyglm(day_sleep~heavy_drink2, design=NHANES, family=quasibinomial(link="log"))
model_10w <- svyglm(day_sleep~depress_cat, design=NHANES, family=quasibinomial(link="log"))


#exponentiated coefficients

exp(model_1w$coefficients)
exp(model_2w$coefficients)
exp(model_3w$coefficients)
exp(model_4w$coefficients)
exp(model_5w$coefficients)
exp(model_6w$coefficients)
exp(model_7w$coefficients)
exp(model_8w$coefficients)
exp(model_9w$coefficients)
exp(model_10w$coefficients)

# p-values
summary(model_1w)
summary(model_2w)
summary(model_3w)
summary(model_4w)
summary(model_5w)
summary(model_6w)
summary(model_7w)
summary(model_8w)
summary(model_9w)
summary(model_10w)


exp(confint(model_1w))
exp(confint(model_2w))
exp(confint(model_3w))
exp(confint(model_4w))
exp(confint(model_5w))
exp(confint(model_6w))
exp(confint(model_7w))
exp(confint(model_8w))
exp(confint(model_9w))
exp(confint(model_10w))

# fit the unweighted models and store the results

model_1u <- glm(day_sleep~RIAGENDR, family=binomial(link="log"), data = unweighted_data)
model_2u <- glm(day_sleep~ageCat_18, family=binomial(link="log"), data = unweighted_data)
#center income to federal poverty line ratio at 1
model_3u <- glm(day_sleep~INDFMPIR, family=binomial(link="log"), data = unweighted_data)
model_4u <- glm(day_sleep~SMQ681, family=binomial(link="log"), data = unweighted_data)
model_5u <- glm(day_sleep~raceEthCat, family=binomial(link="log"), data = unweighted_data)
model_6u <- glm(day_sleep~activity, family=binomial(link="log"), data = unweighted_data)
model_7u <- glm(day_sleep~hi_caffeine, family=binomial(link="log"), data = unweighted_data)
model_8u <- glm(day_sleep~BMI_cat, family=binomial(link="log"), data = unweighted_data)
model_9u <- glm(day_sleep~heavy_drink2, family=binomial(link="log"), data = unweighted_data)
model_10u <- glm(day_sleep~depress_cat, family=binomial(link="log"), data = unweighted_data)


#exponentiated coefficients

exp(model_1u$coefficients)
exp(model_2u$coefficients)
exp(model_3u$coefficients)
exp(model_4u$coefficients)
exp(model_5u$coefficients)
exp(model_6u$coefficients)
exp(model_7u$coefficients)
exp(model_8u$coefficients)
exp(model_9u$coefficients)
exp(model_10u$coefficients)

# p-values
summary(model_1u)
summary(model_2u)
summary(model_3u)
summary(model_4u)
summary(model_5u)
summary(model_6u)
summary(model_7u)
summary(model_8u)
summary(model_9u)
summary(model_10u)

# Part c: Fit initial MLR using all predictors
################################################################################

# Note: logit models failed to converge. Use poisson model. From the svy package:
# svyglm always returns 'model-robust' standard errors; the Horvitz-Thompson-type 
# standard errors used everywhere in the survey package are a generalisation of 
# the model-robust 'sandwich' estimators. In particular, a quasi-Poisson svyglm 
# will return correct standard errors for relative risk regression models.

# If I use a Poisson regression with a sandwich estimator, am I making a fish sandwich?


# Using Exam weights (recommended)

MLR_1 <- svyglm(day_sleep~RIAGENDR+ INDFMPIR + SMQ681 + ageCat_18+
                           + raceEthCat + activity + hi_caffeine + BMI_cat + 
                             heavy_drink2 + depress_cat, design=NHANES_MEC, 
                           family=quasipoisson(link="log"))

#Using Interview weights 
MLR_2 <- svyglm(day_sleep~RIAGENDR + ageCat_18 + INDFMPIR + SMQ681 
                + raceEthCat + activity + hi_caffeine + BMI_cat + 
                  heavy_drink2 + depress_cat, design=NHANES, 
                family=quasipoisson(link="log"))

#Using no weights

MLR_3 <- glm(day_sleep~RIAGENDR + ageCat_18 + INDFMPIR + SMQ681 
             + raceEthCat + activity + hi_caffeine + BMI_cat + 
               heavy_drink2 + depress_cat, family=poisson(link="log"), data = unweighted_data)


summary(MLR_1, df.resid = degf(NHANES_MEC))
summary(MLR_2, df.resid = degf(NHANES))
summary(MLR_3)
coeftest(MLR_3, vcov = sandwich)

linearHypothesis(MLR_1, "ageCat_1818-39=ageCat_1840-59")
linearHypothesis(MLR_1, "ageCat_1818-39=ageCat_1860+")

linearHypothesis(MLR_3, "ageCat_1818-39=ageCat_1840-59")
linearHypothesis(MLR_3, "ageCat_1818-39=ageCat_1860+")


exp(MLR_1$coefficients)
exp(MLR_2$coefficients)
exp(MLR_3$coefficients)


# The standard errors are the same for the survey and MEC designs so I will default to 
# the survey design

exp(confint(MLR_1))
exp(confint(MLR_2))

#The unweighted regression results are still qualitatively the same. I also am not able to easily perform
# regression diagnostics with the survey weighted glm, so I will proceed with the 
# unweighted model 3 for simplicity. 



## RIAGENDR, ageCat_18, and smoking have high VIFs, which is concerning

## Part d: selection of best model using AIC and a priori

################################################################################
### Best subset selection AIC criterion

analytic_data <- na.omit(unweighted_data)

# can do this all at once with step function when done by AIC
model_0 <- glm(day_sleep ~ 1, data = analytic_data, family = poisson(link = "log"))    # start with null model
mod_final <- step(model_0, scope = ~ . + RIAGENDR + ageCat_18 + INDFMPIR-1 + SMQ681 
                  + activity + hi_caffeine + BMI_cat + raceEthCat +
                    heavy_drink2 + depress_cat, family=poisson(link="log"), 
                  data = analytic_data, direction = "forward", na.rm = TRUE)   # add terms one at a time using AIC

# we see the  final model has depression category, race, and age as the most 
coeftest(mod_final, vcov. = sandwich)

summary(mod_final)

exp(mod_final$coefficients)

#vif - there is no concerning inflation

vif(mod_final)


## Model using behavioral risk factors only (a priori)

# mod_behav <- glm(day_sleep~SMQ681 + activity + hi_caffeine + heavy_drink2, 
  #                     family=poisson(link="log"), data = unweighted_data)

#robust variance
# coeftest(mod_behav, vcov = sandwich)
#coefficients
# exp(mod_behav$coefficients)



# Model combining AIC selected and a priori variables (minus race)

mod_final_behav <- glm(day_sleep~depress_cat + ageCat_18 + SMQ681 + activity + raceEthCat +
                          hi_caffeine + heavy_drink2, family=poisson(link="log"), data = unweighted_data)

summary(mod_final_behav)

coeftest(mod_final_behav, vcov = sandwich)


# Part e: AIC and Goodness of Fit (pearson's chi-sq)
################################################################################

with(mod_final_behav, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail=FALSE)))

with(mod_final, cbind(res.deviance = deviance, df = df.residual,
                            p = pchisq(deviance, df.residual, lower.tail=FALSE)))

with(MLR_3, cbind(res.deviance = deviance, df = df.residual,
                            p = pchisq(deviance, df.residual, lower.tail=FALSE)))





# AIC(mod_final_behav)

AIC(mod_final)

AIC(MLR_3)

AIC(mod_final_behav)


# There was some lost data in the best subset regression, the AIC is not as great as it seems
# when run on the full dataset

Mod_bestsub <- glm(day_sleep ~ ageCat_18 + depress_cat + raceEthCat, data = unweighted_data, family = poisson(link = "log"))

AIC(Mod_bestsub)

# Part f: Analyze residuals
###############################################################################

# Part f-1:  Plot studentized residual -vs- predicted values


# Calculate predicted values and residuals in weighted data
fit_dat <- as.data.frame(MLR_1$fitted.values)
pearson_res_w <- residuals(MLR_1, type = "pearson")
fit_dat <- cbind(fit_dat, pearson_res_w)



fit_dat %>%
  ggplot(aes(x = MLR_1$fitted.values, y = pearson_res_w)) + 
  geom_point() + 
  geom_hline(yintercept = c(-2, 0, 2), col="red") + 
  geom_smooth(method = lm) +
  labs(x = "Fitted values", y = "Pearson Residuals", title = "Residuals vs. Fitted values")

ggsave("pearson_resids_weighted.png", width = 6, height = 4, units = "in")


# Calculate predicted values and residuals in unweighted data

fit_dat2 <- as.data.frame(MLR_3$fitted.values)
pearson_res <- residuals(MLR_3, type = "pearson")
fit_dat2 <- cbind(fit_dat2, pearson_res)

fit_dat2 %>%
  ggplot(aes(x = MLR_3$fitted.values, y = pearson_res)) + 
  geom_point() + 
  geom_hline(yintercept = c(-2, 0, 2), col="red") + 
  geom_smooth(method = lm) +
  labs(x = "Fitted values", y = "Pearson Residuals", title = "Residuals vs. Fitted values")

ggsave("pearson_resids_unweighted.png", width = 6, height = 4, units = "in")

## The loess line is flat, which means there is no trend in the residuals. 
## Equal variances/ linear assumption is met.


# Calculate predicted values and residuals in a priori model

fit_dat_fin <- as.data.frame(mod_final_behav$fitted.values)
pearson_res_fin <- residuals(mod_final_behav, type = "pearson")
fit_dat_fin <- cbind(fit_dat_fin, pearson_res_fin)

fit_dat_fin %>%
  ggplot(aes(x = mod_final_behav$fitted.values, y = pearson_res_fin)) + 
  geom_point() + 
  geom_hline(yintercept = c(-2, 0, 2), col="red") + 
  geom_smooth(method = lm) +
  labs(x = "Fitted values", y = "Pearson Residuals", title = "Residuals vs. Fitted values")

ggsave("pearson_resids_final_behave_model.png", width = 6, height = 4, units = "in")

# Calculate predicted values and residuals in best subset model

#fit_dat_fin <- as.data.frame(mod_final$fitted.values)
#pearson_res_fin <- residuals(mod_final, type = "pearson")
#fit_dat_fin <- cbind(fit_dat_fin, pearson_res_fin)

#fit_dat_fin %>%
 # ggplot(aes(x = mod_final$fitted.values, y = pearson_res_fin)) + 
  #geom_point() + 
  #geom_hline(yintercept = c(-2, 0, 2), col="red") + 
  #geom_smooth(method = lm) +
  #labs(x = "Fitted values", y = "Pearson Residuals", title = "Residuals vs. Fitted values")

#ggsave("pearson_resids_best_sub.png", width = 6, height = 4, units = "in")

## notice how there are fewer data points in the best subset. Omitted participants with any NAs



## Q-Q plot of deviance residuals

fit_dat_fin_apriori <- as.data.frame(mod_final_behav$fitted.values)
deviance_res_apriori <- residuals(mod_final_behav, type = "deviance")
fit_dat_fin_apriori <- cbind(fit_dat_fin_apriori, deviance_res_apriori)

fit_dat_fin_apriori %>%
  ggplot(aes(sample = deviance_res_apriori)) +
  geom_qq() + 
  geom_qq_line() + 
  labs(title = "Q-Q plot of deviance residuals")

ggsave("Q-Q_final_behav_mod.png", width = 6, height = 4, units = "in")


fit_dat_fin_all <- as.data.frame(MLR_3$fitted.values)
deviance_res_fin <- residuals(MLR_3, type = "deviance")
fit_dat_fin_all <- cbind(fit_dat_fin_all, deviance_res_fin)

fit_dat_fin_all %>%
  ggplot(aes(sample = deviance_res_fin)) +
  geom_qq() + 
  geom_qq_line() + 
  labs(title = "Q-Q plot of deviance residuals")

ggsave("Q-Q_all_variables.png", width = 6, height = 4, units = "in")


## Under the correct distribution of the response, we expect the points to align 
## with the diagonal line. It is usual to have departures from the diagonal in 
## the extremes other than in the center, even under normality. Assumption is met.

## Part f-2 : check outliers
##################################################################################

fit_dat_fin %>%
  ggplot() +
  geom_boxplot(aes(y = pearson_res_fin)) +
  labs(y="Pearson residuals", title="Boxplot of pearson residuals")

# Looking at the plot we can filter to see which observation is the outlier
fit_dat_fin %>% 
  filter(pearson_res_fin > 2.5)

ggsave("outlier_residuals_apriori.png", width = 6, height = 4, units = "in")

## Part g : check leverage 
#################################################################################

fit_dat_fin <- fit_dat_fin %>%
  mutate(lev = hatvalues(mod_final_behav))

# Boxplot of leverage values
fit_dat_fin %>%
  ggplot() +
  geom_boxplot(aes(y = lev)) +
  labs(y="Leverage values", title="Boxplot of leverages")


summary(fit_dat_fin$lev)
lev_outliers <- 0.004278 + 1.5*(IQR(fit_dat_fin$lev))

fit_dat_fin %>% 
  filter(lev > lev_outliers)

ggsave("Leverages_apriori.png", width = 6, height = 4, units = "in")

# There are very many leverage outliers

#### Part h: Check model: influence
######################################################################################  

# Part h-1:  Make boxplot and list outliers for influence

# Calculate DFITS influence measure -- effect ith obs on overall fit
# Add them to data set
fit_dat_fin <- fit_dat_fin %>%
  mutate(dfits = dffits(mod_final_behav))

# Boxplot of dfits values
fit_dat_fin %>%
  ggplot() +
  geom_boxplot(aes(y = dfits)) +
  labs(y="DFITS values", title="Boxplot of DFIT values")


ggsave("dfits_apriori.png", width = 6, height = 4, units = "in")

# Look at outliers
fit_dat_fin %>% 
  dplyr::select(dfits) %>%
  filter(dfits <= quantile(dfits, .25) - 1.5*IQR(dfits) | 
           dfits >= quantile(dfits, .75) + 1.5*IQR(dfits)) %>%
  arrange(dfits)


# Part h-2:  Leverage -vs- residual squared (L-R) plot

fit_dat_fin %>%
  ggplot(aes(x = pearson_res_fin^2, y = lev)) + 
  geom_point() +
  geom_hline(yintercept = mean(fit_dat_fin$lev), col="red") +
  geom_vline(xintercept = mean(fit_dat_fin$pearson_res^2), col="red") +
  labs(title="Leverage-Residual Plot")

ggsave("L-R plot apriori.png", width = 6, height = 4, units = "in")


#### Part i: Sensitivity analysis:  Re-fit without influential points
###################################################################################### 

# Create new data set without influential points
unweighted_data_noip <- unweighted_data %>%
  filter(SEQN != 2876, SEQN != 5791, SEQN != 1373, SEQN != 5401, SEQN != 135,
         SEQN != 5150, SEQN != 4173, SEQN != 1345)

model_final_b_no_outliers <- glm(day_sleep~depress_cat + ageCat_18 + SMQ681 + activity + raceEthCat +
                                   hi_caffeine + heavy_drink2, family=poisson(link="log"), data = unweighted_data)

summary(model_final_b_no_outliers)

coeftest(mod_final_behav, vcov = sandwich)

exp(model_final_b_no_outliers$coefficients)
exp(mod_final_behav$coefficients)

## Part j: Check variance inflation
###################################################################################

vif(MLR_3) #check VIF of unweighted model with all risk factors
vif(mod_final) # check VIF of best subset model
vif(mod_final_behav) # check VIF of a priori model

# Table 1
#################################################################################

library(tableone)
listVars <- c("RIAGENDR", "ageCat_18", "INDFMPIR", "SMQ681", "raceEthCat", "activity", "hi_caffeine", "BMI_cat", "heavy_drink2", "depress_cat")
catVars <- c("RIAGENDR", "ageCat_18", "SMQ681", "raceEthCat", "activity", "hi_caffeine", "BMI_cat", "heavy_drink2", "depress_cat")

table1 <- CreateTableOne(vars = listVars, data = unweighted_data, factorVars = catVars, strata=c("day_sleep"))
dotable1 <- print(table1, quote=TRUE, noSpaces=TRUE)

png("Barplot Depression.png")
counts <- table(unweighted_data$depress_cat, unweighted_data$day_sleep)
barplot(counts, main="Depression Category Distribution by Daytime Sleepiness",
        ylab = "Number of Individuals",
        xlab="Daytime Sleepiness", col=c("darkblue","purple","orange","yellow","lightblue"),
        legend = c("No Depression", "Mild Depression", "Moderate Depression", "Moderate/severe Depression", "Severe Depression"), beside=TRUE)
dev.off()

png("Barplot Caffeine.png")
counts <- table(unweighted_data$hi_caffeine, unweighted_data$day_sleep)
barplot(counts, main="High Caffeine Intake by Daytime Sleepiness",
        ylab = "Number of Individuals",
        xlab="Daytime Sleepiness", col=c("darkblue","purple"),
        legend = c("Low Caffeine Intake", "High Caffeine Intake"), beside=TRUE)
dev.off()
