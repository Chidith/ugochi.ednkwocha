####### CHAPTER 4 #################
####### RESULTS ###################
# Set the directory and use the required library 

library(VGAM)
library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(dtplyr)
library(stringr)
library(boot)
library(car)
library(Quantpsyc)
library(Rcmdr)
library(nnet)
library(ggplot2)
library(magrittr)
library(MASS)
library(tree)
library(ISLR)
library(RColorBrewer)
library(broom)
library(ggpubr)
library(AICcmodavg)

# data was imported

data_wave1 <- read.csv("covid-19_wave1_survey_cls.csv")

data_wave2 <- read.csv("covid-19_wave2_survey_cls.csv")

data_wave3 <- read.csv("covid-19_wave3_survey_cls.csv")


View(data_wave1)
View(data_wave2)
View(data_wave3)



##### converting to factor
as.factor(data_wave1$cw1_gad2phq2_1)
as.factor(data_wave1$cw1_gad2phq2_2)
as.factor(data_wave1$cw1_gad2phq2_3)
as.factor(data_wave1$cw1_gad2phq2_4)

####removing the invalid answers
data_wave1 <- data_wave1[!(data_wave1$cw1_gad2phq2_1 %in% c(-1, -8, -9)), ]
cw1_gad2phq2_1 <- table(data_wave1$cw1_gad2phq2_1)
cw1_gad2phq2_1

data_wave1 <- data_wave1[!(data_wave1$cw1_gad2phq2_2 %in% c(-1, -8, -9)), ]
cw1_gad2phq2_2 <- table(data_wave1$cw1_gad2phq2_2)
cw1_gad2phq2_2

data_wave1 <- data_wave1[!(data_wave1$cw1_gad2phq2_3 %in% c(-8, -1, -9)), ]
cw1_gad2phq2_3 <- table(data_wave1$cw1_gad2phq2_3)
cw1_gad2phq2_3

data_wave1 <- data_wave1[!(data_wave1$cw1_gad2phq2_4 %in% c(-8, -1, -9)), ]
cw1_gad2phq2_4 <- table(data_wave1$cw1_gad2phq2_4)
cw1_gad2phq2_4

#### mental health scores ###
# Select the mental health variables
mental_health_vars <- data_wave1[, c("cw1_gad2phq2_1", "cw1_gad2phq2_2", "cw1_gad2phq2_3", "cw1_gad2phq2_4")]

mental_health_vars

# Calculate the average mental health score for each participant. #this was used
data_wave1$avg_mental_health_scoresCW1 <- rowMeans(mental_health_vars, na.rm = TRUE)
data_wave1$avg_mental_health_scoresCW1
table(data_wave1$avg_mental_health_scoresCW1)
data_wave1$avg_mental_health_scoresCW1


#recode negative values to 0 in the individual variables
data_wave1$avg_mental_health_scoresCW1[data_wave1$avg_mental_health_scoresCW1 < 0] <- 0
data_wave1$avg_mental_health_scoresCW1

####mean and standard deviation values of average mental health scores
mean_mhCW1 <- mean(data_wave1$avg_mental_health_scoresCW1)
mean_mhCW1

sd_mhCW1 <- sd(data_wave1$avg_mental_health_scoresCW1)
sd_mhCW1


####convert to factor
as.factor(data_wave2$cw2_gad2phq2_1)
as.factor(data_wave2$cw2_gad2phq2_2)
as.factor(data_wave2$cw2_gad2phq2_3)
as.factor(data_wave2$cw2_gad2phq2_4)

####removing the invalid answers
data_wave2 <- data_wave2[!(data_wave2$cw2_gad2phq2_1 %in% c(-1, -8, -9)), ]
cw2_gad2phq2_1 <- table(data_wave2$cw2_gad2phq2_1)
cw2_gad2phq2_1

data_wave2 <- data_wave2[!(data_wave2$cw2_gad2phq2_2 %in% c(-1, -8, -9)), ]
cw2_gad2phq2_2 <- table(data_wave2$cw2_gad2phq2_2)
cw2_gad2phq2_2

data_wave2 <- data_wave2[!(data_wave2$cw2_gad2phq2_3 %in% c(-8, -1, -9)), ]
cw2_gad2phq2_3 <- table(data_wave2$cw2_gad2phq2_3)
cw2_gad2phq2_3

data_wave2 <- data_wave2[!(data_wave2$cw2_gad2phq2_4 %in% c(-8, -1, -9)), ]
cw2_gad2phq2_4 <- table(data_wave2$cw2_gad2phq2_4)
cw2_gad2phq2_4


##### sum mental health 
mental_health_vars2 <- data_wave2[, c("cw2_gad2phq2_1", "cw2_gad2phq2_2", "cw2_gad2phq2_3", "cw2_gad2phq2_4")]


mental_health_vars2

# Calculate the average mental health score for each participant
data_wave2$avg_mental_health_scoresCW2 <- rowMeans(mental_health_vars2, na.rm = TRUE)
data_wave2$avg_mental_health_scoresCW2
table(data_wave2$avg_mental_health_scoresCW2)
data_wave2$avg_mental_health_scoresCW2

#recode negative values to 0 in the individual variables
data_wave2$avg_mental_health_scoresCW2[data_wave2$avg_mental_health_scoresCW2 < 0] <- 0
data_wave2$avg_mental_health_scoresCW2

####mean and standard deviation values of average mental health scores
mean_mhCW2 <- mean(data_wave2$avg_mental_health_scoresCW2)
mean_mhCW2

sd_mhCW2 <- sd(data_wave2$avg_mental_health_scoresCW2)
sd_mhCW2



#### mental health scores ###
is.factor(data_wave1$cw1_gad2phq2_1)
as.factor(data_wave3$cw3_gad2phq2_1)
as.factor(data_wave3$cw3_gad2phq2_2)
as.factor(data_wave3$cw3_gad2phq2_3)
as.factor(data_wave3$cw3_gad2phq2_4)

####removing the invalid answers
data_wave3 <- data_wave3[!(data_wave3$cw3_gad2phq2_1 %in% c(-1, -8, -9)), ]
cw3_gad2phq2_1 <- table(data_wave3$cw3_gad2phq2_1)
cw3_gad2phq2_1

data_wave3 <- data_wave3[!(data_wave3$cw3_gad2phq2_2 %in% c(-1, -8, -9)), ]
cw3_gad2phq2_2 <- table(data_wave3$cw3_gad2phq2_2)
cw3_gad2phq2_2

data_wave3 <- data_wave3[!(data_wave3$cw3_gad2phq2_3 %in% c(-8, -1, -9)), ]
cw3_gad2phq2_3 <- table(data_wave3$cw3_gad2phq2_3)
cw3_gad2phq2_3

data_wave3 <- data_wave3[!(data_wave3$cw3_gad2phq2_4 %in% c(-8, -1, -9)), ]
cw3_gad2phq2_4 <- table(data_wave3$cw3_gad2phq2_4)
cw3_gad2phq2_4


##sum the mental health variables
mental_health_vars3 <- data_wave3[, c("cw3_gad2phq2_1", "cw3_gad2phq2_2", "cw3_gad2phq2_3", "cw3_gad2phq2_4")]

mental_health_vars3

# Calculate the average mental health score for each participant
data_wave3$avg_mental_health_scoresCW3 <- rowMeans(mental_health_vars3, na.rm = TRUE)
data_wave3$avg_mental_health_scoresCW3
table(data_wave3$avg_mental_health_scoresCW3)
data_wave3$avg_mental_health_scoresCW3


#recode negative values to 0 in the individual variables
data_wave3$avg_mental_health_scoresCW3[data_wave3$avg_mental_health_scoresCW3 < 0] <- 0
data_wave3$avg_mental_health_scoresCW3

####mean and standard deviation values of average mental health scores
mean_mhCW3 <- mean(data_wave3$avg_mental_health_scoresCW3)
mean_mhCW3

sd_mhCW3 <- sd(data_wave3$avg_mental_health_scoresCW3)
sd_mhCW3


####removing the invalid answers for general health outcomes wave 1
data_wave1 <- data_wave1[!(data_wave1$cw1_ghq121 %in% c(-1, -8, -9)), ]
cw1_ghq121 <- table(data_wave1$cw1_ghq121)
cw1_ghq121

data_wave1 <- data_wave1[!(data_wave1$cw1_ghq122 %in% c(-1, -8, -9)), ]
cw1_ghq122 <- table(data_wave1$cw1_ghq122)
cw1_ghq122

data_wave1 <- data_wave1[!(data_wave1$cw1_ghq123 %in% c(-1, -8, -9)), ]
cw1_ghq123 <- table(data_wave1$cw1_ghq123)
cw1_ghq123

data_wave1 <- data_wave1[!(data_wave1$cw1_ghq124 %in% c(-1, -8, -9)), ]
cw1_ghq124 <- table(data_wave1$cw1_ghq124)
cw1_ghq124

data_wave1 <- data_wave1[!(data_wave1$cw1_ghq125 %in% c(-1, -8, -9)), ]
cw1_ghq125 <- table(data_wave1$cw1_ghq125)
cw1_ghq125

data_wave1 <- data_wave1[!(data_wave1$cw1_ghq126 %in% c(-1, -8, -9)), ]
cw1_ghq126 <- table(data_wave1$cw1_ghq126)
cw1_ghq126

data_wave1 <- data_wave1[!(data_wave1$cw1_ghq127 %in% c(-1, -8, -9)), ]
cw1_ghq127 <- table(data_wave1$cw1_ghq127)
cw1_ghq127

data_wave1 <- data_wave1[!(data_wave1$cw1_ghq128 %in% c(-1, -8, -9)), ]
cw1_ghq128 <- table(data_wave1$cw1_ghq128)
cw1_ghq128

data_wave1 <- data_wave1[!(data_wave1$cw1_ghq129 %in% c(-1, -8, -9)), ]
cw1_ghq129 <- table(data_wave1$cw1_ghq129)
cw1_ghq129

data_wave1 <- data_wave1[!(data_wave1$cw1_ghq1210 %in% c(-1, -8, -9)), ]
cw1_ghq1210 <- table(data_wave1$cw1_ghq1210)
cw1_ghq1210

data_wave1 <- data_wave1[!(data_wave1$cw1_ghq1211 %in% c(-1, -8, -9)), ]
cw1_ghq1211 <- table(data_wave1$cw1_ghq1211)
cw1_ghq1211

data_wave1 <- data_wave1[!(data_wave1$cw1_ghq1212 %in% c(-1, -8, -9)), ]
cw1_ghq1212 <- table(data_wave1$cw1_ghq1212)
cw1_ghq1212


### Select the general health variables
general_health1_vars <- data_wave1[, c("cw1_ghq121", "cw1_ghq122", "cw1_ghq123", "cw1_ghq124","cw1_ghq125", 
                                       "cw1_ghq126", "cw1_ghq127", "cw1_ghq128", "cw1_ghq129", "cw1_ghq1210", "cw1_ghq1211", "cw1_ghq1212")]

# Calculate the average mental health score for each participant. #this was used
data_wave1$avg_general_health_scoresCW1 <- rowMeans(general_health1_vars, na.rm = TRUE)
data_wave1$avg_general_health_scoresCW1
table(data_wave1$avg_general_health_scoresCW1)

####mean and standard deviation values of average mental health scores
mean_ghCW1 <- mean(data_wave1$avg_general_health_scoresCW1)
mean_ghCW1

sd_ghCW1 <- sd(data_wave1$avg_general_health_scoresCW1)
sd_ghCW1


####removing the invalid answers for general health outcomes wave 2
data_wave2 <- data_wave2[!(data_wave2$cw2_ghq121 %in% c(-1, -8, -9)), ]
cw2_ghq121 <- table(data_wave2$cw2_ghq121)
cw2_ghq121

data_wave2 <- data_wave2[!(data_wave2$cw2_ghq122 %in% c(-1, -8, -9)), ]
cw2_ghq122 <- table(data_wave2$cw2_ghq122)
cw2_ghq122

data_wave2 <- data_wave2[!(data_wave2$cw2_ghq123 %in% c(-1, -8, -9)), ]
cw2_ghq123 <- table(data_wave2$cw2_ghq123)
cw2_ghq123

data_wave2 <- data_wave2[!(data_wave2$cw2_ghq124 %in% c(-1, -8, -9)), ]
cw2_ghq124 <- table(data_wave2$cw2_ghq124)
cw2_ghq124

data_wave2 <- data_wave2[!(data_wave2$cw2_ghq125 %in% c(-1, -8, -9)), ]
cw2_ghq125 <- table(data_wave2$cw2_ghq125)
cw2_ghq125

data_wave2 <- data_wave2[!(data_wave2$cw2_ghq126 %in% c(-1, -8, -9)), ]
cw2_ghq126 <- table(data_wave2$cw2_ghq126)
cw2_ghq126

data_wave2 <- data_wave2[!(data_wave2$cw2_ghq127 %in% c(-1, -8, -9)), ]
cw2_ghq127 <- table(data_wave2$cw2_ghq127)
cw2_ghq127

data_wave2 <- data_wave2[!(data_wave2$cw2_ghq128 %in% c(-1, -8, -9)), ]
cw2_ghq128 <- table(data_wave2$cw2_ghq128)
cw2_ghq128

data_wave2 <- data_wave2[!(data_wave2$cw2_ghq129 %in% c(-1, -8, -9)), ]
cw2_ghq129 <- table(data_wave2$cw2_ghq129)
cw2_ghq129

data_wave2 <- data_wave2[!(data_wave2$cw2_ghq1210 %in% c(-1, -8, -9)), ]
cw2_ghq1210 <- table(data_wave2$cw2_ghq1210)
cw2_ghq1210

data_wave2 <- data_wave2[!(data_wave2$cw2_ghq1211 %in% c(-1, -8, -9)), ]
cw2_ghq1211 <- table(data_wave2$cw2_ghq1211)
cw2_ghq1211

data_wave2 <- data_wave2[!(data_wave2$cw2_ghq1212 %in% c(-1, -8, -9)), ]
cw2_ghq1212 <- table(data_wave2$cw2_ghq1212)
cw2_ghq1212


### Select the general health variables
general_health2_vars <- data_wave2[, c("cw2_ghq121", "cw2_ghq122", "cw2_ghq123", "cw2_ghq124","cw2_ghq125", 
                                       "cw2_ghq126", "cw2_ghq127", "cw2_ghq128", "cw2_ghq129", "cw2_ghq1210", "cw2_ghq1211", "cw2_ghq1212")]

# Calculate the average mental health score for each participant. #this was used
data_wave2$avg_general_health_scoresCW2 <- rowMeans(general_health2_vars, na.rm = TRUE)
data_wave2$avg_general_health_scoresCW2
table(data_wave2$avg_general_health_scoresCW2)

####mean and standard deviation values of average mental health scores
mean_ghCW2 <- mean(data_wave2$avg_general_health_scoresCW2)
mean_ghCW2

sd_ghCW2 <- sd(data_wave2$avg_general_health_scoresCW2) #standard deviation
sd_ghCW2





########################## Logit regression using glm()function

#First, choose the level of outcome that is preferably to be a baseline and specify this in the relevel function. 


###### preparation of the variables by removing missing values

#convert as factor
data_wave1$cw1_psex <- as.factor(data_wave1$cw1_psex)
data_wave1$cw1_psex
data_wave2$cw2_psex <- as.factor(data_wave2$cw2_psex)
data_wave2$cw2_psex
data_wave3$cw3_psex <- as.factor(data_wave3$cw3_psex)
data_wave3$cw3_psex


### region
data_wave1$cw1_region <- as.factor(data_wave1$cw1_region)
data_wave1$cw1_region
data_wave2$cw2_region <- as.factor(data_wave2$cw2_region)
data_wave2$cw2_region
data_wave3$cw3_region <- as.factor(data_wave3$cw3_region)
data_wave3$cw3_region

### convert the pmeddifw
data_wave3$cw3_pmeddifw_1 <- as.factor(data_wave3$cw3_pmeddifw_1)
data_wave3$cw3_pmeddifw_1
data_wave3$cw3_pmeddifw_2 <- as.factor(data_wave3$cw3_pmeddifw_2)
data_wave3$cw3_pmeddifw_2
data_wave3$cw3_pmeddifw_3 <- as.factor(data_wave3$cw3_pmeddifw_3)
data_wave3$cw3_pmeddifw_3

# Remove values -8 and -1 from cw3_region and cw3_pmeddifw_1
data_wave3 <- data_wave3[!(data_wave3$cw3_region %in% c(-8, -1)), ]
# Check the resulting table
cw3_region <- table(data_wave3$cw3_region)
cw3_region

data_wave3 <- data_wave3[!(data_wave3$cw3_pmeddifw_1 %in% c(-8, -1)), ]
# Check the resulting table
cw3_pmeddifw_1 <- table(data_wave3$cw3_pmeddifw_1)
cw3_pmeddifw_1


##reference a category

data_wave2$cw2_psex <- relevel(data_wave2$cw2_psex,ref = "2")
data_wave3$cw3_psex <- relevel(data_wave3$cw3_psex,ref = "2")
data_wave2$cw2_region <- relevel(data_wave2$cw2_region,ref = "8")
data_wave3$cw3_region <- relevel(data_wave3$cw3_region,ref = "8")


#####Logit regression analysis

hfactor3model <- glm(cw3_pmeddifw_1 ~ cw3_psex 
                     + cw3_region,
                     data = data_wave3, family=binomial(link="logit"))

summary(hfactor3model)

## odds ratios only
exp(coef(hfactor3model))

## odds ratios and 95% CI
exp(cbind(OR = coef(hfactor3model), confint(hfactor3model)))


###### confidence interval using profiled log-likelihood
ci3<- confint(hfactor3model)
ci3
## CIs using standard errors
confint.default(hfactor3model)



######pmeddifw_2 Why had difficulty obtaining prescribed medication: Nobody was able to collect it

data_wave3 <- data_wave3[!(data_wave3$cw3_pmeddifw_2 %in% c(-8, -1)), ]
# Check the resulting table
cw3_pmeddifw_2 <- table(data_wave3$cw3_pmeddifw_2)
cw3_pmeddifw_2

#####Logit regression analysis

hfactor4model <- glm(cw3_pmeddifw_2 ~ cw3_psex 
                     + cw3_region,
                     data = data_wave3, family=binomial(link="logit"))

summary(hfactor4model)

## odds ratios only
exp(coef(hfactor4model))

## odds ratios and 95% CI
exp(cbind(OR = coef(hfactor4model), confint(hfactor4model)))

###### confidence interval using profiled log-likelihood
ci4<- confint(hfactor4model)
ci4
## CIs using standard errors
confint.default(hfactor4model)



######pmeddifw_3 Why had difficulty obtaining prescribed medication: other reasons

data_wave3 <- data_wave3[!(data_wave3$cw3_pmeddifw_3 %in% c(-8, -1)), ]
# Check the resulting table
cw3_pmeddifw_3 <- table(data_wave3$cw3_pmeddifw_3)
cw3_pmeddifw_3

#####Logit regression analysis

hfactor5model <- glm(cw3_pmeddifw_3 ~ cw3_psex 
                     + cw3_region,
                     data = data_wave3, family=binomial(link="logit"))

summary(hfactor5model)

## odds ratios only
exp(coef(hfactor5model))

## odds ratios and 95% CI
exp(cbind(OR = coef(hfactor5model), confint(hfactor5model)))

###### confidence interval using profiled log-likelihood
ci5<- confint(hfactor5model)
ci5

## CIs using standard errors
confint.default(hfactor5model)


#####################################################


########################### Multiple Linear regression for objective 2  
#Scenario 1: Outcome Variable is Continuous (Linear Regression)

#If you have a continuous outcome variable, such as  a measure of mental health, you can use linear regression. 
#In this case, the model will help you understand how changes in predictor variables are associated with changes in the continuous outcome.

#Outcome Variable: Mental Health Score (Continuous)

#Predictor Variables:

#Healthcare Disruption (Quantitative measure of disruption)
#Other relevant predictors ( gender, region)

# lm regression objective two across the three waves on gender and region

####convert as factor
data_wave1$cw1_psex <- as.factor(data_wave1$cw1_psex)
data_wave1$cw1_region <- as.factor(data_wave1$cw1_region)
data_wave2$cw2_psex <- as.factor(data_wave2$cw2_psex)
data_wave2$cw2_region <- as.factor(data_wave2$cw2_region)
data_wave3$cw3_psex <- as.factor(data_wave3$cw3_psex)
data_wave3$cw3_region <- as.factor(data_wave3$cw3_region)

# Remove values -8 and -1 from cw3_region
data_wave1 <- data_wave1[!(data_wave1$cw1_region %in% c(-8, -1)), ]
# Check the resulting table
cw1_region <- table(data_wave1$cw1_region)
cw1_region

# Remove values -8 and -1 from cw3_region and cw3_pmeddifw_1
data_wave2 <- data_wave2[!(data_wave2$cw2_region %in% c(-8, -1)), ]
# Check the resulting table
cw2_region <- table(data_wave2$cw2_region)
cw2_region


######### reference a category
data_wave1$cw1_psex <- relevel(data_wave1$cw1_psex,ref= "2")
data_wave1$cw1_region <- relevel(data_wave1$cw1_region,ref = "8")
data_wave2$cw2_psex <- relevel(data_wave2$cw2_psex,ref= "2")
data_wave2$cw2_region <- relevel(data_wave2$cw2_region,ref = "8")
data_wave3$cw3_psex <- relevel(data_wave3$cw3_psex,ref= "2")
data_wave3$cw3_region <- relevel(data_wave3$cw3_region,ref= "8")


###cw1
Hc_Impact1_model <- lm(avg_mental_health_scoresCW1 ~ cw1_psex 
                       + cw1_region, data = data_wave1)
summary(Hc_Impact1_model)

###confidence interval
ci_Hc1 <- confint(Hc_Impact1_model)
ci_Hc1

###cw2
Hc_Impact2_model <- lm(avg_mental_health_scoresCW2 ~ cw2_psex 
                       + cw2_region, data = data_wave2)
summary(Hc_Impact2_model)

###confidence interval
ci_Hc2 <- confint(Hc_Impact2_model)
ci_Hc2


###cw3
Hc_Impact3_model <- lm(avg_mental_health_scoresCW3 ~ cw3_psex + cw3_region, data = data_wave3)
summary(Hc_Impact3_model)

###confidence interval
ci_Hc3 <- confint(Hc_Impact3_model)
ci_Hc3

######### reference a category
data_wave3$cw3_pmeddifw_1 <- relevel(data_wave3$cw3_pmeddifw_1,ref = "2")
data_wave3$cw3_pmeddifw_2 <- relevel(data_wave3$cw3_pmeddifw_2,ref = "2")
data_wave3$cw3_pmeddifw_3 <- relevel(data_wave3$cw3_pmeddifw_3,ref = "2")


######### multiple linear regression analysis on wave 3

Hc_Impact4_model <- lm(avg_mental_health_scoresCW3 ~ cw3_pmeddifw_1 + cw3_pmeddifw_2 + 
                        cw3_pmeddifw_3 + cw3_psex + cw3_region, data = data_wave3)
summary(Hc_Impact4_model)


####confidence interval values
ci_Hc4 <- confint(Hc_Impact4_model)
ci_Hc4

#### to 3 decimal places
Hc4_r <- signif(Hc4, digits = 3)
Hc_r


#####Assumption in graphs

hist(data_wave1$avg_general_health_scoresCW1, 
     xlab = "General Health Outcomes (Wave 1)",
     main = "")
hist

### wave 2
hist(data_wave2$avg_general_health_scoresCW2, 
     xlab = "General Health Outcomes (Wave 2)",
     main = "")
hist


#####
data_wave3[, "cw3_region"] <- data_wave3$cw3_region %>% factor()

plot(avg_mental_health_scoresCW1 ~ cw3_psex, data = data_wave1) +
plot(avg_mental_health_scoresCW3 ~ cw3_region, data = data_wave3)

plot(avg_mental_health_scoresCW1 ~ cw1_psex, data = data_wave1,
     ylab = "Mental Health Scores",
     xlab = "Respondent Sex (Wave 1)")

plot(avg_mental_health_scoresCW2 ~ cw2_psex, data = data_wave2,
     ylab = "Mental Health Scores",
     xlab = "Respondent Sex (Wave 2)")

plot(avg_mental_health_scoresCW3 ~ cw3_psex, data = data_wave3,
     ylab = "Mental Health Scores",
     xlab = "Respondent Sex (Wave 3)")


# Create a factor variable for x-axis with custom labels
data_wave2$cw2_psex_label <- factor(data_wave2$cw2_psex, levels = c(-1, 1, 2), labels = c("Preferred not to say", "Male", "Female"))

# Create a factor variable for y-axis with custom labels
data_wave2$mental_health_label <- factor(data_wave2$avg_mental_health_scoresCW2, levels = c(1, 2, 3, 4), labels = c("Low", "Moderate", "Medium", "High"))

# Create the plot
ggplot(data_wave2, aes(x = cw2_psex_label, y = mental_health_label)) +
  geom_jitter() +
  labs(
    y = "Mental Health Scores",
    x = "Respondent Sex (Wave 2)"
  ) +
  scale_x_discrete(
    breaks = c("Preferred not to say", "Male", "Female"),
    labels = c("-1" = "Preferred not to say", "1" = "Male", "2" = "Female")
  ) +
  scale_y_discrete(
    breaks = c("Low", "Moderate", "Medium", "High"),
    labels = c("1" = "Low", "2" = "Moderate", "3" = "Medium", "4" = "High")
  ) +
  theme_minimal()


# Create the plot
ggplot(data_wave2, aes(x = factor(cw2_psex, levels = c(-1, 1, 2)), y = avg_mental_health_scoresCW2)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightblue") +
  labs(
    y = "Mental Health Scores",
    x = "Respondent Sex (Wave 2)"
  ) +
  scale_x_discrete(
    breaks = c(-1, 1, 2),
    labels = c("-1" = "Preferred not to say", "1" = "Male", "2" = "Female")
  ) +
  theme_minimal()




#### Homoscedasticity check the regression analysis

par(mfrow=c(2,2))
plot(Hc_Impact4_model)
mtext("Homoscedasticity check on impact of healthcare access disruptions on Mental Health Scores",                   # Add main title
      side = 3,
      line = - 2,
      outer = TRUE)
par(mfrow=c(1,1))


HcImp.graph<-ggplot(data_wave3, aes(x=cw3_psex, y=avg_mental_health_scoresCW3))+
  geom_point()
HcImp.graph

HcImp.graph <- HcImp.graph + geom_smooth(method="lm", col="blue")

HcImp.graph


HcImp.graph <- HcImp.graph +
  stat_regline_equation(label.x = 3, label.y = 7)

HcImp.graph

HcImp.graph +
  theme_bw() +
  labs(title = "Average Mental health by gender",
       x = "Sex",
       y = "Average mental health scores") +
       scale_x_discrete(breaks = c(1, 2), labels = c("1" = "Male",
                                                    "2" = "Female")) 
                                                  

######################variable frequencies visualization
# Load necessary libraries
library(ggplot2)

# Assuming you have a data frame 'data' with a 'Region' variable
ggplot(data_wave3, aes(x = cw3_region)) +
  geom_bar() +
  labs(x = "Region", y = "Frequency") +
  theme_minimal()

ggplot(data_wave3, aes(x = cw3_psex)) +
  geom_bar() +
  labs(x = "Sex", y = "Frequency") +
  theme_minimal()

ggplot(data_wave3, aes(x = avg_mental_health_scoresCW3)) +
  geom_bar() +
  labs(x = "Average mental health scores",
       y = "Frequency") +
  scale_x_discrete(breaks = c(1, 2, 3 ,4), labels = c("1" = "Low",
                                                      "2" = "Moderate",
                                                      "3" = "Medium",
                                                      "4" = "High"))+ 
  theme_minimal() 
                                                
ggplot(data_wave3, aes(x = cw3_pmeddifw_2)) +
    geom_bar() +
    labs(x = "shortage of Supply", y = "Frequency") +
    theme_minimal()
#########



#####################enhanced

# Sample data frame 'data' with a 'Region' variable
data <- data.frame(
  Region = c("North East", "North West", "Yorkshire and the Humber", 
             "East Midlands", "West Midlands", "East of England", 
             "London", "South East", "South West", "Wales", "Scotland", 
             "Northern Ireland"),
  Frequency = c(938, 2474, 1921, 1820, 2093, 2446, 2374, 3769, 2316, 1769, 2119, 558) 
                #10, 5)
)
#"Don't Know", "Not Applicable"

# Define custom colors for the regions
region_colors <- c(
  "North East" = "green",
  "North West" = "lightgreen",
  "Yorkshire and the Humber" = "pink",
  "East Midlands" = "red",
  "West Midlands" = "lightblue",
  "East of England" = "lightcoral",
  "London" = "purple",
  "South East" = "gold",
  "South West" = "grey",
  "Wales" = "darkgreen",
  "Scotland" = "blue",
  "Northern Ireland" = "darkred")
 

# "Don't Know" = "gray",
  "Not Applicable" = "lightgray"
#)

# Create the bar chart with custom colors
ggplot(data, aes(x = Region, y = Frequency, fill = Region)) +
  geom_bar(stat = "identity") +
  
  # Customize colors and add legend
  scale_fill_manual(values = region_colors) +
  labs(x = "Region", y = "Frequency", title = "Regions (Wave 3)") +
  
  # Customize the theme
  theme_minimal() +
  
  # Rotate x-axis labels for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # Add a legend and customize its position
  theme(legend.position = "right") +
  
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  
  # Adjust plot dimensions
  theme(plot.margin = margin(10, 10, 10, 10))



####wave2
data <- data.frame(
  Region = c("North East", "North West", "Yorkshire and the Humber", 
             "East Midlands", "West Midlands", "East of England", 
             "London", "South East", "South West", "Wales", "Scotland", 
             "Northern Ireland"),
  Frequency = c(801, 2054, 1661, 1521, 1747, 2014, 1940, 3178, 1989, 1488, 1921,346) 
  #10, 5)
)
#"Don't Know", "Not Applicable"

# Define custom colors for the regions
region_colors <- c(
  "North East" = "skyblue",
  "North West" = "lightgreen",
  "Yorkshire and the Humber" = "pink",
  "East Midlands" = "red",
  "West Midlands" = "lightblue",
  "East of England" = "lightcoral",
  "London" = "purple",
  "South East" = "gold",
  "South West" = "grey",
  "Wales" = "darkgreen",
  "Scotland" = "blue",
  "Northern Ireland" = "darkred")


# "Don't Know" = "gray",
"Not Applicable" = "lightgray"
#)

# Create the bar chart with custom colors
ggplot(data, aes(x = Region, y = Frequency, fill = Region)) +
  geom_bar(stat = "identity") +
  
  # Customize colors and add legend
  scale_fill_manual(values = region_colors) +
  labs(x = "Region", y = "Frequency", title = "Regions (Wave 2)") +
  
  # Customize the theme
  theme_minimal() +
  
  # Rotate x-axis labels for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # Add a legend and customize its position
  theme(legend.position = "right") +
  
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  
  # Adjust plot dimensions
  theme(plot.margin = margin(10, 10, 10, 10))

####wave1

data <- data.frame(
  Region = c("North East", "North West", "Yorkshire and the Humber", 
             "East Midlands", "West Midlands", "East of England", 
             "London", "South East", "South West", "Wales", "Scotland"), 
             #Northern Ireland"),
  Frequency = c(540, 1402, 1081, 1030, 1099, 1453, 1223, 2409, 1392, 1020, 1315) 
  
)


# Define custom colors for the regions
region_colors <- c(
  "North East" = "gray",
  "North West" = "lightgreen",
  "Yorkshire and the Humber" = "pink",
  "East Midlands" = "red",
  "West Midlands" = "lightblue",
  "East of England" = "lightcoral",
  "London" = "purple",
  "South East" = "gold",
  "South West" = "grey",
  "Wales" = "darkgreen",
  "Scotland" = "blue",
  "Northern Ireland" = "darkred")


# Create the bar chart with custom colors
ggplot(data, aes(x = Region, y = Frequency, fill = Region)) +
  geom_bar(stat = "identity") +
  
  # Customize colors and add legend
  scale_fill_manual(values = region_colors) +
  labs(x = "Region", y = "Frequency", title = "Regions (Wave 1)") +
  
  # Customize the theme
  theme_minimal() +
  
  # Rotate x-axis labels for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # Add a legend and customize its position
  theme(legend.position = "right") +
  
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  
  # Adjust plot dimensions
  theme(plot.margin = margin(10, 10, 10, 10))


####### 3 waves 

################################################################
 
## correct graph combinatio of the 3 waves

# Your data frame and preprocessing steps
data_waves <- data.frame(
  Region = c("North East", "North West", "Yorkshire and the Humber", 
             "East Midlands", "West Midlands", "East of England", 
             "London", "South East", "South West", "Wales", "Scotland", 
             "Northern Ireland"),
  Wave1 = c(540, 1402, 1081, 1030, 1099, 1453, 1223, 2409, 1392, 1020, 1315, NA),
  Wave2 = c(801, 2054, 1661, 1521, 1747, 2014, 1940, 3178, 1989, 1488, 1921, 346),
  Wave3 = c(938, 2474, 1921, 1820, 2093, 2446, 2374, 3769, 2316, 1769, 2119, 558)
)

# Replace missing values with appropriate placeholders, e.g., NA
data_waves[is.na(data_waves)] <- 0  # Replace NA with 0 or any other appropriate value

# Reshape the data for plotting
data_long <- pivot_longer(data_waves, cols = -Region, names_to = "Wave", values_to = "Frequency")

# Create a bar plot with rotated x-axis labels, adjusted Frequency axis, and sorted x-axis
ggplot(data_long, aes(x = fct_reorder(Region, Frequency), y = Frequency, fill = Wave)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Region", y = "Frequency") +
  scale_fill_manual(values = c("Wave1" = "lightblue", "Wave2" = "lightgreen", "Wave3" = "lightcoral")) +
  theme_minimal() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() +  # Flip the coordinates for horizontal bars
  scale_y_continuous(breaks = seq(0, max(data_long$Frequency), by = 500))  # Adjust the Frequency axis intervals


########mental/region

# Define the mental health scores and corresponding levels
mental_health_scores <- c(
  1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.25, 3.5, 3.75, 4
)

mental_health_levels <- c(
  "Low", "Low", "Low", "Low",
  "Moderate", "Moderate", "Moderate", "Moderate",
  "Medium", "Medium", "Medium", "Medium",
  "High"
)

# Create a data frame for mental health levels
mental_health_data <- data.frame(
  MentalHealthScore = mental_health_scores,
  MentalHealthLevel = mental_health_levels
)

# Create data for the three waves
wave_data <- data.frame(
  Wave1 = c(6391, 2329, 1874, 1282, 1243, 566,  479,  321,  291,  167,  147, 93, 183),
  Wave2 = c(1044,  439,  462,  361,  364, 170,  149,  112,  120,   71,   58, 36, 89),
  Wave3 = c(1071,  556,  548,  386,  514, 239,  169,  111,  149,   74,  103, 40,  126)
)

# Combine the mental health level data with the wave data
mental_health_wave_data <- cbind(mental_health_data, wave_data)

# Print the resulting data frame
print(mental_health_wave_data)

# Calculate mean and standard deviation for each wave
mean_wave1 <- mean(mental_health_wave_data$Wave1)
sd_wave1 <- sd(mental_health_wave_data$Wave1)

mean_wave2 <- mean(mental_health_wave_data$Wave2)
sd_wave2 <- sd(mental_health_wave_data$Wave2)

mean_wave3 <- mean(mental_health_wave_data$Wave3)
sd_wave3 <- sd(mental_health_wave_data$Wave3)

# Create a data frame for the means and SDs
wave_stats <- data.frame(
  Wave = c("Wave1", "Wave2", "Wave3"),
  Mean = c(mean_wave1, mean_wave2, mean_wave3),
  SD = c(sd_wave1, sd_wave2, sd_wave3)
)

# Create a bar plot with error bars
ggplot(wave_stats, aes(x = Wave, y = Mean, fill = Wave)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "Menatl Health Scores", y = "Mean ± SD") +
  scale_fill_manual(values = c("Wave1" = "lightblue", "Wave2" = "red", "Wave3" = "lightcoral")) +
  theme_minimal() +
  theme(legend.position = "right")

#####################################################


# Define the mental health scores and corresponding levels
mental_health_scores <- c(
  1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.25, 3.5, 3.75, 4
)

mental_health_levels <- c(
  "Low", "Low", "Low", "Low",
  "Moderate", "Moderate", "Moderate", "Moderate",
  "Medium", "Medium", "Medium", "Medium",
  "High"
)

# Create a data frame for mental health levels
mental_health_data <- data.frame(
  MentalHealthScore = mental_health_scores,
  MentalHealthLevel = mental_health_levels
)

# Create data for the three waves
wave_data <- data.frame(
  Wave1 = c(6391, 2329, 1874, 1282, 1243, 566,  479,  321,  291,  167,  147, 93, 183),
  Wave2 = c(1044,  439,  462,  361,  364, 170,  149,  112,  120,   71,   58, 36, 89),
  Wave3 = c(1071,  556,  548,  386,  514, 239,  169,  111,  149,   74,  103, 40,  126)
)

# Combine the mental health level data with the wave data
mental_health_wave_data <- cbind(mental_health_data, wave_data)

# Reshape the data for plotting
mental_health_wave_data_long <- mental_health_wave_data %>%
  gather(Wave, Value, -MentalHealthScore, -MentalHealthLevel)

# Calculate mean and standard deviation within ggplot
mental_health_wave_data_long_summary <- mental_health_wave_data_long %>%
  group_by(MentalHealthLevel, Wave) %>%
  summarize(
    Mean = mean(Value),
    SD = sd(Value)
  )

# Create a bar plot with error bars and text labels for mean and sd
ggplot(mental_health_wave_data_long_summary, aes(x = MentalHealthLevel, y = Mean, fill = Wave)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                position = position_dodge(width = 0.9), width = 0.2) +
  geom_text(aes(label = paste("Mean =", round(Mean, 2), "\nSD =", round(SD, 2))),
            vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(x = "Mental Health Level", y = "Mental Health Score") +
  scale_fill_manual(values = c("Wave1" = "lightblue", "Wave2" = "red", "Wave3" = "lightcoral")) +
  theme_minimal() +
  theme(legend.position = "right")



# This can be used. Create a beautiful bar plot with error bars and text labels for mean and sd
ggplot(mental_health_wave_data_long_summary, aes(x = factor(MentalHealthLevel, levels = c("Low", "Moderate", "Medium", "High")), y = Mean, fill = Wave)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD, width = 0.2), 
                position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste("Mean =", round(Mean, 2), "\nSD =", round(SD, 2))),
            vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(x = "Mental Health Level", y = "Mental Health Score") +
  scale_fill_manual(values = c("Wave1" = "lightblue", "Wave2" = "red", "Wave3" = "lightcoral")) +
  theme_minimal() +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.title=element_blank())  # Remove legend title


########################### graphs for general health outcomes wave 3
avg_general_health_grades <- c(1, 1.08, 1.16, 1.25, 1.33, 1.41, 1.5, 1.58, 1.66, 1.75, 1.83, 1.91, 2, 2.08, 2.16, 2.25, 2.33, 2.41, 2.5, 2.58, 2.66, 2.75, 2.83, 2.91, 3, 3.08, 3.16, 3.25, 3.33, 3.41,3.5, 3.58, 3.66, 3.75, 3.83, 3.91, 4)
avg_general_health_scores <- c(3, 3, 8, 11, 32, 62, 132, 159, 224, 259, 321, 377, 430, 313, 240, 194, 190, 180, 142, 134, 102, 87, 84, 59, 42, 51, 37, 41, 26, 20, 23, 20, 15, 16, 2, 2, 1)

avg_general_health_levels <- c(
  "Low", "Low", "Low", "Low", "Low" , "Low", "Low", "Low", "Low", "Low", 
  "Low", "Low", "Moderate", "Moderate", "Moderate", "Moderate",
  "Moderate", "Moderate", "Moderate", "Moderate","Moderate", "Moderate", "Moderate", "Moderate",
  "Medium", "Medium", "Medium", "Medium", "Medium", "Medium", "Medium", "Medium",
  "Medium", "Medium", "Medium", "Medium",
  "High"
)

# Load the ggplot2 library if not already loaded
library(ggplot2)

# Create a data frame with the provided data
avg_general_health_data <- data.frame(
  Level = avg_general_health_grades,
  Score = avg_general_health_scores,
  Health_Level = avg_general_health_levels
)

# Plot the frequency using ggplot
ggplot(avg_general_health_data, aes(x = Health_Level, fill = Health_Level)) +
  geom_bar() +
  scale_fill_manual(values = c(
    "Low" = "lightblue", "Moderate" = "lightgreen",
    "Medium" = "lightcoral", "High" = "lightyellow"
  )) +
  labs(x = "General Health Levels", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_blank())


#### convert as factor becareful
data_wave3$cw3_ghq121 <- as.factor(data_wave3$cw3_ghq121)
data_wave3$cw3_ghq122 <- as.factor(data_wave3$cw3_ghq122)
data_wave3$cw3_ghq123 <- as.factor(data_wave3$cw3_ghq123)
data_wave3$cw3_ghq124 <- as.factor(data_wave3$cw3_ghq124)
data_wave3$cw3_ghq125 <- as.factor(data_wave3$cw3_ghq125)
data_wave3$cw3_ghq126 <- as.factor(data_wave3$cw3_ghq126)
data_wave3$cw3_ghq127 <- as.factor(data_wave3$cw3_ghq127)
data_wave3$cw3_ghq128 <- as.factor(data_wave3$cw3_ghq128)
data_wave3$cw3_ghq129 <- as.factor(data_wave3$cw3_ghq129)
data_wave3$cw3_ghq1210 <- as.factor(data_wave3$cw3_ghq1210)
data_wave3$cw3_ghq1211 <- as.factor(data_wave3$cw3_ghq1211)
data_wave3$cw3_ghq1212 <- as.factor(data_wave3$cw3_ghq1212)



#### ANOVA test analysis objective 3
####removing the invalid answers for general health outcomes
data_wave3 <- data_wave3[!(data_wave3$cw3_ghq121 %in% c(-1, -8, -9)), ]
cw3_ghq121 <- table(data_wave3$cw3_ghq121)
cw3_ghq121

data_wave3 <- data_wave3[!(data_wave3$cw3_ghq122 %in% c(-1, -8, -9)), ]
cw3_ghq122 <- table(data_wave3$cw3_ghq122)
cw3_ghq122

data_wave3 <- data_wave3[!(data_wave3$cw3_ghq123 %in% c(-1, -8, -9)), ]
cw3_ghq123 <- table(data_wave3$cw3_ghq123)
cw3_ghq123

data_wave3 <- data_wave3[!(data_wave3$cw3_ghq124 %in% c(-1, -8, -9)), ]
cw3_ghq124 <- table(data_wave3$cw3_ghq124)
cw3_ghq124

data_wave3 <- data_wave3[!(data_wave3$cw3_ghq125 %in% c(-1, -8, -9)), ]
cw3_ghq125 <- table(data_wave3$cw3_ghq125)
cw3_ghq125

data_wave3 <- data_wave3[!(data_wave3$cw3_ghq126 %in% c(-1, -8, -9)), ]
cw3_ghq126 <- table(data_wave3$cw3_ghq126)
cw3_ghq126

data_wave3 <- data_wave3[!(data_wave3$cw3_ghq127 %in% c(-1, -8, -9)), ]
cw3_ghq127 <- table(data_wave3$cw3_ghq127)
cw3_ghq127

data_wave3 <- data_wave3[!(data_wave3$cw3_ghq128 %in% c(-1, -8, -9)), ]
cw3_ghq128 <- table(data_wave3$cw3_ghq128)
cw3_ghq128

data_wave3 <- data_wave3[!(data_wave3$cw3_ghq129 %in% c(-1, -8, -9)), ]
cw3_ghq129 <- table(data_wave3$cw3_ghq129)
cw3_ghq129

data_wave3 <- data_wave3[!(data_wave3$cw3_ghq1210 %in% c(-1, -8, -9)), ]
cw3_ghq1210 <- table(data_wave3$cw3_ghq1210)
cw3_ghq1210

data_wave3 <- data_wave3[!(data_wave3$cw3_ghq1211 %in% c(-1, -8, -9)), ]
cw3_ghq1211 <- table(data_wave3$cw3_ghq1211)
cw3_ghq1211

data_wave3 <- data_wave3[!(data_wave3$cw3_ghq1212 %in% c(-1, -8, -9)), ]
cw3_ghq1212 <- table(data_wave3$cw3_ghq1212)
cw3_ghq1212


### Select the general health variables
general_health3_vars <- data_wave3[, c("cw3_ghq121", "cw3_ghq122", "cw3_ghq123", "cw3_ghq124","cw3_ghq125", 
                                       "cw3_ghq126", "cw3_ghq127", "cw3_ghq128", "cw3_ghq129", "cw3_ghq1210", "cw3_ghq1211", "cw3_ghq1212")]

# Calculate the average mental health score for each participant. #this was used
data_wave3$avg_general_health_scoresCW3 <- rowMeans(general_health3_vars, na.rm = TRUE)
data_wave3$avg_general_health_scoresCW3
table(data_wave3$avg_general_health_scoresCW3)

####mean and standard deviation values of average mental health scores
mean_ghCW3 <- mean(data_wave3$avg_general_health_scoresCW3)
mean_ghCW3

sd_ghCW3 <- sd(data_wave3$avg_general_health_scoresCW3)
sd_ghCW3


###converting variables to factors
data_wave3$cw3_appcant_1 <- as.factor(data_wave3$cw3_appcant_1)
data_wave3$cw3_appcant_1
data_wave3$cw3_appcant_2 <- as.factor(data_wave3$cw3_appcant_2)
data_wave3$cw3_appcant_2
data_wave3$cw3_appcant_3 <- as.factor(data_wave3$cw3_appcant_3)
data_wave3$cw3_appcant_3
data_wave3$cw3_appcant_4 <- as.factor(data_wave3$cw3_appcant_4)
data_wave3$cw3_appcant_4
data_wave3$cw3_appmed_1 <- as.factor(data_wave3$cw3_appmed_1)
data_wave3$cw3_appmed_1
data_wave3$cw3_appmed_2 <- as.factor(data_wave3$cw3_appmed_2)
data_wave3$cw3_appmed_2
data_wave3$cw3_appmed_3 <- as.factor(data_wave3$cw3_appmed_3)
data_wave3$cw3_appmed_3
data_wave3$cw3_appmed_4 <- as.factor(data_wave3$cw3_appmed_4)
data_wave3$cw3_appmed_4


with(data_wave3, table(avg_general_health_scoresCW3, cw3_appcant_1))

######## find the best fit

library(AICcmodavg)

#######levene test

leveneTest(data_wave3$avg_general_health_scoresCW3, data_wave3$cw3_appcant_3, center = median)

leveneTest(data_wave3$avg_general_health_scoresCW3, data_wave3$cw3_appcant_3, center = mean)

####### respondents with diffculty
with_health_access <- aov(avg_general_health_scoresCW3 ~  cw3_appcant_1 + cw3_appcant_2 + 
                            cw3_appcant_3 + cw3_appcant_4, data = data_wave3)
# Print the summary of ANOVA
summary(with_health_access)


Iwith_health_access <- aov(avg_general_health_scoresCW3 ~ cw3_appcant_1 + cw3_appcant_2 + 
                             cw3_appcant_3 + cw3_appcant_4*factor(cw3_psex) *(cw3_region), data = data_wave3)
# Print the summary of ANOVA
summary(Iwith_health_access)

Bwith_health_access <- aov(avg_general_health_scoresCW3 ~ cw3_appcant_1 + cw3_appcant_2 + 
                             cw3_appcant_3 + cw3_appcant_4 + factor(cw3_psex) + factor(cw3_region), data = data_wave3)
# Print the summary of ANOVA
summary(Bwith_health_access)

# Perform Tukey's HSD test
w_tukey_result <- TukeyHSD(with_health_access)

# Print the Tukey's HSD results
print(w_tukey_result)

###plot
table(data_wave3$avg_general_health_scoresCW3) %>% as.data.frame() %>%
  ggplot(aes(x = Var1, y = Freq)) + 
  geom_col() +
  labs(x = "Average General Health Outcomes (Wave 3)", y = "Frequency") + #rename x and y axes
  scale_x_discrete(breaks = c(1, 2, 3 ,4), labels = c("1" = "Low",
                                                      "2" = "Moderate",
                                                      "3" = "Medium",
                                                      "4" = "High")) +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "lightgreen", "3" = "lightcoral", "4" = "lightyellow")) +
  theme_minimal()
#saivng plot
  #ggsave("GH3.png", plot = table, device = "png")


######## find the best fit

model.set <- list(with_health_access, Iwith_health_access, Bwith_health_access)
model.names <- c("with_health_access", "Iwith_health_access", "Bwith_health_access")

aictab(model.set, modnames = model.names)

####check for homoscedasticity

par(mfrow=c(2,2))
plot(Bwith_health_access)
mtext("Homoscedasticity check for Interactions between Average General Health and healthcare access difficulty  ",                   # Add main title
      side = 3,
      line = - 2,
      outer = TRUE)
par(mfrow=c(1,1))

data_wave3[, "cw3_appcant_1"] <- data_wave3$cw3_appcant_1 %>% factor()
par(mfrow = c(2,2))
boxplot(data_wave3$avg_general_health_scoresCW3 ~ data_wave3$cw3_appcant_1, 
        main="Healthcare Difficulty",
        xlab = "consultation cancelled", 
        ylab = "AVg General health scores",
        sub = "General Health Outcomes",
        font.main=2, font.lab=2, font.sub=2)
 
boxplot(data_wave3$avg_general_health_scoresCW3 ~ data_wave3$cw3_appcant_2, 
        main="Healthcare Difficulty",
        xlab = "Surgery cancelled", 
        ylab = "Avg General health scores",
        sub = "General Health Outcomes",
        font.main=2, font.lab=2, font.sub=2)

boxplot(data_wave3$avg_general_health_scoresCW3 ~ data_wave3$cw3_appcant_3, 
        main="Healthcare Difficulty",
        xlab = "Psychological therapy cancelled", 
        ylab = "AVg General health scores",
        sub = "General Health Outcomes",
        font.main=2, font.lab=2, font.sub=2)
boxplot(data_wave3$avg_general_health_scoresCW3 ~ data_wave3$cw3_appmed_3, 
        main="Healthcare Difficulty",
        xlab = "Hospital appointment for consultation,treatment cancelled", 
        ylab = "AVg General health scores",
        sub = "General Health Outcomes",
        font.main=2, font.lab=2, font.sub=2)

##########groupwise differences

tukey.plot1.aov<-aov(avg_general_health_scoresCW3 ~ cw3_appcant_3:cw3_appcant_4, data=data_wave3)
tukey.plot1.test<-TukeyHSD(tukey.plot1.aov)
plot(tukey.plot1.test, las = 1)

tukey.plot2.aov<-aov(avg_general_health_scoresCW3 ~ cw3_appmed_2:cw3_appmed_4, data=data_wave3)
tukey.plot2.test<-TukeyHSD(tukey.plot2.aov)
plot(tukey.plot2.test, las = 1)

tukey.plot3.aov<-aov(avg_general_health_scoresCW3 ~ cw3_appmed_1:cw3_appmed_4, data=data_wave3)
tukey.plot3.test<-TukeyHSD(tukey.plot3.aov)
plot(tukey.plot3.test, las = 1)

####### respondents without difficulty

without_health_access1 <- aov(avg_general_health_scoresCW3 ~ factor(cw3_appmed_1) + factor(cw3_appmed_2) 
                              + factor(cw3_appmed_3) + factor(cw3_appmed_4), data = data_wave3)
# Print the summary of ANOVA
summary(without_health_access1)
#without_health_access1 <- aov(avg_general_health_scoresCW3 ~ factor(cw3_appmed_3) * cw3_psex 
                              #*cw3_region, data = data_wave3)
# Print the summary of ANOVA
summary(without_health_access1)

Iwithout_health_access1 <- aov(avg_general_health_scoresCW3 ~ cw3_appmed_1 + cw3_appmed_2 
                               + cw3_appmed_3 + cw3_appmed_4*cw3_psex *cw3_region, data = data_wave3)
# Print the summary of ANOVA
summary(Iwithout_health_access1)

Bwithout_health_access1 <- aov(avg_general_health_scoresCW3 ~ cw3_appmed_1 + cw3_appmed_2 
                               + cw3_appmed_3 + cw3_appmed_4 + cw3_psex + cw3_region, data = data_wave3)
# Print the summary of ANOVA
summary(Bwithout_health_access1)

# Perform Tukey's HSD test
wo_tukey_result <- TukeyHSD(without_health_access1)

# Print the Tukey's HSD results
print(wo_tukey_result)

unique(data_wave3$cw3_appmed_1)


######## find the best fit

model.set <- list(without_health_access, Iwithout_health_access, Bwithout_health_access)
model.names <- c("without_health_access", "Iwithout_health_access", "Bwithout_health_access")

aictab(model.set, modnames = model.names)

##########groupwise differences

tukey.plot1.aov<-aov(avg_general_health_scoresCW3 ~ cw3_appmed_3:cw3_appmed_4, data=data_wave3)
tukey.plot.test<-TukeyHSD(tukey.plot1.aov)
plot(tukey.plot1.test, las = 1)

tukey.plot2.aov<-aov(avg_general_health_scoresCW3 ~ cw3_appmed_2:cw3_appmed_4, data=data_wave3)
tukey.plot.test<-TukeyHSD(tukey.plot2.aov)
plot(tukey.plot2.test, las = 1)





##################################################################################################
#####t.test



##remove invalid answers and recode
 
############# Type of medical appointment cancelled or delayed: Hospital appointment for consultation, investigation or treatment 
data_wave3$cw3_appcant_1 <- ifelse(data_wave3$cw3_appcant_1 == 1, "No Access to Healthcare",
                                   ifelse(data_wave3$cw3_appcant_1 == 2, "Access to Healthcare", NA))

table(data_wave3$cw3_appcant_1)
unique(data_wave3$cw3_appcant_1)

####### Type of medical appointment cancelled or delayed: Hospital appointment for surgery. 
data_wave3$cw3_appcant_2 <- ifelse(data_wave3$cw3_appcant_2 == 1, "No Access to Healthcare",
                                   ifelse(data_wave3$cw3_appcant_2 == 2, "Access to Healthcare", NA))
table(data_wave3$cw3_appcant_2)

#######   Type of medical appointment cancelled or delayed: Appointment for CBT, counselling or psychological therapy
data_wave3$cw3_appcant_3 <- ifelse(data_wave3$cw3_appcant_3 == 1, "No Access to Healthcare",
                                   ifelse(data_wave3$cw3_appcant_3 == 2, "Access to Healthcare", NA))
table(data_wave3$cw3_appcant_3)


##appmed

###### Medical appointments booked: Hospital appointment for consultation, investigation, or treatment. 
# Replace 1 with "Access to Healthcare"
data_wave3$cw3_appmed_1[data_wave3$cw3_appmed_1 == 1] <- "Access to Healthcare"

# Replace 2 with "No Access to Healthcare"
data_wave3$cw3_appmed_1[data_wave3$cw3_appmed_1 == 2] <- "No Access to Healthcare"

# Replace other values (e.g., -8, -9, -1) with NA
data_wave3$cw3_appmed_1[data_wave3$cw3_appmed_1 %in% c(-8, -9, -1)] <- NA

# Check unique values
unique(data_wave3$cw3_appmed_1)
table((data_wave3$cw3_appmed_1))


####### Medical appointments booked: Hospital appointment for surgery. 
# Replace 1 with "Access to Healthcare"
data_wave3$cw3_appmed_2[data_wave3$cw3_appmed_2 == 1] <- "Access to Healthcare"

# Replace 2 with "No Access to Healthcare"
data_wave3$cw3_appmed_2[data_wave3$cw3_appmed_2 == 2] <- "No Access to Healthcare"

# Replace other values (e.g., -8, -9, -1) with NA
data_wave3$cw3_appmed_2[data_wave3$cw3_appmed_2 %in% c(-8, -9, -1)] <- NA

# Check unique values
unique(data_wave3$cw3_appmed_2)
table((data_wave3$cw3_appmed_2))



###### Medical appointments booked: GP appointment. 
# Replace 1 with "Access to Healthcare"
data_wave3$cw3_appmed_3[data_wave3$cw3_appmed_3 == 1] <- "Access to Healthcare"

# Replace 2 with "No Access to Healthcare"
data_wave3$cw3_appmed_3[data_wave3$cw3_appmed_3 == 2] <- "No Access to Healthcare"

# Replace other values (e.g., -8, -9, -1) with NA
data_wave3$cw3_appmed_3[data_wave3$cw3_appmed_3 %in% c(-8, -9, -1)] <- NA

# Check unique values
unique(data_wave3$cw3_appmed_3)
table((data_wave3$cw3_appmed_3))



###### Medical appointments booked: Appointment for CBT, counselling or psychological therapy. 
# Replace 1 with "Access to Healthcare"
data_wave3$cw3_appmed_4[data_wave3$cw3_appmed_4 == 1] <- "Access to Healthcare"

# Replace 2 with "No Access to Healthcare"
data_wave3$cw3_appmed_4[data_wave3$cw3_appmed_4 == 2] <- "No Access to Healthcare"

# Replace other values (e.g., -8, -9, -1) with NA
data_wave3$cw3_appmed_4[data_wave3$cw3_appmed_4 %in% c(-8, -9, -1)] <- NA

# Check unique values
unique(data_wave3$cw3_appmed_4)
table((data_wave3$cw3_appmed_4))


###### t.test analysis objective 3

#### Type of medical appointment cancelled or delayed: Hospital appointment for consultation, investigation or treatment. 

apt.1t_test <- t.test(avg_general_health_scoresCW3 ~ cw3_appcant_1, data = data_wave3)

print(apt.1t_test)


####### Type of medical appointment cancelled or delayed: Hospital appointment for surgery. 

apt.2t_test <- t.test(avg_general_health_scoresCW3 ~ cw3_appcant_2, data = data_wave3)

print(apt.2t_test)


#######   Type of medical appointment cancelled or delayed: Appointment for CBT, counselling or psychological therapy

apt.3t_test <- t.test(avg_general_health_scoresCW3 ~ cw3_appcant_3, data = data_wave3)

print(apt.3t_test)


## appmed
###### Medical appointments booked: Hospital appointment for consultation, investigation, or treatment. 

apmed.1t_test <- t.test(avg_general_health_scoresCW3 ~ cw3_appmed_1, data = data_wave3)

print(apmed.1t_test)


####### Medical appointments booked: Hospital appointment for surgery. 
apmed.2t_test <- t.test(avg_general_health_scoresCW3 ~ cw3_appmed_2, data = data_wave3)

print(apmed.2t_test)

###### Medical appointments booked: GP appointment. 
apmed.3t_test <- t.test(avg_general_health_scoresCW3 ~ cw3_appmed_3, data = data_wave3)

print(apmed.3t_test)


###### Medical appointments booked: Appointment for CBT, counselling or psychological therapy. 
apmed.4t_test <- t.test(avg_general_health_scoresCW3 ~ cw3_appmed_4, data = data_wave3)

print(apmed.4t_test)



########
Hc_Impact4_model <- lm(avg_mental_health_scoresCW3 ~ factor(cw3_appcant_1) + factor(cw3_appcant_2) + 
                         factor(cw3_appcant_3) + cw3_psex + cw3_region, data = data_wave3)
summary(Hc_Impact4_model)


####confidence interval values
ci_Hc4 <- confint(Hc_Impact4_model)
ci_Hc4
