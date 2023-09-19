####### CHAPTER 4 #################
####### T.Test RESULTS ###################
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

# data was imported

data_wave1 <- read.csv("covid-19_wave1_survey_cls.csv")

data_wave2 <- read.csv("covid-19_wave2_survey_cls.csv")

data_wave3 <- read.csv("covid-19_wave3_survey_cls.csv")


View(data_wave1)
View(data_wave2)
View(data_wave3)


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




#####t.test


##remove invalid answers and recode

############# Type of medical appointment cancelled or delayed: Hospital appointment for consultation, investigation or treatment 
data_wave3$cw3_appcant_1 <- ifelse(data_wave3$cw3_appcant_1 == 1, "Healthcare Access Difficulty",
                                   ifelse(data_wave3$cw3_appcant_1 == 2, "No Healthcare Access Difficulty", NA))

table(data_wave3$cw3_appcant_1)
unique(data_wave3$cw3_appcant_1)

####### Type of medical appointment cancelled or delayed: Hospital appointment for surgery. 
data_wave3$cw3_appcant_2 <- ifelse(data_wave3$cw3_appcant_2 == 1, "Healthcare Access Difficulty",
                                   ifelse(data_wave3$cw3_appcant_2 == 2, "No Healthcare Access Difficulty", NA))
table(data_wave3$cw3_appcant_2)

#######   Type of medical appointment cancelled or delayed: Appointment for CBT, counselling or psychological therapy
data_wave3$cw3_appcant_3 <- ifelse(data_wave3$cw3_appcant_3 == 1, "Healthcare Access Difficulty",
                                   ifelse(data_wave3$cw3_appcant_3 == 2, "Healthcare Access Difficulty", NA))
table(data_wave3$cw3_appcant_3)

data_wave3$cw3_appcant_4 <- ifelse(data_wave3$cw3_appcant_4 == 1, "Healthcare Access Difficulty",
                                   ifelse(data_wave3$cw3_appcant_4 == 2, "No Healthcare Access Difficulty", NA))
table(data_wave3$cw3_appcant_4)
##appmed

###### Medical appointments booked: Hospital appointment for consultation, investigation, or treatment. 
# Replace 1 with "Access to Healthcare"
data_wave3$cw3_appmed_1[data_wave3$cw3_appmed_1 == 1] <- "No Healthcare Access Difficulty"

# Replace 2 with "No Access to Healthcare"
data_wave3$cw3_appmed_1[data_wave3$cw3_appmed_1 == 2] <- "Healthcare Access Difficulty"

# Replace other values (e.g., -8, -9, -1) with NA
data_wave3$cw3_appmed_1[data_wave3$cw3_appmed_1 %in% c(-8, -9, -1)] <- NA

# Check unique values
unique(data_wave3$cw3_appmed_1)
table((data_wave3$cw3_appmed_1))


####### Medical appointments booked: Hospital appointment for surgery. 
# Replace 1 with "Access to Healthcare"
data_wave3$cw3_appmed_2[data_wave3$cw3_appmed_2 == 1] <- "No Healthcare Access Difficulty"

# Replace 2 with "No Access to Healthcare"
data_wave3$cw3_appmed_2[data_wave3$cw3_appmed_2 == 2] <- "Healthcare Access Difficulty"

# Replace other values (e.g., -8, -9, -1) with NA
data_wave3$cw3_appmed_2[data_wave3$cw3_appmed_2 %in% c(-8, -9, -1)] <- NA

# Check unique values
unique(data_wave3$cw3_appmed_2)
table((data_wave3$cw3_appmed_2))



###### Medical appointments booked: GP appointment. 
# Replace 1 with "Access to Healthcare"
data_wave3$cw3_appmed_3[data_wave3$cw3_appmed_3 == 1] <- "No Healthcare Access Difficulty"

# Replace 2 with "No Access to Healthcare"
data_wave3$cw3_appmed_3[data_wave3$cw3_appmed_3 == 2] <- "Healthcare Access Difficulty"

# Replace other values (e.g., -8, -9, -1) with NA
data_wave3$cw3_appmed_3[data_wave3$cw3_appmed_3 %in% c(-8, -9, -1)] <- NA

# Check unique values
unique(data_wave3$cw3_appmed_3)
table((data_wave3$cw3_appmed_3))



###### Medical appointments booked: Appointment for CBT, counselling or psychological therapy. 
# Replace 1 with "Access to Healthcare"
data_wave3$cw3_appmed_4[data_wave3$cw3_appmed_4 == 1] <- "No Healthcare Access Difficulty"

# Replace 2 with "No Access to Healthcare"
data_wave3$cw3_appmed_4[data_wave3$cw3_appmed_4 == 2] <- "Healthcare Access Difficulty"

# Replace other values (e.g., -8, -9, -1) with NA
data_wave3$cw3_appmed_4[data_wave3$cw3_appmed_4 %in% c(-8, -9, -1)] <- NA

# Check unique values
unique(data_wave3$cw3_appmed_4)
table((data_wave3$cw3_appmed_4))


###### t.test analysis objective 3

#### Type of medical appointment cancelled or delayed: Hospital appointment for consultation, investigation or treatment. 

apt.1t_test <- t.test(avg_general_health_scoresCW3 ~ cw3_appcant_1, data = data_wave3)

print(apt.1t_test)

standard_error <- apt.1t_test$stderr
summary(standard_error)

##creating t for effect size
t<-apt.1t_test$statistic[[1]]
##creating df executing
df<-apt.1t_test$parameter[[1]]
####calculate r
r<-sqrt(t^2/(t^2+df))

summary(r)
##3 decimal places
round (r,3)
####### Type of medical appointment cancelled or delayed: Hospital appointment for surgery. 

apt.2t_test <- t.test(avg_general_health_scoresCW3 ~ cw3_appcant_2, data = data_wave3)

print(apt.2t_test)

standard_error <- apt.2t_test$stderr
summary(standard_error)

##creating t for effect size
t<-apt.2t_test$statistic[[1]]
##creating df executing
df<-apt.2t_test$parameter[[1]]
####calculate r
r<-sqrt(t^2/(t^2+df))

summary(r)

##3 decimal places
round (r,3)

#######   Type of medical appointment cancelled or delayed: Appointment for CBT, counselling or psychological therapy

apt.3t_test <- t.test(avg_general_health_scoresCW3 ~ cw3_appcant_3, data = data_wave3)

print(apt.3t_test)

standard_error <- apt.3t_test$stderr
summary(standard_error)

##creating t for effect size
t<-apt.3t_test$statistic[[1]]
##creating df executing
df<-apt.3t_test$parameter[[1]]
####calculate r
r<-sqrt(t^2/(t^2+df))

summary(r)
##3 decimal places
round (r,3)

######
apt.4t_test <- t.test(avg_general_health_scoresCW3 ~ cw3_appcant_4, data = data_wave3)

print(apt.4t_test)

standard_error <- apt.4t_test$stderr
summary(standard_error)

##creating t for effect size
t<-apt.4t_test$statistic[[1]]
##creating df executing
df<-apt.4t_test$parameter[[1]]
####calculate r
r<-sqrt(t^2/(t^2+df))

summary(r)
##3 decimal places
round (r,3)

## appmed
###### Medical appointments booked: Hospital appointment for consultation, investigation, or treatment. 

apmed.1t_test <- t.test(avg_general_health_scoresCW3 ~ cw3_appmed_1, data = data_wave3)

print(apmed.1t_test)

standard_error <- apmed.1t_test$stderr
summary(standard_error)

##creating t for effect size
t<-apmed.1t_test$statistic[[1]]
##creating df executing
df<-apmed.1t_test$parameter[[1]]
####calculate r
r<-sqrt(t^2/(t^2+df))

summary(r)
##3 decimal places
round (r,3)

####### Medical appointments booked: Hospital appointment for surgery. 
apmed.2t_test <- t.test(avg_general_health_scoresCW3 ~ cw3_appmed_2, data = data_wave3)

print(apmed.2t_test)

standard_error <- apmed.2t_test$stderr
summary(standard_error)

##creating t for effect size
t<-apmed.2t_test$statistic[[1]]
##creating df executing
df<-apmed.2t_test$parameter[[1]]
####calculate r
r<-sqrt(t^2/(t^2+df))

summary(r)
##3 decimal places
round (r,3)
###### Medical appointments booked: GP appointment. 
apmed.3t_test <- t.test(avg_general_health_scoresCW3 ~ cw3_appmed_3, data = data_wave3)

print(apmed.3t_test)

standard_error <- apmed.3t_test$stderr
summary(standard_error)

##creating t for effect size
t<-apmed.3t_test$statistic[[1]]
##creating df executing
df<-apmed.3t_test$parameter[[1]]
####calculate r
r<-sqrt(t^2/(t^2+df))

summary(r)
##3 decimal places
round (r,3)

###### Medical appointments booked: Appointment for CBT, counselling or psychological therapy. 
apmed.4t_test <- t.test(avg_general_health_scoresCW3 ~ cw3_appmed_4, data = data_wave3)

print(apmed.4t_test)

standard_error <- apmed.4t_test$stderr
summary(standard_error)

##creating t for effect size
t<-apmed.4t_test$statistic[[1]]
##creating df executing
df<-apmed.4t_test$parameter[[1]]
####calculate r
r<-sqrt(t^2/(t^2+df))

summary(r)
##3 decimal places
round (r,3)
# To display a table as a barplot is easy. First save the table and then produce the barplot:
#
tab.appmd1 <- table(data_wave3$cw3_appmed_1)
tab.appmd1
barplot(tab.appmd1, xlab = "booked appointment for consultation, investigation, or treatment", ylab = "Frequency")

#
# Try with gender.
#
#
# We can improve the figure by labelling the barplot better. We can also add colour.
#
barplot(tab.appmd1, names.arg = c("Yes", "No"), 
        col = c("blue", "orange"), xlab = "booked appointment for consultation", ylab = "Frequency")

#


#For cross-tabulation try:
  
  table(data_wave3$avg_general_health_scoresCW3, data_wave3$cw3_appmed_1)

#
# It’s not so easy to read.
# To label the rows and columns of a table:
#     
avghealth.appmed <- table(Health_outcomes= data_wave3$avg_general_health_scoresCW3, Healthcare_services = data_wave3$cw3_appmed_1) 
avghealth.appmed

#For a simple barplot of a cross-tabulation try:
  
barplot(avghealth.appmed, xlab = "Health outcomes by healthcare difficulty", ylab = "Frequency")

  
  
# Outlier detected!
  
hist(data_wave2$avg_general_health_scoresCW2[1:15], xlab = "Average General Health", main = "General Health Outcomes Wave 3")
rug(data_wave2$avg_general_health_scoresCW2[1:15])
legend("topright", legend = c("Mean = 2.12", "SD = 0.48"), lty = c(0, 0))
  
  
#We can produce boxplots for females and males separately on the same vertical axis:
    
boxplot(data_wave3$avg_general_health_scoresCW3[1:15] ~ data_wave3$cw3_psex[1:15], 
        ylab = "Average General Health", main = "General Health", sub = "Stratified by sex")
  
df <-data.frame(data_wave3$cw3_appmed_1, data_wave3$avg_general_health_scoresCW3, data_wave3$cw3_psex)
df

#
sex_f <- factor(data_wave3$cw3_psex, labels = c("Female", "Male"))
# Standard level order, but with user defined labels
sex_f

ggplot(df, aes(x = data_wave3$cw3_appmed_1, y = data_wave3$avg_general_health_scoresCW3, col = sex_f)) + 
  geom_point() +
  ggtitle("Healthcare Access by gender") +
  labs(x = "Booked appointment for consultation and treatment (Wave 3)", y = "Average General Health Scores")

# Load the ggplot2 library
library(ggplot2)

# Create the ggplot visualization
ggplot(data_wave3, aes(x = cw3_appmed_2, y = avg_general_health_scoresCW3, col = sex_f)) +
  geom_point() +
  ggtitle("Healthcare Access Difficulty by gender") +
  labs(x = "Booked appointment(Wave 3)", y = "Average General Health Scores")
