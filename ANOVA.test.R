####### CHAPTER 4 #################
####### ANOVA RESULTS ###################
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
#library(Rcmdr)
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


#### ANOVA test analysis objective 3


#convert as factor
data_wave3$cw3_psex <- as.factor(data_wave3$cw3_psex)
data_wave3$cw3_psex


### region

data_wave3$cw3_region <- as.factor(data_wave3$cw3_region)
data_wave3$cw3_region

# Remove values -8 and -1 from cw3_region 
data_wave3 <- data_wave3[!(data_wave3$cw3_region %in% c(-8, -1)), ]
# Check the resulting table
cw3_region <- table(data_wave3$cw3_region)
cw3_region

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
leveneTest(data_wave3$avg_general_health_scoresCW3, data_wave3$cw3_appcant_1, center = median)
leveneTest(data_wave3$avg_general_health_scoresCW3, data_wave3$cw3_appcant_3, center = median)

leveneTest(data_wave3$avg_general_health_scoresCW3, data_wave3$cw3_appmed_3, center = median)
leveneTest(data_wave3$avg_general_health_scoresCW3, data_wave3$cw3_appmed_2, center = mean)

####### ANOVA test between average general health scores and Cancelled or delayed hospital appointment
with_health_access <- aov(avg_general_health_scoresCW3 ~  cw3_appcant_1 + cw3_appcant_2 + 
                            cw3_appcant_3 + cw3_appcant_4, data = data_wave3)
# Print the summary of ANOVA
summary(with_health_access)

#####interaction 
Iwith_health_access <- aov(avg_general_health_scoresCW3 ~ cw3_appcant_1 + cw3_appcant_2 + 
                             cw3_appcant_3 + cw3_appcant_4*factor(cw3_psex) *factor(cw3_region), data = data_wave3)
# Print the summary of ANOVA
summary(Iwith_health_access)

#######blocking
Bwith_health_access <- aov(avg_general_health_scoresCW3 ~ cw3_appcant_1 + cw3_appcant_2 + 
                             cw3_appcant_3 + cw3_appcant_4 + cw3_psex + cw3_region, data = data_wave3)
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
  scale_x_discrete(breaks = c(1, 2, 3 ,4), labels = c("1" = "Medium",
                                                      "2" = "Somewhat",
                                                      "3" = "Moderate",
                                                      "4" = "High")) +
  labs(title = "Average General Health Scores Wave 3")



# Saving plots:

Region_plot <- ggplot(data, aes(x = Region, y = Frequency, fill = Region)) +
  geom_bar(stat = "identity") +
  
# Customize colors and add legend
scale_fill_manual(values = region_colors) +
labs(x = "Region", y = "Frequency", title = "Frequency of Regions") +
  
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

ggsave("Figure_1.png", plot = Region_plot, device = "png")




######## find the best fit

model.set <- list(with_health_access, Iwith_health_access, Bwith_health_access)
model.names <- c("with_health_access", "Iwith_health_access", "Bwith_health_access")

aictab(model.set, modnames = model.names)

####check for homoscedasticity

par(mfrow=c(2,2))
plot(Bwith_health_access)
mtext("Homoscedasticity check for Blocking between Avg General Health & Cancelled Appointments",                   # Add main title
      front = 4,
      line = -1,
      outer = TRUE)
par(mfrow=c(1,1))

####graphs
data_wave3[, "cw3_appcant_1"] <- data_wave3$cw3_appcant_1 %>% factor()
par(mfrow = c(2,2))
boxplot(data_wave3$avg_general_health_scoresCW3 ~ data_wave3$cw3_appcant_1, 
        main="Health Outcomes & Healthcare Access Difficulty",
        xlab = "Cancelled consultation", 
        ylab = "Avg General health scores",
        sub = "Hospital Appointment",
        font.main=2, font.lab=2, font.sub=2)

boxplot(data_wave3$avg_general_health_scoresCW3 ~ data_wave3$cw3_appcant_2, 
        main="Health Outcomes & Healthcare Access Difficulty",
        xlab = "Cancelled Surgery", 
        ylab = "Avg General health scores",
        sub = "Hospital Appointment",
        font.main=2, font.lab=2, font.sub=2)

boxplot(data_wave3$avg_general_health_scoresCW3 ~ data_wave3$cw3_appcant_3, 
        main="Health Outcomes & Healthcare Access Difficulty",
        xlab = "Cancelled Psychological", 
        ylab = "Avg General health scores",
        sub = "Hospital Appointment",
        font.main=2, font.lab=2, font.sub=2)

boxplot(data_wave3$avg_general_health_scoresCW3 ~ data_wave3$cw3_appmed_4, 
        main="Health Outcomes & Healthcare Access Difficulty",
        xlab = "Booked Hospital appointment for CBT", 
        ylab = "Avg General health scores",
        sub = "Hospital Appointment",
        font.main=2, font.lab=2, font.sub=2)

boxplot(data_wave3$avg_general_health_scoresCW3 ~ data_wave3$cw3_appmed_3, 
        main="Health Outcomes & Healthcare Access Difficulty",
        xlab = "Booked GP Appointment", 
        ylab = "Avg General health scores",
        sub = "Hospital Appointment",
        font.main=4, font.lab=4, font.sub=4)


##########groupwise differences

tukey.plot1.aov<-aov(avg_general_health_scoresCW3 ~ cw3_appcant_1:cw3_appcant_3, data=data_wave3)
tukey.plot1.test<-TukeyHSD(tukey.plot1.aov)
plot(tukey.plot1.test, las = 1)

tukey.plot2.aov<-aov(avg_general_health_scoresCW3 ~ cw3_appmed_2:cw3_appmed_4, data=data_wave3)
tukey.plot2.test<-TukeyHSD(tukey.plot2.aov)
plot(tukey.plot2.test, las = 1)

tukey.plot3.aov<-aov(avg_general_health_scoresCW3 ~ cw3_appmed_4:cw3_psex, data=data_wave3)
tukey.plot3.test<-TukeyHSD(tukey.plot3.aov)
plot(tukey.plot3.test, las = 1)

tukey.plot4.aov<-aov(avg_general_health_scoresCW3 ~ cw3_appcant_2:cw3_psex, data=data_wave3)
tukey.plot4.test<-TukeyHSD(tukey.plot4.aov)
plot(tukey.plot4.test, las = 1)

####### comparison between average general health and booked hospital appointment

without_health_access1 <- aov(avg_general_health_scoresCW3 ~ factor(cw3_appmed_1) + factor(cw3_appmed_2) 
                              + factor(cw3_appmed_3) + factor(cw3_appmed_4), data = data_wave3)
# Print the summary of ANOVA
summary(without_health_access1)

######interaction
Iwithout_health_access1 <- aov(avg_general_health_scoresCW3 ~ cw3_appmed_1 + cw3_appmed_2 
                               + cw3_appmed_3 + cw3_appmed_4*factor(cw3_psex) 
                               *factor(cw3_region), data = data_wave3)
# Print the summary of ANOVA
summary(Iwithout_health_access1)



###locking

Bwithout_health_access1 <- aov(avg_general_health_scoresCW3 ~ cw3_appmed_1 + cw3_appmed_2 
                               + cw3_appmed_3 + cw3_appmed_4 + factor(cw3_psex) 
                               + factor(cw3_region), data = data_wave3)
# Print the summary of ANOVA
summary(Bwithout_health_access1)

### find bet fit

model.set <- list(without_health_access1, Iwithout_health_access1, Bwithout_health_access1)
model.names <- c("without_health_access1", "Iwithout_health_access1", "Bwithout_health_access1")

aictab(model.set, modnames = model.names)

####check for homoscedasticity

par(mfrow=c(2,2))
plot(Bwithout_health_access1)
mtext("Homoscedasticity check for Blocking between Average General Health & Booked Appointment",                   # Add main title
      front = 4,
      line = - 2,
      outer = TRUE)
par(mfrow=c(1,1))

# Perform Tukey's HSD test
wo_tukey_result <- TukeyHSD(without_health_access1)

# Print the Tukey's HSD results
print(wo_tukey_result)

unique(data_wave3$cw3_appmed_1)


######## find the best fit
library(AICcmodavg)


##########groupwise differences

tukey.plot1.aov<-aov(avg_general_health_scoresCW3 ~ cw3_appmed_3:cw3_appmed_4, data=data_wave3)
tukey.plot.test<-TukeyHSD(tukey.plot1.aov)
plot(tukey.plot1.test, las = 1)

tukey.plot2.aov<-aov(avg_general_health_scoresCW3 ~ cw3_appmed_2:cw3_appmed_4, data=data_wave3)
tukey.plot.test<-TukeyHSD(tukey.plot2.aov)
plot(tukey.plot2.test, las = 1)



##############################################################################################


##################################################with/mental
with_health_access <- aov(avg_mental_health_scoresCW3 ~ cw3_appcant_1 + cw3_appcant_2 + cw3_appcant_3 + cw3_appcant_4, data = data_wave3)
# Print the summary of ANOVA
summary(with_health_access)

Iwith_health_access <- aov(avg_mental_health_scoresCW3 ~ cw3_appcant_1 + cw3_appcant_2 + cw3_appcant_3 + cw3_appcant_4*cw3_psex *cw3_region, data = data_wave3)
# Print the summary of ANOVA
summary(Iwith_health_access)

Bwith_health_access <- aov(avg_mental_health_scoresCW3 ~ cw3_appcant_1 + cw3_appcant_2 + cw3_appcant_3 + cw3_appcant_4 + cw3_psex + cw3_region, data = data_wave3)
# Print the summary of ANOVA
summary(Bwith_health_access)

#### find bet fit

model.set <- list(with_health_access, Iwith_health_access, Bwith_health_access)
model.names <- c("with_health_access", "Iwith_health_access", "Bwith_health_access")

aictab(model.set, modnames = model.names)

####check for homoscedasticity

par(mfrow=c(2,2))
plot(Bwith_health_access)
par(mfrow=c(1,1))


####end