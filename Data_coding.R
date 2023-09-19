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
#library(Rcmdr)
library(nnet)
library(ggplot2)
library(magrittr)
library(MASS)
library(tree)
library(ISLR)
library(RColorBrewer)





#Read in datasets from directory

data_wave1 <- read.csv("covid-19_wave1_survey_cls.csv")
data_wave2 <- read.csv("covid-19_wave2_survey_cls.csv")
data_wave3 <- read.csv("covid-19_wave3_survey_cls.csv")




#gad2phq2_1-4
avg_mental_health_scoresCW1



data_wave1$avg_mental_health_scoresCW1[data_wave1$avg_mental_health_scoresCW1==1] <- "Not at all"
data_wave1$avg_mental_health_scoresCW1[data_wave1$avg_mental_health_scoresCW1==2] <- "Several days"
data_wave1$avg_mental_health_scoresCW1[data_wave1$avg_mental_health_scoresCW1==3] <- "More than half the days"
data_wave1$avg_mental_health_scoresCW1[data_wave1$avg_mental_health_scoresCW1==4] <- "Nearly every day"
data_wave1$avg_mental_health_scoresCW1[data_wave1$avg_mental_health_scoresCW1==-9] <- "Don't want to answer"
data_wave1$avg_mental_health_scoresCW1[data_wave1$avg_mental_health_scoresCW1==-8] <- "Don’t Know"
data_wave1$avg_mental_health_scoresCW1[data_wave1$avg_mental_health_scoresCW1==-1] <- "Not Applicable"

temp_avg_mental_health_scoresCW1 <- table(data_wave1$avg_mental_health_scoresCW1)
temp_avg_mental_health_scoresCW1
temp_avg_mental_health_scoresCW1num <- c(temp_avg_mental_health_scoresCW1[5], temp_avg_mental_health_scoresCW1[6], temp_avg_mental_health_scoresCW1[2], 
                                         temp_avg_mental_health_scoresCW1[3], temp_avg_mental_health_scoresCW1[7], temp_avg_mental_health_scoresCW1[1], temp_avg_mental_health_scoresCW1[4])
temp_avg_mental_health_scoresCW1num
temp_per_avg_mental_health_scoresCW1 <- round(100*temp_avg_mental_health_scoresCW1num/ nrow(data_wave1),1)
temp_per_avg_mental_health_scoresCW1

healthcare_tbl1$cw1_npercent[103:109] <- paste0(temp_avg_mental_health_scoresCW1num, "(", temp_per_avg_mental_health_scoresCW1, ")")

healthcare_tbl1

##### avg_mental_healyh_scores

data_wave1$avg_mental_health_scoresCW1[data_wave1$avg_mental_health_scoresCW1==1] <- "Not at all"
data_wave1$avg_mental_health_scoresCW1[data_wave1$avg_mental_health_scoresCW1==2] <- "Several days"
data_wave1$avg_mental_health_scoresCW1[data_wave1$avg_mental_health_scoresCW1==3] <- "More than half the days"
data_wave1$avg_mental_health_scoresCW1[data_wave1$avg_mental_health_scoresCW1==4] <- "Nearly every day"
data_wave1$avg_mental_health_scoresCW1[data_wave1$avg_mental_health_scoresCW1==-9] <- "Don't want to answer"
data_wave1$avg_mental_health_scoresCW1[data_wave1$avg_mental_health_scoresCW1==-8] <- "Don’t Know"
data_wave1$avg_mental_health_scoresCW1[data_wave1$avg_mental_health_scoresCW1==-1] <- "Not Applicable"

temp_avg_mental_health_scoresCW1 <- table(data_wave1$avg_mental_health_scoresCW1)
temp_avg_mental_health_scoresCW1
temp_avg_mental_health_scoresCW1num <- c(temp_avg_mental_health_scoresCW1[5], temp_avg_mental_health_scoresCW1[6], temp_avg_mental_health_scoresCW1[2], 
                                         temp_avg_mental_health_scoresCW1[3], temp_avg_mental_health_scoresCW1[7], temp_avg_mental_health_scoresCW1[1], temp_avg_mental_health_scoresCW1[4])
temp_avg_mental_health_scoresCW1num
temp_per_avg_mental_health_scoresCW1 <- round(100*temp_avg_mental_health_scoresCW1num/ nrow(data_wave1),1)
temp_per_avg_mental_health_scoresCW1

healthcare_tbl1$cw1_npercent[103:109] <- paste0(temp_avg_mental_health_scoresCW1num, "(", temp_per_avg_mental_health_scoresCW1, ")")

healthcare_tbl1


data_wave2$avg_mental_health_scoresCW2[data_wave2$avg_mental_health_scoresCW2==1] <- "Not at all"
data_wave2$avg_mental_health_scoresCW2[data_wave2$avg_mental_health_scoresCW2==2] <- "Several days"
data_wave2$avg_mental_health_scoresCW2[data_wave2$avg_mental_health_scoresCW2==3] <- "More than half the days"
data_wave2$avg_mental_health_scoresCW2[data_wave2$avg_mental_health_scoresCW2==4] <- "Nearly every day"
data_wave2$avg_mental_health_scoresCW2[data_wave2$avg_mental_health_scoresCW2==-9] <- "Don't want to answer"
data_wave2$avg_mental_health_scoresCW2[data_wave2$avg_mental_health_scoresCW2==-8] <- "Don’t Know"
data_wave2$avg_mental_health_scoresCW2[data_wave2$avg_mental_health_scoresCW2==-1] <- "Not Applicable"

temp_avg_mental_health_scoresCW2 <- table(data_wave2$avg_mental_health_scoresCW2)
temp_avg_mental_health_scoresCW2
temp_avg_mental_health_scoresCW2num <- c(temp_avg_mental_health_scoresCW2[6], temp_avg_mental_health_scoresCW2[7], temp_avg_mental_health_scoresCW2[3], 
                                         temp_avg_mental_health_scoresCW2[4], temp_avg_mental_health_scoresCW2[1], temp_avg_mental_health_scoresCW2[2], temp_avg_mental_health_scoresCW2[5])
temp_avg_mental_health_scoresCW2num
temp_per_avg_mental_health_scoresCW2 <- round(100*temp_avg_mental_health_scoresCW2num/ nrow(data_wave2),1)
temp_per_avg_mental_health_scoresCW2

healthcare_tbl1$cw2_npercent[103:109] <- paste0(temp_avg_mental_health_scoresCW2num, "(", temp_per_avg_mental_health_scoresCW2, ")")

healthcare_tbl1



data_wave3$avg_mental_health_scoresCW3[data_wave3$avg_mental_health_scoresCW3==1] <- "Not at all"
data_wave3$avg_mental_health_scoresCW3[data_wave3$avg_mental_health_scoresCW3==2] <- "Several days"
data_wave3$avg_mental_health_scoresCW3[data_wave3$avg_mental_health_scoresCW3==3] <- "More than half the days"
data_wave3$avg_mental_health_scoresCW3[data_wave3$avg_mental_health_scoresCW3==4] <- "Nearly every day"
data_wave3$avg_mental_health_scoresCW3[data_wave3$avg_mental_health_scoresCW3==-9] <- "Don't want to answer"
data_wave3$avg_mental_health_scoresCW3[data_wave3$avg_mental_health_scoresCW3==-8] <- "Don’t Know"
data_wave3$avg_mental_health_scoresCW3[data_wave3$avg_mental_health_scoresCW3==-1] <- "Not Applicable"

temp_avg_mental_health_scoresCW3 <- table(data_wave3$avg_mental_health_scoresCW3)
temp_avg_mental_health_scoresCW3
temp_avg_mental_health_scoresCW3num <- c(temp_avg_mental_health_scoresCW3[6], temp_avg_mental_health_scoresCW3[7], temp_avg_mental_health_scoresCW3[3], 
                                         temp_avg_mental_health_scoresCW3[4], temp_avg_mental_health_scoresCW3[2], temp_avg_mental_health_scoresCW3[1], temp_avg_mental_health_scoresCW3[5])
temp_avg_mental_health_scoresCW3num
temp_per_avg_mental_health_scoresCW3 <- round(100*temp_avg_mental_health_scoresCW3num/ nrow(data_wave3),1)
temp_per_avg_mental_health_scoresCW3

healthcare_tbl1$cw1_npercent[103:109] <- paste0(temp_avg_mental_health_scoresCW1num, "(", temp_per_avg_mental_health_scoresCW1, ")")

healthcare_tbl1



####### cbind mental_health-scores

data_wave1$cw1_mental_health_scores[data_wave1$cw1_mental_health_scores==1] <- "Not at all"
data_wave1$cw1_mental_health_scores[data_wave1$cw1_mental_health_scores==2] <- "Several days"
data_wave1$cw1_mental_health_scores[data_wave1$cw1_mental_health_scores==3] <- "More than half the days"
data_wave1$cw1_mental_health_scores[data_wave1$cw1_mental_health_scores==4] <- "Nearly every day"
data_wave1$cw1_mental_health_scores[data_wave1$cw1_mental_health_scores==-9] <- "Don't want to answer"
data_wave1$cw1_mental_health_scores[data_wave1$cw1_mental_health_scores==-8] <- "Don’t Know"
data_wave1$cw1_mental_health_scores[data_wave1$cw1_mental_health_scores==-1] <- "Not Applicable"

temp_cw1_mental_health_scores <- table(data_wave1$cw1_mental_health_scores)
temp_cw1_mental_health_scores
temp_cw1_mental_health_scoresnum <- c(temp_cw1_mental_health_scores[3], temp_cw1_mental_health_scores[6], temp_cw1_mental_health_scores[5], 
                                      temp_cw1_mental_health_scores[4], temp_cw1_mental_health_scores[7], temp_cw1_mental_health_scores[1], temp_cw1_mental_health_scores[2])
temp_cw1_mental_health_scoresnum
temp_per_cw1_mental_health_scores <- round(100*temp_cw1_mental_health_scoresnum/ nrow(data_wave1),1)
temp_per_cw1_mental_health_scores

healthcare_tbl1$cw1_npercent[103:109] <- paste0(temp_cw1_mental_health_scoresnum, "(", temp_per_cw1_mental_health_scores, ")")

healthcare_tbl1


data_wave2$cw2_mental_health_scores[data_wave2$cw2_mental_health_scores==1] <- "Not at all"
data_wave2$cw2_mental_health_scores[data_wave2$cw2_mental_health_scores==2] <- "Several days"
data_wave2$cw2_mental_health_scores[data_wave2$cw2_mental_health_scores==3] <- "More than half the days"
data_wave2$cw2_mental_health_scores[data_wave2$cw2_mental_health_scores==4] <- "Nearly every day"
data_wave2$cw2_mental_health_scores[data_wave2$cw2_mental_health_scores==-9] <- "Don't want to answer"
data_wave2$cw2_mental_health_scores[data_wave2$cw2_mental_health_scores==-8] <- "Don’t Know"
data_wave2$cw2_mental_health_scores[data_wave2$cw2_mental_health_scores==-1] <- "Not Applicable"

temp_cw2_mental_health_scores <- table(data_wave2$cw2_mental_health_scores)
temp_cw2_mental_health_scores
temp_cw2_mental_health_scoresnum <- c(temp_cw2_mental_health_scores[3], temp_cw2_mental_health_scores[6], temp_cw1_mental_health_scores[5], 
                                      temp_cw2_mental_health_scores[4], temp_cw2_mental_health_scores[7], temp_cw1_mental_health_scores[1], temp_cw1_mental_health_scores[2])
temp_cw1_mental_health_scoresnum
temp_per_cw2_mental_health_scores <- round(100*temp_cw2_mental_health_scoresnum/ nrow(data_wave2),1)
temp_per_cw2_mental_health_scores

healthcare_tbl1$cw2_npercent[103:109] <- paste0(temp_cw2_mental_health_scoresnum, "(", temp_per_cw2_mental_health_scores, ")")

healthcare_tbl1


data_wave3$avg_mental_health_scoresCW3[data_wave3$avg_mental_health_scoresCW3==1] <- "Not at all"
data_wave3$avg_mental_health_scoresCW3[data_wave3$avg_mental_health_scoresCW3==2] <- "Several days"
data_wave3$avg_mental_health_scoresCW3[data_wave3$avg_mental_health_scoresCW3==3] <- "More than half the days"
data_wave3$avg_mental_health_scoresCW3[data_wave3$avg_mental_health_scoresCW3==4] <- "Nearly every day"
data_wave3$avg_mental_health_scoresCW3[data_wave3$avg_mental_health_scoresCW3==-9] <- "Don't want to answer"
data_wave3$avg_mental_health_scoresCW3[data_wave3$avg_mental_health_scoresCW3==-8] <- "Don’t Know"
data_wave3$avg_mental_health_scoresCW3[data_wave3$avg_mental_health_scoresCW3==-1] <- "Not Applicable"

temp_avg_mental_health_scoresCW3 <- table(data_wave3$avg_mental_health_scoresCW3)
temp_avg_mental_health_scoresCW3
temp_avg_mental_health_scoresCW3num <- c(temp_avg_mental_health_scoresCW3[6], temp_avg_mental_health_scoresCW3[7], temp_avg_mental_health_scoresCW3[3], 
                                         temp_avg_mental_health_scoresCW3[4], temp_avg_mental_health_scoresCW3[2], temp_avg_mental_health_scoresCW3[1], temp_avg_mental_health_scoresCW3[5])
temp_avg_mental_health_scoresCW3num
temp_per_avg_mental_health_scoresCW3 <- round(100*temp_avg_mental_health_scoresCW3num/ nrow(data_wave3),1)
temp_per_avg_mental_health_scoresCW3

healthcare_tbl1$cw1_npercent[103:109] <- paste0(temp_avg_mental_health_scoresCW1num, "(", temp_per_avg_mental_health_scoresCW1, ")")

healthcare_tbl1







#---------------------------------------------------------------------------------
#Create function to transform dataframestransform_data <- function(dataframe) {
  
  
  # Remove any unnecessary white space and retain dataframe structure
  dataframe <-as.data.frame(apply(dataframe,2, str_remove_all, " "))
  
  # Count ids for each  cohort
  ncdsid_count <-  sum(dataframe$ncdsid!="")
  bcsid_count <-  sum(dataframe$bcsid!="")
  nsid_count <-  sum(dataframe$nsid!="")
  mcsid_count <-  sum(dataframe$mcsid!="")
  
  
  # Check that number of ids counted is same as rows in the dataframe (one id per row)
  if(nrow(dataframe) != ncdsid_count + bcsid_count + nsid_count+ mcsid_count){
    stop("Error: Some rows in your dataframe likely have more than one id")
    
  } else {
    
    # Create vector of id types
    cohort_vector<-rep(c("ncdsid","bcsid","nsid","mcsid"),times=c(ncdsid_count,bcsid_count,nsid_count,mcsid_count))
    cohort_vector <- as.factor(cohort_vector) # convert id_types to vectors for ease of use later for analysis
    
    
    # Combine the id columns int one column called 'id'
    dataframe <- unite(dataframe, id, c(ncdsid,bcsid,nsid,mcsid))
    
    # Remove spaces & underscores that might result from combination
    dataframe <-as.data.frame(apply(dataframe,2, str_remove_all, "_"))
    #dataframe <-as.data.frame(apply(dataframe,2, str_remove_all, " "))
    
    # Add cohort_vector to dataframe as 'id_type' column
    dataframe$cohort <- cohort_vector
    
    # Move cohort vector to beginning of dataframe
    dataframe<- dataframe %>% relocate(cohort)
    
  }
  
  # return transformed dataframe
  return(dataframe)
  
  # End of function
}


# transform datasets using "transform_data" function
covid19_wave1_survey_trans <- transform_data(data_wave1)
covid19_wave2_survey_trans <- transform_data(data_wave2)
covid19_wave3_survey_trans <- transform_data(data_wave3)


# list of column names to confirm single column for all 'id' and 'id_type' column added
colnames(covid19_wave1_survey_trans)

# levels of id_type column showing you the different id_type that exist
levels(covid19_wave1_survey_trans$cohort)

# View transformed data
View(data_wave1)
View(data_wave2)
View(data_wave3)


# Add combined identifier for each respondent and wave
covid19_wave1_survey_trans2 <- covid19_wave1_survey_trans%>%
  mutate(wave_id = paste(id,"_1"))%>% 
  relocate(wave_id)

covid19_wave2_survey_trans2 <- covid19_wave2_survey_trans%>%
  mutate(wave_id = paste(id,"_2"))%>% 
  relocate(wave_id)

covid19_wave3_survey_trans2 <- covid19_wave3_survey_trans%>%
  mutate(wave_id = paste(id,"_3"))%>% 
  relocate(wave_id)

# View transformed data after adding identifier
View(covid19_wave1_survey_trans2)
View(covid19_wave2_survey_trans2)
View(covid19_wave3_survey_trans2)

#Perform a full_join of the three dataframes 
# This full join combines all the respondents and all the variables/questions from the 3 cohorts
list_df = list(covid19_wave1_survey_trans2, covid19_wave2_survey_trans2, covid19_wave3_survey_trans2)
covid19_waves_survey_trans_all <- list_df%>% reduce(full_join, by = c('wave_id','cohort','id'))

View(covid19_waves_survey_trans_all)

# ncol(covid19_waves_survey_trans_all)
# nrow(covid19_waves_survey_trans_all)
# 
# ncol(covid19_wave1_survey_trans2)+ncol(covid19_wave2_survey_trans2)+ncol(covid19_wave3_survey_trans2)
# nrow(covid19_wave1_survey_trans2)+nrow(covid19_wave2_survey_trans2)+nrow(covid19_wave3_survey_trans2)


######## building table

healthcare_tbl1 <- read.csv("nftable.csv")

#psex

data_wave1$cw1_psex[data_wave1$cw1_psex==1] <- "Male"
data_wave1$cw1_psex[data_wave1$cw1_psex==2] <- "Female"
data_wave1$cw1_psex[data_wave1$cw1_psex==-1] <- "Not Applicable"
temp_cw1_psex <- table(data_wave1$cw1_psex)
temp_cw1_psex
temp_cw1_psexnum <- c(temp_cw1_psex[2], temp_cw1_psex[1], temp_cw1_psex[3])
temp_cw1_psexnum
temp_per_cw1_psex <- round(100*temp_cw1_psexnum / nrow(data_wave1),1)
temp_per_cw1_psex

healthcare_tbl1$cw1_npercent[1:3] <- paste0(temp_cw1_psexnum, "(", temp_per_cw1_psex, ")")

healthcare_tbl1

data_wave2$cw2_psex[data_wave2$cw2_psex==1] <- "Male"
data_wave2$cw2_psex[data_wave2$cw2_psex==2] <- "Female"
data_wave2$cw2_psex[data_wave2$cw2_psex==-1] <- "Not Applicable"
temp_cw2_psex <- table(data_wave2$cw2_psex)
temp_cw2_psex
temp_cw2_psexnum <- c(temp_cw2_psex[2], temp_cw2_psex[1], temp_cw2_psex[3])
temp_cw2_psexnum
temp_per_cw2_psex <- round(100*temp_cw2_psexnum / nrow(data_wave2),1)
temp_per_cw2_psex

healthcare_tbl1$cw2_npercent[1:3] <- paste0(temp_cw2_psexnum, "(", temp_per_cw2_psex, ")")

healthcare_tbl1


data_wave3$cw3_psex[data_wave3$cw3_psex==1] <- "Male"
data_wave3$cw3_psex[data_wave3$cw3_psex==2] <- "Female"
data_wave3$cw3_psex[data_wave3$cw3_psex==-1] <- "Not Applicable"
temp_cw3_psex <- table(data_wave3$cw3_psex)
temp_cw3_psex
temp_cw3_psexnum <- c(temp_cw3_psex[2], temp_cw3_psex[1], temp_cw3_psex[3])
temp_cw3_psexnum
temp_per_cw3_psex <- round(100*temp_cw3_psexnum / nrow(data_wave3),1)
temp_per_cw3_psex

healthcare_tbl1$cw3_npercent[1:3] <- paste0(temp_cw3_psexnum, "(", temp_per_cw3_psex, ")")

healthcare_tbl1

#region

data_wave1$cw1_region[data_wave1$cw1_region==1] <- "North East"
data_wave1$cw1_region[data_wave1$cw1_region==2] <- "North West"
data_wave1$cw1_region[data_wave1$cw1_region==3] <- "Yorkshire and the Humber"
data_wave1$cw1_region[data_wave1$cw1_region==4] <- "East Midlands"
data_wave1$cw1_region[data_wave1$cw1_region==5] <- "West Midlands"
data_wave1$cw1_region[data_wave1$cw1_region==6] <- "East of England"
data_wave1$cw1_region[data_wave1$cw1_region==7] <- " London"
data_wave1$cw1_region[data_wave1$cw1_region==8] <- "South East"
data_wave1$cw1_region[data_wave1$cw1_region==9] <- "South West"
data_wave1$cw1_region[data_wave1$cw1_region==10] <- "Wales"
data_wave1$cw1_region[data_wave1$cw1_region==11] <- "Scotland"
data_wave1$cw1_region[data_wave1$cw1_region==12] <- "Northern Ireland"
data_wave1$cw1_region[data_wave1$cw1_region==-8] <- "Don't Know"
data_wave1$cw1_region[data_wave1$cw1_region==-1] <- "Not Applicable"

temp_cw1_region <- table(data_wave1$cw1_region)
temp_cw1_region
temp_cw1_regionnum <- c(temp_cw1_region[5], temp_cw1_region[6], temp_cw1_region[14], 
                        temp_cw1_region[3], temp_cw1_region[13], temp_cw1_region[4], 
                        temp_cw1_region[1], temp_cw1_region[10], temp_cw1_region[11], temp_cw1_region[12],
                        temp_cw1_region[9], temp_cw1_region[7], temp_cw1_region[2], temp_cw1_region[8])
temp_cw1_regionnum
temp_per_cw1_region <- round(100*temp_cw1_regionnum / nrow(data_wave1),1)
temp_per_cw1_region

healthcare_tbl1$cw1_npercent[4:17] <- paste0(temp_cw1_regionnum, "(", temp_per_cw1_region, ")")

healthcare_tbl1


data_wave2$cw2_region[data_wave2$cw2_region==1] <- "North East"
data_wave2$cw2_region[data_wave2$cw2_region==2] <- "North West"
data_wave2$cw2_region[data_wave2$cw2_region==3] <- "Yorkshire and the Humber"
data_wave2$cw2_region[data_wave2$cw2_region==4] <- "East Midlands"
data_wave2$cw2_region[data_wave2$cw2_region==5] <- "West Midlands"
data_wave2$cw2_region[data_wave2$cw2_region==6] <- "East of England"
data_wave2$cw2_region[data_wave2$cw2_region==7] <- " London"
data_wave2$cw2_region[data_wave2$cw2_region==8] <- "South East"
data_wave2$cw2_region[data_wave2$cw2_region==9] <- "South West"
data_wave2$cw2_region[data_wave2$cw2_region==10] <- "Wales"
data_wave2$cw2_region[data_wave2$cw2_region==11] <- "Scotland"
data_wave2$cw2_region[data_wave2$cw2_region==12] <- "Northern Ireland"
data_wave2$cw2_region[data_wave2$cw2_region==-8] <- "Don't Know"
data_wave2$cw2_region[data_wave2$cw2_region==-1] <- "Not Applicable"

temp_cw2_region <- table(data_wave2$cw2_region)
temp_cw2_region
temp_cw2_regionnum <- c(temp_cw2_region[5], temp_cw2_region[6], temp_cw2_region[14], 
                        temp_cw2_region[3], temp_cw2_region[13], temp_cw2_region[4], 
                        temp_cw2_region[1], temp_cw2_region[10], temp_cw2_region[11], temp_cw2_region[12],
                        temp_cw2_region[9], temp_cw2_region[7], temp_cw2_region[2], temp_cw2_region[8])
temp_cw2_regionnum
temp_per_cw2_region <- round(100*temp_cw2_regionnum / nrow(data_wave2),1)
temp_per_cw2_region

healthcare_tbl1$cw2_npercent[4:17] <- paste0(temp_cw2_regionnum, "(", temp_per_cw2_region, ")")

healthcare_tbl1


data_wave3$cw3_region[data_wave3$cw3_region==1] <- "North East"
data_wave3$cw3_region[data_wave3$cw3_region==2] <- "North West"
data_wave3$cw3_region[data_wave3$cw3_region==3] <- "Yorkshire and the Humber"
data_wave3$cw3_region[data_wave3$cw3_region==4] <- "East Midlands"
data_wave3$cw3_region[data_wave3$cw3_region==5] <- "West Midlands"
data_wave3$cw3_region[data_wave3$cw3_region==6] <- "East of England"
data_wave3$cw3_region[data_wave3$cw3_region==7] <- " London"
data_wave3$cw3_region[data_wave3$cw3_region==8] <- "South East"
data_wave3$cw3_region[data_wave3$cw3_region==9] <- "South West"
data_wave3$cw3_region[data_wave3$cw3_region==10] <- "Wales"
data_wave3$cw3_region[data_wave3$cw3_region==11] <- "Scotland"
data_wave3$cw3_region[data_wave3$cw3_region==12] <- "Northern Ireland"
data_wave3$cw3_region[data_wave3$cw3_region==-8] <- "Don't Know"
data_wave3$cw3_region[data_wave3$cw3_region==-1] <- "Not Applicable"

temp_cw3_region <- table(data_wave3$cw3_region)
temp_cw3_region
temp_cw3_regionnum <- c(temp_cw3_region[5], temp_cw3_region[6], temp_cw3_region[14], 
                        temp_cw3_region[3], temp_cw3_region[13], temp_cw3_region[4], 
                        temp_cw3_region[1], temp_cw3_region[10], temp_cw3_region[11], temp_cw3_region[12],
                        temp_cw3_region[9], temp_cw3_region[7], temp_cw3_region[2], temp_cw3_region[8])
temp_cw3_regionnum
temp_per_cw3_region <- round(100*temp_cw3_regionnum / nrow(data_wave3),1)
temp_per_cw3_region

healthcare_tbl1$cw3_npercent[4:17] <- paste0(temp_cw3_regionnum, "(", temp_per_cw3_region, ")")

healthcare_tbl1




#####pmeddifw_1 Why had difficulty obtaining prescribed medication: Shortage of supply. 

data_wave3$cw3_pmeddifw_1[data_wave3$cw3_pmeddifw_1==1] <- "Yes"
data_wave3$cw3_pmeddifw_1[data_wave3$cw3_pmeddifw_1==2] <- "No"
data_wave3$cw3_pmeddifw_1[data_wave3$cw3_pmeddifw_1==-8] <- "Don't Know"
data_wave3$cw3_pmeddifw_1[data_wave3$cw3_pmeddifw_1==-1] <- "Not Applicable"
temp_cw3_pmeddifw_1 <- table(data_wave3$cw3_pmeddifw_1)
temp_cw3_pmeddifw_1
temp_cw3_pmeddifw_1num <- c(temp_cw3_pmeddifw_1[4], temp_cw3_pmeddifw_1[2], temp_cw3_pmeddifw_1[1], 
                            temp_cw3_pmeddifw_1[3])
temp_cw3_pmeddifw_1num
temp_per_cw3_pmeddifw_1 <- round(100*temp_cw3_pmeddifw_1num/ nrow(data_wave3),1)
temp_per_cw3_pmeddifw_1

healthcare_tbl1$cw3_npercent[18:22] <- paste0(temp_cw3_pmeddifw_1num, "(", temp_per_cw3_pmeddifw_1, ")")

healthcare_tbl1

###### Why had difficulty obtaining prescribed medication: Nobody was able to collect it. 

data_wave3$cw3_pmeddifw_2[data_wave3$cw3_pmeddifw_2==1] <- "Yes"
data_wave3$cw3_pmeddifw_2[data_wave3$cw3_pmeddifw_2==2] <- "No"
data_wave3$cw3_pmeddifw_2[data_wave3$cw3_pmeddifw_2==-8] <- "Don't Know"
data_wave3$cw3_pmeddifw_2[data_wave3$cw3_pmeddifw_2==-1] <- "Not Applicable"

temp_cw3_pmeddifw_2 <- table(data_wave3$cw3_pmeddifw_2)
temp_cw3_pmeddifw_2
temp_cw3_pmeddifw_2num <- c(temp_cw3_pmeddifw_2[4], temp_cw3_pmeddifw_2[2], temp_cw3_pmeddifw_2[1], 
                            temp_cw3_pmeddifw_2[3])
temp_cw3_pmeddifw_2num
temp_per_cw3_pmeddifw_2 <- round(100*temp_cw3_pmeddifw_2num/ nrow(data_wave3),1)
temp_per_cw3_pmeddifw_2

healthcare_tbl1$cw3_npercent[23:26] <- paste0(temp_cw3_pmeddifw_2num, "(", temp_per_cw3_pmeddifw_2, ")")

healthcare_tbl1


######## Why had difficulty obtaining prescribed medication: Other reason. 

data_wave3$cw3_pmeddifw_3[data_wave3$cw3_pmeddifw_3==1] <- "Yes"
data_wave3$cw3_pmeddifw_3[data_wave3$cw3_pmeddifw_3==2] <- "No"
data_wave3$cw3_pmeddifw_3[data_wave3$cw3_pmeddifw_3==-8] <- "Don't Know"
data_wave3$cw3_pmeddifw_3[data_wave3$cw3_pmeddifw_3==-1] <- "Not Applicable"

temp_cw3_pmeddifw_3 <- table(data_wave3$cw3_pmeddifw_3)
temp_cw3_pmeddifw_3
temp_cw3_pmeddifw_3num <- c(temp_cw3_pmeddifw_3[4], temp_cw3_pmeddifw_3[2], temp_cw3_pmeddifw_3[1], 
                            temp_cw3_pmeddifw_3[3])
temp_cw3_pmeddifw_3num
temp_per_cw3_pmeddifw_3 <- round(100*temp_cw3_pmeddifw_3num/ nrow(data_wave3),1)
temp_per_cw3_pmeddifw_3

healthcare_tbl1$cw3_npercent[27:30] <- paste0(temp_cw3_pmeddifw_3num, "(", temp_per_cw3_pmeddifw_3, ")")

healthcare_tbl1


##### Medical appointments booked: Hospital appointment for consultation, investigation, or treatment. 

data_wave3$cw3_appmed_1[data_wave3$cw3_appmed_1==1] <- "Yes"
data_wave3$cw3_appmed_1[data_wave3$cw3_appmed_1==2] <- "No"
data_wave3$cw3_appmed_1[data_wave3$cw3_appmed_1==-9] <- "Don't want to answer"
data_wave3$cw3_appmed_1[data_wave3$cw3_appmed_1==-8] <- "Don't Know"
data_wave3$cw3_appmed_1[data_wave3$cw3_appmed_1==-1] <- "Not Applicable"

temp_cw3_appmed_1 <- table(data_wave3$cw3_appmed_1)
temp_cw3_appmed_1
temp_cw3_appmed_1num <- c(temp_cw3_appmed_1[5], temp_cw3_appmed_1[3], temp_cw3_appmed_1[2], 
                          temp_cw3_appmed_1[1], temp_cw3_appmed_1[4])
temp_cw3_appmed_1num
temp_per_cw3_appmed_1 <- round(100*temp_cw3_appmed_1num/ nrow(data_wave3),1)
temp_per_cw3_appmed_1

healthcare_tbl1$cw3_npercent[31:35] <- paste0(temp_cw3_appmed_1num, "(", temp_per_cw3_appmed_1, ")")

healthcare_tbl1


###### Medical appointments booked: Hospital appointment for surgery. 

data_wave3$cw3_appmed_2[data_wave3$cw3_appmed_2==1] <- "Yes"
data_wave3$cw3_appmed_2[data_wave3$cw3_appmed_2==2] <- "No"
data_wave3$cw3_appmed_2[data_wave3$cw3_appmed_2==-9] <- "Don't want to answer"
data_wave3$cw3_appmed_2[data_wave3$cw3_appmed_2==-8] <- "Don't Know"
data_wave3$cw3_appmed_2[data_wave3$cw3_appmed_2==-1] <- "Not Applicable"

temp_cw3_appmed_2 <- table(data_wave3$cw3_appmed_2)
temp_cw3_appmed_2
temp_cw3_appmed_2num <- c(temp_cw3_appmed_2[5], temp_cw3_appmed_2[3], temp_cw3_appmed_2[2], 
                          temp_cw3_appmed_2[1], temp_cw3_appmed_2[4])
temp_cw3_appmed_2num
temp_per_cw3_appmed_2 <- round(100*temp_cw3_appmed_2num/ nrow(data_wave3),1)
temp_per_cw3_appmed_2

healthcare_tbl1$cw3_npercent[36:40] <- paste0(temp_cw3_appmed_2num, "(", temp_per_cw3_appmed_2, ")")

healthcare_tbl1


###### Medical appointments booked: GP appointment.

data_wave3$cw3_appmed_3[data_wave3$cw3_appmed_3==1] <- "Yes"
data_wave3$cw3_appmed_3[data_wave3$cw3_appmed_3==2] <- "No"
data_wave3$cw3_appmed_3[data_wave3$cw3_appmed_3==-9] <- "Don't want to answer"
data_wave3$cw3_appmed_3[data_wave3$cw3_appmed_3==-8] <- "Don't Know"
data_wave3$cw3_appmed_3[data_wave3$cw3_appmed_3==-1] <- "Not Applicable"

temp_cw3_appmed_3 <- table(data_wave3$cw3_appmed_3)
temp_cw3_appmed_3
temp_cw3_appmed_3num <- c(temp_cw3_appmed_3[5], temp_cw3_appmed_3[3], temp_cw3_appmed_3[2], 
                          temp_cw3_appmed_3[1], temp_cw3_appmed_3[4])
temp_cw3_appmed_3num
temp_per_cw3_appmed_3 <- round(100*temp_cw3_appmed_3num/ nrow(data_wave3),1)
temp_per_cw3_appmed_3

healthcare_tbl1$cw3_npercent[41:45] <- paste0(temp_cw3_appmed_3num, "(", temp_per_cw3_appmed_3, ")")

healthcare_tbl1


###### Medical appointments booked: Appointment for CBT, counselling or psychological therapy

data_wave3$cw3_appmed_4[data_wave3$cw3_appmed_4==1] <- "Yes"
data_wave3$cw3_appmed_4[data_wave3$cw3_appmed_4==2] <- "No"
data_wave3$cw3_appmed_4[data_wave3$cw3_appmed_4==-9] <- "Don't want to answer"
data_wave3$cw3_appmed_4[data_wave3$cw3_appmed_4==-8] <- "Don't Know"
data_wave3$cw3_appmed_4[data_wave3$cw3_appmed_4==-1] <- "Not Applicable"

temp_cw3_appmed_4 <- table(data_wave3$cw3_appmed_4)
temp_cw3_appmed_4
temp_cw3_appmed_4num <- c(temp_cw3_appmed_4[5], temp_cw3_appmed_4[3], temp_cw3_appmed_4[2], 
                          temp_cw3_appmed_4[1], temp_cw3_appmed_4[4])
temp_cw3_appmed_4num
temp_per_cw3_appmed_4 <- round(100*temp_cw3_appmed_4num/ nrow(data_wave3),1)
temp_per_cw3_appmed_4

healthcare_tbl1$cw3_npercent[46:50] <- paste0(temp_cw3_appmed_4num, "(", temp_per_cw3_appmed_4, ")")

healthcare_tbl1


#Type of medical appointment cancelled or delayed: Hospital appointment for consultation, investigation or treatment 

data_wave3$cw3_appcant_1[data_wave3$cw3_appcant_1==1] <- "Yes"
data_wave3$cw3_appcant_1[data_wave3$cw3_appcant_1==2] <- "No"
data_wave3$cw3_appcant_1[data_wave3$cw3_appcant_1==-8] <- "Don't Know"
data_wave3$cw3_appcant_1[data_wave3$cw3_appcant_1==-1] <- "Not Applicable"

temp_cw3_appcant_1 <- table(data_wave3$cw3_appcant_1)
temp_cw3_appcant_1
temp_cw3_appcant_1num <- c(temp_cw3_appcant_1[4], temp_cw3_appcant_1[2], temp_cw3_appcant_1[1], 
                           temp_cw3_appcant_1[3])
temp_cw3_appcant_1num
temp_per_cw3_appcant_1 <- round(100*temp_cw3_appcant_1num/ nrow(data_wave3),1)
temp_per_cw3_appcant_1

healthcare_tbl1$cw3_npercent[51:54] <- paste0(temp_cw3_appcant_1num, "(", temp_per_cw3_appcant_1, ")")

healthcare_tbl1


#### Type of medical appointment cancelled or delayed: Hospital appointment for surgery. 

data_wave3$cw3_appcant_2[data_wave3$cw3_appcant_2==1] <- "Yes"
data_wave3$cw3_appcant_2[data_wave3$cw3_appcant_2==2] <- "No"
data_wave3$cw3_appcant_2[data_wave3$cw3_appcant_2==-8] <- "Don't Know"
data_wave3$cw3_appcant_2[data_wave3$cw3_appcant_2==-1] <- "Not Applicable"

temp_cw3_appcant_2 <- table(data_wave3$cw3_appcant_2)
temp_cw3_appcant_2
temp_cw3_appcant_2num <- c(temp_cw3_appcant_2[4], temp_cw3_appcant_2[2], temp_cw3_appcant_2[1], 
                           temp_cw3_appcant_2[3])
temp_cw3_appcant_2num
temp_per_cw3_appcant_2 <- round(100*temp_cw3_appcant_2num/ nrow(data_wave3),1)
temp_per_cw3_appcant_2

healthcare_tbl1$cw3_npercent[55:58] <- paste0(temp_cw3_appcant_2num, "(", temp_per_cw3_appcant_2, ")")

healthcare_tbl1

##### Type of medical appointment cancelled or delayed: Appointment for CBT, counselling or psychological therapy. 

data_wave3$cw3_appcant_3[data_wave3$cw3_appcant_3==1] <- "Yes"
data_wave3$cw3_appcant_3[data_wave3$cw3_appcant_3==2] <- "No"
data_wave3$cw3_appcant_3[data_wave3$cw3_appcant_3==-8] <- "Don't Know"
data_wave3$cw3_appcant_3[data_wave3$cw3_appcant_3==-1] <- "Not Applicable"

temp_cw3_appcant_3 <- table(data_wave3$cw3_appcant_3)
temp_cw3_appcant_3
temp_cw3_appcant_3num <- c(temp_cw3_appcant_3[4], temp_cw3_appcant_3[2], temp_cw3_appcant_3[1], 
                           temp_cw3_appcant_3[3])
temp_cw3_appcant_3num
temp_per_cw3_appcant_3 <- round(100*temp_cw3_appcant_3num/ nrow(data_wave3),1)
temp_per_cw3_appcant_3

healthcare_tbl1$cw3_npercent[59:62] <- paste0(temp_cw3_appcant_3num, "(", temp_per_cw3_appcant_3, ")")

healthcare_tbl1

### Type of medical appointment cancelled or delayed: Any other medical appointment

data_wave3$cw3_appcant_4[data_wave3$cw3_appcant_4==1] <- "Yes"
data_wave3$cw3_appcant_4[data_wave3$cw3_appcant_4==2] <- "No"
data_wave3$cw3_appcant_4[data_wave3$cw3_appcant_4==-8] <- "Don't Know"
data_wave3$cw3_appcant_4[data_wave3$cw3_appcant_4==-1] <- "Not Applicable"

temp_cw3_appcant_4 <- table(data_wave3$cw3_appcant_4)
temp_cw3_appcant_4
temp_cw3_appcant_4num <- c(temp_cw3_appcant_4[4], temp_cw3_appcant_4[2], temp_cw3_appcant_4[1], 
                           temp_cw3_appcant_4[3])
temp_cw3_appcant_4num
temp_per_cw3_appcant_4 <- round(100*temp_cw3_appcant_4num/ nrow(data_wave3),1)
temp_per_cw3_appcant_4

healthcare_tbl1$cw3_npercent[63:66] <- paste0(temp_cw3_appcant_4num, "(", temp_per_cw3_appcant_4, ")")

healthcare_tbl1


##### Whether been able to concentrate 

data_wave1$cw1_ghq121[data_wave1$cw1_ghq121==1] <- "Better than usual"
data_wave1$cw1_ghq121[data_wave1$cw1_ghq121==2] <- "Same as usual"
data_wave1$cw1_ghq121[data_wave1$cw1_ghq121==3] <- "Less than usual"
data_wave1$cw1_ghq121[data_wave1$cw1_ghq121==4] <- "Much less than usual"
data_wave1$cw1_ghq121[data_wave1$cw1_ghq121==-9] <- "Don't want to answer"
data_wave1$cw1_ghq121[data_wave1$cw1_ghq121==-8] <- "No information"
data_wave1$cw1_ghq121[data_wave1$cw1_ghq121==-1] <- "Not Applicable"

temp_cw1_ghq121 <- table(data_wave1$cw1_ghq121)
temp_cw1_ghq121
temp_cw1_ghq121num <- c(temp_cw1_ghq121[1], temp_cw1_ghq121[6], temp_cw1_ghq121[2], 
                        temp_cw1_ghq121[3], temp_cw1_ghq121[7], temp_cw1_ghq121[4], temp_cw1_ghq121[5])
temp_cw1_ghq121num
temp_per_cw1_ghq121 <- round(100*temp_cw1_ghq121num/ nrow(data_wave1),1)
temp_per_cw1_ghq121

healthcare_tbl1$cw1_npercent[64:70] <- paste0(temp_cw1_ghq121num, "(", temp_per_cw1_ghq121, ")")

healthcare_tbl1



data_wave2$cw2_ghq121[data_wave2$cw2_ghq121==1] <- "Better than usual"
data_wave2$cw2_ghq121[data_wave2$cw2_ghq121==2] <- "Same as usual"
data_wave2$cw2_ghq121[data_wave2$cw2_ghq121==3] <- "Less than usual"
data_wave2$cw2_ghq121[data_wave2$cw2_ghq121==4] <- "Much less than usual"
data_wave2$cw2_ghq121[data_wave2$cw2_ghq121==-9] <- "Don't want to answer"
data_wave2$cw2_ghq121[data_wave2$cw2_ghq121==-8] <- "No information"
data_wave2$cw2_ghq121[data_wave2$cw2_ghq121==-1] <- "Not Applicable"

temp_cw2_ghq121 <- table(data_wave2$cw2_ghq121)
temp_cw2_ghq121
temp_cw2_ghq121num <- c(temp_cw2_ghq121[1], temp_cw2_ghq121[7], temp_cw2_ghq121[3], 
                        temp_cw2_ghq121[4], temp_cw2_ghq121[2], temp_cw2_ghq121[5], temp_cw2_ghq121[6])
temp_cw2_ghq121num
temp_per_cw2_ghq121 <- round(100*temp_cw2_ghq121num/ nrow(data_wave2),1)
temp_per_cw2_ghq121

healthcare_tbl1$cw2_npercent[64:70] <- paste0(temp_cw2_ghq121num, "(", temp_per_cw2_ghq121, ")")

healthcare_tbl1


data_wave3$cw3_ghq121[data_wave3$cw3_ghq121==1] <- "Better than usual"
data_wave3$cw3_ghq121[data_wave3$cw3_ghq121==2] <- "Same as usual"
data_wave3$cw3_ghq121[data_wave3$cw3_ghq121==3] <- "Less than usual"
data_wave3$cw3_ghq121[data_wave3$cw3_ghq121==4] <- "Much less than usual"
data_wave3$cw3_ghq121[data_wave3$cw3_ghq121==-9] <- "Don't want to answer"
data_wave3$cw3_ghq121[data_wave3$cw3_ghq121==-8] <- "No information"
data_wave3$cw3_ghq121[data_wave3$cw3_ghq121==-1] <- "Not Applicable"

temp_cw3_ghq121 <- table(data_wave3$cw3_ghq121)
temp_cw3_ghq121
temp_cw3_ghq121num <- c(temp_cw3_ghq121[1], temp_cw3_ghq121[7], temp_cw3_ghq121[3], 
                        temp_cw3_ghq121[4], temp_cw3_ghq121[2], temp_cw3_ghq121[5], temp_cw3_ghq121[6])
temp_cw3_ghq121num
temp_per_cw3_ghq121 <- round(100*temp_cw3_ghq121num/ nrow(data_wave3),1)
temp_per_cw3_ghq121

healthcare_tbl1$cw3_npercent[64:70] <- paste0(temp_cw3_ghq121num, "(", temp_per_cw3_ghq121, ")")

healthcare_tbl1

#### Whether lost much sleep over worry

data_wave1$cw1_ghq122[data_wave1$cw1_ghq122==1] <- "Not at all"
data_wave1$cw1_ghq122[data_wave1$cw1_ghq122==2] <- "No more than usual"
data_wave1$cw1_ghq122[data_wave1$cw1_ghq122==3] <- "Rather more than usual"
data_wave1$cw1_ghq122[data_wave1$cw1_ghq122==4] <- "Much more than usual"
data_wave1$cw1_ghq122[data_wave1$cw1_ghq122==-9] <- "Don't want to answer"
data_wave1$cw1_ghq122[data_wave1$cw1_ghq122==-8] <- "No information"
data_wave1$cw1_ghq122[data_wave1$cw1_ghq122==-1] <- "Not Applicable"

temp_cw1_ghq122 <- table(data_wave1$cw1_ghq122)
temp_cw1_ghq122
temp_cw1_ghq122num <- c(temp_cw1_ghq122[5], temp_cw1_ghq122[3], temp_cw1_ghq122[6], 
                        temp_cw1_ghq122[1], temp_cw1_ghq122[7], temp_cw1_ghq122[2], temp_cw1_ghq122[4])
temp_cw1_ghq122num
temp_per_cw1_ghq122 <- round(100*temp_cw1_ghq122num/ nrow(data_wave1),1)
temp_per_cw1_ghq122

healthcare_tbl1$cw1_npercent[71:77] <- paste0(temp_cw1_ghq122num, "(", temp_per_cw1_ghq122, ")")

healthcare_tbl1


data_wave2$cw2_ghq122[data_wave2$cw2_ghq122==1] <- "Not at all"
data_wave2$cw2_ghq122[data_wave2$cw2_ghq122==2] <- "No more than usual"
data_wave2$cw2_ghq122[data_wave2$cw2_ghq122==3] <- "Rather more than usual"
data_wave2$cw2_ghq122[data_wave2$cw2_ghq122==4] <- "Much more than usual"
data_wave2$cw2_ghq122[data_wave2$cw2_ghq122==-9] <- "Don't want to answer"
data_wave2$cw2_ghq122[data_wave2$cw2_ghq122==-8] <- "No information"
data_wave2$cw2_ghq122[data_wave2$cw2_ghq122==-1] <- "Not Applicable"

temp_cw2_ghq122 <- table(data_wave2$cw2_ghq122)
temp_cw2_ghq122
temp_cw2_ghq122num <- c(temp_cw2_ghq122[5], temp_cw2_ghq122[3], temp_cw2_ghq122[6], 
                        temp_cw2_ghq122[2], temp_cw2_ghq122[1], temp_cw2_ghq122[7], temp_cw2_ghq122[4])
temp_cw2_ghq122num
temp_per_cw2_ghq122 <- round(100*temp_cw2_ghq122num/ nrow(data_wave2),1)
temp_per_cw2_ghq122

healthcare_tbl1$cw2_npercent[71:77] <- paste0(temp_cw2_ghq122num, "(", temp_per_cw2_ghq122, ")")

healthcare_tbl1


data_wave3$cw3_ghq122[data_wave3$cw3_ghq122==1] <- "Not at all"
data_wave3$cw3_ghq122[data_wave3$cw3_ghq122==2] <- "No more than usual"
data_wave3$cw3_ghq122[data_wave3$cw3_ghq122==3] <- "Rather more than usual"
data_wave3$cw3_ghq122[data_wave3$cw3_ghq122==4] <- "Much more than usual"
data_wave3$cw3_ghq122[data_wave3$cw3_ghq122==-9] <- "Don't want to answer"
data_wave3$cw3_ghq122[data_wave3$cw3_ghq122==-8] <- "No information"
data_wave3$cw3_ghq122[data_wave3$cw3_ghq122==-1] <- "Not Applicable"

temp_cw3_ghq122 <- table(data_wave3$cw3_ghq122)
temp_cw3_ghq122
temp_cw3_ghq122num <- c(temp_cw3_ghq122[6], temp_cw3_ghq122[4], temp_cw3_ghq122[7], 
                        temp_cw3_ghq122[2], temp_cw3_ghq122[1], temp_cw3_ghq122[3], temp_cw3_ghq122[5])
temp_cw3_ghq122num
temp_per_cw3_ghq122 <- round(100*temp_cw3_ghq122num/ nrow(data_wave3),1)
temp_per_cw3_ghq122

healthcare_tbl1$cw3_npercent[71:77] <- paste0(temp_cw3_ghq122num, "(", temp_per_cw3_ghq122, ")")

healthcare_tbl1

###### Whether felt that playing a useful part in things

data_wave1$cw1_ghq123[data_wave1$cw1_ghq123==1] <- "more so than usual "
data_wave1$cw1_ghq123[data_wave1$cw1_ghq123==2] <- "Same as usual"
data_wave1$cw1_ghq123[data_wave1$cw1_ghq123==3] <- "Less useful than usual"
data_wave1$cw1_ghq123[data_wave1$cw1_ghq123==4] <- "Much less useful"
data_wave1$cw1_ghq123[data_wave1$cw1_ghq123==-9] <- "Don't want to answer"
data_wave1$cw1_ghq123[data_wave1$cw1_ghq123==-8] <- "No information"
data_wave1$cw1_ghq123[data_wave1$cw1_ghq123==-1] <- "Not Applicable"

temp_cw1_ghq123 <- table(data_wave1$cw1_ghq123)
temp_cw1_ghq123
temp_cw1_ghq123num <- c(temp_cw1_ghq123[2], temp_cw1_ghq123[6], temp_cw1_ghq123[1], 
                        temp_cw1_ghq123[3], temp_cw1_ghq123[7], temp_cw1_ghq123[4], temp_cw1_ghq123[5])
temp_cw1_ghq123num
temp_per_cw1_ghq123 <- round(100*temp_cw1_ghq123num/ nrow(data_wave1),1)
temp_per_cw1_ghq123

healthcare_tbl1$cw1_npercent[78:84] <- paste0(temp_cw1_ghq123num, "(", temp_per_cw1_ghq123, ")")

healthcare_tbl1


data_wave2$cw2_ghq123[data_wave2$cw2_ghq123==1] <- "more so than usual"
data_wave2$cw2_ghq123[data_wave2$cw2_ghq123==2] <- "Same as usual"
data_wave2$cw2_ghq123[data_wave2$cw2_ghq123==3] <-"Less useful than usual"
data_wave2$cw2_ghq123[data_wave2$cw2_ghq123==4] <- "Much less useful"
data_wave2$cw2_ghq123[data_wave2$cw2_ghq123==-9] <- "Don't want to answer"
data_wave2$cw2_ghq123[data_wave2$cw2_ghq123==-8] <- "No information"
data_wave2$cw2_ghq123[data_wave2$cw2_ghq123==-1] <- "Not Applicable"

temp_cw2_ghq123 <- table(data_wave2$cw2_ghq123)
temp_cw2_ghq123
temp_cw2_ghq123num <- c(temp_cw2_ghq123[3], temp_cw2_ghq123[7], temp_cw2_ghq123[2], 
                        temp_cw2_ghq123[4], temp_cw2_ghq123[1], temp_cw2_ghq123[5], temp_cw2_ghq123[6])
temp_cw2_ghq123num
temp_per_cw2_ghq123 <- round(100*temp_cw2_ghq123num/ nrow(data_wave2),1)
temp_per_cw2_ghq123

healthcare_tbl1$cw2_npercent[78:84] <- paste0(temp_cw2_ghq123num, "(", temp_per_cw2_ghq123, ")")

healthcare_tbl1


data_wave3$cw3_ghq123[data_wave3$cw3_ghq123==1] <- "more so than usual"
data_wave3$cw3_ghq123[data_wave3$cw3_ghq123==2] <- "Same as usual"
data_wave3$cw3_ghq123[data_wave3$cw3_ghq123==3] <- "Less useful than usual"
data_wave3$cw3_ghq123[data_wave3$cw3_ghq123==4] <- "Much less useful"
data_wave3$cw3_ghq123[data_wave3$cw3_ghq123==-9] <- "Don't want to answer"
data_wave3$cw3_ghq123[data_wave3$cw3_ghq123==-8] <- "No information"
data_wave3$cw3_ghq123[data_wave3$cw3_ghq123==-1] <- "Not Applicable"

temp_cw3_ghq123 <- table(data_wave3$cw3_ghq123)
temp_cw3_ghq123
temp_cw3_ghq123num <- c(temp_cw3_ghq123[3], temp_cw3_ghq123[7], temp_cw3_ghq123[2], 
                        temp_cw3_ghq123[4], temp_cw3_ghq123[1], temp_cw3_ghq123[5], temp_cw3_ghq123[6])
temp_cw3_ghq123num
temp_per_cw3_ghq123<- round(100*temp_cw3_ghq123num/ nrow(data_wave3),1)
temp_per_cw3_ghq123

healthcare_tbl1$cw3_npercent[78:84] <- paste0(temp_cw3_ghq123num, "(", temp_per_cw3_ghq123, ")")

healthcare_tbl1


##### Whether felt capable of making decisions about things 

data_wave1$cw1_ghq124[data_wave1$cw1_ghq124==1] <- "more so than usual "
data_wave1$cw1_ghq124[data_wave1$cw1_ghq124==2] <- "Same as usual"
data_wave1$cw1_ghq124[data_wave1$cw1_ghq124==3] <- "Less than usual"
data_wave1$cw1_ghq124[data_wave1$cw1_ghq124==4] <- "Much less useful"
data_wave1$cw1_ghq124[data_wave1$cw1_ghq124==-9] <- "Don't want to answer"
data_wave1$cw1_ghq124[data_wave1$cw1_ghq124==-8] <- "No information"
data_wave1$cw1_ghq124[data_wave1$cw1_ghq124==-1] <- "Not Applicable"

temp_cw1_ghq124 <- table(data_wave1$cw1_ghq124)
temp_cw1_ghq124
temp_cw1_ghq124num <- c(temp_cw1_ghq124[1], temp_cw1_ghq124[6], temp_cw1_ghq124[2], 
                        temp_cw1_ghq124[3], temp_cw1_ghq124[7], temp_cw1_ghq124[4], temp_cw1_ghq124[5])
temp_cw1_ghq124num
temp_per_cw1_ghq124 <- round(100*temp_cw1_ghq124num/ nrow(data_wave1),1)
temp_per_cw1_ghq124

healthcare_tbl1$cw1_npercent[85:91] <- paste0(temp_cw1_ghq124num, "(", temp_per_cw1_ghq124, ")")

healthcare_tbl1


data_wave2$cw2_ghq124[data_wave2$cw2_ghq124==1] <- "more so than usual"
data_wave2$cw2_ghq124[data_wave2$cw2_ghq124==2] <- "Same as usual"
data_wave2$cw2_ghq124[data_wave2$cw2_ghq124==3] <- "Less than usual"
data_wave2$cw2_ghq124[data_wave2$cw2_ghq124==4] <- "Much less useful"
data_wave2$cw2_ghq124[data_wave2$cw2_ghq124==-9] <- "Don't want to answer"
data_wave2$cw2_ghq124[data_wave2$cw2_ghq124==-8] <- "No information"
data_wave2$cw2_ghq124[data_wave2$cw2_ghq124==-1] <- "Not Applicable"

temp_cw2_ghq124 <- table(data_wave2$cw2_ghq124)
temp_cw2_ghq124
temp_cw2_ghq124num <- c(temp_cw2_ghq124[1], temp_cw2_ghq124[7], temp_cw2_ghq124[3], 
                        temp_cw2_ghq124[4], temp_cw2_ghq124[2], temp_cw2_ghq124[5], temp_cw2_ghq124[6])
temp_cw2_ghq124num
temp_per_cw2_ghq124 <- round(100*temp_cw2_ghq124num/ nrow(data_wave2),1)
temp_per_cw2_ghq124

healthcare_tbl1$cw2_npercent[85:91] <- paste0(temp_cw2_ghq124num, "(", temp_per_cw2_ghq124, ")")

healthcare_tbl1


data_wave3$cw3_ghq124[data_wave3$cw3_ghq124==1] <- "more so than usual"
data_wave3$cw3_ghq124[data_wave3$cw3_ghq124==2] <- "Same as usual"
data_wave3$cw3_ghq124[data_wave3$cw3_ghq124==3] <- "Less than usual"
data_wave3$cw3_ghq124[data_wave3$cw3_ghq124==4] <- "Much less useful"
data_wave3$cw3_ghq124[data_wave3$cw3_ghq124==-9] <- "Don't want to answer"
data_wave3$cw3_ghq124[data_wave3$cw3_ghq124==-8] <- "No information"
data_wave3$cw3_ghq124[data_wave3$cw3_ghq124==-1] <- "Not Applicable"

temp_cw3_ghq124 <- table(data_wave3$cw3_ghq124)
temp_cw3_ghq124
temp_cw3_ghq124num <- c(temp_cw3_ghq124[1], temp_cw3_ghq124[7], temp_cw3_ghq124[3], 
                        temp_cw3_ghq124[4], temp_cw3_ghq124[2], temp_cw3_ghq124[5], temp_cw3_ghq124[6])
temp_cw3_ghq124num
temp_per_cw3_ghq124 <- round(100*temp_cw3_ghq124num/ nrow(data_wave3),1)
temp_per_cw3_ghq124

healthcare_tbl1$cw3_npercent[85:91] <- paste0(temp_cw3_ghq124num, "(", temp_per_cw3_ghq124, ")")

healthcare_tbl1


##### Whether felt constantly under strain

data_wave1$cw1_ghq125[data_wave1$cw1_ghq125==1] <- "Not at all"
data_wave1$cw1_ghq125[data_wave1$cw1_ghq125==2] <- "No more than usual"
data_wave1$cw1_ghq125[data_wave1$cw1_ghq125==3] <- "Rather more than usual"
data_wave1$cw1_ghq125[data_wave1$cw1_ghq125==4] <- "Much more than usual"
data_wave1$cw1_ghq125[data_wave1$cw1_ghq125==-9] <- "Don't want to answer"
data_wave1$cw1_ghq125[data_wave1$cw1_ghq125==-8] <- "No information"
data_wave1$cw1_ghq125[data_wave1$cw1_ghq125==-1] <- "Not Applicable"

temp_cw1_ghq125 <- table(data_wave1$cw1_ghq125)
temp_cw1_ghq125
temp_cw1_ghq125num <- c(temp_cw1_ghq125[5], temp_cw1_ghq125[3], temp_cw1_ghq125[6], 
                        temp_cw1_ghq125[1], temp_cw1_ghq125[7], temp_cw1_ghq125[2], temp_cw1_ghq125[4])
temp_cw1_ghq125num
temp_per_cw1_ghq125 <- round(100*temp_cw1_ghq125num/ nrow(data_wave1),1)
temp_per_cw1_ghq125

healthcare_tbl1$cw1_npercent[92:98] <- paste0(temp_cw1_ghq125num, "(", temp_per_cw1_ghq125, ")")

healthcare_tbl1


data_wave2$cw2_ghq125[data_wave2$cw2_ghq125==1] <- "Not at all"
data_wave2$cw2_ghq125[data_wave2$cw2_ghq125==2] <- "No more than usual"
data_wave2$cw2_ghq125[data_wave2$cw2_ghq125==3] <- "Rather more than usual"
data_wave2$cw2_ghq125[data_wave2$cw2_ghq125==4] <- "Much more than usual"
data_wave2$cw2_ghq125[data_wave2$cw2_ghq125==-9] <- "Don't want to answer"
data_wave2$cw2_ghq125[data_wave2$cw2_ghq125==-8] <- "No information"
data_wave2$cw2_ghq125[data_wave2$cw2_ghq125==-1] <- "Not Applicable"

temp_cw2_ghq125 <- table(data_wave2$cw2_ghq125)
temp_cw2_ghq125
temp_cw2_ghq125num <- c(temp_cw2_ghq125[5], temp_cw2_ghq125[3], temp_cw2_ghq125[6], 
                        temp_cw2_ghq125[2], temp_cw2_ghq125[1], temp_cw2_ghq125[7], temp_cw2_ghq125[4])
temp_cw2_ghq125num
temp_per_cw2_ghq125 <- round(100*temp_cw2_ghq125num/ nrow(data_wave2),1)
temp_per_cw2_ghq125

healthcare_tbl1$cw2_npercent[92:98] <- paste0(temp_cw2_ghq125num, "(", temp_per_cw2_ghq125, ")")

healthcare_tbl1


data_wave3$cw3_ghq125[data_wave3$cw3_ghq125==1] <- "Not at all"
data_wave3$cw3_ghq125[data_wave3$cw3_ghq125==2] <- "No more than usual"
data_wave3$cw3_ghq125[data_wave3$cw3_ghq125==3] <- "Rather more than usual"
data_wave3$cw3_ghq125[data_wave3$cw3_ghq125==4] <- "Much more than usual"
data_wave3$cw3_ghq125[data_wave3$cw3_ghq125==-9] <- "Don't want to answer"
data_wave3$cw3_ghq125[data_wave3$cw3_ghq125==-8] <- "No information"
data_wave3$cw3_ghq125[data_wave3$cw3_ghq125==-1] <- "Not Applicable"

temp_cw3_ghq125 <- table(data_wave3$cw3_ghq125)
temp_cw3_ghq125
temp_cw3_ghq125num <- c(temp_cw3_ghq125[6], temp_cw3_ghq125[4], temp_cw3_ghq125[7], 
                        temp_cw3_ghq125[2], temp_cw3_ghq125[1], temp_cw3_ghq125[3], temp_cw3_ghq125[5])
temp_cw3_ghq125num
temp_per_cw3_ghq125 <- round(100*temp_cw3_ghq125num/ nrow(data_wave3),1)
temp_per_cw3_ghq125

healthcare_tbl1$cw3_npercent[92:98] <- paste0(temp_cw3_ghq125num, "(", temp_per_cw3_ghq125, ")")

healthcare_tbl1


##### Whether couldn’t? overcome difficulties 

data_wave1$cw1_ghq126[data_wave1$cw1_ghq126==1] <- "Not at all"
data_wave1$cw1_ghq126[data_wave1$cw1_ghq126==2] <- "No more than usual"
data_wave1$cw1_ghq126[data_wave1$cw1_ghq126==3] <- "Rather more than usual"
data_wave1$cw1_ghq126[data_wave1$cw1_ghq126==4] <- "Much more than usual"
data_wave1$cw1_ghq126[data_wave1$cw1_ghq126==-9] <- "Don't want to answer"
data_wave1$cw1_ghq126[data_wave1$cw1_ghq126==-8] <- "No information"
data_wave1$cw1_ghq126[data_wave1$cw1_ghq126==-1] <- "Not Applicable"

temp_cw1_ghq126 <- table(data_wave1$cw1_ghq126)
temp_cw1_ghq126
temp_cw1_ghq126num <- c(temp_cw1_ghq126[5], temp_cw1_ghq126[3], temp_cw1_ghq126[6], 
                        temp_cw1_ghq126[1], temp_cw1_ghq126[7], temp_cw1_ghq126[2], temp_cw1_ghq126[4])
temp_cw1_ghq126num
temp_per_cw1_ghq126 <- round(100*temp_cw1_ghq126num/ nrow(data_wave1),1)
temp_per_cw1_ghq126

healthcare_tbl1$cw1_npercent[99:105] <- paste0(temp_cw1_ghq126num, "(", temp_per_cw1_ghq126, ")")

healthcare_tbl1


data_wave2$cw2_ghq126[data_wave2$cw2_ghq126==1] <- "Not at all"
data_wave2$cw2_ghq126[data_wave2$cw2_ghq126==2] <- "No more than usual"
data_wave2$cw2_ghq126[data_wave2$cw2_ghq126==3] <- "Rather more than usual"
data_wave2$cw2_ghq126[data_wave2$cw2_ghq126==4] <- "Much more than usual"
data_wave2$cw2_ghq126[data_wave2$cw2_ghq126==-9] <- "Don't want to answer"
data_wave2$cw2_ghq126[data_wave2$cw2_ghq126==-8] <- "No information"
data_wave2$cw2_ghq126[data_wave2$cw2_ghq126==-1] <- "Not Applicable"

temp_cw2_ghq126 <- table(data_wave2$cw2_ghq126)
temp_cw2_ghq126
temp_cw2_ghq126num <- c(temp_cw2_ghq126[5], temp_cw2_ghq126[3], temp_cw2_ghq126[6], 
                        temp_cw2_ghq126[2], temp_cw2_ghq126[1], temp_cw2_ghq126[7], temp_cw2_ghq126[4])
temp_cw2_ghq126num
temp_per_cw2_ghq126 <- round(100*temp_cw2_ghq126num/ nrow(data_wave2),1)
temp_per_cw2_ghq126

healthcare_tbl1$cw2_npercent[99:105] <- paste0(temp_cw2_ghq126num, "(", temp_per_cw2_ghq126, ")")

healthcare_tbl1


data_wave3$cw3_ghq126[data_wave3$cw3_ghq126==1] <- "Not at all"
data_wave3$cw3_ghq126[data_wave3$cw3_ghq126==2] <- "No more than usual"
data_wave3$cw3_ghq126[data_wave3$cw3_ghq126==3] <- "Rather more than usual"
data_wave3$cw3_ghq126[data_wave3$cw3_ghq126==4] <- "Much more than usual"
data_wave3$cw3_ghq126[data_wave3$cw3_ghq126==-9] <- "Don't want to answer"
data_wave3$cw3_ghq126[data_wave3$cw3_ghq126==-8] <- "No information"
data_wave3$cw3_ghq126[data_wave3$cw3_ghq126==-1] <- "Not Applicable"

temp_cw3_ghq126 <- table(data_wave3$cw3_ghq126)
temp_cw3_ghq126
temp_cw3_ghq126num <- c(temp_cw3_ghq126[6], temp_cw3_ghq126[4], temp_cw3_ghq126[7], 
                        temp_cw3_ghq126[2], temp_cw3_ghq126[1], temp_cw3_ghq126[3], temp_cw3_ghq126[5])
temp_cw3_ghq126num
temp_per_cw3_ghq126 <- round(100*temp_cw3_ghq126num/ nrow(data_wave3),1)
temp_per_cw3_ghq126

healthcare_tbl1$cw3_npercent[99:105] <- paste0(temp_cw3_ghq126num, "(", temp_per_cw3_ghq126, ")")

healthcare_tbl1


##### Whether been able to enjoy normal day to day activities

data_wave1$cw1_ghq127[data_wave1$cw1_ghq127==1] <- "more so than usual "
data_wave1$cw1_ghq127[data_wave1$cw1_ghq127==2] <- "Same as usual"
data_wave1$cw1_ghq127[data_wave1$cw1_ghq127==3] <- "Less than usual"
data_wave1$cw1_ghq127[data_wave1$cw1_ghq127==4] <- "Much less than usual"
data_wave1$cw1_ghq127[data_wave1$cw1_ghq127==-9] <- "Don't want to answer"
data_wave1$cw1_ghq127[data_wave1$cw1_ghq127==-8] <- "No information"
data_wave1$cw1_ghq127[data_wave1$cw1_ghq127==-1] <- "Not Applicable"

temp_cw1_ghq127 <- table(data_wave1$cw1_ghq127)
temp_cw1_ghq12
temp_cw1_ghq127num <- c(temp_cw1_ghq127[2], temp_cw1_ghq127[6], temp_cw1_ghq127[1], 
                        temp_cw1_ghq127[3], temp_cw1_ghq127[4], temp_cw1_ghq127[7], temp_cw1_ghq127[5])
temp_cw1_ghq127num
temp_per_cw1_ghq127 <- round(100*temp_cw1_ghq127num/ nrow(data_wave1),1)
temp_per_cw1_ghq127

healthcare_tbl1$cw1_npercent[106:112] <- paste0(temp_cw1_ghq127num, "(", temp_per_cw1_ghq127, ")")

healthcare_tbl1


data_wave2$cw2_ghq127[data_wave2$cw2_ghq127==1] <- "more so than usual"
data_wave2$cw2_ghq127[data_wave2$cw2_ghq127==2] <- "Same as usual"
data_wave2$cw2_ghq127[data_wave2$cw2_ghq127==3] <- "Less than usual"
data_wave2$cw2_ghq127[data_wave2$cw2_ghq127==4] <- "Much less than usual"
data_wave2$cw2_ghq127[data_wave2$cw2_ghq127==-9] <- "Don't want to answer"
data_wave2$cw2_ghq127[data_wave2$cw2_ghq127==-8] <- "No information"
data_wave2$cw2_ghq127[data_wave2$cw2_ghq127==-1] <- "Not Applicable"

temp_cw2_ghq127 <- table(data_wave2$cw2_ghq127)
temp_cw2_ghq127
temp_cw2_ghq127num <- c(temp_cw2_ghq127[3], temp_cw2_ghq127[7], temp_cw2_ghq127[2], 
                        temp_cw2_ghq127[4], temp_cw2_ghq127[1], temp_cw2_ghq127[5], temp_cw2_ghq127[6])
temp_cw2_ghq127num
temp_per_cw2_ghq127 <- round(100*temp_cw2_ghq127num/ nrow(data_wave2),1)
temp_per_cw2_ghq127

healthcare_tbl1$cw2_npercent[106:112] <- paste0(temp_cw2_ghq127num, "(", temp_per_cw2_ghq127, ")")

healthcare_tbl1


data_wave3$cw3_ghq127[data_wave3$cw3_ghq127==1] <- "more so than usual"
data_wave3$cw3_ghq127[data_wave3$cw3_ghq127==2] <- "Same as usual"
data_wave3$cw3_ghq127[data_wave3$cw3_ghq127==3] <- "Less than usual"
data_wave3$cw3_ghq127[data_wave3$cw3_ghq127==4] <- "Much less than usual"
data_wave3$cw3_ghq127[data_wave3$cw3_ghq127==-9] <- "Don't want to answer"
data_wave3$cw3_ghq127[data_wave3$cw3_ghq127==-8] <- "No information"
data_wave3$cw3_ghq127[data_wave3$cw3_ghq127==-1] <- "Not Applicable"

temp_cw3_ghq127 <- table(data_wave3$cw3_ghq127)
temp_cw3_ghq127
temp_cw3_ghq127num <- c(temp_cw3_ghq127[3], temp_cw3_ghq127[7], temp_cw3_ghq127[2], 
                        temp_cw3_ghq127[4], temp_cw3_ghq127[1], temp_cw3_ghq127[5], temp_cw3_ghq127[6])
temp_cw3_ghq127num
temp_per_cw3_ghq127 <- round(100*temp_cw3_ghq127num/ nrow(data_wave3),1)
temp_per_cw3_ghq127

healthcare_tbl1$cw3_npercent[106:112] <- paste0(temp_cw3_ghq127num, "(", temp_per_cw3_ghq127, ")")

healthcare_tbl1

###### Whether been able to face up to problems

data_wave1$cw1_ghq128[data_wave1$cw1_ghq128==1] <- "more so than usual "
data_wave1$cw1_ghq128[data_wave1$cw1_ghq128==2] <- "Same as usual"
data_wave1$cw1_ghq128[data_wave1$cw1_ghq128==3] <- "Less than usual"
data_wave1$cw1_ghq128[data_wave1$cw1_ghq128==4] <- "Much less able"
data_wave1$cw1_ghq128[data_wave1$cw1_ghq128==-9] <- "Don't want to answer"
data_wave1$cw1_ghq128[data_wave1$cw1_ghq128==-8] <- "No information"
data_wave1$cw1_ghq128[data_wave1$cw1_ghq128==-1] <- "Not Applicable"

temp_cw1_ghq128 <- table(data_wave1$cw1_ghq128)
temp_cw1_ghq128
temp_cw1_ghq128num <- c(temp_cw1_ghq128[2], temp_cw1_ghq128[6], temp_cw1_ghq128[1], 
                        temp_cw1_ghq128[3], temp_cw1_ghq128[7], temp_cw1_ghq128[4], temp_cw1_ghq128[5])
temp_cw1_ghq128num
temp_per_cw1_ghq128 <- round(100*temp_cw1_ghq128num/ nrow(data_wave1),1)
temp_per_cw1_ghq128

healthcare_tbl1$cw1_npercent[113:119] <- paste0(temp_cw1_ghq128num, "(", temp_per_cw1_ghq128, ")")

healthcare_tbl1


data_wave2$cw2_ghq128[data_wave2$cw2_ghq128==1] <- "more so than usual"
data_wave2$cw2_ghq128[data_wave2$cw2_ghq128==2] <- "Same as usual"
data_wave2$cw2_ghq128[data_wave2$cw2_ghq128==3] <- "Less than usual"
data_wave2$cw2_ghq128[data_wave2$cw2_ghq128==4] <- "Much less able"
data_wave2$cw2_ghq128[data_wave2$cw2_ghq128==-9] <- "Don't want to answer"
data_wave2$cw2_ghq128[data_wave2$cw2_ghq128==-8] <- "No information"
data_wave2$cw2_ghq128[data_wave2$cw2_ghq128==-1] <- "Not Applicable"

temp_cw2_ghq128 <- table(data_wave2$cw2_ghq128)
temp_cw2_ghq128
temp_cw2_ghq128num <- c(temp_cw2_ghq128[3], temp_cw2_ghq128[7], temp_cw2_ghq128[2], 
                        temp_cw2_ghq128[4], temp_cw2_ghq128[1], temp_cw2_ghq128[5], temp_cw2_ghq128[6])
temp_cw2_ghq128num
temp_per_cw2_ghq128 <- round(100*temp_cw2_ghq128num/ nrow(data_wave2),1)
temp_per_cw2_ghq128

healthcare_tbl1$cw2_npercent[113:119] <- paste0(temp_cw2_ghq128num, "(", temp_per_cw2_ghq128, ")")

healthcare_tbl1


data_wave3$cw3_ghq128[data_wave3$cw3_ghq128==1] <- "more so than usual"
data_wave3$cw3_ghq128[data_wave3$cw3_ghq128==2] <- "Same as usual"
data_wave3$cw3_ghq128[data_wave3$cw3_ghq128==3] <- "Less than usual"
data_wave3$cw3_ghq128[data_wave3$cw3_ghq128==4] <- "Much less able"
data_wave3$cw3_ghq128[data_wave3$cw3_ghq128==-9] <- "Don't want to answer"
data_wave3$cw3_ghq128[data_wave3$cw3_ghq128==-8] <- "No information"
data_wave3$cw3_ghq128[data_wave3$cw3_ghq128==-1] <- "Not Applicable"

temp_cw3_ghq128 <- table(data_wave3$cw3_ghq128)
temp_cw3_ghq128
temp_cw3_ghq128num <- c(temp_cw3_ghq128[3], temp_cw3_ghq128[7], temp_cw3_ghq128[2], 
                        temp_cw3_ghq128[4], temp_cw3_ghq128[1], temp_cw3_ghq128[5], temp_cw3_ghq128[6])
temp_cw3_ghq128num
temp_per_cw3_ghq128 <- round(100*temp_cw3_ghq128num/ nrow(data_wave3),1)
temp_per_cw3_ghq128

healthcare_tbl1$cw3_npercent[113:119] <- paste0(temp_cw3_ghq128num, "(", temp_per_cw3_ghq128, ")")

healthcare_tbl1


##### Whether been feeling unhappy or depressed 

data_wave1$cw1_ghq129[data_wave1$cw1_ghq129==1] <- "Not at all"
data_wave1$cw1_ghq129[data_wave1$cw1_ghq129==2] <- "No more than usual"
data_wave1$cw1_ghq129[data_wave1$cw1_ghq129==3] <- "Rather more than usual"
data_wave1$cw1_ghq129[data_wave1$cw1_ghq129==4] <- "Much more than usual"
data_wave1$cw1_ghq129[data_wave1$cw1_ghq129==-9] <- "Don't want to answer"
data_wave1$cw1_ghq129[data_wave1$cw1_ghq129==-8] <- "No information"
data_wave1$cw1_ghq129[data_wave1$cw1_ghq129==-1] <- "Not Applicable"

temp_cw1_ghq129 <- table(data_wave1$cw1_ghq129)
temp_cw1_ghq129
temp_cw1_ghq129num <- c(temp_cw1_ghq129[5], temp_cw1_ghq129[3], temp_cw1_ghq129[6], 
                        temp_cw1_ghq129[1], temp_cw1_ghq129[7], temp_cw1_ghq129[2], temp_cw1_ghq129[4])
temp_cw1_ghq129num
temp_per_cw1_ghq129 <- round(100*temp_cw1_ghq129num/ nrow(data_wave1),1)
temp_per_cw1_ghq129

healthcare_tbl1$cw1_npercent[120:126] <- paste0(temp_cw1_ghq129num, "(", temp_per_cw1_ghq129, ")")

healthcare_tbl1


data_wave2$cw2_ghq129[data_wave2$cw2_ghq129==1] <- "Not at all"
data_wave2$cw2_ghq129[data_wave2$cw2_ghq129==2] <- "No more than usual"
data_wave2$cw2_ghq129[data_wave2$cw2_ghq129==3] <- "Rather more than usual"
data_wave2$cw2_ghq129[data_wave2$cw2_ghq129==4] <- "Much more than usual"
data_wave2$cw2_ghq129[data_wave2$cw2_ghq129==-9] <- "Don't want to answer"
data_wave2$cw2_ghq129[data_wave2$cw2_ghq129==-8] <- "No information"
data_wave2$cw2_ghq129[data_wave2$cw2_ghq129==-1] <- "Not Applicable"

temp_cw2_ghq129 <- table(data_wave2$cw2_ghq129)
temp_cw2_ghq129
temp_cw2_ghq129num <- c(temp_cw2_ghq129[5], temp_cw2_ghq129[3], temp_cw2_ghq129[6], 
                        temp_cw2_ghq129[2], temp_cw2_ghq129[1], temp_cw2_ghq129[7], temp_cw2_ghq129[4])
temp_cw2_ghq129num
temp_per_cw2_ghq129 <- round(100*temp_cw2_ghq129num/ nrow(data_wave2),1)
temp_per_cw2_ghq129

healthcare_tbl1$cw2_npercent[120:126] <- paste0(temp_cw2_ghq129num, "(", temp_per_cw2_ghq129, ")")

healthcare_tbl1


data_wave3$cw3_ghq129[data_wave3$cw3_ghq129==1] <- "Not at all"
data_wave3$cw3_ghq129[data_wave3$cw3_ghq129==2] <- "No more than usual"
data_wave3$cw3_ghq129[data_wave3$cw3_ghq129==3] <- "Rather more than usual"
data_wave3$cw3_ghq129[data_wave3$cw3_ghq129==4] <- "Much more than usual"
data_wave3$cw3_ghq129[data_wave3$cw3_ghq129==-9] <- "Don't want to answer"
data_wave3$cw3_ghq129[data_wave3$cw3_ghq129==-8] <- "No information"
data_wave3$cw3_ghq129[data_wave3$cw3_ghq129==-1] <- "Not Applicable"

temp_cw3_ghq129 <- table(data_wave3$cw3_ghq129)
temp_cw3_ghq129
temp_cw3_ghq129num <- c(temp_cw3_ghq129[6], temp_cw3_ghq129[4], temp_cw3_ghq129[7], 
                        temp_cw3_ghq129[2], temp_cw3_ghq129[1], temp_cw3_ghq129[3], temp_cw3_ghq129[5])
temp_cw3_ghq129num
temp_per_cw3_ghq129 <- round(100*temp_cw3_ghq129num/ nrow(data_wave3),1)
temp_per_cw3_ghq129

healthcare_tbl1$cw3_npercent[120:126] <- paste0(temp_cw3_ghq129num, "(", temp_per_cw3_ghq129, ")")

healthcare_tbl1

##### Whether been losing confidence in self 

data_wave1$cw1_ghq1210[data_wave1$cw1_ghq1210==1] <- "Not at all"
data_wave1$cw1_ghq1210[data_wave1$cw1_ghq1210==2] <- "No more than usual"
data_wave1$cw1_ghq1210[data_wave1$cw1_ghq1210==3] <- "Rather more than usual"
data_wave1$cw1_ghq1210[data_wave1$cw1_ghq1210==4] <- "Much more than usual"
data_wave1$cw1_ghq1210[data_wave1$cw1_ghq1210==-9] <- "Don't want to answer"
data_wave1$cw1_ghq1210[data_wave1$cw1_ghq1210==-8] <- "No information"
data_wave1$cw1_ghq1210[data_wave1$cw1_ghq1210==-1] <- "Not Applicable"

temp_cw1_ghq1210 <- table(data_wave1$cw1_ghq1210)
temp_cw1_ghq1210
temp_cw1_ghq1210num <- c(temp_cw1_ghq1210[5], temp_cw1_ghq1210[3], temp_cw1_ghq1210[6], 
                        temp_cw1_ghq1210[1], temp_cw1_ghq1210[7], temp_cw1_ghq1210[2], temp_cw1_ghq1210[4])
temp_cw1_ghq1210num
temp_per_cw1_ghq1210 <- round(100*temp_cw1_ghq1210num/ nrow(data_wave1),1)
temp_per_cw1_ghq1210

healthcare_tbl1$cw1_npercent[127:133] <- paste0(temp_cw1_ghq1210num, "(", temp_per_cw1_ghq1210, ")")

healthcare_tbl1


data_wave2$cw2_ghq1210[data_wave2$cw2_ghq1210==1] <- "Not at all"
data_wave2$cw2_ghq1210[data_wave2$cw2_ghq1210==2] <- "No more than usual"
data_wave2$cw2_ghq1210[data_wave2$cw2_ghq1210==3] <- "Rather more than usual"
data_wave2$cw2_ghq1210[data_wave2$cw2_ghq1210==4] <- "Much more than usual"
data_wave2$cw2_ghq1210[data_wave2$cw2_ghq1210==-9] <- "Don't want to answer"
data_wave2$cw2_ghq1210[data_wave2$cw2_ghq1210==-8] <- "No information"
data_wave2$cw2_ghq1210[data_wave2$cw2_ghq1210==-1] <- "Not Applicable"

temp_cw2_ghq1210 <- table(data_wave2$cw2_ghq1210)
temp_cw2_ghq1210
temp_cw2_ghq1210num <- c(temp_cw2_ghq1210[5], temp_cw2_ghq1210[3], temp_cw2_ghq1210[6], 
                        temp_cw2_ghq1210[2], temp_cw2_ghq1210[1], temp_cw2_ghq1210[7], temp_cw2_ghq1210[4])
temp_cw2_ghq1210num
temp_per_cw2_ghq1210 <- round(100*temp_cw2_ghq1210num/ nrow(data_wave2),1)
temp_per_cw2_ghq1210

healthcare_tbl1$cw2_npercent[127:133] <- paste0(temp_cw2_ghq1210num, "(", temp_per_cw2_ghq1210, ")")

healthcare_tbl1


data_wave3$cw3_ghq1210[data_wave3$cw3_ghq1210==1] <- "Not at all"
data_wave3$cw3_ghq1210[data_wave3$cw3_ghq1210==2] <- "No more than usual"
data_wave3$cw3_ghq1210[data_wave3$cw3_ghq1210==3] <- "Rather more than usual"
data_wave3$cw3_ghq1210[data_wave3$cw3_ghq1210==4] <- "Much more than usual"
data_wave3$cw3_ghq1210[data_wave3$cw3_ghq1210==-9] <- "Don't want to answer"
data_wave3$cw3_ghq1210[data_wave3$cw3_ghq1210==-8] <- "No information"
data_wave3$cw3_ghq1210[data_wave3$cw3_ghq1210==-1] <- "Not Applicable"

temp_cw3_ghq1210 <- table(data_wave3$cw3_ghq1210)
temp_cw3_ghq1210
temp_cw3_ghq1210num <- c(temp_cw3_ghq1210[6], temp_cw3_ghq1210[4], temp_cw3_ghq1210[7], 
                        temp_cw3_ghq1210[2], temp_cw3_ghq1210[1], temp_cw3_ghq1210[3], temp_cw3_ghq1210[5])
temp_cw3_ghq1210num
temp_per_cw3_ghq1210 <- round(100*temp_cw3_ghq1210num/ nrow(data_wave3),1)
temp_per_cw3_ghq1210

healthcare_tbl1$cw3_npercent[127:133] <- paste0(temp_cw3_ghq1210num, "(", temp_per_cw3_ghq1210, ")")

healthcare_tbl1

#### Whether been thinking of self worthless person

data_wave1$cw1_ghq1211[data_wave1$cw1_ghq1211==1] <- "Not at all"
data_wave1$cw1_ghq1211[data_wave1$cw1_ghq1211==2] <- "No more than usual"
data_wave1$cw1_ghq1211[data_wave1$cw1_ghq1211==3] <- "Rather more than usual"
data_wave1$cw1_ghq1211[data_wave1$cw1_ghq1211==4] <- "Much more than usual"
data_wave1$cw1_ghq1211[data_wave1$cw1_ghq1211==-9] <- "Don't want to answer"
data_wave1$cw1_ghq1211[data_wave1$cw1_ghq1211==-8] <- "No information"
data_wave1$cw1_ghq1211[data_wave1$cw1_ghq1211==-1] <- "Not Applicable"

temp_cw1_ghq1211 <- table(data_wave1$cw1_ghq1211)
temp_cw1_ghq1211
temp_cw1_ghq1211num <- c(temp_cw1_ghq1211[5], temp_cw1_ghq1211[3], temp_cw1_ghq1211[6], 
                         temp_cw1_ghq1211[1], temp_cw1_ghq1211[7], temp_cw1_ghq1211[2], temp_cw1_ghq1211[4])
temp_cw1_ghq1211num
temp_per_cw1_ghq1211 <- round(100*temp_cw1_ghq1211num/ nrow(data_wave1),1)
temp_per_cw1_ghq1211

healthcare_tbl1$cw1_npercent[134:140] <- paste0(temp_cw1_ghq1211num, "(", temp_per_cw1_ghq1211, ")")

healthcare_tbl1


data_wave2$cw2_ghq1211[data_wave2$cw2_ghq1211==1] <- "Not at all"
data_wave2$cw2_ghq1211[data_wave2$cw2_ghq1211==2] <- "No more than usual"
data_wave2$cw2_ghq1211[data_wave2$cw2_ghq1211==3] <- "Rather more than usual"
data_wave2$cw2_ghq1211[data_wave2$cw2_ghq1211==4] <- "Much more than usual"
data_wave2$cw2_ghq1211[data_wave2$cw2_ghq1211==-9] <- "Don't want to answer"
data_wave2$cw2_ghq1211[data_wave2$cw2_ghq1211==-8] <- "No information"
data_wave2$cw2_ghq1211[data_wave2$cw2_ghq1211==-1] <- "Not Applicable"

temp_cw2_ghq1211 <- table(data_wave2$cw2_ghq1211)
temp_cw2_ghq1211
temp_cw2_ghq1211num <- c(temp_cw2_ghq1211[6], temp_cw2_ghq1211[4], temp_cw2_ghq1211[7], 
                         temp_cw2_ghq1211[2], temp_cw2_ghq1211[1], temp_cw2_ghq1211[3], temp_cw2_ghq1211[5])
temp_cw2_ghq1211num
temp_per_cw2_ghq1211 <- round(100*temp_cw2_ghq1211num/ nrow(data_wave2),1)
temp_per_cw2_ghq1211

healthcare_tbl1$cw2_npercent[134:140] <- paste0(temp_cw2_ghq1211num, "(", temp_per_cw2_ghq1211, ")")

healthcare_tbl1


data_wave3$cw3_ghq1211[data_wave3$cw3_ghq1211==1] <- "Not at all"
data_wave3$cw3_ghq1211[data_wave3$cw3_ghq1211==2] <- "No more than usual"
data_wave3$cw3_ghq1211[data_wave3$cw3_ghq1211==3] <- "Rather more than usual"
data_wave3$cw3_ghq1211[data_wave3$cw3_ghq1211==4] <- "Much more than usual"
data_wave3$cw3_ghq1211[data_wave3$cw3_ghq1211==-9] <- "Don't want to answer"
data_wave3$cw3_ghq1211[data_wave3$cw3_ghq1211==-8] <- "No information"
data_wave3$cw3_ghq1211[data_wave3$cw3_ghq1211==-1] <- "Not Applicable"

temp_cw3_ghq1211 <- table(data_wave3$cw3_ghq1211)
temp_cw3_ghq1211
temp_cw3_ghq1211num <- c(temp_cw3_ghq1211[6], temp_cw3_ghq1211[4], temp_cw3_ghq1211[7], 
                         temp_cw3_ghq1211[2], temp_cw3_ghq1211[1], temp_cw3_ghq1211[3], temp_cw3_ghq1211[5])
temp_cw3_ghq1211num
temp_per_cw3_ghq1211 <- round(100*temp_cw3_ghq1211num/ nrow(data_wave3),1)
temp_per_cw3_ghq1211

healthcare_tbl1$cw3_npercent[134:140] <- paste0(temp_cw3_ghq1211num, "(", temp_per_cw3_ghq1211, ")")

healthcare_tbl1


######## Whether been feeling reasonably happy

data_wave1$cw1_ghq1212[data_wave1$cw1_ghq1212==1] <- "more so than usual "
data_wave1$cw1_ghq1212[data_wave1$cw1_ghq1212==2] <- "Same as usual"
data_wave1$cw1_ghq1212[data_wave1$cw1_ghq1212==3] <- "Less than usual"
data_wave1$cw1_ghq1212[data_wave1$cw1_ghq1212==4] <- "Much less useful"
data_wave1$cw1_ghq1212[data_wave1$cw1_ghq1212==-9] <- "Don't want to answer"
data_wave1$cw1_ghq1212[data_wave1$cw1_ghq1212==-8] <- "No information"
data_wave1$cw1_ghq1212[data_wave1$cw1_ghq1212==-1] <- "Not Applicable"

temp_cw1_ghq1212<- table(data_wave1$cw1_ghq1212)
temp_cw1_ghq1212
temp_cw1_ghq1212num <- c(temp_cw1_ghq1212[2], temp_cw1_ghq1212[6], temp_cw1_ghq1212[1], 
                        temp_cw1_ghq1212[3], temp_cw1_ghq1212[7], temp_cw1_ghq1212[4], temp_cw1_ghq1212[5])
temp_cw1_ghq1212num
temp_per_ghq1212 <- round(100*temp_cw1_ghq1212num/ nrow(data_wave1),1)
temp_per_ghq1212

healthcare_tbl1$cw1_npercent[141:147] <- paste0(temp_cw1_ghq1212num, "(", temp_per_cw1_ghq1212, ")")

healthcare_tbl1


data_wave2$cw2_ghq1212[data_wave2$cw2_ghq1212==1] <- "more so than usual"
data_wave2$cw2_ghq1212[data_wave2$cw2_ghq1212==2] <- "Same as usual"
data_wave2$cw2_ghq1212[data_wave2$cw2_ghq1212==3] <- "Less than usual"
data_wave2$cw2_ghq1212[data_wave2$cw2_ghq1212==4] <- "Much less useful"
data_wave2$cw2_ghq1212[data_wave2$cw2_ghq1212==-9] <- "Don't want to answer"
data_wave2$cw2_ghq1212[data_wave2$cw2_ghq1212==-8] <- "No information"
data_wave2$cw2_ghq1212[data_wave2$cw2_ghq1212==-1] <- "Not Applicable"

temp_cw2_ghq1212 <- table(data_wave2$cw2_ghq1212)
temp_cw2_ghq1212
temp_cw2_ghq1212num <- c(temp_cw2_ghq1212[3], temp_cw2_ghq1212[7], temp_cw2_ghq1212[2], 
                        temp_cw2_ghq1212[4], temp_cw2_ghq1212[1], temp_cw2_ghq1212[5], temp_cw2_ghq1212[6])
temp_cw2_ghq1212num
temp_per_cw2_ghq1212<- round(100*temp_cw2_ghq1212num/ nrow(data_wave2),1)
temp_per_cw2_ghq1212

healthcare_tbl1$cw2_npercent[141:147] <- paste0(temp_cw2_ghq1212num, "(", temp_per_cw2_ghq1212, ")")

healthcare_tbl1


data_wave3$cw3_ghq1212[data_wave3$cw3_ghq1212==1] <- "more so than usual"
data_wave3$cw3_ghq1212[data_wave3$cw3_ghq1212==2] <- "Same as usual"
data_wave3$cw3_ghq1212[data_wave3$cw3_ghq1212==3] <- "Less than usual"
data_wave3$cw3_ghq1212[data_wave3$cw3_ghq1212==4] <- "Much less useful"
data_wave3$cw3_ghq1212[data_wave3$cw3_ghq1212==-9] <- "Don't want to answer"
data_wave3$cw3_ghq1212[data_wave3$cw3_ghq1212==-8] <- "No information"
data_wave3$cw3_ghq1212[data_wave3$cw3_ghq1212==-1] <- "Not Applicable"

temp_cw3_ghq1212 <- table(data_wave3$cw3_ghq1212)
temp_cw3_ghq1212
temp_cw3_ghq1212num <- c(temp_cw3_ghq1212[3], temp_cw3_ghq1212[7], temp_cw3_ghq1212[2], 
                        temp_cw3_ghq1212[4], temp_cw3_ghq1212[1], temp_cw3_ghq1212[5], temp_cw3_ghq1212[6])
temp_cw3_ghq1212num
temp_per_cw3_ghq1212 <- round(100*temp_cw3_ghq1212num/ nrow(data_wave3),1)
temp_per_cw3_ghq1212

healthcare_tbl1$cw3_npercent[141:147] <- paste0(temp_cw3_ghq1212num, "(", temp_per_cw3_ghq1212, ")")

healthcare_tbl1



##### Past 2 weeks Feeling Nervous, Anxious or On Edge 


data_wave1$cw1_gad2phq2_1[data_wave1$cw1_gad2phq2_1==1] <- "Not at all"
data_wave1$cw1_gad2phq2_1[data_wave1$cw1_gad2phq2_1==2] <- "Several days"
data_wave1$cw1_gad2phq2_1[data_wave1$cw1_gad2phq2_1==3] <- "More than half the days"
data_wave1$cw1_gad2phq2_1[data_wave1$cw1_gad2phq2_1==4] <- "Nearly every day"
data_wave1$cw1_gad2phq2_1[data_wave1$cw1_gad2phq2_1==-9] <- "Don't want to answer"
data_wave1$cw1_gad2phq2_1[data_wave1$cw1_gad2phq2_1==-8] <- "Don’t Know"
data_wave1$cw1_gad2phq2_1[data_wave1$cw1_gad2phq2_1==-1] <- "Not Applicable"

temp_cw1_gad2phq2_1 <- table(data_wave1$cw1_gad2phq2_1)
temp_cw1_gad2phq2_1
temp_cw1_gad2phq2_1num <- c(temp_cw1_gad2phq2_1[4], temp_cw1_gad2phq2_1[5], temp_cw1_gad2phq2_1[2], 
                            temp_cw1_gad2phq2_1[3], temp_cw1_gad2phq2_1[6], temp_cw1_gad2phq2_1[1], temp_cw1_gad2phq2_1[7])
temp_cw1_gad2phq2_1num
temp_per_cw1_gad2phq2_1 <- round(100*temp_cw1_gad2phq2_1num/ nrow(data_wave1),1)
temp_per_cw1_gad2phq2_1

healthcare_tbl1$cw1_npercent[148:154] <- paste0(temp_cw1_gad2phq2_1num, "(", temp_per_cw1_gad2phq2_1, ")")

healthcare_tbl1


data_wave2$cw2_gad2phq2_1[data_wave2$cw2_gad2phq2_1==1] <- "Not at all"
data_wave2$cw2_gad2phq2_1[data_wave2$cw2_gad2phq2_1==2] <- "Several days"
data_wave2$cw2_gad2phq2_1[data_wave2$cw2_gad2phq2_1==3] <- "More than half the days"
data_wave2$cw2_gad2phq2_1[data_wave2$cw2_gad2phq2_1==4] <- "Nearly every day"
data_wave2$cw2_gad2phq2_1[data_wave2$cw2_gad2phq2_1==-9] <- "Don't want to answer"
data_wave2$cw2_gad2phq2_1[data_wave2$cw2_gad2phq2_1==-8] <- "Don’t Know"
data_wave2$cw2_gad2phq2_1[data_wave2$cw2_gad2phq2_1==-1] <- "Not Applicable"

temp_cw2_gad2phq2_1 <- table(data_wave2$cw2_gad2phq2_1)
temp_cw2_gad2phq2_1
temp_cw2_gad2phq2_1num <- c(temp_cw2_gad2phq2_1[6], temp_cw2_gad2phq2_1[7], temp_cw2_gad2phq2_1[3], 
                            temp_cw2_gad2phq2_1[4], temp_cw2_gad2phq2_1[1], temp_cw2_gad2phq2_1[2], temp_cw2_gad2phq2_1[5])
temp_cw2_gad2phq2_1num
temp_per_cw2_gad2phq2_1 <- round(100*temp_cw2_gad2phq2_1num/ nrow(data_wave2),1)
temp_per_cw2_gad2phq2_1

healthcare_tbl1$cw2_npercent[148:154] <- paste0(temp_cw2_gad2phq2_1num, "(", temp_per_cw2_gad2phq2_1, ")")

healthcare_tbl1


data_wave3$cw3_gad2phq2_1[data_wave3$cw3_gad2phq2_1==1] <- "Not at all"
data_wave3$cw3_gad2phq2_1[data_wave3$cw3_gad2phq2_1==2] <- "Several days"
data_wave3$cw3_gad2phq2_1[data_wave3$cw3_gad2phq2_1==3] <- "More than half the days"
data_wave3$cw3_gad2phq2_1[data_wave3$cw3_gad2phq2_1==4] <- "Nearly every day"
data_wave3$cw3_gad2phq2_1[data_wave3$cw3_gad2phq2_1==-9] <- "Don't want to answer"
data_wave3$cw3_gad2phq2_1[data_wave3$cw3_gad2phq2_1==-8] <- "Don’t Know"
data_wave3$cw3_gad2phq2_1[data_wave3$cw3_gad2phq2_1==-1] <- "Not Applicable"

temp_cw3_gad2phq2_1 <- table(data_wave3$cw3_gad2phq2_1)
temp_cw3_gad2phq2_1
temp_cw3_gad2phq2_1num <- c(temp_cw3_gad2phq2_1[6], temp_cw3_gad2phq2_1[7], temp_cw3_gad2phq2_1[3], 
                            temp_cw3_gad2phq2_1[4], temp_cw3_gad2phq2_1[1], temp_cw3_gad2phq2_1[2], temp_cw3_gad2phq2_1[5])
temp_cw3_gad2phq2_1num
temp_per_cw3_gad2phq2_1 <- round(100*temp_cw3_gad2phq2_1num/ nrow(data_wave3),1)
temp_per_cw3_gad2phq2_1

healthcare_tbl1$cw3_npercent[148:154] <- paste0(temp_cw3_gad2phq2_1num, "(", temp_per_cw3_gad2phq2_1, ")")

healthcare_tbl1


##### Past 2 weeks Not Being Able to Stop or Control Worrying 

data_wave1$cw1_gad2phq2_2[data_wave1$cw1_gad2phq2_2==1] <- "Not at all"
data_wave1$cw1_gad2phq2_2[data_wave1$cw1_gad2phq2_2==2] <- "Several days"
data_wave1$cw1_gad2phq2_2[data_wave1$cw1_gad2phq2_2==3] <- "More than half the days"
data_wave1$cw1_gad2phq2_2[data_wave1$cw1_gad2phq2_2==4] <- "Nearly every day"
data_wave1$cw1_gad2phq2_2[data_wave1$cw1_gad2phq2_2==-9] <- "Don't want to answer"
data_wave1$cw1_gad2phq2_2[data_wave1$cw1_gad2phq2_2==-8] <- "Don’t Know"
data_wave1$cw1_gad2phq2_2[data_wave1$cw1_gad2phq2_2==-1] <- "Not Applicable"

temp_cw1_gad2phq2_2 <- table(data_wave1$cw1_gad2phq2_2)
temp_cw1_gad2phq2_2
temp_cw1_gad2phq2_2num <- c(temp_cw1_gad2phq2_2[4], temp_cw1_gad2phq2_2[5], temp_cw1_gad2phq2_2[2], 
                            temp_cw1_gad2phq2_2[3], temp_cw1_gad2phq2_2[6], temp_cw1_gad2phq2_2[1], temp_cw1_gad2phq2_2[7])
temp_cw1_gad2phq2_2num
temp_per_cw1_gad2phq2_2 <- round(100*temp_cw1_gad2phq2_2num/ nrow(data_wave1),1)
temp_per_cw1_gad2phq2_2

healthcare_tbl1$cw1_npercent[155:161] <- paste0(temp_cw1_gad2phq2_2num, "(", temp_per_cw1_gad2phq2_2, ")")

healthcare_tbl1


data_wave2$cw2_gad2phq2_2[data_wave2$cw2_gad2phq2_2==1] <- "Not at all"
data_wave2$cw2_gad2phq2_2[data_wave2$cw2_gad2phq2_2==2] <- "Several days"
data_wave2$cw2_gad2phq2_2[data_wave2$cw2_gad2phq2_2==3] <- "More than half the days"
data_wave2$cw2_gad2phq2_2[data_wave2$cw2_gad2phq2_2==4] <- "Nearly every day"
data_wave2$cw2_gad2phq2_2[data_wave2$cw2_gad2phq2_2==-9] <- "Don't want to answer"
data_wave2$cw2_gad2phq2_2[data_wave2$cw2_gad2phq2_2==-8] <- "Don’t Know"
data_wave2$cw2_gad2phq2_2[data_wave2$cw2_gad2phq2_2==-1] <- "Not Applicable"

temp_cw2_gad2phq2_2 <- table(data_wave2$cw2_gad2phq2_2)
temp_cw2_gad2phq2_2
temp_cw2_gad2phq2_2num <- c(temp_cw2_gad2phq2_2[6], temp_cw2_gad2phq2_2[7], temp_cw2_gad2phq2_2[3], 
                            temp_cw2_gad2phq2_2[4], temp_cw2_gad2phq2_2[1], temp_cw2_gad2phq2_2[2], temp_cw2_gad2phq2_2[5])
temp_cw2_gad2phq2_2num
temp_per_cw2_gad2phq2_2 <- round(100*temp_cw2_gad2phq2_2num/ nrow(data_wave2),1)
temp_per_cw2_gad2phq2_2

healthcare_tbl1$cw2_npercent[155:161] <- paste0(temp_cw2_gad2phq2_2num, "(", temp_per_cw2_gad2phq2_2, ")")

healthcare_tbl1


data_wave3$cw3_gad2phq2_2[data_wave3$cw3_gad2phq2_2==1] <- "Not at all"
data_wave3$cw3_gad2phq2_2[data_wave3$cw3_gad2phq2_2==2] <- "Several days"
data_wave3$cw3_gad2phq2_2[data_wave3$cw3_gad2phq2_2==3] <- "More than half the days"
data_wave3$cw3_gad2phq2_2[data_wave3$cw3_gad2phq2_2==4] <- "Nearly every day"
data_wave3$cw3_gad2phq2_2[data_wave3$cw3_gad2phq2_2==-9] <- "Don't want to answer"
data_wave3$cw3_gad2phq2_2[data_wave3$cw3_gad2phq2_2==-8] <- "Don’t Know"
data_wave3$cw3_gad2phq2_2[data_wave3$cw3_gad2phq2_2==-1] <- "Not Applicable"

temp_cw3_gad2phq2_2 <- table(data_wave3$cw3_gad2phq2_2)
temp_cw3_gad2phq2_2
temp_cw3_gad2phq2_2num <- c(temp_cw3_gad2phq2_2[6], temp_cw3_gad2phq2_2[7], temp_cw3_gad2phq2_2[3], 
                            temp_cw3_gad2phq2_2[4], temp_cw3_gad2phq2_2[1], temp_cw3_gad2phq2_2[2], temp_cw3_gad2phq2_2[5])
temp_cw3_gad2phq2_2num
temp_per_cw3_gad2phq2_2 <- round(100*temp_cw3_gad2phq2_2num/ nrow(data_wave3),1)
temp_per_cw3_gad2phq2_2

healthcare_tbl1$cw3_npercent[155:161] <- paste0(temp_cw3_gad2phq2_2num, "(", temp_per_cw3_gad2phq2_2, ")")

healthcare_tbl1



##### Past 2 weeks Little Interest or Pleasure in Doing Things 

data_wave1$cw1_gad2phq2_3[data_wave1$cw1_gad2phq2_3==1] <- "Not at all"
data_wave1$cw1_gad2phq2_3[data_wave1$cw1_gad2phq2_3==2] <- "Several days"
data_wave1$cw1_gad2phq2_3[data_wave1$cw1_gad2phq2_3==3] <- "More than half the days"
data_wave1$cw1_gad2phq2_3[data_wave1$cw1_gad2phq2_3==4] <- "Nearly every day"
data_wave1$cw1_gad2phq2_3[data_wave1$cw1_gad2phq2_3==-9] <- "Don't want to answer"
data_wave1$cw1_gad2phq2_3[data_wave1$cw1_gad2phq2_3==-8] <- "Don’t Know"
data_wave1$cw1_gad2phq2_3[data_wave1$cw1_gad2phq2_3==-1] <- "Not Applicable"

temp_cw1_gad2phq2_3 <- table(data_wave1$cw1_gad2phq2_3)
temp_cw1_gad2phq2_3
temp_cw1_gad2phq2_3num <- c(temp_cw1_gad2phq2_3[4], temp_cw1_gad2phq2_3[5], temp_cw1_gad2phq2_3[2], 
                            temp_cw1_gad2phq2_3[3], temp_cw1_gad2phq2_3[6], temp_cw1_gad2phq2_3[1], temp_cw1_gad2phq2_3[7])
temp_cw1_gad2phq2_3num
temp_per_cw1_gad2phq2_3 <- round(100*temp_cw1_gad2phq2_3num/ nrow(data_wave1),1)
temp_per_cw1_gad2phq2_3

healthcare_tbl1$cw1_npercent[162:168] <- paste0(temp_cw1_gad2phq2_3num, "(", temp_per_cw1_gad2phq2_3, ")")

healthcare_tbl1


data_wave2$cw2_gad2phq2_3[data_wave2$cw2_gad2phq2_3==1] <- "Not at all"
data_wave2$cw2_gad2phq2_3[data_wave2$cw2_gad2phq2_3==2] <- "Several days"
data_wave2$cw2_gad2phq2_3[data_wave2$cw2_gad2phq2_3==3] <- "More than half the days"
data_wave2$cw2_gad2phq2_3[data_wave2$cw2_gad2phq2_3==4] <- "Nearly every day"
data_wave2$cw2_gad2phq2_3[data_wave2$cw2_gad2phq2_3==-9] <- "Don't want to answer"
data_wave2$cw2_gad2phq2_3[data_wave2$cw2_gad2phq2_3==-8] <- "Don’t Know"
data_wave2$cw2_gad2phq2_3[data_wave2$cw2_gad2phq2_3==-1] <- "Not Applicable"

temp_cw2_gad2phq2_3 <- table(data_wave2$cw2_gad2phq2_3)
temp_cw2_gad2phq2_3
temp_cw2_gad2phq2_3num <- c(temp_cw2_gad2phq2_3[6], temp_cw2_gad2phq2_3[7], temp_cw2_gad2phq2_3[3], 
                            temp_cw2_gad2phq2_3[4], temp_cw2_gad2phq2_3[1], temp_cw2_gad2phq2_3[2], temp_cw2_gad2phq2_3[5])
temp_cw2_gad2phq2_3num
temp_per_cw2_gad2phq2_3 <- round(100*temp_cw2_gad2phq2_3num/ nrow(data_wave2),1)
temp_per_cw2_gad2phq2_3

healthcare_tbl1$cw2_npercent[162:168] <- paste0(temp_cw2_gad2phq2_3num, "(", temp_per_cw2_gad2phq2_3, ")")

healthcare_tbl1


data_wave3$cw3_gad2phq2_3[data_wave3$cw3_gad2phq2_3==1] <- "Not at all"
data_wave3$cw3_gad2phq2_3[data_wave3$cw3_gad2phq2_3==2] <- "Several days"
data_wave3$cw3_gad2phq2_3[data_wave3$cw3_gad2phq2_3==3] <- "More than half the days"
data_wave3$cw3_gad2phq2_3[data_wave3$cw3_gad2phq2_3==4] <- "Nearly every day"
data_wave3$cw3_gad2phq2_3[data_wave3$cw3_gad2phq2_3==-9] <- "Don't want to answer"
data_wave3$cw3_gad2phq2_3[data_wave3$cw3_gad2phq2_3==-8] <- "Don’t Know"
data_wave3$cw3_gad2phq2_3[data_wave3$cw3_gad2phq2_3==-1] <- "Not Applicable"

temp_cw3_gad2phq2_3 <- table(data_wave3$cw3_gad2phq2_3)
temp_cw3_gad2phq2_3
temp_cw3_gad2phq2_3num <- c(temp_cw3_gad2phq2_3[6], temp_cw3_gad2phq2_3[7], temp_cw3_gad2phq2_3[3], 
                            temp_cw3_gad2phq2_3[4], temp_cw3_gad2phq2_3[1], temp_cw3_gad2phq2_3[2], temp_cw3_gad2phq2_3[5])
temp_cw3_gad2phq2_3num
temp_per_cw3_gad2phq2_3 <- round(100*temp_cw3_gad2phq2_3num/ nrow(data_wave3),1)
temp_per_cw3_gad2phq2_3

healthcare_tbl1$cw3_npercent[162:168] <- paste0(temp_cw3_gad2phq2_3num, "(", temp_per_cw3_gad2phq2_3, ")")

healthcare_tbl1



##### Past 2 weeks Feeling Down, Depressed or Hopeless 

data_wave1$cw1_gad2phq2_4[data_wave1$cw1_gad2phq2_4==1] <- "Not at all"
data_wave1$cw1_gad2phq2_4[data_wave1$cw1_gad2phq2_4==2] <- "Several days"
data_wave1$cw1_gad2phq2_4[data_wave1$cw1_gad2phq2_4==3] <- "More than half the days"
data_wave1$cw1_gad2phq2_4[data_wave1$cw1_gad2phq2_4==4] <- "Nearly every day"
data_wave1$cw1_gad2phq2_4[data_wave1$cw1_gad2phq2_4==-9] <- "Don't want to answer"
data_wave1$cw1_gad2phq2_4[data_wave1$cw1_gad2phq2_4==-8] <- "Don’t Know"
data_wave1$cw1_gad2phq2_4[data_wave1$cw1_gad2phq2_4==-1] <- "Not Applicable"

temp_cw1_gad2phq2_4 <- table(data_wave1$cw1_gad2phq2_4)
temp_cw1_gad2phq2_4
temp_cw1_gad2phq2_4num <- c(temp_cw1_gad2phq2_4[4], temp_cw1_gad2phq2_4[5], temp_cw1_gad2phq2_4[2], 
                            temp_cw1_gad2phq2_4[3], temp_cw1_gad2phq2_4[6], temp_cw1_gad2phq2_4[1], temp_cw1_gad2phq2_4[7])
temp_cw1_gad2phq2_4num
temp_per_cw1_gad2phq2_4 <- round(100*temp_cw1_gad2phq2_4num/ nrow(data_wave1),1)
temp_per_cw1_gad2phq2_4

healthcare_tbl1$cw1_npercent[169:175] <- paste0(temp_cw1_gad2phq2_4num, "(", temp_per_cw1_gad2phq2_4, ")")

healthcare_tbl1

data_wave2$cw2_gad2phq2_4[data_wave2$cw2_gad2phq2_4==1] <- "Not at all"
data_wave2$cw2_gad2phq2_4[data_wave2$cw2_gad2phq2_4==2] <- "Several days"
data_wave2$cw2_gad2phq2_4[data_wave2$cw2_gad2phq2_4==3] <- "More than half the days"
data_wave2$cw2_gad2phq2_4[data_wave2$cw2_gad2phq2_4==4] <- "Nearly every day"
data_wave2$cw2_gad2phq2_4[data_wave2$cw2_gad2phq2_4==-9] <- "Don't want to answer"
data_wave2$cw2_gad2phq2_4[data_wave2$cw2_gad2phq2_4==-8] <- "Don’t Know"
data_wave2$cw2_gad2phq2_4[data_wave2$cw2_gad2phq2_4==-1] <- "Not Applicable"

temp_cw2_gad2phq2_4 <- table(data_wave2$cw2_gad2phq2_4)
temp_cw2_gad2phq2_4
temp_cw2_gad2phq2_4num <- c(temp_cw2_gad2phq2_4[6], temp_cw2_gad2phq2_4[7], temp_cw2_gad2phq2_4[3], 
                            temp_cw2_gad2phq2_4[4], temp_cw2_gad2phq2_4[1], temp_cw2_gad2phq2_4[2], temp_cw2_gad2phq2_4[5])
temp_cw2_gad2phq2_4num
temp_per_cw2_gad2phq2_4 <- round(100*temp_cw2_gad2phq2_4num/ nrow(data_wave2),1)
temp_per_cw2_gad2phq2_4

healthcare_tbl1$cw2_npercent[169:175] <- paste0(temp_cw2_gad2phq2_4num, "(", temp_per_cw2_gad2phq2_4, ")")

healthcare_tbl1


data_wave3$cw3_gad2phq2_4[data_wave3$cw3_gad2phq2_4==1] <- "Not at all"
data_wave3$cw3_gad2phq2_4[data_wave3$cw3_gad2phq2_4==2] <- "Several days"
data_wave3$cw3_gad2phq2_4[data_wave3$cw3_gad2phq2_4==3] <- "More than half the days"
data_wave3$cw3_gad2phq2_4[data_wave3$cw3_gad2phq2_4==4] <- "Nearly every day"
data_wave3$cw3_gad2phq2_4[data_wave3$cw3_gad2phq2_4==-9] <- "Don't want to answer"
data_wave3$cw3_gad2phq2_4[data_wave3$cw3_gad2phq2_4==-8] <- "Don’t Know"
data_wave3$cw3_gad2phq2_4[data_wave3$cw3_gad2phq2_4==-1] <- "Not Applicable"

temp_cw3_gad2phq2_4 <- table(data_wave3$cw3_gad2phq2_4)
temp_cw3_gad2phq2_4
temp_cw3_gad2phq2_4num <- c(temp_cw3_gad2phq2_4[6], temp_cw3_gad2phq2_4[7], temp_cw3_gad2phq2_4[3], 
                            temp_cw3_gad2phq2_4[4], temp_cw3_gad2phq2_4[1], temp_cw3_gad2phq2_4[2], temp_cw3_gad2phq2_4[5])
temp_cw3_gad2phq2_4num
temp_per_cw3_gad2phq2_4 <- round(100*temp_cw3_gad2phq2_4num/ nrow(data_wave3),1)
temp_per_cw3_gad2phq2_4

healthcare_tbl1$cw3_npercent[169:175] <- paste0(temp_cw3_gad2phq2_4num, "(", temp_per_cw3_gad2phq2_4, ")")

healthcare_tbl1


write.csv(healthcare_tbl1, "output/table1.csv", row.names = F)


###### using multinom()function and vglm()function

#### multinom function does not require the data to be reshaped (as the mlogit package does)

#First, choose the level of outcome that is preferrably to be a baseline and specify this in the relevel function. 
#The multinom package does not include p-value calculation for the regression coefficients, so p-values are calculated using Wald tests (z-tests).
#data_wave2$cw2_psex <- releveldata_wave2$cw2_psex, ref = "female")

####checking and converting variables as factor
is.factor(data_wave2$cw2_pmeddif)  #to check if it is a factor

as.factor(data_wave2$cw2_pmeddif)  #to convert as factor

# Convert the pmediff variable to a factor
data_wave2$cw2_pmeddif <- as.factor(data_wave2$cw2_pmeddif)
is.factor(data_wave3$cw3_pmeddif)



###hfactor1
hfactor1model <- multinom(cw2_pmeddif ~ cw2_psex 
                          + cw2_region, 
                          data = data_wave2)
summary(hfactor1model)

#The multinom package does not include p-value calculation for the regression coefficients
#calculate p-values using Wald tests (z-tests).

z <- summary(hfactor1model)$coefficients/summary(hfactor1model)$standard.errors
z

#2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

## extract the coefficients from the model and exponentiate
exp(coef(hfactor1model))

#calculate predicted probabilities for each of our outcome levels using the fitted function

head(pp <- fitted(hfactor1model))

####vgml()function

hfactor1model <- vglm(cw2_pmeddif ~ cw2_psex 
                      + cw2_region,
                      data = data_wave2,
                      family = multinomial)

# Print the model summary
summary(hfactor1model)

###hfactor2
hfactor2model <- multinom(cw3_pmeddifw_1 ~ cw3_psex 
                          + cw3_region, 
                          data = data_wave3)
summary(hfactor2model)

#The multinom package does not include p-value calculation for the regression coefficients
#calculate p-values using Wald tests (z-tests).

z2 <- summary(hfactor2model)$coefficients/summary(hfactor2model)$standard.errors
z2

#2-tailed z test
p2 <- (1 - pnorm(abs(z2), 0, 1)) * 2
p2

## extract the coefficients from the model and exponentiate
exp(coef(hfactor2model))

#calculate predicted probabilities for each of our outcome levels using the fitted function

head(pp2 <- fitted(hfactor2model))


### vglm()function
hfactor2model <- vglm(cw3_pmeddif ~ cw3_psex 
                      + cw3_region,
                      data = data_wave3,
                      family = multinomial)

# Print the model summary
summary(hfactor2model)

####hfactor3
hfactor3model <- multinom(cw3_pmeddifw_1 ~ cw3_psex 
                          + cw3_region,
                          data = data_wave3)
summary(hfactor3model)

#The multinom package does not include p-value calculation for the regression coefficients
#calculate p-values using Wald tests (z-tests).

z3 <- summary(hfactor3model)$coefficients/summary(hfactor3model)$standard.errors
z3

#2-tailed z test
p3 <- (1 - pnorm(abs(z3), 0, 1)) * 2
p3

## extract the coefficients from the model and exponentiate
exp(coef(hfactor3model))

#calculate predicted probabilities for each of our outcome levels using the fitted function

head(pp3 <- fitted(hfactor3model))

####vglm
hfactor3model <- vglm(cw3_pmeddifw_1 ~ cw3_psex 
                      + cw3_region,
                      data = data_wave3,
                      family = multinomial)

# Print the model summary
summary(hfactor3model)

#####hfactor4
hfactor4model <- multinom(cw3_pmeddifw_2 ~ cw3_psex 
                          + cw3_region, 
                          data = data_wave3)
summary(hfactor4model)

#The multinom package does not include p-value calculation for the regression coefficients
#calculate p-values using Wald tests (z-tests).

z4 <- summary(hfactor4model)$coefficients/summary(hfactor4model)$standard.errors
z4

#2-tailed z test
p4 <- (1 - pnorm(abs(z4), 0, 1)) * 2
p4

## extract the coefficients from the model and exponentiate
exp(coef(hfactor4model))

#calculate predicted probabilities for each of our outcome levels using the fitted function

head(pp4 <- fitted(hfactor4model))


####vglm
hfactor4model <- vglm(cw3_pmeddifw_2 ~ cw3_psex 
                      + cw3_region,
                      data = data_wave3,
                      family = multinomial)

# Print the model summary
summary(hfactor4model)

###hfactor5
hfactor5model<- multinom(cw3_pmeddifw_3 ~ cw3_psex 
                         + cw3_region, 
                         data = data_wave3)
summary(hfactor5model)

#The multinom package does not include p-value calculation for the regression coefficients
#calculate p-values using Wald tests (z-tests).

z5 <- summary(hfactor5model)$coefficients/summary(hfactor5model)$standard.errors
z5

#2-tailed z test
p5 <- (1 - pnorm(abs(z5), 0, 1)) * 2
p5

## extract the coefficients from the model and exponentiate
exp(coef(hfactor5model))

#calculate predicted probabilities for each of our outcome levels using the fitted function

head(pp5 <- fitted(hfactor5model))

####vglm
hfactor5model <- vglm(cw3_pmeddifw_3 ~ cw3_psex 
                      + cw3_region,
                      data = data_wave3,
                      family = multinomial)

# Print the model summary
summary(hfactor5model)




###################################################################

data_wave3 <- data_wave3[!(data_wave3$cw3_appcant_1 %in% c(-1, -8)), ]
cw3_appcant_1 <- table(data_wave3$cw3_appcant_1)
cw3_appcant_1
data_wave3 <- data_wave3 %>% filter(data_wave3$cw3_appcant_1 != "-1" &
                                      data_wave3$cw3_appcant_1 != "-8")
table(data_wave3$cw3_appcant_1)
cw3_appcant_1 <- data_wave3 %>% filter(cw3_appcant_1 != "-1" &
                                         cw3_appcant_1 != "-8") %$%
  data_wave3$cw3_appcant_1 <- ifelse(data_wave3$cw3_appcant_1 =="No Access to Healthcare", 1,
                                     ifelse(data_wave3$cw3_appcant_1 =="Access to Healthcare", 2, NA_character_, data_wave3))  

factor(cw3_appcant_1, labels = c("1" = "Access to Healthcare",
                                 "2" = "No Access to Healthcare"))
cw3_appcant_1 <- data_wave3 %>% filter(cw3_appcant_1 != "-1" &
                                         cw3_appcant_1 != "-8") %$%
  factor(cw3_appcant_1, labels = c("1" = "Access to Healthcare",
                                   "2" = "No Access to Healthcare"))
