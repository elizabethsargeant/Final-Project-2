## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")

##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################


##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
table(data$Topic_Convo, data$Tone_Convo)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
chisq.test(table(data$Topic_Convo, data$Tone_Convo))

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
aov(Scene_Length ~ Topic_Convo, data = data)

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
cor(data$Number_Characters, data$Scene_Length)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(Number_Characters ~ Scene_Length, data = data)
summary(linear_relationship)

##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
linear_plot <- plot(data$Scene_Length, data$Number_Characters)
print(linear_plot)
abline(linear_relationship, col = "red")
abline(v = 61.45, h = 4.18, col = "red")

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$Scene_Length, residuals(linear_relationship))
abline(h = 0, col = "red")