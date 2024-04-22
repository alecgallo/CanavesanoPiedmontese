rm(list=ls())
#--------------------------------------
# load libraries

# reading/writing data
library(readr) 
library(writexl)
library(openxlsx)
library(base)
library(stats)

# reading/writing data
library(readr) 
library(writexl)

library(phonR)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(lmtest)
library(tuneR)
library(seewave)

setwd("D:/ProgettoRivarolese/")
data <- readRDS("data1.rds")


## Descriptive Statistics
# Initialize empty vectors to store the mean values and standard deviations
mean_f1 <- numeric()
mean_f2 <- numeric()
mean_f3 <- numeric()
mean_duration <- numeric()
sd_f1 <- numeric()
sd_f2 <- numeric()
sd_f3 <- numeric()
sd_duration <- numeric()

# Get unique phonemes
unique_phonemes <- unique(data$phoneme)

# Loop through each phoneme and calculate the mean values and standard deviations
for (phoneme in unique_phonemes) {
  mean_f1 <- c(mean_f1, mean(data$F1[data$phoneme == phoneme]))
  mean_f2 <- c(mean_f2, mean(data$F2[data$phoneme == phoneme]))
  mean_f3 <- c(mean_f3, mean(data$F3[data$phoneme == phoneme]))
  mean_duration <- c(mean_duration, mean(data$duration[data$phoneme == phoneme]))
  
  sd_f1 <- c(sd_f1, sd(data$F1[data$phoneme == phoneme]))
  sd_f2 <- c(sd_f2, sd(data$F2[data$phoneme == phoneme]))
  sd_f3 <- c(sd_f3, sd(data$F3[data$phoneme == phoneme]))
  sd_duration <- c(sd_duration, sd(data$duration[data$phoneme == phoneme]))
}

# Create a data frame to store the results
mean_values <- data.frame(
  Phoneme = unique_phonemes,
  Mean_F1 = mean_f1,
  Mean_F2 = mean_f2,
  Mean_F3 = mean_f3,
  Mean_Duration = mean_duration,
  SD_F1 = sd_f1,
  SD_F2 = sd_f2,
  SD_F3 = sd_f3,
  SD_Duration = sd_duration
)






















