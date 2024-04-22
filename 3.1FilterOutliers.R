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


setwd("D:/ProgettoRivarolese/")
data <- readRDS("dataNormalized.rds")

#filter outliers with 3SD away from the mean
unique_phonemes <- unique(data$phoneme)
data$outlier <- FALSE  # Initialize outlier column as FALSE for all observations

for (phoneme in unique_phonemes) {
  mean_f1_phoneme <- mean(data$f1_original_scale[data$phoneme == phoneme])
  mean_f2_phoneme <- mean(data$f2_original_scale[data$phoneme == phoneme])
  
  sd_f1_phoneme <- sd(data$F1[data$phoneme == phoneme])
  sd_f2_phoneme <- sd(data$F2[data$phoneme == phoneme])
  
  f1_outlier <- data$F1[data$phoneme == phoneme] > (mean_f1_phoneme + 3 * sd_f1_phoneme) | data$F1[data$phoneme == phoneme] < (mean_f1_phoneme - 3 * sd_f1_phoneme)
  f2_outlier <- data$F2[data$phoneme == phoneme] > (mean_f2_phoneme + 3 * sd_f2_phoneme) | data$F2[data$phoneme == phoneme] < (mean_f2_phoneme - 3 * sd_f2_phoneme)
 
  # Mark outliers as TRUE in the data$outlier column
  data$outlier[data$phoneme == phoneme] <- data$outlier[data$phoneme == phoneme] | f1_outlier | f2_outlier
}

# exclude outliers
data_filtered <- data[data$outlier == FALSE, ]

saveRDS(data_filtered, file = "D:/ProgettoRivarolese/dataFiltered.rds")





