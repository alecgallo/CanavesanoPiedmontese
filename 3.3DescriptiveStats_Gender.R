rm(list=ls())
#--------------------------------------
# load libraries

# reading/writing data
library(readr) 
library(writexl)
library(openxlsx)
library(base)
library(stats)
library(flextable)
library(officer)

# reading/writing data
library(readr) 
library(writexl)
library(apaTables)

library(phonR)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

#-------------------------------------
setwd("D:/ProgettoRivarolese/")

# Load the data
data <- readRDS("dataNormalized.rds")

#-------------------------------------

# Initialize empty vectors for Stressed/Unstressed and Male/Female
mean_f1_stressed_male <- numeric()
mean_f2_stressed_male <- numeric()
mean_f3_stressed_male <- numeric()
mean_duration_stressed_male <- numeric()

mean_f1_stressed_female <- numeric()
mean_f2_stressed_female <- numeric()
mean_f3_stressed_female <- numeric()
mean_duration_stressed_female <- numeric()

mean_f1_unstressed_male <- numeric()
mean_f2_unstressed_male <- numeric()
mean_f3_unstressed_male <- numeric()
mean_duration_unstressed_male <- numeric()

mean_f1_unstressed_female <- numeric()
mean_f2_unstressed_female <- numeric()
mean_f3_unstressed_female <- numeric()
mean_duration_unstressed_female <- numeric()

# Get unique phonemes
unique_phonemes <- unique(data$phoneme)

# Loop through each phoneme and calculate the mean values by stress and gender
for (phoneme in unique_phonemes) {
  
  # Stressed + Male
  mean_f1_stressed_male <- c(mean_f1_stressed_male,
                             mean(data$F1[
                               data$phoneme == phoneme & 
                                 data$Type == "Stressed" &
                                 data$gender == "male"
                             ], na.rm = TRUE))
  mean_f2_stressed_male <- c(mean_f2_stressed_male,
                             mean(data$F2[
                               data$phoneme == phoneme & 
                                 data$Type == "Stressed" &
                                 data$gender == "male"
                             ], na.rm = TRUE))
  mean_f3_stressed_male <- c(mean_f3_stressed_male,
                             mean(data$F3[
                               data$phoneme == phoneme & 
                                 data$Type == "Stressed" &
                                 data$gender == "male"
                             ], na.rm = TRUE))
  mean_duration_stressed_male <- c(mean_duration_stressed_male,
                                   mean(data$duration[
                                     data$phoneme == phoneme & 
                                       data$Type == "Stressed" &
                                       data$gender == "male"
                                   ], na.rm = TRUE))
  
  # Stressed + Female
  mean_f1_stressed_female <- c(mean_f1_stressed_female,
                               mean(data$F1[
                                 data$phoneme == phoneme & 
                                   data$Type == "Stressed" &
                                   data$gender == "female"
                               ], na.rm = TRUE))
  mean_f2_stressed_female <- c(mean_f2_stressed_female,
                               mean(data$F2[
                                 data$phoneme == phoneme & 
                                   data$Type == "Stressed" &
                                   data$gender == "female"
                               ], na.rm = TRUE))
  mean_f3_stressed_female <- c(mean_f3_stressed_female,
                               mean(data$F3[
                                 data$phoneme == phoneme & 
                                   data$Type == "Stressed" &
                                   data$gender == "female"
                               ], na.rm = TRUE))
  mean_duration_stressed_female <- c(mean_duration_stressed_female,
                                     mean(data$duration[
                                       data$phoneme == phoneme & 
                                         data$Type == "Stressed" &
                                         data$gender == "female"
                                     ], na.rm = TRUE))
  
  # Unstressed + Male
  mean_f1_unstressed_male <- c(mean_f1_unstressed_male,
                               mean(data$F1[
                                 data$phoneme == phoneme & 
                                   data$Type == "Unstressed" &
                                   data$gender == "male"
                               ], na.rm = TRUE))
  mean_f2_unstressed_male <- c(mean_f2_unstressed_male,
                               mean(data$F2[
                                 data$phoneme == phoneme & 
                                   data$Type == "Unstressed" &
                                   data$gender == "male"
                               ], na.rm = TRUE))
  mean_f3_unstressed_male <- c(mean_f3_unstressed_male,
                               mean(data$F3[
                                 data$phoneme == phoneme & 
                                   data$Type == "Unstressed" &
                                   data$gender == "male"
                               ], na.rm = TRUE))
  mean_duration_unstressed_male <- c(mean_duration_unstressed_male,
                                     mean(data$duration[
                                       data$phoneme == phoneme & 
                                         data$Type == "Unstressed" &
                                         data$gender == "male"
                                     ], na.rm = TRUE))
  
  # Unstressed + Female
  mean_f1_unstressed_female <- c(mean_f1_unstressed_female,
                                 mean(data$F1[
                                   data$phoneme == phoneme & 
                                     data$Type == "Unstressed" &
                                     data$gender == "female"
                                 ], na.rm = TRUE))
  mean_f2_unstressed_female <- c(mean_f2_unstressed_female,
                                 mean(data$F2[
                                   data$phoneme == phoneme & 
                                     data$Type == "Unstressed" &
                                     data$gender == "female"
                                 ], na.rm = TRUE))
  mean_f3_unstressed_female <- c(mean_f3_unstressed_female,
                                 mean(data$F3[
                                   data$phoneme == phoneme & 
                                     data$Type == "Unstressed" &
                                     data$gender == "female"
                                 ], na.rm = TRUE))
  mean_duration_unstressed_female <- c(mean_duration_unstressed_female,
                                       mean(data$duration[
                                         data$phoneme == phoneme & 
                                           data$Type == "Unstressed" &
                                           data$gender == "female"
                                       ], na.rm = TRUE))
}

# Put phonemes in slashes
vowels_with_slashes <- paste0("/", unique_phonemes, "/")

# Create data frames
mean_values_stressed_male <- data.frame(
  Vowel    = vowels_with_slashes,
  F1       = round(mean_f1_stressed_male),
  F2       = round(mean_f2_stressed_male),
  F3       = round(mean_f3_stressed_male),
  Duration = mean_duration_stressed_male
)

mean_values_stressed_female <- data.frame(
  Vowel    = vowels_with_slashes,
  F1       = round(mean_f1_stressed_female),
  F2       = round(mean_f2_stressed_female),
  F3       = round(mean_f3_stressed_female),
  Duration = mean_duration_stressed_female
)

mean_values_unstressed_male <- data.frame(
  Vowel    = vowels_with_slashes,
  F1       = round(mean_f1_unstressed_male),
  F2       = round(mean_f2_unstressed_male),
  F3       = round(mean_f3_unstressed_male),
  Duration = mean_duration_unstressed_male
)

mean_values_unstressed_female <- data.frame(
  Vowel    = vowels_with_slashes,
  F1       = round(mean_f1_unstressed_female),
  F2       = round(mean_f2_unstressed_female),
  F3       = round(mean_f3_unstressed_female),
  Duration = mean_duration_unstressed_female
)

# Example of how to create and format a flextable for "Stressed Male"
library(flextable)

flex_table_stressed_male <- flextable(mean_values_stressed_male) %>%
  flextable::set_formatter(
    F1 = function(x) format(x, big.mark = "", decimal.mark = "."),
    F2 = function(x) format(x, big.mark = "", decimal.mark = "."),
    F3 = function(x) format(x, big.mark = "", decimal.mark = ".")
  )

# Print the flextable
print(flex_table_stressed_male)

# You can repeat similarly for:
# flex_table_stressed_female, flex_table_unstressed_male, flex_table_unstressed_female

