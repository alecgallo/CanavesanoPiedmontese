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
library(tidyr)

#-------------------------------------
setwd("D:/ProgettoRivarolese/")

# Load the data
data <- readRDS("dataNormalized.rds")

colnames(data)[colnames(data) == "type"] <- "Type"

# Initialize empty vectors for mean values
mean_f1_stressed <- numeric()
mean_f2_stressed <- numeric()
mean_f3_stressed <- numeric()
mean_f1_stressed_or <- numeric()
mean_f2_stressed_or <- numeric()
mean_f3_stressed_or <- numeric()
mean_duration_stressed <- numeric()

mean_f1_unstressed <- numeric()
mean_f2_unstressed <- numeric()
mean_f3_unstressed <- numeric()
mean_f1_unstressed_or <- numeric()
mean_f2_unstressed_or <- numeric()
mean_f3_unstressed_or <- numeric()
mean_duration_unstressed <- numeric()

# Get unique phonemes
unique_phonemes <- unique(data$phoneme)

# Compute mean values for each phoneme
for (phoneme in unique_phonemes) {
  # Stressed
  mean_f1_stressed <- c(mean_f1_stressed, mean(data$f1lobanov[data$phoneme == phoneme & data$Type == "Stressed"], na.rm = TRUE))
  mean_f2_stressed <- c(mean_f2_stressed, mean(data$f2lobanov[data$phoneme == phoneme & data$Type == "Stressed"], na.rm = TRUE))
  mean_f3_stressed <- c(mean_f3_stressed, mean(data$f3lobanov[data$phoneme == phoneme & data$Type == "Stressed"], na.rm = TRUE))
  mean_duration_stressed <- c(mean_duration_stressed, mean(data$duration[data$phoneme == phoneme & data$Type == "Stressed"], na.rm = TRUE))
  
  mean_f1_stressed_or <- c(mean_f1_stressed_or, mean(data$F1[data$phoneme == phoneme & data$Type == "Stressed"], na.rm = TRUE))
  mean_f2_stressed_or <- c(mean_f2_stressed_or, mean(data$F2[data$phoneme == phoneme & data$Type == "Stressed"], na.rm = TRUE))
  mean_f3_stressed_or <- c(mean_f3_stressed_or, mean(data$F3[data$phoneme == phoneme & data$Type == "Stressed"], na.rm = TRUE))
  
  # Unstressed
  mean_f1_unstressed <- c(mean_f1_unstressed, mean(data$f1lobanov[data$phoneme == phoneme & data$Type == "Unstressed"], na.rm = TRUE))
  mean_f2_unstressed <- c(mean_f2_unstressed, mean(data$f2lobanov[data$phoneme == phoneme & data$Type == "Unstressed"], na.rm = TRUE))
  mean_f3_unstressed <- c(mean_f3_unstressed, mean(data$f3lobanov[data$phoneme == phoneme & data$Type == "Unstressed"], na.rm = TRUE))
  mean_duration_unstressed <- c(mean_duration_unstressed, mean(data$duration[data$phoneme == phoneme & data$Type == "Unstressed"], na.rm = TRUE))
  
  mean_f1_unstressed_or <- c(mean_f1_unstressed_or, mean(data$F1[data$phoneme == phoneme & data$Type == "Unstressed"], na.rm = TRUE))
  mean_f2_unstressed_or <- c(mean_f2_unstressed_or, mean(data$F2[data$phoneme == phoneme & data$Type == "Unstressed"], na.rm = TRUE))
  mean_f3_unstressed_or <- c(mean_f3_unstressed_or, mean(data$F3[data$phoneme == phoneme & data$Type == "Unstressed"], na.rm = TRUE))
}

# Format phonemes
vowels_with_slashes <- paste0("/", unique_phonemes, "/")

# Create data frame for stressed phonemes
mean_values_stressed <- data.frame(
  Vowel = vowels_with_slashes,
  F1_Lobanov = mean_f1_stressed,
  F2_Lobanov = mean_f2_stressed,
  F3_Lobanov = mean_f3_stressed,
  F1_Original = mean_f1_stressed_or,
  F2_Original = mean_f2_stressed_or,
  F3_Original = mean_f3_stressed_or,
  Duration = mean_duration_stressed
)

# Create data frame for unstressed phonemes
mean_values_unstressed <- data.frame(
  Vowel = vowels_with_slashes,
  F1_Lobanov = mean_f1_unstressed,
  F2_Lobanov = mean_f2_unstressed,
  F3_Lobanov = mean_f3_unstressed,
  F1_Original = mean_f1_unstressed_or,
  F2_Original = mean_f2_unstressed_or,
  F3_Original = mean_f3_unstressed_or,
  Duration = mean_duration_unstressed
)

# Create flextable
flex_table_stressed <- flextable(mean_values_stressed)

# Format numbers
flex_table_stressed <- flex_table_stressed %>%
  flextable::set_formatter(
    F1_Lobanov = function(x) format(x, big.mark = "", decimal.mark = "."),
    F2_Lobanov = function(x) format(x, big.mark = "", decimal.mark = "."),
    F3_Lobanov = function(x) format(x, big.mark = "", decimal.mark = "."),
    F1_Original = function(x) format(x, big.mark = "", decimal.mark = "."),
    F2_Original = function(x) format(x, big.mark = "", decimal.mark = "."),
    F3_Original = function(x) format(x, big.mark = "", decimal.mark = ".")
  )

# Save table as image
flextable::save_as_image(flex_table_stressed, "APA7_Descriptive_Table_Stressed.png")

# Create flextable for unstressed phonemes
flex_table_unstressed <- flextable(mean_values_unstressed)

# Save table as image
flextable::save_as_image(flex_table_unstressed, "APA7_Descriptive_Table_Unstressed.png")


vowel_counts <- data %>%
  group_by(phoneme, Type) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Type, values_from = count, values_fill = list(count = 0))
