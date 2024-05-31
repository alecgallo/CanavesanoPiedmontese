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
setwd("E:/ProgettoRivarolese/")

# Load the data
data <- readRDS("dataFiltered.rds")

# Initialize empty vectors to store the mean values
mean_f1 <- numeric()
mean_f2 <- numeric()
mean_f3 <- numeric()

# Get unique phonemes
unique_phonemes <- unique(data$phoneme)

# Loop through each phoneme and calculate the mean values
for (phoneme in unique_phonemes) {
  mean_f1 <- c(mean_f1, mean(data$f1_original_scale[data$phoneme == phoneme]))
  mean_f2 <- c(mean_f2, mean(data$f2_original_scale[data$phoneme == phoneme]))
  mean_f3 <- c(mean_f3, mean(data$f3_original_scale[data$phoneme == phoneme]))
}

# Enclose phonemes in slashes
vowels_with_slashes <- paste0("/", unique_phonemes, "/")

# Create a data frame to store the results
mean_values <- data.frame(
  Vowel = vowels_with_slashes,
  F1 = round(mean_f1),
  F2 = round(mean_f2),
  F3 = round(mean_f3)
)

# Create flextable
flex_table <- flextable(mean_values)

# Customize number formatting (remove comma for thousands)
flex_table <- flex_table %>%
  flextable::set_formatter(
    F1 = function(x) format(x, big.mark = "", decimal.mark = "."),
    F2 = function(x) format(x, big.mark = "", decimal.mark = "."),
    F3 = function(x) format(x, big.mark = "", decimal.mark = ".")
  )

# Save as image
flextable::save_as_image(flex_table, "APA7_Descriptive_Table.png")

