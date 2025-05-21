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

#-------------------------------------------
setwd("D:/ProgettoRivarolese/")
data <- readRDS("data1.rds")

colnames(data)[colnames(data) == "type"] <- "Type"

#filter outliers with 3SD away from the mean
unique_phonemes <- unique(data$phoneme)
data$outlier <- FALSE  # Initialize outlier column as FALSE for all observations

data$outlier <- FALSE

# Get unique combinations of phoneme and Type
unique_combos <- unique(data[, c("phoneme", "Type")])

for (i in 1:nrow(unique_combos)) {
  current_phoneme <- unique_combos$phoneme[i]
  current_type <- unique_combos$Type[i]
  
  # Subset the data for the current phoneme and type
  subset_indices <- which(data$phoneme == current_phoneme & data$Type == current_type)
  if (length(subset_indices) < 2) next  # Skip groups with <2 observations
  
  subset_data <- data[subset_indices, ]
  
  # Calculate mean and SD for F1, F2, F3
  mean_f1 <- mean(subset_data$F1)
  mean_f2 <- mean(subset_data$F2)
  mean_f3 <- mean(subset_data$F3)
  
  sd_f1 <- sd(subset_data$F1)
  sd_f2 <- sd(subset_data$F2)
  sd_f3 <- sd(subset_data$F3)
  
  # Identify outliers (3 SD from the mean)
  f1_outlier <- subset_data$F1 > (mean_f1 + 3 * sd_f1) | subset_data$F1 < (mean_f1 - 3 * sd_f1)
  f2_outlier <- subset_data$F2 > (mean_f2 + 3 * sd_f2) | subset_data$F2 < (mean_f2 - 3 * sd_f2)
  f3_outlier <- subset_data$F3 > (mean_f3 + 3 * sd_f3) | subset_data$F3 < (mean_f3 - 3 * sd_f3)
  
  # Mark outliers in the main dataset
  data$outlier[subset_indices] <- f1_outlier | f2_outlier | f3_outlier
}

# Remove outliers
data_filtered <- data[!data$outlier, ]

df_counts <- as.data.frame(table(data_filtered$Type))
colnames(df_counts) <- c("Type", "Count")
df_counts

data_unstressed <- subset(data_filtered, Type == "Unstressed")
df_counts <- data_unstressed %>%
  filter(grepl("ɛ", phoneme)) %>%  # Filter for "i" and stressed
  summarise(Count = n())  # Count the occurrences

# Display the result
df_counts

saveRDS(data_filtered, file = "D:/ProgettoRivarolese/dataFiltered.rds")
