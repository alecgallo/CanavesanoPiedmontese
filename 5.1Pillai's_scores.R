rm(list=ls())
#--------------------------------------
# load libraries

# reading/writing data
library(readr) 
library(writexl)

# tidyverse
library(dplyr)
library(tidyr)
library(tidyverse)

# plotting
library(ggplot2)
library(cowplot)
library(ggpubr)

# regression
library(lme4) # mixed models
library(lmerTest) # pvalues for factors in mixed models, estimating dfs...
library(car) # to get vif
library(MuMIn) # for r.squaredGLMM

# visualization of models
library(sjPlot)
library(ggeffects)
library(glmmTMB)
#---------------------

setwd("E:/ProgettoRivarolese/")
data <- readRDS("dataNormalized.rds")

library(lme4)
library(multcomp)

##########################################################3
# Pillai's score


data_stressed <- subset(data, Type == "Stressed")

# List of vowel phonemes you want to compare
vowel_pairs <- list(
  c("ə", "a"),
  c("ə", "ɛ"),
  c("ə", "i"),
  c("ə", "o"),
  c("ə", "u"),
  c("ə", "y"),
  c("a", "ɛ"),
  c("a", "i"),
  c("a", "o"),
  c("a", "u"),
  c("a", "y"),
  c("ɛ", "i"),
  c("ɛ", "o"),
  c("ɛ", "u"),
  c("ɛ", "y"),
  c("i", "o"),
  c("i", "u"),
  c("i", "y"),
  c("o", "u"),
  c("o", "y"),
  c("u", "y")
)

# Loop over each vowel pair to calculate the Pillai score for stressed vowels
pillai_results_stressed <- data.frame(pair = character(0), pillai_score = numeric(0))

for (pair in vowel_pairs) {
  
  # Subset the stressed data for the current vowel pair
  vowel_data_stressed <- subset(data_stressed, phoneme %in% pair)
  
  # Ensure phoneme is a factor
  vowel_data_stressed$phoneme <- factor(vowel_data_stressed$phoneme)
  
  # Define dependent variables (F1 and F2)
  dependent_vars <- cbind(vowel_data_stressed$F1, vowel_data_stressed$F2)
  
  # Fit the MANOVA model
  manova_model <- manova(dependent_vars ~ phoneme, data = vowel_data_stressed)
  
  # Extract the Pillai score
  summary_manova <- summary(manova_model, test = "Pillai")
  pillai_score <- summary_manova$stats[1, "Pillai"]
  
  # Store results
  pillai_results_stressed <- rbind(pillai_results_stressed, data.frame(pair = paste(pair, collapse = "-"), pillai_score = pillai_score))
}

# Print the results for stressed vowels
print(pillai_results_stressed)

## for each speaker

# List of vowel phonemes you want to compare
vowel_pairs <- list(
  c("ə", "a"),
  c("ə", "ɛ"),
  c("ə", "i"),
  c("ə", "o"),
  c("ə", "u"),
  c("ə", "y"),
  c("a", "ɛ"),
  c("a", "i"),
  c("a", "o"),
  c("a", "u"),
  c("a", "y"),
  c("ɛ", "i"),
  c("ɛ", "o"),
  c("ɛ", "u"),
  c("ɛ", "y"),
  c("i", "o"),
  c("i", "u"),
  c("i", "y"),
  c("o", "u"),
  c("o", "y"),
  c("u", "y")
)

# Get the list of speakers in the dataset
speakers <- unique(data_stressed$speaker)

# Initialize an empty data frame to store results
pillai_results_per_speaker <- data.frame(speaker = character(0), pair = character(0), pillai_score = numeric(0))

# Loop over each speaker
for (speaker in speakers) {
  
  # Subset the stressed data for the current speaker
  vowel_data_stressed_speaker <- subset(data_stressed, speaker == speaker)
  
  # Loop over each vowel pair to calculate the Pillai score
  for (pair in vowel_pairs) {
    
    # Subset the data for the current vowel pair
    vowel_data_stressed <- subset(vowel_data_stressed_speaker, phoneme %in% pair)
    
    # Ensure phoneme is a factor
    vowel_data_stressed$phoneme <- factor(vowel_data_stressed$phoneme)
    
    # Define dependent variables (F1 and F2)
    dependent_vars <- cbind(vowel_data_stressed$F1, vowel_data_stressed$F2)
    
    # Fit the MANOVA model
    manova_model <- manova(dependent_vars ~ phoneme, data = vowel_data_stressed)
    
    # Extract the Pillai score
    summary_manova <- summary(manova_model, test = "Pillai")
    pillai_score <- summary_manova$stats[1, "Pillai"]
    
    # Store results
    pillai_results_per_speaker <- rbind(pillai_results_per_speaker, data.frame(speaker = speaker, pair = paste(pair, collapse = "-"), pillai_score = pillai_score))
  }
}

# Print the results for each speaker
print(pillai_results_per_speaker)




#############UNSTRESSED VOWELS

data_unstressed <- subset(data, Type == "Unstressed")

# List of vowel phonemes you want to compare
vowel_pairs <- list(
  c("ə", "a"),
  c("ə", "i"),
  c("ə", "o"),
  c("ə", "u"),
  c("ə", "y"),
  c("a", "i"),
  c("a", "o"),
  c("a", "u"),
  c("a", "y"),
  c("i", "o"),
  c("i", "u"),
  c("i", "y"),
  c("o", "u"),
  c("o", "y"),
  c("u", "y")
)

# Initialize an empty data frame to store results
pillai_results_all_unstressed <- data.frame(pair = character(0), pillai_score = numeric(0))

# Loop over each vowel pair to calculate the Pillai score for the entire dataset
for (pair in vowel_pairs) {
  
  # Subset the data for the current vowel pair
  vowel_data_unstressed <- subset(data_unstressed, phoneme %in% pair)
  
  # Ensure phoneme is a factor
  vowel_data_unstressed$phoneme <- factor(vowel_data_unstressed$phoneme)
  
  # Define dependent variables (F1 and F2)
  dependent_vars <- cbind(vowel_data_unstressed$F1, vowel_data_unstressed$F2)
  
  # Fit the MANOVA model
  manova_model <- manova(dependent_vars ~ phoneme, data = vowel_data_unstressed)
  
  # Extract the Pillai score
  summary_manova <- summary(manova_model, test = "Pillai")
  pillai_score <- summary_manova$stats[1, "Pillai"]
  
  # Store results
  pillai_results_all_unstressed <- rbind(pillai_results_all_unstressed, data.frame(pair = paste(pair, collapse = "-"), pillai_score = pillai_score))
}

# Print the results for all unstressed vowel pairs
print(pillai_results_all_unstressed)


# Initialize an empty data frame to store results for each speaker
pillai_results_per_speaker_unstressed <- data.frame(speaker = character(0), pair = character(0), pillai_score = numeric(0))

# Get the list of speakers in the dataset
speakers <- unique(data_unstressed$speaker)

# Loop over each speaker
for (speaker in speakers) {
  
  # Subset the unstressed data for the current speaker
  vowel_data_unstressed_speaker <- subset(data_unstressed, speaker == speaker)
  
  # Loop over each vowel pair to calculate the Pillai score for the current speaker
  for (pair in vowel_pairs) {
    
    # Subset the data for the current vowel pair
    vowel_data_unstressed <- subset(vowel_data_unstressed_speaker, phoneme %in% pair)
    
    # Ensure phoneme is a factor
    vowel_data_unstressed$phoneme <- factor(vowel_data_unstressed$phoneme)
    
    # Define dependent variables (F1 and F2)
    dependent_vars <- cbind(vowel_data_unstressed$F1, vowel_data_unstressed$F2)
    
    # Fit the MANOVA model
    manova_model <- manova(dependent_vars ~ phoneme, data = vowel_data_unstressed)
    
    # Extract the Pillai score
    summary_manova <- summary(manova_model, test = "Pillai")
    pillai_score <- summary_manova$stats[1, "Pillai"]
    
    # Store results for the current speaker
    pillai_results_per_speaker_unstressed <- rbind(pillai_results_per_speaker_unstressed, data.frame(speaker = speaker, pair = paste(pair, collapse = "-"), pillai_score = pillai_score))
  }
}

# Print the results for each speaker
print(pillai_results_per_speaker_unstressed)


# Write the results to a CSV file
write.xlsx(participant_pillai_scores, "participant_pillai_scoresɛ_vs_Schwa.xlsx", row.names = FALSE)

