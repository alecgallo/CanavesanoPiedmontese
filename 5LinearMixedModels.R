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
library(lme4)
library(lmerTest) 
library(car)
library(MuMIn)

# visualization of models
library(sjPlot)
library(ggeffects)
library(glmmTMB)
library(xtable)
#---------------------

setwd("E:/ProgettoRivarolese/")
data <- readRDS("dataFiltered.rds")

library(lme4)
library(multcomp)
library(car)


##############

phonemes_of_interest <- c("ə", "ɛ", "a")
filtered_data <- subset(data, phoneme %in% phonemes_of_interest)

# Ensure phoneme is a factor and relevel to set schwa as the reference level
filtered_data$phoneme <- factor(filtered_data$phoneme)
filtered_data$phoneme <- relevel(filtered_data$phoneme, ref = "ə")

# Fit the linear mixed-effects model
lmer_model <- lmer(F1 ~ phoneme + (1 | speaker) + (1 | word), data = filtered_data)

# Print the summary of the model
summary_lmer <- summary(lmer_model)
print(summary_lmer)

# Perform ANOVA on the model
anova_lmer <- anova(lmer_model)
print(anova_lmer)

# Post-hoc pairwise comparisons
# Apply Bonferroni correction
adjusted_comparisons <- summary(glht(lmer_model, linfct = mcp(phoneme = "Tukey")), test = adjusted("bonferroni"))
print(adjusted_comparisons)

#######################
# F2

phonemes_of_interest <- c("ə", "ɛ", "a")
filtered_data <- subset(data, phoneme %in% phonemes_of_interest)

# Ensure phoneme is a factor and relevel to set schwa as the reference level
filtered_data$phoneme <- factor(filtered_data$phoneme)
filtered_data$phoneme <- relevel(filtered_data$phoneme, ref = "ə")

# Fit the linear mixed-effects model
lmer_model <- lmer(F2 ~ phoneme + (1 | speaker) + (1 | word), data = filtered_data)

# Print the summary of the model
summary_lmer <- summary(lmer_model)
print(summary_lmer)

# Perform ANOVA on the model
anova_lmer <- anova(lmer_model)
print(anova_lmer)

# Post-hoc pairwise comparisons
# Apply Bonferroni correction
adjusted_comparisons <- summary(glht(lmer_model, linfct = mcp(phoneme = "Tukey")), test = adjusted("bonferroni"))
print(adjusted_comparisons)


#############
## for each speaker
speakers <- unique(data$speaker)

# Create empty lists to store results
lmer_summaries <- list()
anova_results <- list()
posthoc_comparisons <- list()

# Loop through each speaker
for (speaker_id in speakers) {
  # Subset data for the current speaker
  speaker_data <- subset(data, speaker == speaker_id)
  
  # Filter data for phonemes of interest
  filtered_data <- subset(speaker_data, phoneme %in% phonemes_of_interest)
  
  # Ensure phoneme is a factor and relevel to set schwa as the reference level
  filtered_data$phoneme <- factor(filtered_data$phoneme)
  filtered_data$phoneme <- relevel(filtered_data$phoneme, ref = "ə")
  
  # Fit the linear mixed-effects model
  lmer_model <- lmer(F2 ~ phoneme + (1 | word), data = filtered_data)
  
  # Save the summary of the model
  lmer_summaries[[speaker_id]] <- summary(lmer_model)
  
  # Perform ANOVA on the model
  anova_results[[speaker_id]] <- anova(lmer_model)
  
  # Post-hoc pairwise comparisons
  # Apply Bonferroni correction
  adjusted_comparisons <- summary(glht(lmer_model, linfct = mcp(phoneme = "Tukey")), test = adjusted("bonferroni"))
  posthoc_comparisons[[speaker_id]] <- adjusted_comparisons
}






