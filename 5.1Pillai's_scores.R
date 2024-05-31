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
data <- readRDS("dataFiltered.rds")

library(lme4)
library(multcomp)

##########################################################3
# Pillai's score

# schwa vs a
vowel_data <- subset(data, phoneme %in% c("ə", "a"))

# Ensure factors are properly coded
vowel_data$phoneme <- factor(vowel_data$phoneme)

# Define the dependent variables (formants)
dependent_vars <- cbind(vowel_data$F1, vowel_data$F2)

# Fit the MANOVA model
manova_model <- manova(dependent_vars ~ phoneme, data = vowel_data)

# Print summary of the MANOVA model
summary_manova <- summary(manova_model, test = "Pillai")
print(summary_manova)

# Extract and print the Pillai score
pillai_score <- summary_manova$stats[1, "Pillai"]
print(paste("Pillai Score:", pillai_score))

# Perform additional diagnostics and visualize results if needed
# Plot the distribution of F1 and F2 for each vowel
mean_points <- vowel_data %>%
  group_by(phoneme) %>%
  summarize(mean_f1 = mean(f1_original_scale),
            mean_f2 = mean(f2_original_scale))

# Plot without legend and with labels
plot <- ggplot(vowel_data, aes(y = f1_original_scale, x = f2_original_scale, color = phoneme)) +
  geom_point(alpha = 0.6, size = 2) +  # Add black border to points
  stat_ellipse(level = 0.50, aes(fill = phoneme), alpha = 0.2, geom = "polygon", color = "black") +  # Color ellipses by phoneme
  geom_text(data = mean_points, aes(x = mean_f2, y = mean_f1, label = phoneme), 
            size = 10, color = "black") +  # Add labels
  theme_minimal() +
  scale_y_reverse() + # Reverse the y-axis
  scale_x_reverse() +
  labs(x = "Normalized F2 Frequency (Hz)",
       y = "Normalized F1 Frequency (Hz)") +
  scale_color_manual(values = c("ə" = "blue", "a" = "grey")) +
  scale_fill_manual(values = c("ə" = "blue", "a" = "grey")) +  # Set fill colors for ellipses
  theme(legend.position = "none",  # Remove legend
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.line = element_line(color = "black"),  # Add black border to axes
        axis.ticks = element_line(color = "black"),  # Add black ticks to axes
        axis.text = element_text(color = "black", size = 16),  # Change axis text color and size
        plot.margin = margin(10, 10, 10, 10, "pt"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))  # Adjust axis title sizes

print(plot)


###
# schwa vs ɛ
vowel_data <- subset(data, phoneme %in% c("ə", "ɛ"))

# Ensure factors are properly coded
vowel_data$phoneme <- factor(vowel_data$phoneme)

# Define the dependent variables (formants)
dependent_vars <- cbind(vowel_data$f1_original_scale, vowel_data$f2_original_scale)

# Fit the MANOVA model
manova_model <- manova(dependent_vars ~ phoneme, data = vowel_data)

# Print summary of the MANOVA model
summary_manova <- summary(manova_model, test = "Pillai")
print(summary_manova)

# Extract and print the Pillai score
pillai_score <- summary_manova$stats[1, "Pillai"]
print(paste("Pillai Score:", pillai_score))

mean_points <- vowel_data %>%
  group_by(phoneme) %>%
  summarize(mean_f1 = mean(f1_original_scale),
            mean_f2 = mean(f2_original_scale))

# Perform additional diagnostics and visualize results if needed
# Plot the distribution of F1 and F2 for each vowel
p1 <- ggplot(vowel_data, aes(y = f1_original_scale, x = f2_original_scale, color = phoneme)) +
  geom_point(alpha = 0.6, size = 2) +  # Add black border to points
  stat_ellipse(level = 0.50, aes(fill = phoneme), alpha = 0.2, geom = "polygon", color = "black") +  # Color ellipses by phoneme
  geom_text(data = mean_points, aes(x = mean_f2, y = mean_f1, label = phoneme), 
            size = 10, color = "black") +  # Add labels
  theme_minimal() +
  scale_y_reverse() + # Reverse the y-axis
  scale_x_reverse() +
  labs(x = "Normalized F2 Frequency (Hz)",
       y = "Normalized F1 Frequency (Hz)") +
  scale_color_manual(values = c("ə" = "blue", "ɛ" = "orange")) +
  scale_fill_manual(values = c("ə" = "blue", "ɛ" = "orange")) +  # Set fill colors for ellipses
  theme(legend.position = "none",  # Remove legend
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        axis.line = element_line(color = "black"),  # Add black border to axes
        axis.ticks = element_line(color = "black"),  # Add black ticks to axes
        axis.text = element_text(color = "black", size = 16),  # Change axis text color and size
        plot.margin = margin(10, 10, 10, 10, "pt"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))  # Adjust axis title sizes

print(p1)


###################
# per each speaker: schwa vs a
calculate_pillai_scores <- function(data, phonemes) {
  # Initialize an empty data frame to store results
  result_df <- data.frame(Speaker = character(), "Pillai score" = numeric())
  
  # Iterate through each participant
  for (speaker_id in unique(data$speaker)) {
    # Subset the data for the current participant and phonemes
    participant_data <- filter(data, speaker == speaker_id & phoneme %in% phonemes)
    
    # Ensure factors are properly coded
    participant_data$phoneme <- factor(participant_data$phoneme, levels = phonemes)
    
    # Define the dependent variables (formants)
    dependent_vars <- cbind(participant_data$F1, participant_data$F2)
    
    # Fit the MANOVA model
    manova_model <- manova(dependent_vars ~ phoneme, data = participant_data)
    
    # Extract and store the Pillai's score
    pillai_score <- summary(manova_model, test = "Pillai")$stats[1, "Pillai"]
    
    # Append results to the data frame
    result_df <- bind_rows(result_df, data.frame(Speaker = speaker_id, "Pillai score" = pillai_score))
  }
  
  # Add a third column "Threshold" with the specified value
  result_df$Threshold <- 0.09060937
  
  # Return the data frame
  return(result_df)
}

# Call the function to calculate Pillai's scores for each participant for "a" and "schwa"
participant_pillai_scores <- calculate_pillai_scores(data, c("ə", "a"))

# Print or use the obtained scores as needed
print(participant_pillai_scores)

# Write the results to a CSV file
write.xlsx(participant_pillai_scores, "participant_pillai_scoresA_vs_Schwa.xlsx", row.names = FALSE)



### 
# schwa vs ɛ

calculate_pillai_scores <- function(data, phonemes) {
  # Initialize an empty data frame to store results
  result_df <- data.frame(Speaker = character(), "Pillai score" = numeric())
  
  # Iterate through each participant
  for (speaker_id in unique(data$speaker)) {
    # Subset the data for the current participant and phonemes
    participant_data <- filter(data, speaker == speaker_id & phoneme %in% phonemes)
    
    # Ensure factors are properly coded
    participant_data$phoneme <- factor(participant_data$phoneme, levels = phonemes)
    
    # Define the dependent variables (formants)
    dependent_vars <- cbind(participant_data$F1, participant_data$F2)
    
    # Fit the MANOVA model
    manova_model <- manova(dependent_vars ~ phoneme, data = participant_data)
    
    # Extract and store the Pillai's score
    pillai_score <- summary(manova_model, test = "Pillai")$stats[1, "Pillai"]
    
    # Append results to the data frame
    result_df <- bind_rows(result_df, data.frame(Speaker = speaker_id, "Pillai score" = pillai_score))
  }
  
  # Add a third column "Threshold" with the specified value
  result_df$Threshold <- 0.09060937
  
  # Return the data frame
  return(result_df)
}

# Call the function to calculate Pillai's scores for each participant for "a" and "schwa"
participant_pillai_scores <- calculate_pillai_scores(data, c("ə", "ɛ"))

# Print or use the obtained scores as needed
print(participant_pillai_scores)

# Write the results to a CSV file
write.xlsx(participant_pillai_scores, "participant_pillai_scoresɛ_vs_Schwa.xlsx", row.names = FALSE)

