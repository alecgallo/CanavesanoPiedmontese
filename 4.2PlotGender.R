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
library(ggforce)
library(dplyr)
library(broom)
library(ggpubr)
library(lmtest)
library(tuneR)
library(seewave)

#-------------------------------
setwd("D:/ProgettoRivarolese/")
data <- readRDS("dataNormalized.rds")

stressed_data <- data %>% filter(Type == "Stressed")

# Check if the filtered dataset has any valid values
if (nrow(stressed_data) > 0 && 
    any(!is.na(stressed_data$F2)) && 
    any(!is.na(stressed_data$F1))) {
  
  # Get max values for F2 and F1 if there is valid data
  max_f2 <- max(stressed_data$F2, na.rm = TRUE)
  max_f1 <- max(stressed_data$F1, na.rm = TRUE)
  
  # If max returns -Inf or NA, set fallback values
  max_f2 <- ifelse(is.finite(max_f2), max_f2, 1000)  # Default max value for F2
  max_f1 <- ifelse(is.finite(max_f1), max_f1, 500)   # Default max value for F1
  
  # Define custom labels for gender
  gender_labels <- c("male" = "Males", "female" = "Females")
  
  # Plot the stressed vowels data by gender using facet_wrap()
  p1 <- stressed_data %>%
    ggplot(aes(x = F2, y = F1, color = phoneme)) + 
    geom_point(alpha = 0.6, shape = 21) +  # Use shape 21 for circles
    stat_ellipse(aes(fill = phoneme), geom = "polygon", alpha = 0.2, level = 0.5) + 
    theme_minimal() +
    scale_x_reverse(breaks = seq(0, max_f2, by = 250)) +
    scale_y_reverse(breaks = seq(0, max_f1, by = 100)) +
    scale_color_manual(values = c("a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
                                  "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
                                  "ø" = "#7FDBFF", "ə" = "#8C564B", "y" = "#E377C2"),
                       labels = c("a", "e", "i", "ɛ", "u", "o", "ø", "ə", "y")) + 
    scale_fill_manual(values = c("a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
                                 "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
                                 "ø" = "#7FDBFF", "ə" = "#8C564B", "y" = "#E377C2"),
                      labels = c("a", "e", "i", "ɛ", "u", "o", "ø", "ə", "y")) +
    geom_text(data = stressed_data %>% 
                group_by(gender, phoneme) %>%  # Include gender in grouping
                summarise(center_x = mean(F2, na.rm = TRUE), 
                          center_y = mean(F1, na.rm = TRUE), 
                          .groups = "drop"), 
              aes(x = center_x, y = center_y, label = phoneme), 
              size = 7, fontface = "bold", color = "black") + 
    theme(legend.position = "none", 
          plot.margin = margin(10, 10, 10, 10, "pt"),
          axis.line = element_line(color = "black"),  # Add black border to axes
          axis.ticks = element_line(color = "black"),  # Add black ticks to axes
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),  
          axis.text = element_text(size = 18),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
    labs(x = "F2 (Hz)", y = "F1 (Hz)") +
    facet_wrap(~gender, labeller = labeller(gender = gender_labels)) +  # Facet by gender with custom labels
    theme(strip.text = element_text(size = 20)) 
  # Print the plot
  print(p1)
}


ggsave(filename = paste0("Gender_plot.png"), plot = p1, width = 10, height = 7)
###################


mean_data <- stressed_data %>%
  group_by(gender, phoneme) %>%
  summarize(center_x = mean(F2), 
            center_y = mean(F1))

# Plot the combined data with overlapping points for males and females
p2 <- ggplot() + 
  # Plot the mean points for males and females
  geom_point(data = mean_data %>% filter(gender == "male"), 
             aes(x = center_x, y = center_y, color = phoneme), 
             size = 5, shape = 21, fill = "blue") +
  geom_point(data = mean_data %>% filter(gender == "female"), 
             aes(x = center_x, y = center_y, color = phoneme), 
             size = 5, shape = 21, fill = "red") +
  
  # Add phoneme labels at the mean points
  geom_text(data = mean_data, 
            aes(x = center_x, y = center_y, label = phoneme, color = phoneme), 
            size = 5, fontface = "bold", vjust = -1) +
  
  # Ellipses for each vowel (phoneme) at 50% confidence level, colored by gender
  stat_ellipse(
    data = stressed_data %>% filter(gender == "male"), 
    aes(x = F2, y = F1, group = phoneme),
    level = 0.5, alpha = 0.4, size = 1.2,
    color = "blue", fill = "blue"
  ) +
  stat_ellipse(
    data = stressed_data %>% filter(gender == "female"), 
    aes(x = F2, y = F1, group = phoneme),
    level = 0.5, alpha = 0.4, size = 1.2,
    color = "red", fill = "red"
  ) +
  
  # Keep the phoneme colors for points/text as before
  scale_color_manual(values = c(
    "a" = "black", "e" = "black", "i" = "black", 
    "o" = "black", "u" = "black", "ɛ" = "black", 
    "ø" = "black", "ə" = "black", "y" = "black"
  )) +
  
  # Customize plot aesthetics
  theme_minimal() +
  scale_x_reverse(breaks = seq(0, max_f2, by = 250)) +
  scale_y_reverse(breaks = seq(0, max_f1, by = 100)) +
  labs(x = "F2 (Hz)", y = "F1 (Hz)") +
  
  # Additional theme customization
  theme(
    legend.position = "none", 
    plot.margin = margin(10, 10, 10, 10, "pt"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),  
    axis.text = element_text(size = 18),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

print(p2)

ggsave(filename = paste0("Gender_plot.png"), plot = p2, width = 10, height = 7)

########

mean_data <- stressed_data %>%
  group_by(gender, phoneme) %>%
  summarize(center_x = mean(f2lobanov), 
            center_y = mean(f1lobanov))

# Plot the combined data with overlapping points for males and females
p2 <- ggplot() + 
  # Plot the mean points for males and females
  geom_point(data = mean_data %>% filter(gender == "male"), 
             aes(x = center_x, y = center_y, color = phoneme), 
             size = 5, shape = 21, fill = "blue") +
  geom_point(data = mean_data %>% filter(gender == "female"), 
             aes(x = center_x, y = center_y, color = phoneme), 
             size = 5, shape = 21, fill = "red") +
  
  # Add phoneme labels at the mean points
  geom_text(data = mean_data, 
            aes(x = center_x, y = center_y, label = phoneme, color = phoneme), 
            size = 5, fontface = "bold", vjust = -1) +
  
  # Ellipses for each vowel (phoneme) at 50% confidence level, colored by gender
  stat_ellipse(
    data = stressed_data %>% filter(gender == "male"), 
    aes(x = f2lobanov, y = f1lobanov, group = phoneme),
    level = 0.5, alpha = 0.4, size = 1.2,
    color = "blue", fill = "blue"
  ) +
  stat_ellipse(
    data = stressed_data %>% filter(gender == "female"), 
    aes(x = f2lobanov, y = f1lobanov, group = phoneme),
    level = 0.5, alpha = 0.4, size = 1.2,
    color = "red", fill = "red"
  ) +
  
  # Keep the phoneme colors for points/text as before
  scale_color_manual(values = c(
    "a" = "black", "e" = "black", "i" = "black", 
    "o" = "black", "u" = "black", "ɛ" = "black", 
    "ø" = "black", "ə" = "black", "y" = "black"
  )) +
  
  # Customize plot aesthetics
  theme_minimal() +
  scale_x_reverse(breaks = seq(floor(min(stressed_data$f2lobanov, na.rm = TRUE)), 
                               max_f2, by = 0.5)) +
  scale_y_reverse(breaks = seq(floor(min(stressed_data$f1lobanov, na.rm = TRUE)), 
                               max_f1, by = 0.5)) +
  labs(x = "Speaker-normalized F2 (Z-score)", y = "Speaker-normalized F1 (Z-score)") +
  
  # Additional theme customization
  theme(
    legend.position = "none", 
    plot.margin = margin(10, 10, 10, 10, "pt"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),  
    axis.text = element_text(size = 18),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

print(p2)

ggsave(filename = paste0("Gender_plot_lobanov.png"), plot = p2, width = 10, height = 7)

##############
#unstressed

unstressed_data <- data %>% filter(Type == "Unstressed")
unstressed_data <- data %>%
  filter(Type == "Unstressed" & phoneme != "ɛ")

# Check if the filtered dataset has any valid values
if (nrow(unstressed_data) > 0 && 
    any(!is.na(unstressed_data$f2_original_scale)) && 
    any(!is.na(unstressed_data$f1_original_scale))) {
  
  # Get max values for F2 and F1 if there is valid data
  max_f2 <- max(unstressed_data$f2_original_scale, na.rm = TRUE)
  max_f1 <- max(unstressed_data$f1_original_scale, na.rm = TRUE)
  
  # If max returns -Inf or NA, set fallback values
  max_f2 <- ifelse(is.finite(max_f2), max_f2, 1000)  # Default max value for F2
  max_f1 <- ifelse(is.finite(max_f1), max_f1, 500)   # Default max value for F1
  
  # Define custom labels for gender
  gender_labels <- c("male" = "Males", "female" = "Females")
  
  # Plot the stressed vowels data by gender using facet_wrap()
  p1 <- unstressed_data %>%
    ggplot(aes(x = f2_original_scale, y = f1_original_scale, color = phoneme)) + 
    geom_point(alpha = 0.6, shape = 21) +  # Use shape 21 for circles
    stat_ellipse(aes(fill = phoneme), geom = "polygon", alpha = 0.2, level = 0.5) + 
    theme_minimal() +
    scale_x_reverse(breaks = seq(0, max_f2, by = 250)) +
    scale_y_reverse(breaks = seq(0, max_f1, by = 100)) +
    scale_color_manual(values = c("a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
                                  "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
                                  "ø" = "#7FDBFF", "ə" = "#8C564B", "y" = "#E377C2"),
                       labels = c("a", "e", "i", "ɛ", "u", "o", "ø", "ə", "y")) + 
    scale_fill_manual(values = c("a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
                                 "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
                                 "ø" = "#7FDBFF", "ə" = "#8C564B", "y" = "#E377C2"),
                      labels = c("a", "e", "i", "ɛ", "u", "o", "ø", "ə", "y")) +
    geom_text(data = unstressed_data %>% group_by(phoneme) %>% summarize(center_x = mean(f2_original_scale), center_y = mean(f1_original_scale)),
              aes(x = center_x, y = center_y, label = phoneme), size = 7, fontface = "bold", color = "black") +
    theme(legend.position = "none", 
          plot.margin = margin(10, 10, 10, 10, "pt"),
          axis.line = element_line(color = "black"),  # Add black border to axes
          axis.ticks = element_line(color = "black"),  # Add black ticks to axes
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),  
          axis.text = element_text(size = 18),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
    labs(x = "Speaker-normalized F2 (Hz)", y = "Speaker-normalized F1 (Hz)") +
    facet_wrap(~gender, labeller = labeller(gender = gender_labels)) +  # Facet by gender with custom labels
    theme(strip.text = element_text(size = 20)) 
  # Print the plot
  print(p1)
}


ggsave(filename = paste0("Gender_Unstressed_plot.png"), plot = p1, width = 10, height = 7)
###################


mean_data <- unstressed_data %>%
  group_by(gender, phoneme) %>%
  summarize(center_x = mean(f2_original_scale), 
            center_y = mean(f1_original_scale))

# Plot the combined data with overlapping points for males and females
p2 <- ggplot() + 
  # Plot the mean points for males and females
  geom_point(data = mean_data %>% filter(gender == "male"), 
             aes(x = center_x, y = center_y, color = phoneme), 
             size = 5, shape = 21, fill = "blue") +
  geom_point(data = mean_data %>% filter(gender == "female"), 
             aes(x = center_x, y = center_y, color = phoneme), 
             size = 5, shape = 21, fill = "red") +
  
  # Add phoneme labels at the mean points
  geom_text(data = mean_data, 
            aes(x = center_x, y = center_y, label = phoneme, color = phoneme), 
            size = 5, fontface = "bold", vjust = -1) +
  
  # Ellipses for each vowel (phoneme) at 50% confidence level, colored by gender
  stat_ellipse(
    data = unstressed_data %>% filter(gender == "male"), 
    aes(x = f2_original_scale, y = f1_original_scale, group = phoneme),
    level = 0.5, alpha = 0.4, size = 1.2,
    color = "blue", fill = "blue"
  ) +
  stat_ellipse(
    data = unstressed_data %>% filter(gender == "female"), 
    aes(x = f2_original_scale, y = f1_original_scale, group = phoneme),
    level = 0.5, alpha = 0.4, size = 1.2,
    color = "red", fill = "red"
  ) +
  
  # Keep the phoneme colors for points/text as before
  scale_color_manual(values = c(
    "a" = "black", "e" = "black", "i" = "black", 
    "o" = "black", "u" = "black", "ɛ" = "black", 
    "ø" = "black", "ə" = "black", "y" = "black"
  )) +
  
  # Customize plot aesthetics
  theme_minimal() +
  scale_x_reverse(breaks = seq(0, max_f2, by = 250)) +
  scale_y_reverse(breaks = seq(0, max_f1, by = 100)) +
  labs(x = "Speaker-normalized F2 (Hz)", y = "Speaker-normalized F1 (Hz)") +
  
  # Additional theme customization
  theme(
    legend.position = "none", 
    plot.margin = margin(10, 10, 10, 10, "pt"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),  
    axis.text = element_text(size = 18),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

print(p2)

ggsave(filename = paste0("Gender_plot.png"), plot = p2, width = 10, height = 7)
