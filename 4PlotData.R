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

#-------------------------------
setwd("D:/ProgettoRivarolese/")
data <- readRDS("dataNormalized.rds")
write.xlsx(data, file="dataNormalized.xls")

colnames(data)[colnames(data) == "type"] <- "Type"

# plot the data
p1 <- data %>%
  ggplot(aes(x = f2_original_scale, y = f1_original_scale, color = phoneme)) + 
  geom_point(alpha = 0.6, shape = 21) +  # Use shape 21 for circles
  stat_ellipse(aes(fill = phoneme), geom = "polygon", alpha = 0.2, level = 0.5) +  # Filled ellipses with transparency
  theme_minimal() +
  scale_x_reverse(breaks = seq(0, max(data$f2_original_scale, na.rm = TRUE), by = 250)) +
  scale_y_reverse(breaks = seq(0, max(data$f1_original_scale, na.rm = TRUE), by = 100)) +
  scale_color_manual(values = c("a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
                                "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
                                "ø" = "#7FDBFF", "ə" = "#8C564B", "y" = "#E377C2"),
                     labels = c("a", "e", "i", "ɛ", "u", "o", "ø", "ə", "y")) + 
  scale_fill_manual(values = c("a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
                               "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
                               "ø" = "#7FDBFF", "ə" = "#8C564B", "y" = "#E377C2"),
                    labels = c("a", "e", "i", "ɛ", "u", "o", "ø", "ə", "y")) +
  geom_text(data = data %>% group_by(phoneme) %>% summarize(center_x = mean(f2_original_scale), center_y = mean(f1_original_scale)),
            aes(x = center_x, y = center_y, label = phoneme), size = 7, fontface = "bold", color = "black") +
  theme(legend.position = "none", 
        plot.margin = margin(10, 10, 10, 10, "pt"),
        axis.line = element_line(color = "black"),  # Add black border to axes
        axis.ticks = element_line(color = "black"),  # Add black ticks to axes
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),  
        axis.text = element_text(size = 20),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  labs(x = "Speaker-normalized F2 (Hz)", y = "Speaker-normalized F1 (Hz)")

p1


plot(p1)


# get it for stressed vowels only

stressed_data <- data %>% filter(Type == "Stressed")

# Check if the filtered dataset has any valid values
if (nrow(stressed_data) > 0 && 
    any(!is.na(stressed_data$f2lobanov)) && 
    any(!is.na(stressed_data$f1lobanov))) {
  
  # Get max values for F2 and F1 if there is valid data
  max_f2 <- max(stressed_data$f2lobanov, na.rm = TRUE)
  max_f1 <- max(stressed_data$f1lobanov, na.rm = TRUE)
  
  # If max returns -Inf or NA, set fallback values
  max_f2 <- ifelse(is.finite(max_f2), max_f2, 1000)  # Default max value for F2
  max_f1 <- ifelse(is.finite(max_f1), max_f1, 500)   # Default max value for F1
  
  # Plot the stressed vowels data
  p1 <- stressed_data %>%
    ggplot(aes(x = f2lobanov, y = f1lobanov, color = phoneme)) + 
    geom_point(alpha = 0.6, shape = 21) +  # Use shape 21 for circles
    stat_ellipse(aes(fill = phoneme), geom = "polygon", alpha = 0.2, level = 0.95) +  # Filled ellipses with transparency
    theme_minimal() +
    scale_x_reverse(breaks = seq(floor(min(stressed_data$f2lobanov, na.rm = TRUE)), 
                                 max_f2, by = 0.5)) +
    scale_y_reverse(breaks = seq(floor(min(stressed_data$f1lobanov, na.rm = TRUE)), 
                                 max_f1, by = 0.5)) +
    scale_color_manual(values = c("a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
                                  "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
                                  "ø" = "#7FDBFF", "ə" = "#8C564B", "y" = "#E377C2"),
                       labels = c("a", "e", "i", "ɛ", "u", "o", "ø", "ə", "y")) + 
    scale_fill_manual(values = c("a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
                                 "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
                                 "ø" = "#7FDBFF", "ə" = "#8C564B", "y" = "#E377C2"),
                      labels = c("a", "e", "i", "ɛ", "u", "o", "ø", "ə", "y")) +
    geom_text(data = stressed_data %>% group_by(phoneme) %>% summarize(center_x = mean(f2lobanov), center_y = mean(f1lobanov)),
              aes(x = center_x, y = center_y, label = phoneme), size = 7, fontface = "bold", color = "black") +
    theme(legend.position = "none", 
          plot.margin = margin(10, 10, 10, 10, "pt"),
          axis.line = element_line(color = "black"),  # Add black border to axes
          axis.ticks = element_line(color = "black"),  # Add black ticks to axes
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),  
          axis.text = element_text(size = 20),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
    labs(x = "Speaker-normalized F2 (Z-score)", y = "Speaker-normalized F1 (Z-score)")
  
  # Print the plot
  print(p1)
}

### in Hz
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
  
  # Plot the stressed vowels data
  p2 <- stressed_data %>%
    ggplot(aes(x = F2, y = F1, color = phoneme)) + 
    geom_point(alpha = 0.6, shape = 21) +  # Use shape 21 for circles
    stat_ellipse(aes(fill = phoneme), geom = "polygon", alpha = 0.2, level = 0.5) +  # Filled ellipses with transparency
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
    geom_text(data = stressed_data %>% group_by(phoneme) %>% summarize(center_x = mean(F2), center_y = mean(F1)),
              aes(x = center_x, y = center_y, label = phoneme), size = 7, fontface = "bold", color = "black") +
    theme(legend.position = "none", 
          plot.margin = margin(10, 10, 10, 10, "pt"),
          axis.line = element_line(color = "black"),  # Add black border to axes
          axis.ticks = element_line(color = "black"),  # Add black ticks to axes
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),  
          axis.text = element_text(size = 20),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
    labs(x = "F2 (Hz)", y = "F1 (Hz)")
  
  # Print the plot
  print(p2)
}

####
#for unstressed vowels only in Lobanov

unstressed_data <- data %>% filter(Type == "Unstressed")
unstressed_data <- data %>%
  filter(Type == "Unstressed" & phoneme != "ɛ")

# Check if the filtered dataset has any valid values
if (nrow(unstressed_data) > 0 && 
    any(!is.na(unstressed_data$f2lobanov)) && 
    any(!is.na(unstressed_data$f1lobanov))) {
  
  # Get max values for F2 and F1 if there is valid data
  max_f2 <- max(unstressed_data$f2lobanov, na.rm = TRUE)
  max_f1 <- max(unstressed_data$f1lobanov, na.rm = TRUE)
  
  # If max returns -Inf or NA, set fallback values
  max_f2 <- ifelse(is.finite(max_f2), max_f2, 1000)  # Default max value for F2
  max_f1 <- ifelse(is.finite(max_f1), max_f1, 500)   # Default max value for F1
  
  # Plot the unstressed vowels data
  p3 <- unstressed_data %>%
    ggplot(aes(x = f2lobanov, y = f1lobanov, color = phoneme)) + 
    geom_point(alpha = 0.6, shape = 21) +  # Use shape 21 for circles
    stat_ellipse(aes(fill = phoneme), geom = "polygon", alpha = 0.2, level = 0.4) +  # Filled ellipses with transparency
    theme_minimal() +
    scale_x_reverse(breaks = seq(floor(min(stressed_data$f2lobanov, na.rm = TRUE)), 
                                 max_f2, by = 0.5)) +
    scale_y_reverse(breaks = seq(floor(min(stressed_data$f1lobanov, na.rm = TRUE)), 
                                 max_f1, by = 0.5)) +
    scale_color_manual(values = c("a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
                                  "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
                                  "ø" = "#7FDBFF", "ə" = "#8C564B", "y" = "#E377C2"),
                       labels = c("a", "e", "i", "ɛ", "u", "o", "ø", "ə", "y")) + 
    scale_fill_manual(values = c("a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
                                 "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
                                 "ø" = "#7FDBFF", "ə" = "#8C564B", "y" = "#E377C2"),
                      labels = c("a", "e", "i", "ɛ", "u", "o", "ø", "ə", "y")) +
    geom_text(data = unstressed_data %>% group_by(phoneme) %>% summarize(center_x = mean(f2lobanov), center_y = mean(f1lobanov)),
              aes(x = center_x, y = center_y, label = phoneme), size = 7, fontface = "bold", color = "black") +
    theme(legend.position = "none", 
          plot.margin = margin(10, 10, 10, 10, "pt"),
          axis.line = element_line(color = "black"),  # Add black border to axes
          axis.ticks = element_line(color = "black"),  # Add black ticks to axes
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),  
          axis.text = element_text(size = 20),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
    labs(x = "Speaker-normalized F2 (Z-score)", y = "Speaker-normalized F1 (Z-score)")
  
  # Print the plot
  print(p3)}

#for unstressed vowels only in Hz

unstressed_data <- data %>% filter(Type == "Unstressed")
unstressed_data <- data %>%
  filter(Type == "Unstressed" & phoneme != "ɛ")

# Check if the filtered dataset has any valid values
if (nrow(unstressed_data) > 0 && 
    any(!is.na(unstressed_data$F2)) && 
    any(!is.na(unstressed_data$F1))) {
  
  # Get max values for F2 and F1 if there is valid data
  max_f2 <- max(unstressed_data$F2, na.rm = TRUE)
  max_f1 <- max(unstressed_data$F1, na.rm = TRUE)
  
  # If max returns -Inf or NA, set fallback values
  max_f2 <- ifelse(is.finite(max_f2), max_f2, 1000)  # Default max value for F2
  max_f1 <- ifelse(is.finite(max_f1), max_f1, 500)   # Default max value for F1
  
  # Plot the unstressed vowels data
  p4 <- unstressed_data %>%
    ggplot(aes(x = F2, y = F1, color = phoneme)) + 
    geom_point(alpha = 0.6, shape = 21) +  # Use shape 21 for circles
    stat_ellipse(aes(fill = phoneme), geom = "polygon", alpha = 0.2, level = 0.4) +  # Filled ellipses with transparency
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
    geom_text(data = unstressed_data %>% group_by(phoneme) %>% summarize(center_x = mean(F2), center_y = mean(F1)),
              aes(x = center_x, y = center_y, label = phoneme), size = 7, fontface = "bold", color = "black") +
    theme(legend.position = "none", 
          plot.margin = margin(10, 10, 10, 10, "pt"),
          axis.line = element_line(color = "black"),  # Add black border to axes
          axis.ticks = element_line(color = "black"),  # Add black ticks to axes
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),  
          axis.text = element_text(size = 20),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
    labs(x = "F2 (Hz)", y = "F1 (Hz)")
  
  # Print the plot
  print(p4)}




# per city
cities <- c("Rivarolo", "Pont", "Busano", "Salassa")

# Filter for relevant data
facet_data <- stressed_data %>%
  filter(city %in% cities)

# Compute label positions: mean F1/F2 per phoneme per city
label_data <- facet_data %>%
  group_by(city, phoneme) %>%
  summarize(center_x = mean(f2lobanov, na.rm = TRUE),
            center_y = mean(f1lobanov, na.rm = TRUE),
            .groups = "drop")
facet_data$city <- factor(facet_data$city, levels = c("Rivarolo", "Busano", "Salassa", "Pont"))

# Plot with facet_wrap() ensuring the correct order
p_all <- ggplot(facet_data, aes(x = f2lobanov, y = f1lobanov, color = phoneme)) +
  geom_point(alpha = 2, shape = 21) +
  stat_ellipse(aes(fill = phoneme), geom = "polygon", alpha = 0.3, level = 0.5) +
  geom_text(data = label_data, aes(x = center_x, y = center_y, label = phoneme),
            inherit.aes = FALSE,
            size = 6, fontface = "bold", color = "black") +
  scale_x_reverse() +
  scale_y_reverse() +
  scale_color_manual(values = c(
    "a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
    "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
    "ø" = "#7FDBFF", "ə" = "#8C564B", "y" = "#E377C2"
  )) +
  scale_fill_manual(values = c(
    "a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
    "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
    "ø" = "#7FDBFF", "ə" = "#8C564B", "y" = "#E377C2"
  )) +
  facet_wrap(~ city, ncol = 2) +  # 2 columns for correct layout
  theme_minimal() +
  labs(
    title = "Stressed Vowel Spaces by City",
    x = "Speaker-normalized F2 (Z-score)",
    y = "Speaker-normalized F1 (Z-score)"
  ) +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 20, face = "bold"),
    legend.position = "none"
  )

# Print the final plot
print(p_all)


# plot only three words

stressed_data <- data %>% filter(Type == "Stressed")

# Keep only specific schwa tokens
stressed_data <- stressed_data %>% 
  filter(!(phoneme == "ə" & !word %in% c("tərzɛnt", "vəsko", "sədəs")))

# Check if the filtered dataset has any valid values
if (nrow(stressed_data) > 0 && 
    any(!is.na(stressed_data$f2lobanov)) && 
    any(!is.na(stressed_data$f1lobanov))) {
  
  # Get max values for F2 and F1 if there is valid data
  max_f2 <- max(stressed_data$f2lobanov, na.rm = TRUE)
  max_f1 <- max(stressed_data$f1lobanov, na.rm = TRUE)
  
  # If max returns -Inf or NA, set fallback values
  max_f2 <- ifelse(is.finite(max_f2), max_f2, 1000)  # Default max value for F2
  max_f1 <- ifelse(is.finite(max_f1), max_f1, 500)   # Default max value for F1
  
  # Plot the stressed vowels data
  p1 <- stressed_data %>%
    ggplot(aes(x = f2lobanov, y = f1lobanov, color = phoneme)) + 
    geom_point(alpha = 0.6, shape = 21) +  # Use shape 21 for circles
    stat_ellipse(aes(fill = phoneme), geom = "polygon", alpha = 0.2, level = 0.5) +  # Filled ellipses with transparency
    theme_minimal() +
    scale_x_reverse(breaks = seq(floor(min(stressed_data$f2lobanov, na.rm = TRUE)), 
                                 max_f2, by = 0.5)) +
    scale_y_reverse(breaks = seq(floor(min(stressed_data$f1lobanov, na.rm = TRUE)), 
                                 max_f1, by = 0.5)) +
    scale_color_manual(values = c("a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
                                  "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
                                  "ø" = "#7FDBFF", "ə" = "black", "y" = "#E377C2"),
                       labels = c("a", "e", "i", "ɛ", "u", "o", "ø", "ə", "y")) + 
    scale_fill_manual(values = c("a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
                                 "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
                                 "ø" = "#7FDBFF", "ə" = "black", "y" = "#E377C2"),
                      labels = c("a", "e", "i", "ɛ", "u", "o", "ø", "ə", "y")) +
    geom_text(data = stressed_data %>% group_by(phoneme) %>% summarize(center_x = mean(f2lobanov), center_y = mean(f1lobanov)),
              aes(x = center_x, y = center_y, label = phoneme), size = 7, fontface = "bold", color = "black") +
    theme(legend.position = "none", 
          plot.margin = margin(10, 10, 10, 10, "pt"),
          axis.line = element_line(color = "black"),  # Add black border to axes
          axis.ticks = element_line(color = "black"),  # Add black ticks to axes
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),  
          axis.text = element_text(size = 20),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
    labs(x = "Speaker-normalized F2 (Z-score)", y = "Speaker-normalized F1 (Z-score)")
  
  # Print the plot
  print(p1)
}


mean_values <- stressed_data %>% 
  filter(word %in% c("tərdəs", "vəsko", "sədəs")) %>% 
  group_by(word) %>% 
  summarize(mean_f1 = mean(F1, na.rm = TRUE), 
            mean_f2 = mean(F2, na.rm = TRUE))

# Print the mean values
print(mean_values)

############3
stressed_data <- data %>% filter(Type == "Stressed")

# Define stressed versions of the words with the IPA stress symbol
word_labels <- c("tərdəs" = "/ˈtərdəs/", "vəsko" = "/ˈvəsko/", "sədəs" = "/ˈsədəs/")

# Keep only schwa tokens from the selected words
schwa_data <- stressed_data %>% 
  filter(phoneme == "ə" & word %in% names(word_labels))

# Get mean coordinates for other vowels (excluding schwa)
vowel_means <- stressed_data %>%
  #filter(phoneme != "ə") %>%
  group_by(phoneme) %>%
  summarize(center_x = mean(f2lobanov, na.rm = TRUE), 
            center_y = mean(f1lobanov, na.rm = TRUE))

# Get mean coordinates for schwa in each word
schwa_means <- schwa_data %>% 
  group_by(word) %>% 
  summarize(center_x = mean(f2lobanov, na.rm = TRUE), 
            center_y = mean(f1lobanov, na.rm = TRUE))

# Replace word column with IPA-stressed labels
schwa_means$word <- word_labels[schwa_means$word]

# Check if the filtered dataset has any valid values
if (nrow(schwa_data) > 0 && 
    any(!is.na(schwa_data$f2lobanov)) && 
    any(!is.na(schwa_data$f1lobanov))) {
  
  # Define colors for the three words with stressed IPA labels
  word_colors <- c("ˈtərdəs" = "#1F77B4", "ˈvəsko" = "#FF7F0E", "ˈsədəs" = "#2CA02C")
  
  # Plot the schwa tokens for the three words
  p1 <- schwa_data %>%
    ggplot(aes(x = f2lobanov, y = f1lobanov, color = word)) + 
    # Add a dot for the mean of each schwa (per word)
    geom_point(data = schwa_means, aes(x = center_x, y = center_y), 
               shape = 16, size = 4, color = "black") +  # Black dot for mean
    
    theme_minimal() +
    scale_x_reverse() +
    scale_y_reverse() +
    scale_color_manual(values = word_colors) + 
    scale_fill_manual(values = word_colors) +
    
    # Add the word labels at their mean points (with stress symbol)
    geom_text(data = schwa_means, 
              aes(x = center_x, y = center_y, label = word), 
              size = 7, fontface = "bold", color = "black", nudge_y = -0.1) + 
    
    # Add the mean points of the other vowels (symbol only)
    geom_text(data = vowel_means, 
              aes(x = center_x, y = center_y, label = phoneme), 
              size = 6, fontface = "bold", color = "black") +
    
    theme(legend.position = "none", 
          plot.margin = margin(10, 10, 10, 10, "pt"),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),  
          axis.text = element_text(size = 20),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
    labs(x = "Speaker-normalized F2 (Z-score)", y = "Speaker-normalized F1 (Z-score)")
  
  # Print the plot
  print(p1)
}



# Convert to milliseconds and filter
duration_data <- data %>%
  filter(
    !is.na(duration),
    phoneme %in% c("a", "e", "i", "o", "u", "ɛ", "ø", "ə", "y"),
    !(phoneme == "ɛ" & Type == "Unstressed")
  ) %>%
  mutate(duration_ms = duration * 1000)  # convert to milliseconds

# Plot without x-axis title
duration_plot <- duration_data %>%
  ggplot(aes(x = phoneme, y = duration_ms, fill = Type, color = Type)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA, width = 0.6, position = position_dodge(width = 0.8)) +
  geom_jitter(position = position_jitter(width = 0.2, height = 0.1), size = 1.5, alpha = 0.3) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_fill_manual(values = c("Stressed" = "#E15759", "Unstressed" = "#4E79A7")) +
  scale_color_manual(values = c("Stressed" = "#E15759", "Unstressed" = "#4E79A7")) +
  labs(y = "Duration (ms)", x = NULL) +  # Remove x-axis title
  theme_minimal(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 18)
  )

# Show the plot
print(duration_plot)


