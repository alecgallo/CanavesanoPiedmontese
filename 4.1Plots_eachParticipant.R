rm(list=ls())
#--------------------------------------
# load libraries

# reading/writing data
library(readr) 
library(writexl)
library(openxlsx)
library(base)
library(stats)
library(cowplot)

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

#----------------------------------
setwd("E:/ProgettoRivarolese/")
data <- readRDS("dataFiltered.rds")

# plot vowel inventories for each speaker
speakers <- unique(data$speaker)

for (i in seq_along(speakers)) {
  speaker <- speakers[i]
  
  # Filter data for the current speaker
  speaker_data <- data %>% filter(speaker == !!speaker)
  
  # Create plot for the current speaker
  p <- speaker_data %>%
    ggplot(aes(x = f2_original_scale, y = f1_original_scale, color = phoneme)) + 
    geom_point(alpha = 0.5, shape = 21) +  # Use shape 21 for circles
    stat_ellipse(aes(fill = phoneme), geom = "polygon", alpha = 0.2, level = 0.5) +  # Filled ellipses with transparency
    theme_minimal() +
    scale_x_reverse(breaks = seq(0, max(speaker_data$f2_original_scale, na.rm = TRUE), by = 250)) +
    scale_y_reverse(breaks = seq(0, max(speaker_data$f1_original_scale, na.rm = TRUE), by = 100)) +
    scale_color_manual(values = c("a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
                                  "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
                                  "ø" = "#7FDBFF", "ə" = "#8C564B", "y" = "#E377C2"),
                       labels = c("a", "e", "i", "ɛ", "u", "o", "ø", "ə", "y")) + 
    scale_fill_manual(values = c("a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
                                 "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
                                 "ø" = "#7FDBFF", "ə" = "#8C564B", "y" = "#E377C2"),
                      labels = c("a", "e", "i", "ɛ", "u", "o", "ø", "ə", "y")) +
    geom_text(data = speaker_data %>% group_by(phoneme) %>% summarize(center_x = mean(f2_original_scale), center_y = mean(f1_original_scale)),
              aes(x = center_x, y = center_y, label = phoneme), size = 7, fontface = "bold", color = "black") +
    theme(legend.position = "none", 
          plot.margin = margin(10, 10, 10, 10, "pt"),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),  
          axis.text = element_text(size = 16)) +  
    labs(x = "Speaker-normalized F2 (Hz)", y = "Speaker-normalized F1 (Hz)") +
    annotate("text", x = Inf, y = -Inf, label = paste0("Speaker ", i), hjust = 1.1, vjust = -1.5, size = 6, fontface = "bold", color = "black")
  
  # Save the plot to a file
  ggsave(filename = paste0("Speaker_", i, ".png"), plot = p, width = 10, height = 7)
}
