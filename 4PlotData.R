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
setwd("E:/ProgettoRivarolese/")
data <- readRDS("dataFiltered.rds")

# plot the data
p1 <- data %>%
  ggplot(aes(x = f2_original_scale, y = f1_original_scale, color = phoneme)) + 
  geom_point(alpha = 0.5, shape = 21) +  # Use shape 21 for circles
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
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),  
        axis.text = element_text(size = 16)) +  
  labs(x = "Speaker-normalized F2 (Hz)", y = "Speaker-normalized F1 (Hz)")

p1

