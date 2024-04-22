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


data <- readRDS("dataFiltered.rds")


p1<-data %>%
  ggplot(aes(x = f2_original_scale, y = f1_original_scale, color = phoneme, shape = phoneme)) + 
  geom_point(alpha = 0.5) +
  stat_ellipse() +
  theme_minimal() +
  scale_x_reverse() +
  scale_y_reverse() +
  scale_color_manual(values = c("a" = "#FF7F0E", "e" = "#2CA02C", "i" = "#1F77B4", 
                                "o" = "#FFD700", "u" = "#9467BD", "ɛ" = "#D62728", 
                                "ø" = "#7FDBFF", "ə" = "#8C564B", "y" = "#E377C2"),
                     labels = c("a", "e", "i", "ɛ", "u", "o", "ø", "ə", "y")) + 
  scale_shape_manual(values = 1:9,
                     labels = c("a", "e", "i", "ɛ", "u", "o", "ø", "ə", "y")) + 
  ggtitle("Vowel Inventory (in Hz)") +
  geom_text(data = data %>% group_by(phoneme) %>% summarize(center_x = mean(f2_original_scale), center_y = mean(f1_original_scale)),
            aes(x = center_x, y = center_y, label = phoneme), size = 7, fontface = "bold", color = "black") +
  theme(legend.position = "bottom", legend.box = "horizontal", plot.margin = margin(10, 10, 10, 10, "pt")) +
  labs(x = "Speaker-normalized F2 (Hz)", y = "Speaker-normalized F1 (Hz)", font = 8)

p1
