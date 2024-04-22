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


setwd("D:/ProgettoRivarolese/")
data <- readRDS("data1.rds")

datalobanov = data %>% 
  group_by(speaker) %>% 
  mutate(
    f1lobanov = normLobanov(F1), 
    f2lobanov = normLobanov(F2)
  )

# Calculate the grand mean and average standard deviation of all ten speakers for F1 and F2
grand_mean_f1 <- mean(datalobanov$F1)
grand_mean_f2 <- mean(datalobanov$F2)
avg_sd_f1 <- mean(sd(datalobanov$F1))
avg_sd_f2 <- mean(sd(datalobanov$F2))

# Convert z-scores back to original scale (Hz)
datalobanov <- datalobanov %>%
  mutate(
    f1_original_scale = f1lobanov * avg_sd_f1 + grand_mean_f1,
    f2_original_scale = f2lobanov * avg_sd_f2 + grand_mean_f2
  )


saveRDS(datalobanov, file = "D:/ProgettoRivarolese/dataNormalized.rds")
