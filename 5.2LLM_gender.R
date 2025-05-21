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
library(gridExtra)
library(grid)
#---------------------

setwd("D:/ProgettoRivarolese/")
data <- readRDS("dataNormalized.rds")

data_stressed <- data %>% filter(Type == "Stressed")

#gender stats
model1 <- lmer(F1 ~ phoneme*gender +  city + age + (1 | speaker), 
               data = data_stressed)

model_summary <- summary(model1)

emm_results <- emmeans(model1, ~ gender | phoneme)

pairwise_gender <- pairs(emm_results, adjust = "bonferroni")

# View results
summary(pairwise_gender)

#F2
model1 <- lmer(F2 ~ phoneme*gender +  city + age + (1 | speaker), 
               data = data_stressed)

model_summary <- summary(model1)

emm_results <- emmeans(model1, ~ gender | phoneme)

pairwise_gender <- pairs(emm_results, adjust = "bonferroni")

# View results
summary(pairwise_gender)


################
# unstressed

data_unstressed <- data %>% filter(Type == "Unstressed")

#gender stats
model1 <- lmer(F1 ~ phoneme*gender +  city + age + (1 | speaker), 
               data = data_unstressed)

model_summary <- summary(model1)

emm_results <- emmeans(model1, ~ gender | phoneme)

pairwise_gender <- pairs(emm_results, adjust = "bonferroni")

# View results
summary(pairwise_gender)

#F2
model1 <- lmer(F2 ~ phoneme*gender +  city + age + (1 | speaker), 
               data = data_unstressed)

model_summary <- summary(model1)

emm_results <- emmeans(model1, ~ gender | phoneme)

pairwise_gender <- pairs(emm_results, adjust = "bonferroni")

# View results
summary(pairwise_gender)


