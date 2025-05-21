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
library(brms)

# plotting
library(ggplot2)
library(cowplot)
library(ggpubr)

# regression
library(lme4) # mixed models
library(lmerTest) # pvalues for factors in mixed models, estimating dfs...
library(car) # to get vif
library(MuMIn) # for r.squaredGLMM
library(emmeans)

# visualization of models
library(sjPlot)
library(ggeffects)
library(glmmTMB)
library(xtable)
#---------------------

setwd("D:/ProgettoRivarolese/")
data <- readRDS("dataNormalized.rds")

library(lme4)
library(multcomp)
library(car)


##############
#F1
data_stressed <- data %>% filter(Type == "Stressed")

model1 <- lmer(f1lobanov ~ phoneme + city + age + (1 | speaker) + (1|word), 
               data = data_stressed)

model_summary <- summary(model1)

# pairwise comparisons
# phonemes
phoneme_comparisons <- emmeans(model1, ~ phoneme)
pairwise_phoneme <- pairs(phoneme_comparisons, adjust = "bonferroni")
summary(pairwise_phoneme)


#F2
data_stressed <- data %>% filter(Type == "Stressed")

model2 <- lmer(f2lobanov ~ phoneme + city + age + (1 | speaker) + (1|word), 
               data = data_stressed)

model_summary <- summary(model2)

# pairwise comparisons
# phone
phoneme_comparisons <- emmeans(model2, ~ phoneme)
pairwise_phoneme_2 <- pairs(phoneme_comparisons, adjust = "bonferroni")
summary(pairwise_phoneme_2)


# unstressed

#F1
data_unstressed <- data %>% filter(Type == "Unstressed")

model1 <- lmer(f1lobanov ~ phoneme + city + age + (1 | speaker) + (1|word), 
               data = data_unstressed)

model_summary <- summary(model1)

# pairwise comparisons
# phone
phoneme_comparisons <- emmeans(model1, ~ phoneme)
pairwise_phoneme <- pairs(phoneme_comparisons, adjust = "bonferroni")
summary(pairwise_phoneme)

#F2
data_unstressed <- data %>% filter(Type == "Unstressed")

model2 <- lmer(f2lobanov ~ phoneme + city + age + (1 | speaker) + (1|word), 
               data = data_unstressed)

model_summary <- summary(model2)

# pairwise comparisons
# phone
phoneme_comparisons <- emmeans(model2, ~ phoneme)
pairwise_phoneme <- pairs(phoneme_comparisons, adjust = "bonferroni")
summary(pairwise_phoneme)

#interaction
phonemecity_comparisons <- emmeans(model2, ~ phoneme | city)
pairwise_phonemecity <- pairs(phonemecity_comparisons, adjust = "bonferroni")
summary(pairwise_phonemecity)




##########


model <- lmer(duration ~ phoneme*Type + (1 | speaker) + (1 | word), data = duration_data)

summary(model)

comparisons <- emmeans(model, ~ Type|phoneme)
pairwise_phoneme <- pairs(comparisons, adjust = "bonferroni")
summary(pairwise_phoneme)




