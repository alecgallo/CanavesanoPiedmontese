rm(list=ls())
#--------------------------------------
# load libraries

# reading/writing data
library(readr) 
library(writexl)
library(openxlsx)
library(dplyr)

data <- read.csv("D:/ProgettoRivarolese/FormantExtraction/Gianna_formants.csv")

## replace vowels symbols with IPA symbols
# Define the substitutions for the schwa
substitutions <- list("EE" = "EO", "EO" = "ə")

# Perform substitutions on the phoneme and word columns
for (key in names(substitutions)) {
  data$phoneme <- gsub(key, substitutions[[key]], data$phoneme)
}

for (key in names(substitutions)) {
  data$word <- gsub(key, substitutions[[key]], data$word)
}

# Define the substitutions for ɛ and ø
substitutions <- list("E" = "ɛ", "\xf8" = "ø", "\xf1" = "ñ")

# Perform substitutions on the phoneme column
for (key in names(substitutions)) {
  data$phoneme <- gsub(key, substitutions[[key]], data$phoneme)
}

for (key in names(substitutions)) {
  data$word <- gsub(key, substitutions[[key]], data$word)
}


# create a subset with vowels of interest only

data1 <- subset(data, phoneme %in% c("a", "e", "ə", "ɛ", "ø", "i", "o", "u", "y"))

# filter the final "i" of "ninsøi"
data1 <- data1[!(data1$word == "ninsøi" & data1$phoneme == "i"), ]

# rename column file with speaker
names(data1)[names(data1) == "file"] <- "speaker"

saveRDS(data1, file = "D:/ProgettoRivarolese/data1.rds")
