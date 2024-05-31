rm(list=ls())
#--------------------------------------
# load libraries

# reading/writing data
library(readr) 
library(writexl)
library(openxlsx)
library(dplyr)

#--------------------------------------
# load spreadsheet with formant values for each participant

data <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker1formants.csv")
data1 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker2formants.csv")
data2 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker3formants.csv")
data3 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker4formants.csv")
data4 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker5formants.csv")
data5 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker6formants.csv")
data6 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker7formants.csv")
data7 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker8formants.csv")
data8 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker9formants.csv")
data9 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker10formants.csv")
data10 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker11formants.csv")
data11 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker12formants.csv")
data12 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker13formants.csv")
data13 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker14formants.csv")
data14 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker15formants.csv")
data15 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker16formants.csv")
data16 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker17formants.csv")
data17 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker18formants.csv")
data18 <- read.csv("E:/ProgettoRivarolese/FormantExtraction/speaker19formants.csv")

#combine datasets
data <- rbind(data, data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11, data12, data13, data14, data15, data16, data17, data18)

## replace vowels symbols with IPA symbols
# Define the substitutions for the schwa
substitutions <- list("EE" = "EO", "EO" = "ə")

# substitute the phoneme and word columns
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

saveRDS(data1, file = "E:/ProgettoRivarolese/data1.rds")

