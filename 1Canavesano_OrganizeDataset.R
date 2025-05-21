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

data <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X01formants.xlsx")
data1 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X02formants.xlsx")
data2 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X03formants.xlsx")
data3 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X04formants.xlsx")
data4 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X05formants.xlsx")
data5 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X06formants.xlsx")
data6 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X07formants.xlsx")
data7 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X08formants.xlsx")
data8 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X09formants.xlsx")
data9 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X10formants.xlsx")
data10 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X11formants.xlsx")
data11 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X12formants.xlsx")
data12 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X13formants.xlsx")
data13 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X14formants.xlsx")
data14 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X15formants.xlsx")
data15 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X16formants.xlsx")
data16 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X17formants.xlsx")
data17 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X18formants.xlsx")
data18 <- read.xlsx("D:/ProgettoRivarolese/FormantExtraction_Duration/X19formants.xlsx")

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
data1 <- data1[!(data1$word == "øi" & data1$phoneme == "i"), ]
data_cleaned <- data %>%
  filter(!(type == "unstressed" & phoneme == "ɛ"))


# rename column file with speaker
names(data1)[names(data1) == "file"] <- "speaker"


saveRDS(data1, file = "D:/ProgettoRivarolese/data1.rds")

