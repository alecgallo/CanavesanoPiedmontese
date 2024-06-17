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

setwd("E:/ProgettoRivarolese/")
data <- readRDS("dataFiltered.rds")

generate_plots_per_participant <- function(data) {
  unique_speakers <- unique(data$speaker)
  
  for (speaker_id in unique_speakers) {
    # Filter data for the current speaker
    vowel_data <- subset(data, speaker == speaker_id & phoneme %in% c("ə", "ɛ"))
    
    # Ensure factors are properly coded
    vowel_data$phoneme <- factor(vowel_data$phoneme)
    
    # Define the dependent variables (formants)
    dependent_vars <- cbind(vowel_data$f1_original_scale, vowel_data$f2_original_scale)
    
    # Fit the MANOVA model
    manova_model <- manova(dependent_vars ~ phoneme, data = vowel_data)
    
    # Print summary of the MANOVA model
    summary_manova <- summary(manova_model, test = "Pillai")
    print(summary_manova)
    
    # Extract and print the Pillai score
    pillai_score <- summary_manova$stats[1, "Pillai"]
    print(paste("Pillai Score for speaker", speaker_id, ":", pillai_score))
    
    mean_points <- vowel_data %>%
      group_by(phoneme) %>%
      summarize(mean_f1 = mean(f1_original_scale),
                mean_f2 = mean(f2_original_scale))
    
    # Find the range of the axes
    x_range <- range(vowel_data$f2_original_scale)
    y_range <- range(vowel_data$f1_original_scale)
    
    # Calculate positions for the annotations
    x_pos_pillai <- x_range[1] - 0.05 * diff(x_range)
    y_pos_pillai <- y_range[1] - 0.05 * diff(y_range)
    
    x_title <- mean(x_range)
    y_title <- y_range[2] + 0.05 * diff(y_range)
    
    # Create the plot
    p1 <- ggplot(vowel_data, aes(y = f1_original_scale, x = f2_original_scale, color = phoneme)) +
      geom_point(alpha = 0.6, size = 2) +  # Add black border to points
      stat_ellipse(level = 0.50, aes(fill = phoneme), alpha = 0.2, geom = "polygon", color = "black") +  # Color ellipses by phoneme
      geom_text(data = mean_points, aes(x = mean_f2, y = mean_f1, label = phoneme), 
                size = 10, color = "black") +  # Add labels
      theme_minimal() +
      coord_cartesian(xlim = c(1200, 2250), ylim = c(300, 900)) +  # Set axis limits
      labs(x = "Speaker-normalized F2 (Hz)",
           y = "Speaker-normalized F1 (Hz)",
           title = paste(speaker_id)) +
      scale_color_manual(values = c("ə" = "#1F77B4", "ɛ" = "#D62728")) +
      scale_fill_manual(values = c("ə" = "#1F77B4", "ɛ" = "#D62728")) +  # Set fill colors for ellipses
      theme_minimal() +
      theme(legend.position = "none",  # Remove legend
            axis.line = element_line(color = "black"),  # Add black border to axes
            axis.ticks = element_line(color = "black"),  # Add black ticks to axes
            axis.text = element_text(color = "black", size = 23),  # Change axis text color and size
            plot.margin = margin(10, 10, 10, 10, "pt"),
            axis.title.x = element_text(size = 25),
            axis.title.y = element_text(size = 25), 
            plot.title = element_text(hjust = 0.5, size = 30),
            panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
      annotate("text", x = 2300, y = 900, 
               label = paste("Pillai Score:", round(pillai_score, 3)), 
               hjust = 1, vjust = 1, size = 10, color = "black")  # Annotate Pillai score in top right
    
    vp <- viewport(x = unit(0, "npc"), y = unit(1, "npc"),
                   width = unit(0, "npc"), height = unit(0, "npc"),
                   just = c("left", "top"))
    
    # Draw 'A' annotation outside the plot
    grid.draw(textGrob("A", x = unit(0, "npc"), y = unit(1, "npc"),
                       just = c("left", "top"), gp = gpar(fontsize = 12, fontface = "bold")), vp = vp)
    
    # Add a bit of spacing
    grid.text("", x = unit(0.1, "npc"), y = unit(1, "npc"))
    
    print(p1)
    
    # Save the plot
   # ggsave(filename = paste0("participant_", speaker_id, "_plot.png"), plot = p1)
  }
}

# Assuming 'data' is your dataframe and it includes the necessary columns
generate_plots_per_participant(data)





