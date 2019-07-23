###################
# observations with large squared error tend to be those where outcome is far from mean of training data
# Input: assumes that prep_predictions.R has been run
# Output: 6 graphs [outcome]_unitmse_truth.pdf
# Runtime: seconds on a laptop
# Code by Matt Salganik (minor edits by Alex Kindel)

library(tidyverse)
library(here)
library(magrittr)
library(ggridges)
library(gridExtra)

# Set directory information
code.dir <- file.path(here(), "code")
data.dir <- file.path(here(), "data")
private.data.dir <- file.path(data.dir, "private")
working.data.dir <- file.path(data.dir, "intermediate_files")
results.dir <- file.path(here(), "results", "figures")

# Set ggplot2 theme
theme_set(theme_bw())

# Source helper functions
source(file.path(code.dir, "outcome_labels.R"))

# Load data
load(file.path(working.data.dir, "processed_predictions.RData"))

# Join true values, training data mean
# Also compute absolute difference between them
unit_outcome %<>%
  left_join(predictions %>%
              filter(account == "ADSgrp5") %>%
              select(challengeID, outcome, truth),
            by=c("challengeID", "outcome")) %>%
  left_join(predictions %>%
              group_by(outcome) %>%
              summarize(mean_training = unique(ybar_train)),
            by = "outcome") %>%
  mutate(diff_truth_mean_training = truth - mean_training,
         abs_diff_truth_mean_training = abs(diff_truth_mean_training))

# mean_training_data <- predictions %>% 
#   group_by(outcome) %>% 
#   summarize(mean_training = unique(ybar_train))

# Set plot text label values
text_label_values <- tribble(
  ~outcome, ~x_value, ~y_value,
  #--|--|----
  "gpa", 2.5, 2.72,
  "grit", 3, 3.32,
  "materialHardship", 0.3, 0.06, 
  "eviction", 0.5, 0.15,
  "layoff", 0.4, 0.13, 
  "jobTraining", 0.35, 0.15
)

# Begin building plots
plots <- list()
outcomes_to_plot <- c("gpa", "grit", "materialHardship", "eviction", "layoff", "jobTraining")
for (outcome_to_plot in outcomes_to_plot) {

  # Subset to non-missing values for this outcome
  data_to_plot <- unit_outcome %>%
    filter(outcome == outcome_to_plot) %>%
    filter(!is.na(truth))
  
  # Construct ridge plot
  p <- ggplot(data_to_plot, 
              aes(x = mse_unit_outcome, y = truth, group = truth)) + 
    geom_density_ridges(rel_min_height = 0.01) +
    xlab("Difficulty") +
    ylab(clean_outcome_label(outcome_to_plot)) +
    ggtitle(clean_outcome_label(outcome_to_plot)) +
    theme_ridges() +
    geom_hline(yintercept = unit_outcome %>% filter(outcome == outcome_to_plot) %>% pull(mean_training), 
               linetype = "dashed") +
    annotate("text", 
             x = text_label_values %>% filter(outcome == outcome_to_plot) %>% pull(x_value),
             y = text_label_values %>% filter(outcome == outcome_to_plot) %>% pull(y_value), 
             label = "mean training data")
  
  # Do outcome-specific plot manipulation
  if (outcome_to_plot %in% c("eviction", "layoff", "jobTraining")) {
    # Binary outcomes should be scaled a little differently
    p <- p + scale_y_continuous(breaks = c(0, 1))
  }
  if (outcome_to_plot %in% c("materialHardship")) {
    # Material Hardship is an 11 point scale and the max value in the data is 9/11
    p <- p + scale_y_continuous(breaks = round(c(0, 1/11, 2/11, 3/11, 4/11, 5/11, 6/11, 7/11, 8/11, 9/11), 2))
  }
  
  # Save plot
  plots[[outcome_to_plot]] <- p
}

# Save plots
ggsave(plot = grid.arrange(grobs=plots, ncol=3),
       filename = "s14_distribution_unitmse_truth_alloutcomes.pdf",
       path = results.dir, 
       device = "pdf",
       dpi = 300,
       width = 10, height = 7.5, units = "in")
