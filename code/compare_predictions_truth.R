# Compares predictions to each other and truth
# Input: processed_predictions.RData
# Output: comparing_predictions_truth_alloutcomes_histograms_only_cases_wtruth.pdf
# By Matt Salganik (minor edits by Alex Kindel)
# Runtime: Less than 1 minute on a laptop

library(tidyverse)
library(forcats)
library(magrittr)
library(here)
library(broom)
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

# Create data objects
summary_results <- data.frame(outcomes = c("materialHardship", "gpa", "grit", "eviction", "layoff", "jobTraining"),
                              metric = "ratio_mse",
                              cases = "only_cases_wtruth")
histograms_list <- list()

# Filter out non-truth cases
predictions %<>% filter(!is.na(truth))

# For each outcome...
for (outcome_var in summary_results$outcomes) {
  print(paste("outcome:", outcome_var))
  
  # Restrict to only valid predictions for this outcome
  pred_this_outcome <- predictions %>% 
    filter(outcome == outcome_var) %>%
    filter(beatingBaseline == TRUE) %>%
    select(account, challengeID, prediction) %>%
    mutate(account = as.character(account))

  # Also, store true values column using any account (e.g. ADSgrp5; any account is fine)
  true_values <- predictions %>% 
    filter(outcome == outcome_var) %>%
    filter(account == "ADSgrp5") %>%
    select(account, challengeID, truth) %>%
    rename(prediction = truth)
  true_values$account <- "truth"
  
  # Store true values with predictions for this outcome
  pred_this_outcome <- bind_rows(pred_this_outcome, true_values)
  
  # Generate rank order of accounts by MSE
  predictions %>%
    filter(outcome == outcome_var) %>%
    filter(beatingBaseline == TRUE) %>%
    select(account, mse.account.outcome.rank.random) %>%
    group_by(account) %>%
    summarize(mse.account.outcome.rank.random = unique(mse.account.outcome.rank.random)) %>%
    arrange(mse.account.outcome.rank.random) ->
    account_order
  
  # Create matrix of prediction vectors for each account, including truth
  row_to_add <- tibble(account = "truth", 
                       mse.account.outcome.rank.random = 0)
  account_order$account <- as.character(account_order$account) # convert to character for bind_rows
  account_order <- bind_rows(row_to_add, account_order)
  account_order$account <- fct_reorder(account_order$account, 
                                       account_order$mse.account.outcome.rank.random)
  
  pred_this_outcome_reshaped <- spread(pred_this_outcome,
                                       key = challengeID,
                                       value = prediction)
  accounts <- pred_this_outcome_reshaped %>%
    pull(account) %>%
    as.character()
  pred_this_outcome_reshaped <- pred_this_outcome_reshaped %>%
    select(-account)
  predictions_matrix <- data.matrix(pred_this_outcome_reshaped) 
  rownames(predictions_matrix) <- accounts
  rm(accounts, pred_this_outcome_reshaped)
  
  # Get all possible 2-combinations of teams
  # We'll use this to compare their predictions
  compare_preds <- t(combn(as.character(account_order$account), 2))
  
  # Convert team pairs to tibble
  # Also, use the account order ranking to order the pairs
  colnames(compare_preds) <- c("account_1", "account_2")
  compare_preds <- as_tibble(compare_preds)
  compare_preds$account_1 <- factor(compare_preds$account_1, levels = account_order$account)
  compare_preds$account_2 <- factor(compare_preds$account_2, levels = account_order$account)
  compare_preds$outcome <- outcome_var
  
  # Compute mean squared error between each pairs' prediction sets
  compare_preds$mse <- apply(compare_preds, 1, function(x){
    predictions_1 <- predictions_matrix[x[1], ]
    predictions_2 <- predictions_matrix[x[2], ]
    mean((predictions_1 - predictions_2)^2, na.rm = TRUE)
  })
  
  # # Compute mean absolute error between two sets of predictions
  # TODO: NOT USED
  # comp$mae <- apply(comp, 1, function(x){
  #   predictions_1 <- predictions_matrix[x[1], ]
  #   predictions_2 <- predictions_matrix[x[2], ]
  #   mean(abs(predictions_1 - predictions_2), na.rm = TRUE)
  # })
  
  # Compute RMSE; add indicator for rows with truth comparison
  compare_preds %<>% mutate(rmse = sqrt(mse),
                            include_truth = ((account_1 == "truth") | (account_2 == "truth")))
  
  # Compute MSE between two predictions
  mse_predictions <- compare_preds %>% 
    filter(include_truth == FALSE) %>% 
    pull(mse) %>% 
    summary() %>%
    tidy()
  
  # MSE between prediction and truth
  mse_prediction_truth <- compare_preds %>% 
    filter(include_truth == TRUE) %>% 
    pull(mse) %>% 
    summary() %>%
    tidy()

  # Check that prediction distance distributions compare as expected
  if (mse_prediction_truth["minimum"] > mse_predictions["maximum"]) {
    print("The smallest distance (MSE) between a prediction and the truth is greater than the biggest distance (MSE) between predictions.")
  } else {
    warning("UNEXPECTED: The smallest distance (MSE) between a prediction and the truth is NOT greater than the biggest distance (MSE) between predictions.",
            immediate. = TRUE)
  }
  
  # Build plot facet labels
  label_names <- c(
    `TRUE` = "Comparing\nprediction & truth",
    `FALSE` = "Comparing\nprediction & predicton"
  )
  
  # Store histogram of prediction comparisons for this outcome
  histograms_list[[outcome_var]] <- ggplot(compare_preds, aes(x = mse)) + 
    geom_histogram(bins = 30) + # 30 is arbitrary 
    facet_wrap(~ include_truth, 
               labeller = as_labeller(label_names),
               scales = "free_y") +
    labs(title = clean_outcome_label(outcome_var), 
         x = "Mean squared error", 
         y = "Count") + 
    theme(strip.text.x = element_text(size = 9),
          axis.text.x = element_text(angle = 45, hjust = 1))

}

# Arrange and write out figure
figure <- grid.arrange(grobs = histograms_list, nrow = 3)
ggsave(plot = figure,
       filename = "s19_comparing_predictions_truth_alloutcomes_histograms_only_cases_wtruth.pdf",
       path = results.dir, 
       device = "pdf", 
       dpi = 300,
       width = 8.5, height = 6, units = "in")
