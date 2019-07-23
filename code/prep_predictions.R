# Data prep for analysis of predictions 
# Input: submissions.csv
# Output: processed_predictions.RData (working dataframes)
# Runtime: a few seconds on a laptop
# By Matt Salganik (with minor edits by Alex Kindel)

library(tidyverse)
library(magrittr)
library(here)

# Set directory information
data.dir <- file.path(here(), "data")
private.data.dir <- file.path(data.dir, "private")
working.data.dir <- file.path(data.dir, "intermediate_files")
results.dir <- file.path(here(), "results")

# Set ggplot2 theme
theme_set(theme_bw())

# Load data
predictions <- read_csv(file = file.path(data.dir, "submissions.csv"))

# calculate error for each prediction
predictions %<>% 
  mutate(err = prediction - truth,
         abs.err = abs(prediction - truth),
         sq.err = (prediction - truth)^2)

# Compute MSE by account per outcome; compute ranks, deciles
predictions %>%
  group_by(account, outcome) %>%
  summarize(mse.account.outcome = mean(sq.err, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(outcome) %>%
  mutate(mse.account.outcome.rank.random = rank(mse.account.outcome, ties.method = "random"),
         mse.account.outcome.rank.average = rank(mse.account.outcome, ties.method = "average"),
         mse.account.outcome.decile = ntile(mse.account.outcome, 10)) %>%
  ungroup() ->
  account.outcome

# Join to full prediction set; compute risk deciles
predictions %<>%
  full_join(account.outcome, by = c("account", "outcome")) %>%
  group_by(account, outcome) %>%
  mutate(risk.decile = ntile(prediction, 10)) %>%
  ungroup()

# Compute summary statistics by unit
# Restrict to predictions over baseline
predictions %>%
  filter(beatingBaseline == TRUE) %>%
  group_by(challengeID, outcome) %>%
  summarize(mse_unit_outcome = mean(sq.err, na.rm = TRUE), 
            median_sq_err_unit_outcome = median(sq.err),
            mean_abs_err_unit_outcome = mean(abs.err),
            median_abs_err_unit_outcome = median(abs.err)) %>%
  ungroup() %>%
  group_by(outcome) %>%
  mutate(mse_unit_outcome_rank_random = rank(mse_unit_outcome, ties.method = "random"),
         mse_unit_outcome_rank_average = rank(mse_unit_outcome, ties.method = "average"),
         mse_unit_outcome_decile = ntile(mse_unit_outcome, 10)) %>%
  ungroup() ->
  unit_outcome

# Join unit summary statistics to full prediction set
predictions %<>% full_join(unit_outcome, by = c("challengeID", "outcome"))

# Create indicator for missingness on any outcome
predictions %>%
  group_by(challengeID, outcome) %>%
  summarize(outcome.truth = unique(truth)) %>%
  summarize(missing.any.outcome = any(is.na(outcome.truth))) ->
  missingness

# Join missingness indicators with full prediction set
predictions %<>% full_join(missingness, by = "challengeID")

# Save working dataframes to disk
save(predictions, unit_outcome,
     file = file.path(working.data.dir, "processed_predictions.RData"))
