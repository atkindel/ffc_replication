# Code to identify number of non-missing cases in each set
# for the introduction of the Socius Special Collection
# about the Fragile Families Challenge
# Code by Ian Lundberg (ilundberg at princeton dot edu)

library(tidyverse)
library(foreach)
library(magrittr)
library(here)

# Set ggplot2 theme
theme_set(theme_bw())

# Set the data directory
data.dir <- file.path(here(), "data")
private.data.dir <- file.path(data.dir, "private")

# Set the results location
results.dir <- file.path(here(), "results")

# Load data
train <- read_csv(file.path(private.data.dir, "train.csv"))
holdout <- read_csv(file.path(private.data.dir, "test.csv"))
leaderboard <- read_csv(file.path(private.data.dir, "leaderboardUnfilled.csv")) %>%
  # The leaderboard has rows for all 4,242 to check that submissions have 4,242,
  # but the leaderboard is NA for the other sets. Remove them here.
  filter(!(challengeID %in% c(train$challengeID,holdout$challengeID)))

# Identify outcome names
outcomes <- colnames(train)[-1]

# Produce the result
num_nonmissing_cases <- foreach(outcome = outcomes, .combine = "rbind") %do% {
  data.frame(Outcome = outcome,
             Training = sum(!is.na(train[,outcome])),
             Leaderboard = sum(!is.na(leaderboard[,outcome])),
             Holdout = sum(!is.na(holdout[,outcome])))
} %>%
  bind_rows(data.frame(Outcome = "Total possible",
                       Training = nrow(train),
                       Leaderboard = nrow(leaderboard),
                       Holdout = nrow(holdout)))

# Save the result
write_csv(num_nonmissing_cases, path = file.path(results.dir, "num_nonmissing_cases.csv"))
