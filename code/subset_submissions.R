#####################################
## Data subsetting for:            ##
## Measuring the Predictability of ##
## Life Outcomes with a Scientific ##
## Mass Collaboration              ##
#####################################

## Code by Ian Lundberg
## ilundberg at princeton dot edu

# Load packages
library(tidyverse)
library(reshape2)
library(foreach)
library(readr)
library(here)

# Set the data directory
data.dir <- file.path(here(), "data")
private.data.dir <- file.path(data.dir, "private")

# Set the results location
results.dir <- file.path(here(), "results")

# Load the outcomes data
test <- read_csv(file.path(private.data.dir, "test.csv"))

# Get the outcome variable names
outcomes <- colnames(test)[-1]

# Read the file names of the final submissions on the leaderboard
unzip(file.path(data.dir, "leaderboard_nodupes.zip"), exdir=data.dir)
unlink(file.path(data.dir, "__MACOSX"), recursive = T)  # Remove junk files
leaderboard.submissions <- list.files(file.path(data.dir, "leaderboard_nodupes/"))

# Remove the extra metadata files that are not submission.zip files
print("About to remove metadata")
for (s in leaderboard.submissions[!grepl("submission.zip",leaderboard.submissions)]) {
  print(s)
  # file.remove(paste0("leaderboard_nodupes/",s))
}
rm(leaderboard.submissions)
print("Removed metadata successfully")

# Identify file names
file.names <- list.files(file.path(data.dir, "leaderboard_nodupes/"))
if (!all(grepl("submission.zip",file.names))) {
  stop("ERROR: Did not get rid of all irrelevant files.")
}

# Read submissions
print("About to read files")
submissions <- foreach(name = file.names, .combine = "rbind") %do% {
  submission <- read.csv(unz(file.path(data.dir, "leaderboard_nodupes/", name),
                             filename = "prediction.csv")) %>%
    arrange(challengeID) %>%
    mutate(eviction = case_when(eviction == "True" ~ 1,
                                eviction == "False" ~ 0,
                                T ~ as.numeric(eviction)),
           layoff = case_when(layoff == "True" ~ 1,
                              layoff == "False" ~ 0,
                              T ~ as.numeric(layoff)),
           jobTraining = case_when(jobTraining == "True" ~ 1,
                                   jobTraining == "False" ~ 0,
                                   T ~ as.numeric(jobTraining))) %>%
    melt(id = "challengeID", variable.name = "outcome", value.name = "prediction") %>%
    mutate(account = gsub(" .*","",name))
  return(submission)
}
print("Read files successfully")

# Indicate whether they are beating the baseline
submissions_with_score_indicator <- test %>%
  melt(id = "challengeID", variable.name = "outcome", value.name = "truth") %>%
  left_join(submissions %>%
              filter(account != "baseline"),
            by = c("challengeID","outcome")) %>%
  left_join(submissions %>%
              filter(account == "baseline") %>%
              rename(ybar_train = prediction) %>%
              select(challengeID, outcome, ybar_train),
            by = c("challengeID","outcome")) %>%
  group_by(outcome, account) %>%
  mutate(r2_holdout = 1 - mean((truth - prediction) ^ 2, na.rm = T) / mean((truth - ybar_train) ^ 2, na.rm = T),
         beatingBaseline = r2_holdout > 10^-4) %>%  # Set threshold a little over 0 to avoid numerical precision issues
  mutate(outcome_name = case_when(outcome == "materialHardship" ~ "A. Material\nhardship",
                                  outcome == "gpa" ~ "B. GPA",
                                  outcome == "grit" ~ "C. Grit",
                                  outcome == "eviction" ~ "D. Eviction",
                                  outcome == "jobTraining" ~ "E. Job\ntraining",
                                  outcome == "layoff" ~ "F. Layoff")) %>%
  select(outcome, outcome_name, account, challengeID, prediction, truth, ybar_train, r2_holdout, beatingBaseline) %>%
  arrange(outcome_name, account, challengeID)

print("Writing files")
write_csv(submissions_with_score_indicator,
          path = file.path(data.dir, "submissions.csv"))

# Get numbers for supplement
write_csv(
  submissions_with_score_indicator %>%
    group_by(outcome, account) %>%
    mutate(different_from_baseline = !(all(prediction == ybar_train))) %>%
    filter((1:n()) == 1) %>%
    group_by(outcome_name) %>%
    summarize(num_valid = n(),
              num_differentFromBaseline = sum(different_from_baseline),
              num_beatingBaseline = sum(beatingBaseline)),
  path = file.path(results.dir, "restrictions.csv")
)
