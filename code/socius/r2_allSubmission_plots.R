# Empirical CDF and R^2_Train vs. R^2_Holdout scatter
# plots for introduction to the Socius Special Collection
# about the Fragile Families Challenge
# Code by Ian Lundberg (ilundberg at princeton dot edu)
# Modifications by Alex Kindel (akindel at princeton dot edu)

library(tidyverse)
library(magrittr)
library(reshape2)
library(foreach)
library(scales)
library(here)

# Set ggplot2 theme
theme_set(theme_bw())

# Set the data directory
data.dir <- file.path(here(), "data")
private.data.dir <- file.path(data.dir, "private")

# Set the results location
results.dir <- file.path(here(), "results", "socius")

# Load the outcomes data
test <- cbind(read_csv(file.path(private.data.dir, "test.csv")), set="test")
train <- cbind(read_csv(file.path(private.data.dir, "train.csv")), set="train")
leaderboard_filled <- cbind(read_csv(file.path(private.data.dir, "leaderboard.csv")), set="leaderboard_filled")
leaderboard_unfilled <- cbind(read_csv(file.path(private.data.dir, "leaderboardUnfilled.csv")), set="leaderboard_unfilled")

# Get the outcome variable names
outcomes <- colnames(test)[-c(1, 8)]

# Get challenge IDs for each set
test_cid <- data.frame(challengeID = test$challengeID, set = "test")
train_cid <- data.frame(challengeID = train$challengeID, set = "train")
ldb_cid <- which(!(1:4242 %in% c(test_cid$challengeID, train_cid$challengeID)))

# Build outcome data frame
all_outcomes <- rbind(test, train,
                      leaderboard_filled %>% filter(challengeID %in% ldb_cid),
                      leaderboard_unfilled %>% filter(challengeID %in% ldb_cid)) %>%
  melt(id.vars=c("challengeID", "set"), variable.name="outcome", value.name="truth")
sets <- c("train","leaderboard_unfilled","leaderboard_filled","test")

# Read the file names of the final submissions on the leaderboard.
leaderboard.submissions <- list.files(file.path(data.dir, "leaderboard_nodupes"))

# Read submissions
print("About to read files")
submissions <- foreach(name = leaderboard.submissions, .combine = "rbind") %do% {
  submission <- read.csv(unz(file.path(data.dir, "leaderboard_nodupes", name),
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

######################
# Empirical CDF plot #
######################

# Get baseline MSE
baseline_mse <- all_outcomes %>%
  left_join(submissions %>% 
              filter(account == "baseline"), 
            by = c("challengeID","outcome")) %>%
  group_by(set, outcome) %>%
  summarize(baseline = mean((truth - prediction) ^ 2, na.rm=T))

# Compute R2 over baseline (naive mean) submission
submissions_with_truth <- all_outcomes %>%
  left_join(submissions, by = c("challengeID","outcome")) %>%
  filter(account != "baseline") %>%
  group_by(outcome, account, set) %>%
  summarize(mse = mean((truth - prediction) ^ 2, na.rm=T)) %>%
  left_join(baseline_mse, by = c("outcome","set")) %>%
  mutate(r2 = 1 - mse / baseline) %>%
  # Restrict to those with R^2_Holdout > 0.0001
  group_by(account, outcome) %>%
  mutate(r2_holdout = mean(case_when(set == "test" ~ r2), na.rm = T)) %>%
  filter(r2_holdout > 10^-4) %>%
  select(-r2_holdout) %>%
  group_by()

# Empirical CDF plot for each outcome
sets <- c("train","leaderboard_unfilled","leaderboard_filled","test")
for(outcome_case in outcomes) {
  for(set_case in sets) {
    forPlot <- submissions_with_truth %>%
      filter(outcome == outcome_case & set == set_case) %>%
      group_by(set) %>%
      arrange(r2) %>%
      mutate(prop_submissions = (1:n()) / n(),
             count_submissions = 1:n()) %>%
      filter(r2 >= 0)
    forPlot %>%
      ggplot(aes(x = r2, y = prop_submissions)) +
      geom_step() +
      scale_y_continuous(name = element_blank(),
                         sec.axis = sec_axis(~(.)*max(forPlot$count_submissions))) +
      scale_x_continuous(name = element_blank(),
                         sec.axis = sec_axis(~(.),
                                             labels = function(x) round((1 - x)*forPlot$baseline[1], digits = 3))) +
      #sec.axis = sec_axis(~(1 - .)*forPlot$baseline[1])) +
      theme_bw() +
      theme(axis.text.x.bottom = element_text(angle = 45, hjust = 1),
            axis.text.x.top = element_text(angle = 45, hjust = 0)) +
      ggsave(file.path(results.dir,paste0("6_plot_",outcome_case,"_",set_case,".pdf")),
             height = 2, width = 2)
  }
}

##############################################
# Scatterplot of training vs. holdout scores #
##############################################

submissions_with_truth %>%
  filter(set %in% c("train","test")) %>%
  select(outcome, account, set, r2) %>%
  spread(key = set, value = r2) %>%
  group_by(outcome) %>%
  mutate(r2_correlation = cor(train, test),
         label_correlation = case_when((1:n()) == 1 ~ paste0("Correlation = ", format(round(r2_correlation, digits = 2), nsmall = 2)))) %>%
  group_by() %>%
  mutate(outcome = case_when(outcome == "materialHardship" ~ "A. Material hardship",
                             outcome == "gpa" ~ "B. GPA",
                             outcome == "grit" ~ "C. Grit",
                             outcome == "eviction" ~ "D. Eviction",
                             outcome == "jobTraining" ~ "E. Job training",
                             outcome == "layoff" ~ "F. Layoff")) %>%
  ggplot(aes(x = train, y = test)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  geom_point(size = .5, alpha = .5) +
  geom_label(aes(x = .35, y = .9, label = label_correlation),
             size = 2) +
  facet_wrap(~outcome) +
  scale_x_continuous(limits = c(0,1)) + #, 
                     #label = function(x) ifelse(x == 0 ~ "0",format(x, nsmall = 2, digits = 2))) + 
  ylim(c(0,1)) +
  coord_fixed() +
  xlab(expression({R^2}[Train])) +
  ylab(expression({R^2}[Holdout])) +
  theme_bw() + 
  theme(panel.spacing = unit(1, "lines")) +
  ggsave(file.path(results.dir, "7_r2_train_holdout.pdf"),
         height = 4, width = 6.5)
  
  