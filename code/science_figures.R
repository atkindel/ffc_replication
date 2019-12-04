#####################################
## Plots for:                      ##
## Measuring the Predictability of ##
## Life Outcomes with a Scientific ##
## Mass Collaboration              ##
#####################################

## Code by Ian Lundberg
## ilundberg at princeton dot edu

############################
## Load required packages ##
############################

library(tidyverse)
library(magrittr)
library(haven)
library(forcats)
library(reshape2)
library(foreach)
library(readstata13)
library(Amelia)
library(ranger)
library(quadprog)
library(readr)
library(here)

# Set directory information
data.dir <- file.path(here(), "data")
private.data.dir <- file.path(data.dir, "private")
results.dir <- file.path(here(), "results")

# Set ggplot2 theme
theme_set(theme_bw())

###############
## Load data ##
###############

background <- read.dta13(file.path(private.data.dir, "background.dta"), convert.factors = F)
train <- read_csv(file.path(private.data.dir, "train.csv"))
test <- read_csv(file.path(private.data.dir, "test.csv"))
outcomes <- colnames(train)[-1]
submissions <- read_csv(file.path(data.dir, "submissions.csv"))

#############################################
# Plot training distribution for supplement #
#############################################

train %>%
  melt(id = "challengeID",
       variable.name = "outcome",
       value.name = "truth") %>%
  mutate(outcome = fct_recode(outcome,
                              GPA = "gpa",
                              Grit = "grit",
                              "Material hardship" = "materialHardship",
                              "Eviction" = "eviction",
                              "Layoff" = "layoff",
                              "Job training" = "jobTraining")) %>%
  mutate(outcome = fct_relevel(outcome, 
                               "Material hardship","GPA","Grit",
                               "Eviction", "Job training","Layoff")) %>%
  filter(!is.na(truth)) %>%
  group_by(outcome, truth) %>%
  summarize(num = n()) %>%
  group_by(outcome) %>%
  mutate(prop = num / sum(num),
         biggest = max(num),
         label_size = as.numeric(case_when(outcome %in% c("Eviction","Job training","Layoff") ~ 2,
                                           T ~ 1))) %>%
  mutate(truth = factor(truth)) %>%
  ggplot(aes(x = truth, y = prop, label = prettyNum(num, big.mark = ","),
             width = case_when(outcome %in% c("Eviction","Layoff","Job training") ~ 1,
                               outcome == "Material hardship" ~ 1 / 11,
                               outcome %in% c("GPA","Grit") ~ 1 / 4))) +
  geom_point(color = "seagreen4", size = 1.5) +
  geom_segment(aes(xend = truth, y = 0, yend = prop), color = "seagreen4") +
  geom_text(aes(y = prop + .05),
            size = 2.5,
            fontface = "bold", color = "seagreen4", hjust = 0,
            show.legend = F) +
  facet_wrap(~outcome, scales = "free_y") +
  scale_size_continuous(range = c(1.5,4)) +
  scale_x_discrete(labels = function(x) format(round(as.numeric(x),2), nsmall = 2)) +
  scale_y_continuous(limits = c(0,1.25), breaks = seq(0,1,.25)) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  coord_flip() +
  xlab("Training outcome") +
  ylab("Proportion of training sample\n(annotations provide count)")+
  ggsave(file.path(results.dir, "figures", "s3_training_distribution.pdf"),
         height = 4.5, width = 6.5)

###############################
## Benchmarks for supplement ##
###############################

d <- background %>%
  mutate(cm1relf = ifelse(cm1relf == 1, "Married",
                          ifelse(cm1relf == 2, "Cohabiting",
                                 ifelse(cm1relf >= 3, "Other",NA))),
         cm1ethrace = ifelse(cm1ethrace %in% c(1,4), "White/other",
                             ifelse(cm1ethrace == 2, "Black",
                                    ifelse(cm1ethrace == 3, "Hispanic", NA))),
         cm1edu = factor(ifelse(cm1edu >= 1, cm1edu, NA),
                         labels = c("Less than high school",
                                    "High school",
                                    "Some college",
                                    "College")),
         ## For prior measure of GPA, use the teacher report of skills
         ## in language and literacy,
         ## in science and social studies,
         ## and in math,
         ## all coded 1 = far below average to 5 = far above average
         gpa9 = 1/3 * (ifelse(t5c13a > 0, t5c13a, NA) +
                         ifelse(t5c13b > 0, t5c13b, NA) +
                         ifelse(t5c13c > 0, t5c13c, NA)),
         ## For grit, use teacher reports of:
         ## Child persists in completing tasks
         ## Child fails to finish things he or she starts (reverse coded)
         ## Child does not follow through on instructions and fails to finish homework
         ## EXCLUDE Child pays attention well
         ## EXCLUDE Child ignores peer distractions when doing class work
         ## EXCLUDE Child has a short attention span
         ## EXCLUDE Child has distractibility or attention span problem
         grit9 = 1/3 * (ifelse(t5b2b > 0, t5b2b, NA) +
                          ifelse(t5b4y >= 0, 4 - t5b4y, NA) +
                          ifelse(t5b4z >= 0, 4 - t5b4z, NA)),
         materialHardship9 = ifelse(
           m5a2 %in% c(1,2),
           ## Mother's material hardship
           1 / 10 * (
             ifelse(m5f23a > 0, m5f23a == 1, NA) +
               ifelse(m5f23b > 0, m5f23b == 1, NA) +
               ifelse(m5f23c > 0, m5f23c == 1, NA) +
               ifelse(m5f23d > 0, m5f23d == 1, NA) +
               ifelse(m5f23e > 0, m5f23e == 1, NA) +
               ifelse(m5f23f > 0, m5f23f == 1, NA) +
               ifelse(m5f23g > 0, m5f23g == 1, NA) +
               ifelse(m5f23h > 0, m5f23h == 1, NA) +
               ifelse(m5f23i > 0, m5f23i == 1, NA) +
               ifelse(m5f23j > 0, m5f23j == 1, NA)
           ),
           ifelse(f5a2 %in% c(1,2),
                  ## Father's material hardship
                  1 / 10 * (
                    ifelse(f5f23a > 0, f5f23a == 1, NA) +
                      ifelse(f5f23b > 0, f5f23b == 1, NA) +
                      ifelse(f5f23c > 0, f5f23c == 1, NA) +
                      ifelse(f5f23d > 0, f5f23d == 1, NA) +
                      ifelse(f5f23e > 0, f5f23e == 1, NA) +
                      ifelse(f5f23f > 0, f5f23f == 1, NA) +
                      ifelse(f5f23g > 0, f5f23g == 1, NA) +
                      ifelse(f5f23h > 0, f5f23h == 1, NA) +
                      ifelse(f5f23i > 0, f5f23i == 1, NA) +
                      ifelse(f5f23j > 0, f5f23j == 1, NA)
                  ),
                  ## PCG material hardship
                  1 / 10 * (
                    ifelse(n5g1a > 0, n5g1a == 1, NA) +
                      ifelse(n5g1b > 0, n5g1b == 1, NA) +
                      ifelse(n5g1c > 0, n5g1c == 1, NA) +
                      ifelse(n5g1d > 0, n5g1d == 1, NA) +
                      ifelse(n5g1e > 0, n5g1e == 1, NA) +
                      ifelse(n5g1f > 0, n5g1f == 1, NA) +
                      ifelse(n5g1g > 0, n5g1g == 1, NA) +
                      ifelse(n5g1h > 0, n5g1h == 1, NA) +
                      ifelse(n5g1i > 0, n5g1i == 1, NA) +
                      ifelse(n5g1j > 0, n5g1j == 1, NA)
                  ))
         ),
         eviction9 = ifelse(m5a2 %in% c(1,2),
                            ifelse(m5f23d <= 0, NA, m5f23d == 1),
                            ifelse(f5a2 %in% c(1,2),
                                   ifelse(f5f23d <= 0, NA, f5f23d == 1),
                                   NA)),
         ## Use whether did work for pay the week of the age 9 interview
         layoff9 = ifelse(m5a2 %in% c(1,2),
                          ifelse(m5i4 > 0, m5i4 == 2, NA),
                          ifelse(f5a2 %in% c(1,2),
                                 ifelse(f5i4 > 0, f5i4 == 2, NA),
                                 NA)),
         jobTraining9 = ifelse(m5a2 %in% c(1,2),
                               ifelse(m5i3b > 0, m5i3b == 1, NA),
                               ifelse(f5a2 %in% c(1,2),
                                      ifelse(f5i3b > 0, f5i3b == 1, NA),
                                      NA))) %>%
  select(challengeID, cm1ethrace, cm1relf, cm1edu,
         gpa9, grit9, materialHardship9, eviction9, layoff9, jobTraining9) %>%
  left_join(train, by = "challengeID")

## For one row that is missing everything, fill in that race is white/other
## so we can impute everything from there. This case is likely missing
## in the test set and has no training outcomes, so this won't matter.

d[apply(d[,-1],1,function(x) all(is.na(x))),"cm1ethrace"] <- "White/other"

####################
## Fit benchmarks ##
####################

## Function to make OLS predictions
get.benchmark.predictions <- function(outcome, model = "full", data = d) {
  if(model == "full") {
    thisFormula <- formula(paste0(outcome,
                                  " ~ cm1ethrace + cm1relf + cm1edu + ",
                                  outcome,"9"))
    imputed <- amelia(data %>% select(challengeID, cm1ethrace, cm1relf, cm1edu, contains(outcome)),
                      m = 1,
                      noms = c("cm1ethrace","cm1relf"),
                      ords = "cm1edu",
                      idvars = "challengeID")$imputations$imp1
  } else if (model == "lagged") {
    thisFormula <- formula(paste0(outcome,
                                  " ~ ",
                                  outcome,"9"))
    imputed <- amelia(data %>% select(challengeID, contains(outcome)),
                      m = 1,
                      idvars = "challengeID")$imputations$imp1
  } else if (model == "demographic") {
    imputed <- amelia(data %>% 
                        select(challengeID, cm1ethrace, cm1relf, cm1edu, contains(outcome)) %>%
                        select(-contains(paste0(outcome,9))),
                      m = 1,
                      noms = c("cm1ethrace","cm1relf"),
                      ords = "cm1edu",
                      idvars = "challengeID")$imputations$imp1
    thisFormula <- formula(paste0(outcome,
                                  " ~ cm1ethrace + cm1relf + cm1edu"))
  }
  
  # Identify the rows that are missing all predictors
  # This happens if there were no variables with valid values,
  # giving Amelia no data with which to impute.
  missing_all_predictors <- apply(get_all_vars(thisFormula, data = imputed), 1, function(x) all(is.na(x[-1])))
  # Create holders for the predicted values from the models
  ols.yhat <- logit.yhat <- rf.yhat <- rep(NA, nrow(imputed))
  
  # If missing all predictors, impute the grand mean
  ols.yhat[missing_all_predictors] <- 
    logit.yhat[missing_all_predictors] <- 
    rf.yhat[missing_all_predictors] <- 
    mean(imputed[,outcome], na.rm = T)
  
  # Fit models to impute predictions when predictors are available
  
  # OLS
  ols <- lm(formula = thisFormula,
            data = imputed[!is.na(data[,outcome]),])
  ols.yhat[!missing_all_predictors] <- predict(ols, newdata = imputed[!missing_all_predictors,])
  
  # Logit for binary outcomes
  if (length(unique(na.omit(data[,outcome]))) == 2) {
    logit <- glm(formula = thisFormula,
                 family = binomial(link = "logit"),
                 data = imputed[!is.na(data[,outcome]),])
    logit.yhat[!missing_all_predictors] <- predict(ols, newdata = imputed[!missing_all_predictors,], type = "response")
  } else {
    # If not binary, make all logit predictions NA
    logit.yhat <- NA
  }
  
  # Random forest
  rf <- ranger(thisFormula,
               data = imputed[!is.na(data[,outcome]),])
  rf.yhat[!missing_all_predictors] <- predict(rf, data = imputed[!missing_all_predictors,])$predictions
  
  # Combine into one data frame
  # and truncate to observable range
  all_predictions <- data.frame(outcome = outcome,
                                challengeID = imputed$challengeID,
                                ols = ols.yhat,
                                logit = logit.yhat,
                                rf = rf.yhat) %>%
    mutate(ols = case_when(outcome %in% c("grit","gpa") & ols < 1 ~ 1,
                           outcome %in% c("grit","gpa") & ols > 4 ~ 4,
                           outcome %in% c("grit","gpa") ~ ols,
                           ols < 0 ~ 0,
                           ols > 1 ~ 1,
                           T ~ ols),
           logit = case_when(logit < 0 ~ 0,
                             logit > 1 ~ 1,
                             T ~ as.numeric(logit)),
           rf = case_when(outcome %in% c("grit","gpa") & rf < 1 ~ 1,
                          outcome %in% c("grit","gpa") & rf > 4 ~ 4,
                          outcome %in% c("grit","gpa") ~ rf,
                          rf < 0 ~ 0,
                          rf > 1 ~ 1,
                          T ~ rf))
  return(all_predictions)
}

# Get benchmarks on all outcomes
set.seed(08544)
benchmarks <- foreach(thisOutcome = outcomes, .combine = "rbind") %do% {
  foreach(predictor_set = c("full","demographic","lagged"), .combine = "rbind") %do% {
    get.benchmark.predictions(thisOutcome, model = predictor_set) %>%
      mutate(predictors = predictor_set)
  }
}

# Output a version stored like submissions.csv
# Matt will use this in his figures
benchmarks_long <- benchmarks %>%
  select(challengeID, outcome, ols, logit, rf, predictors) %>%
  melt(id = c("challengeID", "outcome", "predictors"),
       variable.name = "account",
       value.name = "prediction") %>%
  mutate(account = paste("benchmark", account, predictors, sep = "_")) %>%
  select(-predictors) %>%
  # Add information so that this is formatted like submissions.csv
  right_join(
    test %>%
      melt(id = "challengeID", variable.name = "outcome", value.name = "truth") %>%
      select(challengeID, outcome, truth),
    by = c("challengeID","outcome")
  ) %>%
  left_join(
    train %>%
      melt(id = "challengeID", variable.name = "outcome") %>%
      group_by(outcome) %>%
      summarize(ybar_train = mean(value, na.rm = T)),
    by = c("outcome")
  ) %>%
  group_by(outcome, account) %>%
  mutate(r2_holdout = 1 - mean((truth - prediction) ^ 2, na.rm = T) / mean((truth - ybar_train) ^ 2, na.rm = T),
         beatingBaseline = r2_holdout > 0,
         outcome_name = case_when(outcome == "materialHardship" ~ "A. Material\nhardship",
                                  outcome == "gpa" ~ "B. GPA",
                                  outcome == "grit" ~ "C. Grit",
                                  outcome == "eviction" ~ "D. Eviction",
                                  outcome == "jobTraining" ~ "E. Job\ntraining",
                                  outcome == "layoff" ~ "F. Layoff")) %>%
  select(outcome, outcome_name, account, challengeID, prediction, truth, ybar_train, r2_holdout, beatingBaseline) %>%
  arrange(outcome_name, account, challengeID)

write_csv(
  benchmarks_long,
  path = file.path(data.dir, "intermediate_files", "benchmarks_long.csv")
)

####################################################
## Get a confidence interval for the maximum R^2  ##
## all of the benchmarks, and comparisons between ## 
## the maximum R^2 and the primary benchmark      ##
####################################################

estimates_with_intervals <- foreach(outcome_case = outcomes, .combine = "rbind") %do% {
  squared_errors <- submissions %>% 
    bind_rows(benchmarks_long) %>%
    bind_rows(submissions %>%
                group_by(outcome, challengeID) %>%
                filter((1:n()) == 1) %>%
                group_by(outcome) %>%
                mutate(prediction = ybar_train,
                       account = "baseline",
                       r2_holdout = 0,
                       beatingBaseline = F)) %>%
    filter(outcome == outcome_case) %>%
    # Remove cases where all predictions are within .001 of the baseline
    group_by(account) %>%
    mutate(predicts_baseline = all(abs(prediction - ybar_train) < .001)) %>%
    filter(!predicts_baseline | account == "baseline") %>%
    filter(!is.na(truth)) %>%
    mutate(sq_error = (truth - prediction) ^ 2) %>%
    select(challengeID, account, sq_error) %>%
    spread(key = account, value = sq_error) %>%
    select(-challengeID)
  
  # Move baseline to the front and benchmarks to the back
  squared_errors <- squared_errors[,c("baseline",
                                      colnames(squared_errors[!(grepl("baseline|benchmark",colnames(squared_errors)))]),
                                      colnames(squared_errors[(grepl("benchmark",colnames(squared_errors)))]))]
  
  # Calculate point estimates
  r2_point <- 1 - colMeans(squared_errors) / colMeans(squared_errors[,"baseline"])
  
  # Calculate point estimates on 10000 bootstrap samples
  r2_boot <- foreach(i = 1:10000, .combine = "rbind") %do% {
    chosen <- sample(1:nrow(squared_errors), replace = T)
    1 - colMeans(squared_errors[chosen,]) / colMeans(squared_errors[chosen,"baseline"])
  }
  
  # Identify the columns that are submissions or baseline
  notBenchmark_indicators <- !grepl("benchmark",colnames(squared_errors))
  # Identify the columns that are submissions
  submission_indicators <- !grepl("baseline|benchmark",colnames(squared_errors))
  
  # Identify submission columns for which all of the squared errors are the same
  # as a previously seen submission column or the baseline
  same <- rep(NA, ncol(squared_errors))
  # For all columns that are not submissions, treat as not duplicates
  same[!submission_indicators] <- F
  # Loop through columns that are submissions and indicate duplicates
  for(i in which(submission_indicators)) {
    this_column <- squared_errors[,i]
    previous_columns <- squared_errors[,1:(i-1)]
    if (i == 2) {
      same[i] <- all(abs(this_column - previous_columns) < .001)
    } else {
      same[i] <- any(apply(previous_columns, 2, function(x) all(abs(this_column - x) < .001)))
    }
  }
  
  # Calculate the maximum R^2
  r2_point_max <- max(r2_point[submission_indicators & !same])
  # Calculate point estimates for comparisons to benchmarks
  if (outcome_case %in% c("gpa","grit","materialHardship")) {
    benchmark <- "benchmark_ols_full"
  } else {
    benchmark <- "benchmark_logit_full"
  }
  point_difference <- r2_point_max - r2_point[benchmark]
  point_performanceMultiplying <- r2_point_max / r2_point[benchmark]
  point_gapClosing <- (r2_point_max - r2_point[benchmark]) / (1 - r2_point[benchmark])
  
  # BOOTSTRAP CIs
  # First: CI for every submission and every benchmark
  ci_all_bootstrap <- apply(r2_boot, 2, function(x) sort(x)[c(250,9750)])
  # Take the best submission in each bootstrap draw
  best_score_draws <- apply(r2_boot[,submission_indicators], 1, max)
  # Make a CI for the maximum R^2
  ci_max_bootstrap <- sort(best_score_draws)[c(250,9750)]
  # Make CI for comparisons to benchmark
  # The bootstrap is the only way we construct these CIs
  ci_difference_bootstrap <- sort(best_score_draws - r2_boot[,benchmark])[c(250,9750)]
  ci_performanceMultiplying_bootstrap <- sort(best_score_draws / r2_boot[,benchmark])[c(250,9750)]
  ci_gapClosing_bootstrap <- sort((best_score_draws - r2_boot[,benchmark]) / (1 - r2_boot[,benchmark]))[c(250,9750)]
  
  # Prepare to return
  rownames(ci_all_bootstrap) <- 
    names(ci_max_bootstrap) <-
    names(ci_difference_bootstrap) <-
    names(ci_performanceMultiplying_bootstrap) <-
    names(ci_gapClosing_bootstrap) <-
    c("ci.min", "ci.max")
  
  return(
    data.frame(t(ci_all_bootstrap),
               point = r2_point,
               account = names(r2_point),
               method = "bootstrap") %>%
      bind_rows(data.frame(t(ci_max_bootstrap), point = r2_point_max, account = "max", method = "bootstrap")) %>%
      bind_rows(data.frame(t(ci_difference_bootstrap), point = point_difference, account = "difference", method = "bootstrap")) %>%
      bind_rows(data.frame(t(ci_performanceMultiplying_bootstrap), point = point_performanceMultiplying, account = "performanceMultiplying", method = "bootstrap")) %>%
      bind_rows(data.frame(t(ci_gapClosing_bootstrap), point = point_gapClosing, account = "gapClosing", method = "bootstrap")) %>%
      mutate(outcome = outcome_case)
  )
} %>%
  mutate(outcome_name = case_when(outcome == "materialHardship" ~ "Material\nhardship",
                                  outcome == "gpa" ~ "GPA",
                                  outcome == "grit" ~ "Grit",
                                  outcome == "eviction" ~ "Eviction",
                                  outcome == "jobTraining" ~ "Job\ntraining",
                                  outcome == "layoff" ~ "Layoff"),
         outcome_name = fct_relevel(outcome_name, "Material\nhardship", "GPA", "Grit",
                                    "Eviction", "Job\ntraining", "Layoff"))
write_csv(estimates_with_intervals,
          path = file.path(data.dir, "intermediate_files", "estimates_with_intervals.csv"))

#######################################
# MAIN PERFORMANCE PLOT OF MAIN PAPER #
#######################################
white_space_plot <- estimates_with_intervals %>%
  filter((account == "max" & method == "bootstrap")) %>%
  left_join(estimates_with_intervals %>%
              filter((account == "benchmark_ols_full" & outcome %in% c("gpa","grit","materialHardship")) |
                       (account == "benchmark_logit_full" & outcome %in% c("eviction","layoff","jobTraining"))) %>%
              rename(benchmark = point) %>%
              select(outcome, benchmark, on=outcome_name),
            by = "outcome") %>%
  ggplot(aes(x = outcome_name, y = point)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_errorbar(aes(ymin = benchmark, ymax = benchmark), color = "black") +
  geom_errorbar(aes(ymin = ci.min, ymax = ci.max), width = .4) +
  geom_text(aes(y = ci.max + .05,
                label = format(round(point,2), digits = 2)),
            fontface = "bold",
            size = 4) +
  annotate(geom = "text",
           x = 1, y = 0.1, color = "black",
           fontface = "bold",
           label = "Line\nindicates\nbenchmark", size = 2.5) +
  ylab(expression({italic(R)^2}[Holdout])) +
  xlab("\nOutcome at child age 15") +
  ylim(c(-.05,1)) +
  ggtitle(bquote("A) Best submission"~{italic(R)^2}[Holdout])) +
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0, vjust = .5),
        panel.grid = element_blank())

for (outcome_case in c("gpa","grit","materialHardship")) {
  lower_limit <- case_when(outcome_case %in% c("gpa","grit") ~ 1,
                           T ~ 0)
  upper_limit <- case_when(outcome_case %in% c("gpa","grit") ~ 4,
                           T ~ 1)
  title <- case_when(outcome_case == "gpa" ~ "C) GPA",
                     outcome_case == "grit" ~ "D) Grit",
                     outcome_case == "materialHardship" ~ "B) Material hardship")
  plot <- submissions %>%
    filter(outcome == outcome_case) %>%
    filter(r2_holdout == max(r2_holdout)) %>%
    bind_rows(benchmarks_long %>%
                filter(outcome == outcome_case & 
                         (account == "benchmark_ols_full" & outcome %in% c("gpa","grit","materialHardship")) |
                         (account == "benchmark_logit_full" & outcome %in% c("eviction","layoff","jobTraining")))) %>%
    mutate(type = ifelse(grepl("benchmark",account), "Benchmark", "Best submission")) %>%
    select(prediction, truth, type) %>%
    filter(!is.na(truth)) %>%
    ggplot(aes(x = truth, y = prediction)) +
    # Plot the training mean
    # Plot the best submission predictions and truth
    geom_point(size = .2) +
    geom_hline(yintercept = mean(train[[outcome_case]], na.rm = T),
               linetype = "dashed") +
    ylab("Prediction\n") +
    xlab("Truth") +
    coord_fixed() +
    scale_x_continuous(limits = c(lower_limit,upper_limit),
                       breaks = c(lower_limit,upper_limit)) +
    scale_y_continuous(limits = c(lower_limit,upper_limit),
                       breaks = c(lower_limit,upper_limit)) +
    ggtitle(title) +
    facet_wrap(~type, ncol = 2) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          strip.background = element_rect(fill = "white"),
          panel.grid.minor = element_blank())
  # Rename that object to call later
  assign(paste0("plot_",outcome_case), plot)
}
for (outcome_case in c("eviction","layoff","jobTraining")) {
  lower_limit <- 0
  upper_limit <- 1
  title <- case_when(outcome_case == "eviction" ~ "E) Eviction",
                     outcome_case == "layoff" ~ "G) Layoff",
                     outcome_case == "jobTraining" ~ "F) Job training")
  plot <- submissions %>%
    filter(outcome == outcome_case) %>%
    filter(r2_holdout == max(r2_holdout)) %>%
    bind_rows(benchmarks_long %>%
                filter(outcome == outcome_case & 
                         (account == "benchmark_ols_full" & outcome %in% c("gpa","grit","materialHardship")) |
                         (account == "benchmark_logit_full" & outcome %in% c("eviction","layoff","jobTraining")))) %>%
    mutate(type = ifelse(grepl("benchmark",account), "Benchmark", "Best submission")) %>%
    select(prediction, truth, type) %>%
    filter(!is.na(truth)) %>%
    mutate(truth = case_when(truth == 1 ~ "Event\noccurred",
                             truth == 0 ~ "Event did\nnot occur")) %>%
    mutate(group = paste0(type,"\n",truth)) %>%
    ggplot(aes(x = prediction)) +
    # Plot the training mean
    geom_vline(xintercept = mean(train[[outcome_case]], na.rm = T),
               linetype = "dashed") +
    # Plot the density of predictions
    geom_density(bw = .025) +
    xlab("Density of predicted\nprobability of event") +
    scale_x_continuous(limits = c(lower_limit,upper_limit),
                       breaks = c(lower_limit,upper_limit)) +
    scale_y_continuous(breaks = NULL, name = element_blank()) +
    ggtitle(title) +
    facet_grid(truth ~ type,
               scales = "free", switch = "y") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          strip.background = element_rect(fill = "white"),
          panel.grid.minor = element_blank())
  # Rename that object to call later
  assign(paste0("plot_",outcome_case), plot)
}
pdf(file.path(results.dir, "figures", "2_results_many_facets_full_benchmark.pdf"),
    height = 5, width = 13.5,
    onefile = F)
arranged <- gridExtra::arrangeGrob(grobs = list(white_space_plot,
                                                plot_materialHardship,plot_gpa,plot_grit,
                                                plot_eviction,plot_jobTraining,plot_layoff),
                                   layout_matrix = rbind(c(1,2,3,4),
                                                         c(1,5,6,7)),
                                   widths = c(5.4,2.7,2.7,2.7),
                                   heights = c(2.5,2.5))
gridExtra::grid.arrange(arranged)
dev.off()


##################################
## SUPPLEMENTAL BENCHMARK PLOTS ##
##################################

# Plots showing only main benchmark results
estimates_with_intervals %>%
  filter((outcome %in% c("gpa","grit","materialHardship") & account == "benchmark_ols_full") |
           (outcome %in% c("eviction","layoff","jobTraining") & account == "benchmark_logit_full") |
           account == "max" & method == "bootstrap") %>%
  mutate(account = ifelse(account == "max", "A. Best submission", "B. Benchmark")) %>%
  ggplot(aes(x = outcome_name, y = point, 
             ymin = ci.min, ymax = ci.max,
             color = account,
             label = format(round(point,2),digits = 2))) +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5)) +
  geom_errorbar(position = position_dodge(width = 1),
                width = .5) +
  geom_label(position = position_dodge(width = 1), 
             size = 2,
             label.padding = unit(0.1, "lines")) +
  annotate(geom = "text", x = .75, y = 0, hjust = 0,
           label = "Best submission",
           color = "blue", size = 2.5, angle = 90) +
  annotate(geom = "text", x = 1.25, y = 0, hjust = 0,
           label = "Benchmark",
           color = "seagreen4", size = 2.5, angle = 90) +
  theme_bw() +
  scale_color_manual(values = c("blue","seagreen4")) +
  scale_x_discrete(name = element_blank()) +
  scale_y_continuous(name = expression(atop({R^2}[Holdout],{}))) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank()) +
  ggsave(file.path(results.dir, "figures", "s7a_benchmarks_A.pdf"),
         height = 2, width = 6.5)


estimates_with_intervals %>%
  filter(account == "difference" & method == "bootstrap") %>%
  ggplot(aes(x = outcome_name, y = point, 
             label = format(round(point,2),digits = 2),
             ymin = ci.min, ymax = ci.max)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar() +
  geom_label() +
  theme_bw() +
  scale_x_discrete(name = element_blank()) +
  scale_y_continuous(name = expression(atop({R^2}[Holdout],"Best - Benchmark"))) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank()) +
  ggsave(file.path(results.dir, "figures", "s7b_benchmarks_B.pdf"),
         height = 2, width = 6.5)

estimates_with_intervals %>%
  filter(account == "gapClosing" & method == "bootstrap") %>%
  ggplot(aes(x = outcome_name, y = point, 
             label = format(round(point,2),digits = 2),
             ymin = ci.min, ymax = ci.max)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar() +
  geom_label() +
  theme_bw() +
  scale_x_discrete(name = element_blank()) +
  scale_y_continuous(name = expression(atop({R^2}[Holdout],frac(Best - Benchmark,1 - Benchmark)))) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_blank()) +
  ggsave(file.path(results.dir, "figures", "s7c_benchmarks_C.pdf"),
         height = 2, width = 6.5)


# Plot showing alternative benchmarks
estimates_with_intervals %>%
  filter(grepl("benchmark", account)) %>%
  mutate(predictors = case_when(
    grepl("full",account) ~ "C. Predictor set:\nAll (4)",
    grepl("lagged",account) ~ "B. Predictor set:\nProxy from prior wave (1)",
    grepl("demographic",account) ~ "A. Predictor set:\nDemographics at birth (3)"
  ),
  account = case_when(grepl("ols",account) ~ "A. OLS",
                      grepl("logit",account) ~ "B. Logistic regression",
                      grepl("rf",account) ~ "C. Random forest"),
  label = case_when(account == "A. OLS" & 
                      predictors == "C. Predictor set:\nAll (4)" &
                      outcome_name %in% c("A. Material\nhardship","B. GPA", "C. Grit") ~ 
                      paste0(format(round(point,2),digits = 2),"*"),
                    account == "B. Logistic regression" & 
                      predictors == "C. Predictor set:\nAll (4)" &
                      outcome_name %in% c("D. Eviction","E. Job\ntraining", "F. Layoff") ~ 
                      paste0(format(round(point,2),digits = 2),"*"),
                    T ~ format(round(point,2),digits = 2))) %>%
  ggplot(aes(x = account, y = point, color = account,
             label = label, ymin = ci.min, ymax = ci.max)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(width = .5) +
  geom_label(size = 2, label.padding = unit(.1, "lines")) +
  theme_bw() +
  scale_x_discrete(name = element_blank()) +
  scale_y_continuous(name = expression({R^2}[Holdout])) +
  theme(panel.grid.major.x = element_blank(),
        strip.text.y = element_text(angle = 0, hjust = 0),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(outcome_name ~ predictors, scales = "free_y") +
  ggsave(file.path(results.dir, "figures", "s8_benchmarks_alternative.pdf"),
         height = 7.5, width = 6.5)

# Note correlation between benchmark and best
correlations <- benchmarks_long %>%
  filter(grepl("full",account)) %>%
  filter((grepl("logit",account) & outcome %in% c("eviction","layoff","jobTraining")) |
           (grepl("ols",account) & outcome %in% c("gpa","grit","materialHardship"))) %>%
  select(outcome_name, challengeID, prediction, truth) %>%
  rename(Benchmark = prediction) %>%
  mutate(challengeID = as.numeric(challengeID)) %>%
  left_join(
    submissions %>%
      group_by(outcome) %>%
      filter(r2_holdout == max(r2_holdout)) %>%
      group_by() %>%
      select(outcome, outcome_name, challengeID, prediction) %>%
      rename(Best = prediction),
    by = c("outcome","outcome_name","challengeID")
  ) %>%
  group_by(outcome, outcome_name) %>%
  summarize(correlation = cor(Benchmark, Best))

# Benchmarks vs. best scatterplot
for (outcome_case in outcomes) {
  this_cor <- format(round(correlations$correlation[correlations$outcome == outcome_case],2),
                     nsmall = 2)
  forPlot <- benchmarks_long %>%
    filter(grepl("full",account)) %>%
    filter((grepl("logit",account) & outcome %in% c("eviction","layoff","jobTraining")) |
             (grepl("ols",account) & outcome %in% c("gpa","grit","materialHardship"))) %>%
    filter(outcome == outcome_case) %>%
    select(outcome, outcome_name, challengeID, prediction) %>%
    rename(Benchmark = prediction) %>%
    mutate(challengeID = as.numeric(challengeID)) %>%
    left_join(
      submissions %>%
        group_by(outcome) %>%
        filter(r2_holdout == max(r2_holdout)) %>%
        select(outcome, outcome_name, challengeID, prediction) %>%
        rename(Best = prediction),
      by = c("outcome","outcome_name","challengeID")
    )
  myTitle <- ifelse(grepl("[\n]",forPlot$outcome_name[1]),
                    forPlot$outcome_name[1],
                    paste0("\n",forPlot$outcome_name[1]))
  plot <- forPlot %>%
    ggplot(aes(x = Benchmark, y = Best)) +
    geom_point(size = .2, alpha = .4) +
    theme_bw() +
    geom_abline(intercept = 0, slope = 1, color = "gray") +
    ggtitle(myTitle) +
    theme(plot.title = element_text(hjust = .5)) +
    xlim(range(c(forPlot$Benchmark,forPlot$Best))) +
    ylim(range(c(forPlot$Benchmark,forPlot$Best))) +
    coord_fixed() +
    annotate(geom = "text",
             x = min(c(forPlot$Benchmark,forPlot$Best)), 
             y = max(c(forPlot$Benchmark,forPlot$Best)),
             label = paste("Correlation = ",this_cor),
             hjust = 0, vjust = 1,
             size = 2)
  assign(paste0("plot_",outcome_case),plot)
}
pdf(file.path(results.dir, "figures", "s9_benchmark_vs_best_scatter.pdf"),
    height = 5, width = 6,
    onefile = F)
gridExtra::grid.arrange(plot_materialHardship,plot_gpa,plot_grit,plot_eviction,plot_jobTraining,plot_layoff,
                        nrow = 2, ncol = 3, padding = unit(.1, "line"))
dev.off()


##############################
## TEXT NOTES IN SUPPLEMENT ##
##############################

# The Columbia Population Research Center (CPRC) interviewed some of the
# last cases to be interviewed, and they asked about job training over a longer period.
# This code shows that these cases were still NA in the challenge file,
# so the CPRC inconsistency does not affect our study.

public_data <- read_dta(file.path(private.data.dir, "FF_Y15_pub.dta")) %>% select(idnum, cp6source)
idLinkage <- read_csv(file.path(private.data.dir, "idLinkage.csv"))
leaderboardUnfilled <- read_csv(file.path(private.data.dir, "leaderboardUnfilled.csv"))

train %>%
  bind_rows(leaderboardUnfilled %>%
              mutate_all(.funs = as.numeric) %>%
              filter(!(challengeID %in% c(train$challengeID,test$challengeID)))) %>%
  bind_rows(test %>%
              mutate_all(.funs = as.numeric)) %>%
  select(challengeID,jobTraining) %>%
  left_join(idLinkage, by = "challengeID") %>%
  left_join(public_data, by = "idnum") %>%
  group_by(jobTraining) %>%
  summarize(prop_no_interview = mean(cp6source == -9),
            prop_westat = mean(cp6source == 1),
            prop_cprc = mean(cp6source == 2))

###############################################
## Plot all submissions with R^2_Holdout > 0 ##
###############################################
outlist = c("materialHardship" = "a", "gpa" = "b", "grit" = "c",
            "eviction" = "d", "jobTraining" = "e", "layoff" = "f")

for (outcome_case in outcomes) {
  # Get a list of valid accounts
  valid_accounts <- submissions %>% filter(beatingBaseline & outcome == outcome_case) %>% distinct(account) %$% account
  
  # Get the maximum R^2 for use in the axis limits of the plot
  max_r2 <- max((submissions %>% filter(outcome == outcome_case))$r2_holdout)
  
  estimates_with_intervals %>%
    # filter(account %in% submissions$account & outcome == outcome_case) %>%
    filter(account %in% valid_accounts & outcome == outcome_case) %>%
    mutate(account = fct_reorder(account, point)) %>%
    ggplot(aes(x = point, y = account)) +
    geom_point() +
    xlab(expression({R^2}[Holdout])) +
    scale_x_continuous(limits = c(0,ceiling(100*max_r2) / 100),
                       breaks = c(0,ceiling(100*max_r2) / 100)) +
    scale_y_discrete(name = element_blank()) +
    theme_bw() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    ggtitle(case_when(outcome_case == "materialHardship" ~ "A. Material\nhardship",
                      outcome_case == "gpa" ~ "B. GPA\n",
                      outcome_case == "grit" ~ "C. Grit\n",
                      outcome_case == "eviction" ~ "D. Eviction\n",
                      outcome_case == "jobTraining" ~ "E. Job training\n",
                      outcome_case == "layoff" ~ "F. Layoff\n")) +
    ggsave(file.path(results.dir, "figures", 
                     paste0("s12", outlist[outcome_case], "_scoresBeatingBaseline_not01_", outcome_case,".pdf")),
           height = 12, width = 3)
}

# Check that this is the number of qualifying submissions
num_account_in_s12 <- estimates_with_intervals %>%
  filter(account %in% submissions$account) %>%
  group_by(account) %>%
  filter((1:n()) == 1) %>%
  group_by() %>%
  summarize(num = n())
# This matches the 137 that we expect, who qualified on at least 1 outcome

#####################################################
# Plot of % of submissions worse than the benchmark #
#####################################################

estimates_with_intervals %>%
  filter(account %in% submissions$account) %>%
  select(outcome_name, account, point) %>%
  # Add a column with the benchmark score
  left_join(
    estimates_with_intervals %>%
      filter(grepl("full",account)) %>%
      filter((grepl("logit",account) & outcome %in% c("eviction","layoff","jobTraining")) |
               (grepl("ols",account) & outcome %in% c("gpa","grit","materialHardship"))) %>%
      select(outcome_name, point) %>%
      rename(benchmark = point),
    by = "outcome_name"
  ) %>%
  group_by(outcome_name) %>%
  summarize(worse_than_benchmark = mean(point < benchmark)) %>%
  group_by() %>%
  mutate(outcome_name = fct_reorder(outcome_name, -worse_than_benchmark)) %>%
  ggplot(aes(x = outcome_name, y = worse_than_benchmark,
             label = format(round(worse_than_benchmark,2),digits = 2))) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(y = .1, color = "white",
            fontface = "bold") +
  ylab("Proportion of submissions\nworse than benchmark") +
  xlab("Outcome") +
  theme_bw() +
  ylim(c(0,1)) +
  ggsave(file.path(results.dir, "figures", "s10_worse_than_benchmark.pdf"),
         height = 3.5, width = 6.5)

########################################
## Figure showing winner is uncertain ##
########################################

winner_bs <- foreach(outcome_case = outcomes, .combine = "rbind") %do% {
  squared_errors <- submissions %>%
    filter(outcome == outcome_case) %>%
    mutate(sq_error = (truth - prediction) ^ 2) %>%
    select(challengeID, account, sq_error) %>%
    spread(key = account, value = sq_error) %>%
    select(-challengeID)
  original_best <- sort(colMeans(squared_errors, na.rm = T))[1]
  name_winner <- names(original_best)
  foreach(i = 1:10000, .combine = "rbind") %do% {
    chosen <- sample(1:nrow(squared_errors), replace = T)
    best_star <- sort(colMeans(squared_errors[chosen,], na.rm = T))[1]
    return(data.frame(same_winner = names(best_star) == name_winner,
                      winner_star = names(best_star)))
  } %>%
    mutate(outcome = outcome_case)
}

winner_bs %>%
  group_by(outcome) %>%
  summarize(prop_same = mean(same_winner)) %>%
  mutate(outcome = fct_recode(outcome,
                              "Material\nhardship" = "materialHardship",
                              "GPA" = "gpa",
                              "Grit" = "grit",
                              "Eviction" = "eviction",
                              "Job\ntraining" = "jobTraining",
                              "Layoff" = "layoff"),
         outcome = fct_reorder(outcome, -prop_same)) %>%
  ggplot(aes(x = outcome, y = prop_same,
             label = format(round(prop_same,2),digits = 2))) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(y = prop_same + .05),
            fontface = "bold") +
  ylab("Proportion of bootstrapped draws\non which highest-scoring submission\nwas the same as the highest-scoring\nsubmission on the original holdout set") +
  xlab("Outcome") +
  theme_bw() +
  ylim(c(0,1)) +
  ggsave(file.path(results.dir, "figures", "s11_unsure_of_winner.pdf"),
         height = 3.5, width = 6.5)

#################################################
# Alternative estimators of out-of-sample error #
#################################################

# Prep the data in a form for the ensembling
data_predictions <- submissions %>%
  select(challengeID, outcome, truth, prediction, account) %>%
  spread(key = account, value = prediction) %>%
  arrange(outcome, truth) %>%
  select(-challengeID)

# Write functions to get the stacked estimator

## Write a function to fit a convex combination ridge regression
fit.convex.ridge <- function(X,y,lambda) {
  if (length(lambda) != 1 | !is.numeric(lambda) | is.na(lambda)) {
    stop(paste0("ERROR: Something wrong with lambda, lambda is ",lambda,
                "length of lambda is ",length(lambda)))
  }
  if (ncol(X) == 0 | nrow(X) == 0) {
    stop("ERROR: X is not a matrix")
  }
  if (!(is.numeric(y) | is.logical(y)) | length(y) == 0) {
    stop(paste0("ERROR: y is not what we expect: ",y))
  }
  if (nrow(X) != length(y)) {
    stop("ERROR: X and y disagree")
  }
  S <- ncol(X) # number of submissions
  ## We find argmin_b (-d'b + 1/2 b'Db)
  d <- t(y) %*% X
  D <- t(X) %*% X
  ## Add the ridge penalty on the diagonal
  D <- D + lambda*diag(S)
  ## Linear constraint terms
  ## The linear constraint is that A'b >= b_0
  ## with a = constraint on the first element
  ## and > constraints on the rest
  A <- cbind(1,diag(S))
  b_0 <- c(1,rep(0,S))
  fit <- solve.QP(Dmat = D, dvec = d,
                  Amat = A, bvec = b_0, meq = 1)
  if (abs(sum(fit$solution) - 1) > .0000001) {
    stop(paste0("ERROR: Weights do not sum to 1. Sum is ",sum(fit$solution)))
  }
  if (any(fit$solution < -.0002)) {
    stop("ERROR: Weights not all positive")
  }
  return(fit)
}

# Write a function to do a sample split
# and get the evaluation set score and variance
evaluation_estimator <- function(data_yhat, outcome_case) {
  # Restrict to this outcome and drop the character column indicating the outcome
  data_yhat <- data_yhat[data_yhat$outcome == outcome_case,]
  data_errors <- apply(data_yhat[,-(1:2)], 2, function(x) (x - data_yhat$truth) ^ 2)
  
  # Choose selection and evaluation cases
  sets <- replicate(ceiling(nrow(data_errors) / 2), sample(1:2))[1:nrow(data_errors)]
  selection_cases <- which(sets == 1)
  evaluation_cases <- which(sets == 2)
  
  # Make separate matrices of data for selection and evaluation
  # We can drop missing rows here
  selection_yhat <- na.omit(data_yhat[selection_cases,])
  evaluation_yhat <- na.omit(data_yhat[evaluation_cases,])
  selection_errors <- na.omit(data_errors[selection_cases,])
  evaluation_errors <- na.omit(data_errors[evaluation_cases,])
  
  # Select the winner on the selection set
  selection_scores <- colMeans(selection_errors[,!(colnames(selection_errors) %in% c("outcome","truth"))])
  winner <- which(selection_scores == min(selection_scores))[1] # 1 just picks first if there are ties
  
  # Get the baseline and winner squared errors
  winner_sq_error <- evaluation_errors[,names(winner)]
  baseline_sq_error <- (evaluation_yhat$truth - colMeans(train[,outcome_case], na.rm = T)) ^ 2
  
  # Get the evaluation MSE for that team and the baseline MSE
  winner_mse <- mean(winner_sq_error)
  baseline_mse <- mean(baseline_sq_error)
  
  # Learn stacked weights in selection set
  # Identify teams with positive R^2
  teams_positive_r2 <- c(F,F,selection_scores < baseline_mse)
  # Get a matrix of predicted values for those teams
  selection_X <- as.matrix(selection_yhat[,teams_positive_r2])
  evaluation_X <- as.matrix(evaluation_yhat[,teams_positive_r2])
  # Get the outcomes
  selection_y <- selection_yhat$truth
  evaluation_y <- evaluation_yhat$truth
  
  ## Fit the community model on the full training data
  fit <- fit.convex.ridge(X = selection_X,
                          y = selection_y,
                          lambda = .01)
  
  ## Extract the community prediction
  ## The as.vector() just turns this from
  ## a 1x1 matrix to a vector.
  stacked_yhat <- as.vector(evaluation_X %*% fit$solution)
  stacked_sq_error <- (evaluation_y - stacked_yhat) ^ 2
  stacked_mse <- mean(stacked_sq_error)
  
  # UNCERTAINTY
  # Get the variance of each MSE
  winner_mse_var <- var(winner_sq_error) / nrow(evaluation_errors)
  stacked_mse_var <- var(baseline_sq_error) / nrow(evaluation_errors)
  baseline_mse_var <- var(baseline_sq_error) / nrow(evaluation_errors)
  mse_cov_winner_baseline <- cov(winner_sq_error,baseline_sq_error) / nrow(evaluation_errors)
  mse_cov_stacked_baseline <- cov(stacked_sq_error,baseline_sq_error) / nrow(evaluation_errors)
  
  # Get the R-squared
  r2_winner <- 1 - winner_mse / baseline_mse
  r2_stacked <- 1 - stacked_mse / baseline_mse
  
  # Get the R-squared variance by Taylor expansion
  r2_winner_var <- (winner_mse / baseline_mse) ^ 2 * (
    winner_mse_var / (winner_mse ^ 2) + 
      baseline_mse_var / (baseline_mse ^ 2) -
      2 * mse_cov_winner_baseline / (winner_mse * baseline_mse)
  )
  r2_stacked_var <- (stacked_mse / baseline_mse) ^ 2 * (
    stacked_mse_var / (stacked_mse ^ 2) + 
      baseline_mse_var / (baseline_mse ^ 2) -
      2 * mse_cov_stacked_baseline / (stacked_mse * baseline_mse)
  )
  
  return(data.frame(outcome = outcome_case,
                    r2_winner = r2_winner,
                    r2_winner_var = r2_winner_var,
                    r2_stacked = r2_stacked,
                    r2_stacked_var = r2_stacked_var))
}

# Apply the evaluation estimator to each outcome many times
num_reps <- 100
draws_evaluation <- foreach(outcome = outcomes, .combine = "rbind") %do% {
  foreach(rep = 1:num_reps, .combine = "rbind") %do% {
    evaluation_estimator(data_yhat = data_predictions, outcome_case = outcome)
  }
}

# Plot the simple max, evaluation, and stacked estimates with sampling intervals
draws_evaluation %>%
  group_by(outcome) %>%
  summarize_all(.funs = mean) %>%
  melt(id = "outcome") %>%
  mutate(variable = gsub("r2_","",variable),
         variable = gsub("winner","splitSample",variable),
         variable = ifelse(!grepl("var",variable), paste0(variable,"_point"), variable)) %>%
  separate(variable, into = c("quantity","variable")) %>%
  spread(key = variable, value = value) %>%
  mutate(ci.min = point - qnorm(.975)*sqrt(var),
         ci.max = point + qnorm(.975)*sqrt(var)) %>%
  select(outcome, quantity, point, ci.min, ci.max) %>%
  bind_rows(estimates_with_intervals %>%
              filter(account == "max" & method == "analytical") %>%
              mutate(quantity = "max") %>%
              select(outcome, quantity, point, ci.min, ci.max)) %>%
  mutate(estimator = case_when(quantity == "max" ~ "A. One set for both\nselection and evaluation",
                               quantity == "splitSample" ~ "B. Separate selection\nand evaluation sets\n(averaged over 100 reps)",
                               quantity == "stacked" ~ "C. Separate sets\nweighted average\n(averaged over 100 reps)"),
         outcome = case_when(outcome == "materialHardship" ~ "Material\nhardship",
                             outcome == "gpa" ~ "GPA",
                             outcome == "grit" ~ "Grit",
                             outcome == "eviction" ~ "Eviction",
                             outcome == "jobTraining" ~ "Job\ntraining",
                             outcome == "layoff" ~ "Layoff"),
         outcome = fct_relevel(outcome, "Material\nhardship", "GPA", "Grit",
                               "Eviction", "Job\ntraining", "Layoff")) %>%
  ggplot(aes(x = outcome, y = point, ymin = ci.min, ymax = ci.max,
             color = estimator, shape = estimator)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(position = position_dodge(width = .8)) +
  geom_point(position = position_dodge(width = .8)) +
  ylab(expression(R^2)) +
  theme_bw() +
  theme(legend.key.height = unit(1.3,"cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(vjust = .5, angle = 0),
        legend.title = element_blank()) +
  ggsave(file.path(results.dir, "figures", "s13_alternative_best_estimators.pdf"),
         height = 4, width = 6.5)

