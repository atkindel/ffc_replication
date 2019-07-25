# participant_analysis.R
# Author: Alex Kindel
# Date: 24 July 2019
# Produces tables describing methods used in the Challenge.

library(here)
library(tidyverse)
library(magrittr)

# Set directory information
data.dir <- file.path(here(), "data")
private.data.dir <- file.path(data.dir, "private")
results.dir <- file.path(here(), "results")

# Set ggplot2 theme
theme_set(theme_bw())

# Load methods survey
methods <- read.csv(file.path(data.dir, "methods_responses_clean.csv"))

# Filter respondents using valid accounts list
valid <- read.csv(file.path(data.dir, "author_methods.csv"))
methods %<>% filter(account %in% valid$account)


# Handle data preparation table

# Extract range of values
# We use the most expansive set known
# (i.e. we merge all form responses for each account)
methods %>%
  select(account, data_preparation) %>%
  separate_rows(data_preparation, sep=",") %>%
  mutate(dp = trimws(data_preparation)) %>%
  select(account, dp) %>%
  distinct(account, dp) ->
  dataprep_values

# unique(dataprep_values$dp)  # To inspect response range manually

# Collapse model-based feature selection
# Save these for next table, then drop them (they're not data preparation)
mbfs <- c("Creating synthetic features with PCA", "Feature selection with with k-means clustering")
dataprep_values %<>% mutate(dp = ifelse(dp %in% mbfs, "Model-based feature selection (e.g. F-test or LASSO)", dp))
dataprep_values %>% filter(dp == "Model-based feature selection (e.g. F-test or LASSO)") %>% transmute(account=account, fs=dp) -> mbfs
dataprep_values %<>% filter(dp != "Model-based feature selection (e.g. F-test or LASSO)")

# Drop invalid other responses
invalid_dp <- c("please use answers submitted by Bingyu Zhao", "Information provided by team member Bingyu Zhao", "")
dataprep_values %<>% filter(!(dp %in% invalid_dp))

# Collapse model-based imputation
mbi <- c("Model-based imputation", "KNN imputation algorithm", "kNN (k-Nearest Neighbors) imputation algorithm")
dataprep_values %<>% mutate(dp = ifelse(dp %in% mbi, "Model-based imputation", dp))

# Collapse logical imputation
logimp <- c("Imputation based on survey structure (e.g. skips)", "Logical imputation of caregiver properties based on other information in the survey")
dataprep_values %<>% mutate(dp = ifelse(dp %in% logimp, "Logical imputation (e.g. using skips, other variables)", dp))

# Collapse dropping variables by logical/type tests
drop_type <- c("Excluded 'constructed scales' with more than 30% missing values", "Dropping variables with more than 70% missing values",
               "dropping columns containing strings")
dataprep_values %<>% mutate(dp = ifelse(dp %in% drop_type, "Dropping variables based on logical or type criteria", dp))

# Produce figure
dataprep_values %>%
  distinct(account, dp) %>%
  ggplot(aes(x=dp, y=reorder(account, desc(account)))) +
  geom_tile() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  labs(x="Reported data preparation steps",
       y="Account") + 
  ggsave(file.path(results.dir, "figures", "s20_reports_dataprep.pdf"),
         height=10, width=12)


# Handle feature selection table
methods %>%
  select(account, feature_selection) %>%
  separate_rows(feature_selection, sep=",") %>%
  mutate(fs = trimws(feature_selection)) %>%
  select(account, fs) %>%
  distinct(account, fs) ->
  fs_values
fs_values %<>% rbind(mbfs)

# unique(fs_values$fs)  # To inspect response range manually

# Drop invalid other responses
invalid_fs <- c("Information provided by team member Bingyu Zhao", "please use answers submitted by Bingyu Zhao", "", "No feature selection",
                "Excluded 'constructed scales' with more than 30% missing values You're", "Excluded 'constructed scales' with more than 30% missing values")
fs_values %<>% filter(!(fs %in% invalid_fs))

# Merge model-based feature selection
mbfs <- c("Model-based feature selection", "Model-based feature selection (e.g. F-test or LASSO)",
          "Select K Best using chi square statistics of each feature", "Correlation based selection",
          "Boruta: Wrapper Algorithm for All Relevant Feature Selection", "Used Boruta package to select features",
          "Extra Trees Regression Algorithm", "Randomized Lasso", "Random Forest using Gini score",
          "Mutual information", "random forest based feature selection", "Extra Trees Regressor algorithm",
          "Sparse Linear Models", "F-test")
fs_values %<>% mutate(fs = ifelse(fs %in% mbfs, "Model-based feature selection (e.g. F-test or LASSO)", fs))

fs_values %>%
  distinct(account, fs) %>%
  ggplot(aes(x=fs, y=reorder(account, desc(account)))) +
  geom_tile() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  labs(x="Reported feature selection steps",
       y="Account") + 
  ggsave(file.path(results.dir, "figures", "s21_reports_featureselection.pdf"),
         height=10, width=12)