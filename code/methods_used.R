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


# Handle data preparation table responses
methods %>%
  select(account, data_preparation) %>%
  separate_rows(data_preparation, sep=",") %>%
  mutate(dp = trimws(data_preparation)) %>%
  select(account, dp) %>%
  distinct(account, dp) ->
  dp_values

# unique(dp_values$dp)  # To inspect response range manually

# Collapse model-based feature selection
# Save these for next table, then drop them (they're not data preparation)
mbfs <- c("Creating synthetic features with PCA", "Feature selection with with k-means clustering")
dp_values %<>% mutate(dp = ifelse(dp %in% mbfs, "Model-based feature selection (e.g. F-test or LASSO)", dp))
dp_values %>% filter(dp == "Model-based feature selection (e.g. F-test or LASSO)") %>% transmute(account=account, fs=dp) -> mbfs
dp_values %<>% filter(dp != "Model-based feature selection (e.g. F-test or LASSO)")

# Drop invalid other responses
invalid_dp <- c("please use answers submitted by Bingyu Zhao", "Information provided by team member Bingyu Zhao", "")
dp_values %<>% filter(!(dp %in% invalid_dp))

# Collapse model-based imputation
mbi <- c("Model-based imputation", "KNN imputation algorithm", "kNN (k-Nearest Neighbors) imputation algorithm")
dp_values %<>% mutate(dp = ifelse(dp %in% mbi, "Model-based imputation", dp))

# Collapse logical imputation
logimp <- c("Imputation based on survey structure (e.g. skips)", "Logical imputation of caregiver properties based on other information in the survey")
dp_values %<>% mutate(dp = ifelse(dp %in% logimp, "Logical imputation (e.g. using skips, other variables)", dp))

# Collapse dropping variables by logical/type tests
drop_type <- c("Excluded 'constructed scales' with more than 30% missing values", "Dropping variables with more than 70% missing values",
               "dropping columns containing strings")
dp_values %<>% mutate(dp = ifelse(dp %in% drop_type, "Dropping variables based on logical or type criteria", dp))

# Produce figure
dp_values %>%
  distinct(account, dp) %>%
  ggplot(aes(x=dp, y=reorder(account, desc(account)))) +
  geom_tile() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  labs(x="Reported data preparation steps",
       y="Account") + 
  ggsave(file.path(results.dir, "figures", "s20_reports_dataprep.pdf"),
         height=10, width=12)


# Handle feature selection table responses
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
mdfs <- c("Model-based feature selection", "Model-based feature selection (e.g. F-test or LASSO)",
          "Select K Best using chi square statistics of each feature", "Correlation based selection",
          "Boruta: Wrapper Algorithm for All Relevant Feature Selection", "Used Boruta package to select features",
          "Extra Trees Regression Algorithm", "Randomized Lasso", "Random Forest using Gini score",
          "Mutual information", "random forest based feature selection", "Extra Trees Regressor algorithm",
          "Sparse Linear Models", "F-test")
fs_values %<>% mutate(fs = ifelse(fs %in% mdfs, "Model-based feature selection (e.g. F-test or LASSO)", fs))

# Produce figure
fs_values %>%
  distinct(account, fs) %>%
  ggplot(aes(x=fs, y=reorder(account, desc(account)))) +
  geom_tile() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  labs(x="Reported feature selection steps",
       y="Account") + 
  ggsave(file.path(results.dir, "figures", "s21_reports_featureselection.pdf"),
         height=10, width=12)


# Handle learning algorithm table responses
methods %>%
  select(account, learning_algorithm) %>%
  separate_rows(learning_algorithm, sep=",") %>%
  mutate(la = trimws(learning_algorithm)) %>%
  select(account, la) %>%
  distinct(account, la) ->
  la_values

# unique(la_values$la)  # To inspect response range manually

# Drop invalid other responses
invalid_la <- c("Information provided by team member Bingyu Zhao", "Please use answers submitted by Bingyu Zhao", "")
la_values %<>% filter(!(la %in% invalid_la))

# Combine kernel methods
kernel <- c("Support Vector Machine", "Support Vector Regression", "Gaussian Process Regression",
            "Kernel methods (e.g. kernel ridge regression or support vector machines)")
la_values %<>% mutate(la = ifelse(la %in% kernel, "Kernel methods (e.g. kernel ridge regression or support vector machines)", la))

# Combine ensemble methods
ensemble <- c("Ensembling", "Bagging")
la_values %<>% mutate(la = ifelse(la %in% ensemble, "Ensembling", la))

# Combine gradient boosting methods
gbt <- c("Stochastic Gradient Boosting", "Gradient-boosted trees")
la_values %<>% mutate(la = ifelse(la %in% gbt, "Gradient-boosted trees", la))

# Combine classification
classif <- c("Multinomial Naive Bayes", "Bernoulli Naive Bayes", "Linear Discriminant Analysis",
             "K-Nearest Neighbors", "k-nearest neighbors", "Naive bayes")
la_values %<>% mutate(la = ifelse(la %in% classif, "Gradient-boosted trees", la))

# Combine LASSO
lasso <- c("LASSO", "Lasso + Least Angle Regression")
la_values %<>% mutate(la = ifelse(la %in% lasso, "Gradient-boosted trees", la))
la_values %<>% rbind(c("cjqian", "Least angle regression"))  # Also save LARS use

# Combine other
other <- c("Least angle regression", "Cubist", "Mars", "Huber regression", "Neural networks", "Spike and slab using Gnet")
la_values %<>% mutate(la = ifelse(la %in% other, "Other (e.g. neural networks or other regression models)", la))

# Produce figure
la_values %>%
  distinct(account, la) %>%
  ggplot(aes(x=la, y=reorder(account, desc(account)))) +
  geom_tile() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  labs(x="Reported learning algorithms used",
       y="Account") + 
  ggsave(file.path(results.dir, "figures", "s22_reports_learningalg.pdf"),
         height=10, width=13)


# Handle model selection table responses
methods %>%
  select(account, model_selection) %>%
  separate_rows(model_selection, sep=",") %>%
  mutate(ms = trimws(model_selection)) %>%
  select(account, ms) %>%
  distinct(account, ms) ->
  ms_values

# unique(ms_values$ms)  # To inspect response range manually

# Drop invalid other responses
invalid_ms <- c("Information provided by team member Bingyu Zhao", "please use answers submitted by Bingyu Zhao", "")
ms_values %<>% filter(!(ms %in% invalid_ms))

# Collapse training set performance responses
tsp <- c("R^2", "R-squared and psuedo R-squared", "precision/accuracy/f1-score performance in cross-validation within the training set",
         "MSE performance in training set", "MSE performance in split test set")
ms_values %<>% mutate(ms = ifelse(ms %in% tsp, "MSE performance in training set", ms))

# Collapse measures of variable importance
mvi <- c("Measures of variable importance (e.g. marginal effects or weights)", "Weights based on out of bag performance of each random forest",
         "Assessing groups/clusters of features")
ms_values %<>% mutate(ms = ifelse(ms %in% mvi, "Measures of variable importance (e.g. marginal effects)", ms))

# Produce figure
ms_values %>%
  distinct(account, ms) %>%
  ggplot(aes(x=ms, y=reorder(account, desc(account)))) +
  geom_tile() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  labs(x="Reported model interpretation steps",
       y="Account") + 
  ggsave(file.path(results.dir, "figures", "s23_reports_modelinterp.pdf"),
         height=10, width=12)
