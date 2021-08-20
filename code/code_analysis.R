# code_analysis.R
# Author: Alex Kindel
# Date: 17 July 2019
# Updated: 9 July 2021
# Produces summary plots for FFC code corpus.

library(dplyr)
library(readr)
library(ggplot2)
library(ggridges)
library(magrittr)
library(tidytext)
library(tidyr)
library(gridExtra)
library(here)

# Set directory information
data.dir <- file.path(here(), "data")
private.data.dir <- file.path(data.dir, "private")
results.dir <- file.path(here(), "results")

# Set ggplot2 theme
theme_set(theme_bw())

# Function to rewrite serialized Python lists as comma delimited
csvec <- function(pyvec) {
  tvec <- gsub("\\[", "", pyvec)
  tvec <- gsub("\\]", "", tvec)
  tvec <- gsub("'", "", tvec)
  return(tvec)
}

# Same as above, but space delimited
docvec <- function(pyvec) {
  tvec <- csvec(pyvec)
  tvec <- gsub(",", "", tvec)
  return(tvec)
}

# Convert serialized Python list to R vector object
rvec <- function(pyvec) {
  tvec <- gsub("\\[", "(", pyvec)
  tvec <- gsub("\\]", ")", tvec)
  tvec <- paste0("c", tvec)
  return(eval(parse(text=tvec)))
}


# Import data
code <- read_csv(here("data/ffc_code.csv"), col_types='cccc')

# How many distinct users are there?
participants <- unique(code$user)
n_participants <- length(participants)

# Convert code vectors to space-delimited documents
code %<>% mutate(code = docvec(code))

# Tokenize documents and identify unique symbols by language
code %>%
  unnest_tokens(symbol, code) %>%
  group_by(extension, symbol) %>%
  summarize(n = n()) ->
  symbols

# Write symbol list to CSV (not run)
# write_csv(symbols, file.path(data.dir, "intermediate_files", ffc_symbols_uncoded.csv"))

# Load hand-coded symbols
symbols <- read_csv(file.path(data.dir, "ffc_symbols_coded.csv"), col_types='ccicccc')

# Unnest pipelines and merge with classification scheme
# We observe 54480 function calls in the full final submission corpus
# Dropping NAs and unclassified functions yields 54385 observed function calls
code %>%
  unnest_tokens(symbol, code) %>%
  left_join(symbols, by=c("symbol"="symbol", "extension"="extension")) %>%
  filter(!is.na(class1) & !(class1 == "unkn")) ->
  pipelines

# Look at popularity of different models
pipelines %>%
  filter(class1 == "model") %>%
  distinct(user, symbol) %>%
  group_by(symbol) %>%
  summarize(n=n()) %>%
  arrange(desc(n)) ->
  model_funcs

# Write model funcs to CSV (not run)
# write_csv(model_funcs, file.path(data.dir, "intermediate_files", "ffc_model_functions_uncoded.csv"))

# Load hand-coded model types
model_funcs <- read_csv(file.path(data.dir, "ffc_model_functions_coded.csv"), col_types="ciiiiiiiiiiiiici")
model_funcs %>%
  select(-one_of(c("n", "notes", "sum"))) %>%
  gather(type, count, -symbol) %>%
  mutate(is.type = !is.na(count)) %>%
  filter(is.type) %>%
  select(symbol, type) ->
  model_types

# Specific model types of interest
linear_models <- model_funcs %>% filter(linear == 1) %$% symbol
logistic_models <- model_funcs %>% filter(logistic == 1) %$% symbol
tree_models <- model_funcs %>% filter(tree == 1) %$% symbol

# Merge model data to pipeline data
# Note this is one to many (models can have multiple types)
pipelines %>%
  select(user, extension, symbol, n, class1, class2) %>%
  left_join(model_types, by="symbol") ->
  pipeline_modeltypes

# How many users of each model type?
pipeline_modeltypes %>%
  filter(!is.na(type)) %>%
  distinct(type, user) ->
  user_modeltypes

# Plot counts
user_modeltypes %>%
  group_by(type) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=type, y=n)) +
  geom_col() +
  labs(x="Model type",
       y="# users") +
  ggsave(file.path(results.dir, "figures", "s16_modelcounts.pdf"),
         height = 4, width = 6.5)


# Compute within-user preparation counts and proportions
pipelines %>%
  group_by(user, class1) %>%
  summarize(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  filter(class1 == "prep") ->
  preparation

# Load holdout performance data
# Collapse submissions data from one row per family-team-outcome to one row per team-outcome with R^2
submissions <- read_csv(file.path(data.dir, "submissions.csv"), col_types="cccdddddl")
submissions %>%
  select(outcome, team=account, r.squared=r2_holdout, beatingBaseline) %>%
  distinct(outcome, team, r.squared, beatingBaseline) %>%
  arrange(outcome, team) ->
  holdout_scores

if (nrow(holdout_scores) != (160 * 6)) stop("ERROR: Not expected number of rows")  # Verify 960 rows
write_csv(holdout_scores, file.path(data.dir, "derived", "r_squared_all.csv"))  # Write file

# Merge outcome to data preparation aggregate statistics
preparation %>%
  left_join(holdout_scores, by=c("user"="team")) %>%
  filter(beatingBaseline) %>%
  left_join(user_modeltypes, by="user") ->
  pipeline_scores

# Plot variation in performance by absolute data preparation
outlist <- c("materialHardship" = "Material hardship", "gpa" = "GPA", "grit" = "Grit",
             "eviction" = "Eviction", "jobTraining" = "Job training", "layoff" = "Layoff")
pipeline_scores %>%
  rowwise() %>%
  mutate(outcome = outlist[outcome]) %>%
  ungroup() %>%
  distinct(user, outcome, .keep_all = T) %>%
  ggplot(aes(x=n, y=r.squared)) +
  geom_point() +
  scale_x_log10(limits=c(1, NA)) +
  facet_wrap(~outcome, scales="free") +
  labs(x="# of data preparation function calls (log scale)", y=expression("Holdout "*R^2)) +
  ggsave(file.path(results.dir, "figures", "s17_dataprep_r2.pdf"),
         height=6, width=8)

# Set axis range for model types figure (S18)
# Necessary because not every outcome-model pair has sufficient data to plot (some facets will be missing y levels otherwise)
pipeline_scores %>%
  filter(!is.na(type)) %>%
  pull(type) %>%
  unique() %>%
  sort() ->
  y_ticks

# Plot variation in performance by model type(s) used
pipeline_scores %>%
  rowwise() %>%
  mutate(outcome = outlist[outcome]) %>%
  ungroup() %>%
  filter(!is.na(type)) %>%
  ggplot(aes(x=r.squared, y=type, group=type)) +
  geom_density_ridges() +
  facet_wrap(~outcome, scales = "free") +
  xlim(0, NA) +
  scale_y_discrete(limits=y_ticks) +
  labs(x=expression("Holdout "*R^2), y="Model category (non-exclusive)") +
  ggsave(file.path(results.dir, "figures", "s18_modeltype_r2.pdf"),
         height=6, width=8)
