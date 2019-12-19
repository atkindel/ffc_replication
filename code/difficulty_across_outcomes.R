# Measure association of difficulty across outcomes
# In families with hard to predict GPA is there also hard to predict eviction?
# Input: processed_predictions.RData
# Output: difficulty_scatterplot_matrix.pdf
# By Matt Salganik (minor edits by Alex Kindel)
# Runtime: Less than 1 minute on a laptop

library(tidyverse)
library(here)
library(magrittr)
library(grid)
library(gridExtra)
library(corrr)
library(scales)

# Set directory information
code.dir <- file.path(here(), "code")
data.dir <- file.path(here(), "data")
private.data.dir <- file.path(data.dir, "private")
working.data.dir <- file.path(data.dir, "intermediate_files")
results.dir <- file.path(here(), "results", "figures")

# Set graphics parameters
theme_set(theme_bw())
theme_update(plot.title = element_text(size = 8))
alpha_val <- 0.2

# Source helper functions
source(file.path(code.dir, "outcome_labels.R"))

# Load data
load(file.path(working.data.dir, "processed_predictions.RData"))

# Compute familywise prediction difficulty
family_difficulties <- predictions %>% 
  filter(beatingBaseline == TRUE) %>%
  filter(missing.any.outcome == FALSE) %>%
  group_by(challengeID, outcome) %>%
  summarise(mse.unit.outcome = unique(mse_unit_outcome)) %>%
  spread(key = outcome, value = mse.unit.outcome) %>%
  ungroup()

# Compute pairwise correlations
family_difficulties %>%
  select(-challengeID) %>%
  correlate(method = "pearson", use = "pairwise.complete.obs", quiet = TRUE) %>%
  stretch() ->
  rs

# Output correlation values
print("Correlation of difficulties of outcomes")
rs %>% 
  arrange(desc(r)) %>%
  print()

print("Absolute value of correlation of difficulties of outcomes > 0.1")
rs %>% 
  arrange(desc(r)) %>%
  filter(abs(r) > 0.1) %>%
  print()

# Construct scatterplot matrix by building up each piece individually
# Builds on https://stackoverflow.com/questions/20552226/make-one-panel-blank-in-ggplot2
print("Creating correlation matrix scatter plots . . . ")

# Encode graphics parameters for each outcome
gbreaks <- list(materialHardship = c(0, 0.2, 0.4),
                gpa = c(0, 1.0, 2.0, 3.0),
                grit = c(0, 1.0, 2.0, 3.0),
                eviction = c(0, 0.4, 0.8),
                jobTraining = c(0, 0.3, 0.6),
                layoff = c(0, 0.3, 0.6))

glimits <- list(materialHardship = c(0, max(family_difficulties$materialHardship)),
                gpa = c(0, max(family_difficulties$gpa)),
                grit = c(0, max(family_difficulties$grit)),
                eviction = c(0, max(family_difficulties$eviction)),
                jobTraining = c(0, max(family_difficulties$jobTraining)),
                layoff = c(0, max(family_difficulties$layoff)))

glabels <- list(materialHardship = "Material hard.",
                gpa = "GPA",
                grit = "Grit",
                eviction = "Eviction",
                jobTraining = "Job training",
                layoff = "Layoff")

# Blank cell
# blank <- grid.rect(gp = gpar(col = "white"))
blank <- rectGrob(gp=gpar(col=NA))

# Function to plot outcome-pairwise prediction difficulty
plot_diff_pred <- function(out_x, out_y) {
  
  # Get correlation coefficient for this pair
  r <- rs %>%
    filter(x == out_x, y == out_y) %>% 
    select(r) %>%
    round(2)
  
  # Return scatterplot
  return(ggplot(family_difficulties, aes_string(x = out_x, y = out_y)) +
           geom_point(alpha = alpha_val) +
    scale_x_continuous(bquote("d"[~.(glabels[[out_x]])]), 
                       breaks = gbreaks[[out_x]],
                       limits = glimits[[out_x]],
                       labels = scales::label_number(accuracy = 0.1, trim=F)) +
    scale_y_continuous(bquote("d"[~.(glabels[[out_y]])]), 
                       breaks = gbreaks[[out_y]],
                       limits = glimits[[out_y]],
                       labels = scales::label_number(accuracy = 0.1, trim=F)) +
    ggtitle(paste("Cor:", r)) +
      theme(axis.text.x = element_text(size=6),
            axis.text.y = element_text(size=6)))
}

# Function to plot outcome-pairwise prediction difficulty
plot_diff_pred_u <- function(out_x, out_y) {
  
  # Get correlation coefficient for this pair
  r <- rs %>%
    filter(x == out_x, y == out_y) %>% 
    select(r) %>%
    round(2)
  
  # Return scatterplot
  return(ggplot(family_difficulties, aes_string(x = out_x, y = out_y)) +
           geom_point(alpha = alpha_val) +
           scale_x_continuous(bquote("d"[~.(glabels[[out_x]])]), 
                              breaks = gbreaks[[out_x]],
                              limits = glimits[[out_x]],
                              labels = scales::label_number(accuracy = 0.1, trim=F)) +
           scale_y_continuous(bquote("d"[~.(glabels[[out_y]])]), 
                              breaks = gbreaks[[out_y]],
                              limits = glimits[[out_y]],
                              labels = scales::label_number(accuracy = 0.1, trim=F)) +
           ggtitle(paste("Cor:", r)) + 
           coord_flip() +
           theme(axis.text.x = element_text(size=6),
                 axis.text.y = element_text(size=6)))
}

# Produce a scatterplot for each outcome pair
outcome_combos <- t(combn(c("materialHardship", "gpa", "grit", "eviction", "jobTraining", "layoff"), 2))
plots <- mapply(plot_diff_pred, outcome_combos[,1], outcome_combos[,2], SIMPLIFY=F)
plots_u <- mapply(plot_diff_pred_u, outcome_combos[,1], outcome_combos[,2], SIMPLIFY=F)

# Arrange scatterplots in a matrix; don't plot redundant cells
# Use marrangeGrob() instead of grid.arrange() to avoid errors
p <- marrangeGrob(grobs = list(blank, plots_u[[1]], plots_u[[2]], plots_u[[3]], plots_u[[4]], plots_u[[5]],
                               plots[[1]], blank, plots_u[[6]], plots_u[[7]], plots_u[[8]], plots_u[[9]],
                               plots[[2]], plots[[6]], blank, plots_u[[10]], plots_u[[11]], plots_u[[12]],
                               plots[[3]], plots[[7]], plots[[10]], blank, plots_u[[13]], plots_u[[14]],
                               plots[[4]], plots[[8]], plots[[11]], plots[[13]], blank, plots_u[[15]],
                               plots[[5]], plots[[9]], plots[[12]], plots[[14]], plots[[15]], blank),
                  ncol = 6, nrow = 6, top=NULL)

# Save figure to disk
ggsave(plot = p,
       filename = "s14_difficulty_scatterplot_matrix.pdf",
       path = results.dir, 
       device = "pdf",
       dpi = 300,
       width = 7.5, height = 10, units = "in")
