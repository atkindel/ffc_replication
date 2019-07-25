# Input: processed_predictions.RData
# Outputs: 1) heatmaps of squared error for all six outcomes, Fig 3 from paper
#          2) table of R^2 values for fixed effects models that goes in SM
# By Matt Salganik (minor edits + optimization? by Alex Kindel)
# Runtime: A few minutes on a laptop 

library(tidyverse)
library(forcats)
library(magrittr)
library(here)
library(stargazer)
library(gridExtra)
library(broom)

# Set directory information
code.dir <- file.path(here(), "code")
data.dir <- file.path(here(), "data")
private.data.dir <- file.path(data.dir, "private")
working.data.dir <- file.path(data.dir, "intermediate_files")
results.dir <- file.path(here(), "results", "figures")

# Set ggplot2 theme
theme_set(theme_bw())

# Label helper functions
clean_outcome_label <- function(outcome_label) {
  pretty_outcome_label <- switch(outcome_label,
                                 materialHardship = "A. Material hardship",
                                 gpa = "B. GPA",
                                 grit = "C. Grit",
                                 layoff = "D. Layoff",
                                 eviction = "E. Eviction",
                                 jobTraining = "F. Job training",
                                 outcome_label)
  return(pretty_outcome_label)
}

# Load data
load(file.path(working.data.dir, "processed_predictions.RData"))

# Create data objects
plots <- list()
fits.challengeID <- list()
fits.account <- list()
outcomes.to.plot <- c("materialHardship", "gpa", "grit", "eviction", "jobTraining", "layoff")

# For each outcome...
for (outcome.to.plot in outcomes.to.plot) {
  print(paste("Beginning to process:", outcome.to.plot))
  
  # Get predictions for this outcome
  data.to.plot <- predictions %>%
    filter(outcome == outcome.to.plot) %>%
    filter(beatingBaseline == TRUE) %>%
    filter(!is.na(truth)) %>%
    select(account, challengeID, sq.err, mse.account.outcome, mse_unit_outcome) %>%
    ungroup()

  # Estimate squared error by observation and by account
  fits.challengeID[[outcome.to.plot]] <- lm(sq.err ~ as.factor(challengeID), data.to.plot)
  fits.account[[outcome.to.plot]] <- lm(sq.err ~ account, data.to.plot)

  # Order observation IDs and accounts by MSE on this outcome
  data.to.plot$account <- fct_reorder(as.factor(data.to.plot$account), data.to.plot$mse.account.outcome)
  data.to.plot$challengeID <- fct_reorder(as.factor(data.to.plot$challengeID), data.to.plot$mse_unit_outcome)

  # Note that portions of the heatmap can look "flat" because of a resolution limit
  # For example, from the heatmap it is hard to see the difference between 0.001 and 0.002 (depending on scale)
  
  # Check if any two rows are the exactly the same
  data.to.plot.wide <- data.to.plot %>% 
    select(account, challengeID, sq.err) %>%
    spread(challengeID, sq.err)
  if (!(nrow(distinct(data.to.plot.wide)) == nrow(data.to.plot.wide))) {
    stop("ERROR: two rows are exactly the same")
  }

  # Construct heatmap of prediction errors
  plots[[outcome.to.plot]] <- ggplot(data.to.plot, aes(x = challengeID, y = account)) +
    geom_raster(aes(fill = sq.err), hjust = 0, vjust = 0) + 
    scale_fill_viridis_c(option = "magma", direction = -1) +
    theme(axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(), 
          legend.position = c(0.755, 1.295),
          legend.direction = "horizontal",
          legend.background = element_blank(),
          legend.title = element_text(size = 7), 
          legend.text = element_text(size = 7),
          legend.key.height = unit(0.1, "in"),
          legend.key.width = unit(0.3, "in"), 
          axis.title = element_text(size = rel(0.7))) +
    guides(fill = guide_colourbar(ticks = FALSE, title.vjust = 0.9, label.vjust = 2)) +
  ggtitle(clean_outcome_label(outcome.to.plot)) +
  labs(x = "Family", y = "Team", fill = "Squared error") 
  
  print(paste("Finished processing:", outcome.to.plot))
}

# Save heatmaps as PDF and PNG
p <- grid.arrange(grobs = plots, nrow = 6)
ggsave(plot = p,
       filename = "3_heatmaps_sqerr_6outcomes.pdf",
       path = results.dir, 
       device = "pdf", 
       dpi = 300,
       width = 4.75, height = 8, units = "in")

ggsave(plot = p,
       filename = "3_heatmaps_sqerr_6outcomes.png",
       path = results.dir,
       device = "png", 
       dpi = 300,
       width = 4.75, height = 8, units = "in")

# Build table of model fit results
summary.fits.challengeID <- map_dfr(fits.challengeID, glance, .id = "outcome")
summary.fits.account <- map_dfr(fits.account, glance, .id = "outcome")

# Print R2 by outcome per row and column
summary.fits.challengeID %>%
  select(outcome, r.squared) %>%
  print()

summary.fits.account %>%
  select(outcome, r.squared) %>%
  print()

# Generate Table S6 (model fit for models of prediction error by account, observation)
table_1 <- summary.fits.challengeID %>%
  select(outcome, r.squared) %>%
  rename(family.fixed.effects.model = r.squared)

table_2 <- summary.fits.account %>%
  select(outcome, r.squared) %>%
  rename(accounts.fixed.effects.model = r.squared)

table_to_print <- left_join(table_1,
                            table_2,
                            by = "outcome")

for (row in 1:6) {
  table_to_print[row, "outcome"] <- clean_outcome_label(table_to_print[row, "outcome"])
}

table_to_print$family.fixed.effects.model <- round(table_to_print$family.fixed.effects.model, 5)
table_to_print$accounts.fixed.effects.model <- round(table_to_print$accounts.fixed.effects.model, 5)

print("This table appears in the Supporting Materials:")
stargazer(table_to_print, summary = FALSE)
