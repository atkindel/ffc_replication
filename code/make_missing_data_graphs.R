# Makes two plots about missing data in the challenge file
# Input: Challenge data file (background.csv format)
# Output: Graphs: missing_data_background_bar.pdf, missing_data_background_heatmap.pdf
# Written by Tom Hartshorne (slight modifications by Matt Salganik and Alex Kindel)
# Run time: About 5 minutes on a laptop (most time-consuming step is writing missing_data_background_heatmap.pdf to file)
# Note: This code assumes that everything that is not (NA, -1, -2, -3, -4, -5, -6, -7, -8, -9) is meaningful data
# Note: This code requires a lot of free memory (for writing file to disk), you might need to clear memory before running

library(tidyverse)
library(magrittr)
library(reshape2)
library(scales)
library(here)

# Set ggplot2 theme
theme_set(theme_bw())

# Set the data directory
data.dir <- file.path(here(), "data")
private.data.dir <- file.path(data.dir, "private")

# Set the results location
results.dir <- file.path(here(), "results", "figures")

# Read in background data
print("Preparing background data file...")
background <- read.csv(file.path(private.data.dir, "background.csv"))

# Prepare data (this takes a while):
#  1. Sort background data on missing data counts
#  2. Convert to long, retaining challengeID
#  3. Classify missingness of each value in data frame
background %<>%
  mutate(missing_count = apply(background, 1, function(x) sum(x %in% c(-9:-1, NA)))) %>%
  arrange(missing_count) %>%
  mutate(rn = row_number()) %>%  # Store an ID number for plotting in missingness order later
  select(-missing_count, -challengeID) %>%  # Drop count column
  melt(id = "rn") %>%
  mutate(missing_type = as.factor(ifelse(value %in% c(-9:-1, NA), "Missing", "Not missing")),
         value = ifelse(missing_type == "Not missing", 1, value))
print("Done preparing background data file.")

# Summarize missingness data; plot proportion by type
print("Plotting missingness proportions...")
background %>% 
  group_by(value) %>% 
  # Store missingness type frequencies, labels, fill colors
  summarize(freq = n() / nrow(.)) %>%
  rowwise() %>%
  mutate(value_label = as.factor(switch(as.character(value),
                                        "-1" = "-1 Refused",
                                        "-2" = "-2 Don't know",
                                        "-3" = "-3 Missing",
                                        "-4" = "-4 Multiple Ans",
                                        "-5" = "-5 Not Asked",
                                        "-6" = "-6 Skip",
                                        "-7" = "-7 N/A",
                                        "-8" = "-8 Out of Range",
                                        "-9" = "-9 Not in Wave",
                                        "NA" = "Missing (NAs)",
                                        "Not missing"))) %>%
  mutate(color = ifelse(value %in% c(-9:-1, NA), "grey", "blue")) %>%
  # Build barplot of frequencies
  ggplot(aes(x = value_label, y = freq, fill = color)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = scales::percent(freq)), 
            position = position_dodge(width = 1),
            hjust = -0.5) +
  # Adjust plot aesthetics
  scale_fill_manual("legend", values = c("grey" = "grey30", "blue" = "blue")) +
  scale_y_continuous(labels=scales::percent, limits=c(0, 0.45)) +
  theme(legend.position="none") +
  labs(x="Data entry",
       y="Percent of total data") +
  coord_flip() ->
  p1

# Plot heatmap of missingness by cell ("swiss cheese" plot)
print("Plotting missingness by cell...")
background %>%
  ggplot(aes(x=variable, y=rn, fill=missing_type)) +
  geom_raster() +
  scale_fill_manual(values=c("#8856a7", "#9ebcda")) +
  coord_fixed(ratio = 1) +
  labs(y="Family", title="Variable") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank()) ->
  p2
print("Done plotting.")

# Write plots to disk
print("Writing plots to disk...")
ggsave(plot = p1, filename = "s2b_missing_data_background_bar.pdf", path = results.dir, 
       device = "pdf", dpi = 300, width = 3, height = 4.25, units = "in")
ggsave(plot = p2, filename = "s2a_missing_data_background_heatmap.pdf", path = results.dir, 
       device = "pdf", dpi = 300, width = 4.25, height = 2.5, units = "in")
