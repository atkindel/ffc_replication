#!/bin/sh
# Script to build results (figures, tables) for introduction to Socius special collection

# Write a new log 
rm -f log.txt
touch log.txt
echo "Log from run: $(date)" >> log.txt

# Produces missing data summary plots in Figure 3
Rscript ./make_missing_data_graphs.R >> log.txt

# Produces the table showing the number of non-missing cases for each outcome in the training, leaderboard, and holdout sets.
Rscript ./num_nonmissing_cases.R >> log.txt

# Reports the percent of entries made missing in our privacy and ethics audit.
# We also cite this number in the community paper.
Rscript ./privacy_redaction_count.R >> log.txt

# Produces the 3D tree visualization (Figure 5).
Rscript ./tree_plot.R >> log.txt

# Produces the empirical CDF of R^2 for each outcome and data set (Figure 6).
# Also produces the scatterplot of R^2_Training and R^2_Holdout (Figure 7).
Rscript ./r2_allSubmission_plots.R >> log.txt