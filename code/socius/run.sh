#!/bin/sh
# Script to build results (figures, tables) for introduction to Socius special collection

# Write a new log 
rm -f log.txt
touch log.txt
echo "Log from run: $(date)" | tee -a log.txt

# Produces missing data summary plots in Figure 3
echo "BEGIN make_missing_data_graphs.R" | tee -a log.txt
Rscript ./make_missing_data_graphs.R | tee -a log.txt
echo "END make_missing_data_graphs.R" | tee -a log.txt

# Produces the table showing the number of non-missing cases for each outcome in the training, leaderboard, and holdout sets.
echo "BEGIN num_nonmissing_cases.R" | tee -a log.txt
Rscript ./num_nonmissing_cases.R | tee -a log.txt
echo "END num_nonmissing_cases.R" | tee -a log.txt

# Reports the percent of entries made missing in our privacy and ethics audit.
# We also cite this number in the community paper.
echo "BEGIN privacy_redaction_count.R" | tee -a log.txt
Rscript ./privacy_redaction_count.R | tee -a log.txt
echo "END privacy_redaction_count.R" | tee -a log.txt

# Produces the 3D tree visualization (Figure 5).
# Run this script manually to generate the tree plot (must be manually saved)
# echo "BEGIN tree_plot.R" | tee -a log.txt
# Rscript ./tree_plot.R | tee -a log.txt
# echo "END tree_plot.R" | tee -a log.txt

# Ensure leaderboard data is decompressed
rm -rf /ffc_replication/data/leaderboard_nodupes/
yes | unzip /ffc_replication/data/leaderboard_nodupes.zip

# Produces the empirical CDF of R^2 for each outcome and data set (Figure 6).
# Also produces the scatterplot of R^2_Training and R^2_Holdout (Figure 7).
echo "BEGIN r2_allSubmission_plots.R" | tee -a log.txt
Rscript ./r2_allSubmission_plots.R | tee -a log.txt
echo "END r2_allSubmission_plots.R" | tee -a log.txt

rm -f Rplots.pdf