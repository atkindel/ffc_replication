#!/bin/sh
# Measuring the Predictability of Life Outcomes with a Scientific Mass Collaboration   
# Script to build results (figures, tables) from scratch

# Write a new log 
rm -f log.txt
touch log.txt
echo "Log from run: $(date)" | tee log.txt

## Package installation ##
# These are pre-installed on the Docker image; uncomment if running on own machine
# echo "BEGIN install_requirements.R" | tee log.txt
# Rscript ./install_requirements.R | tee log.txt
# echo "END install_requirements.R" | tee log.txt


## Data preparation ##

# Ian: Transforms all submissions into a single data file
echo "BEGIN subset_submissions.R" | tee log.txt
Rscript ./subset_submissions.R | tee log.txt
echo "END subset_submissions.R" | tee log.txt

# Matt: Transforms Ian submission file into working data frames
echo "BEGIN prep_predictions.R" | tee log.txt
Rscript ./prep_predictions.R | tee log.txt
echo "END prep_predictions.R" | tee log.txt

# Alex: Performs data preparation for code analysis (experimental)
# NOTE: Requires python3 and dependencies specified in requirements.txt
# This generates ffc_code.csv, but may be buggy depending on your setup, so we don't run it by default
# Just use the included ffc_code.csv if you don't want to try running this
# bash ./prep_data.sh | tee log.txt
# pip3 install -r requirements.txt
# yes | python3 ./code_analysis.py | tee log.txt

## Reproduce main paper and supplementary material (SM) figures ##

# Ian: Generates several figures:
#  - Main paper figure 3 (main challenge results)
#  - SM figure S2 (training data distributions)
#  - SM figures S6a-d; S7-S10 (performance benchmarks and uncertainty)
#  - SM figure S11 (out-of-sample error estimates)
#  - SM figures S15a-f (team scores by outcome)
echo "BEGIN science_figures.R" | tee log.txt
Rscript ./science_figures.R | tee log.txt
echo "END science_figures.R" | tee log.txt

# Matt: Generates main paper figure 4 (prediction heatmaps)
#  Also outputs SM table S7 (model fit for models of prediction error by account, observation)
#  Generating all of the heatmaps at once requires ~6gb memory
echo "BEGIN heatmaps.R" | tee log.txt
Rscript ./heatmaps.R | tee log.txt
echo "END heatmaps.R" | tee log.txt

# Matt: Generates SM figures S1a, S1b (missing data)
#  Generating the heatmap in S1a requires ~14gb memory
echo "BEGIN make_missing_data_graphs.R" | tee log.txt
Rscript ./make_missing_data_graphs.R | tee log.txt
echo "END make_missing_data_graphs.R" | tee log.txt

# Alex: Generates SM figures S3a, S3b, S4 (participant demographics)
echo "BEGIN participant_analysis.R" | tee log.txt
Rscript ./participant_analysis.R | tee log.txt
echo "END participant_analysis.R" | tee log.txt

# Matt: Generates SM figure S13 (prediction difficulty in extreme cases)
echo "BEGIN hard_to_predict_cases.R" | tee log.txt
Rscript ./hard_to_predict_cases.R | tee log.txt
echo "END hard_to_predict_cases.R" | tee log.txt

# Matt: Generates SM figure S14 (prediction difficulty across outcomes within families)
echo "BEGIN difficulty_across_outcomes.R" | tee log.txt
Rscript ./difficulty_across_outcomes.R | tee log.txt
echo "END difficulty_across_outcomes.R" | tee log.txt

# Alex: Generates SM figures S16, S17, S18 (code submission analysis)
echo "BEGIN code_analysis.R" | tee log.txt
Rscript ./code_analysis.R | tee log.txt
echo "END code_analysis.R" | tee log.txt

# Matt: Generates SM figure S12 (prediction distances compared to truth)
echo "BEGIN compare_predictions_truth.R" | tee log.txt
Rscript ./compare_predictions_truth.R | tee log.txt
echo "END compare_predictions_truth.R" | tee log.txt

# Alex: Generates SM figures S19-22 (methods used in the Challenge)
echo "BEGIN methods_used.R" | tee log.txt
Rscript ./methods_used.R | tee log.txt
echo "END methods_used.R" | tee log.txt

# Ian: Generates privacy redaction counts
echo "BEGIN privacy_redaction_count.R" | tee log.txt
Rscript ./privacy_redaction_count.R | tee log.txt
echo "END privacy_redaction_count.R" | tee log.txt

# Produces table S4 showing the number of non-missing cases for each outcome in the training, leaderboard, and holdout sets.
echo "BEGIN num_nonmissing_cases.R" | tee log.txt
Rscript ./num_nonmissing_cases.R | tee log.txt
echo "END num_nonmissing_cases.R" | tee log.txt

# Remove Rplots.pdf
rm -f ./Rplots.pdf

echo "Replication code run complete." | tee log.txt
echo "" | tee log.txt