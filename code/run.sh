#!/bin/sh
# Measuring the Predictability of Life Outcomes with a Scientific Mass Collaboration   
# Script to build results (figures, tables) from scratch

# Write a new log 
rm -f log.txt
touch log.txt
echo "Log from run: $(date)" >> log.txt


## Data preparation ##

# Ian: Transforms all submissions into a single data file
Rscript ./subset_submissions.R >> log.txt

# Matt: Transforms Ian submission file into working data frames
Rscript ./prep_predictions.R >> log.txt

# Alex: Performs data preparation for code analysis (experimental)
# NOTE: Requires python3 and dependencies specified in requirements.txt
# This generates ffc_code.csv, but may be buggy depending on your setup, so we don't run it by default
# bash ./prep_data.sh >> log.txt
# pip3 install -r requirements.txt
# yes | python3 ./code_analysis.py >> log.txt

## Reproduce main paper and supplementary material (SM) figures ##

# Ian: Generates several figures:
#  - Main paper figure 2 (main challenge results)
#  - SM figure S3 (training data distributions)
#  - SM figures S7a-d; S8-S11 (performance benchmarks and uncertainty)
#  - SM figures S12a-f (team scores by outcome)
#  - SM figure S13 (out-of-sample error estimates)
Rscript ./science_figures.R >> log.txt

# Matt: Generates main paper figure 3 (prediction heatmaps)
#  Also outputs SM table S6 (model fit for models of prediction error by account, observation)
Rscript ./heatmaps.R >> log.txt

# Matt: Generates SM figures S2a, S2b (missing data)
Rscript ./make_missing_data_graphs.R >> log.txt

# Alex: Generates SM figures S4a, S4b, S5 (participant demographics)
Rscript ./participant_analysis.R >> log.txt

# Matt: Generates SM figure S14 (prediction difficulty in extreme cases)
Rscript ./hard_to_predict_cases.R >> log.txt

# Matt: Generates SM figure S15 (prediction difficulty across outcomes within families)
Rscript ./difficulty_across_outcomes.R >> log.txt

# Alex: Generates SM figures S16, S17, S18 (code submission analysis)
Rscript ./code_analysis.R >> log.txt

# Matt: Generates SM figure S19 (prediction distances compared to truth)
Rscript ./compare_predictions_truth.R >> log.txt

# Ian: Generates privacy redaction counts
Rscript ./privacy_redaction_count.R >> log.txt


echo "Replication code run complete." >> log.txt
echo "" >> log.txt