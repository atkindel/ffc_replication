#!/bin/sh
# Measuring the Predictability of Life Outcomes with a Scientific Mass Collaboration   
# Script to build results (figures, tables) from scratch

# Write log 
rm -f log.txt
touch log.txt
echo "Log from run: $(date)" >> log.txt

# Ian: Transforms all submissions into a single data file
Rscript ./subset_submissions.R >> log.txt

# Ian: Generates several sets of figures:
#  - Main paper figure 2 (main challenge results)
#  - SM figure S3 (training data distributions)
#  - SM figures S7a-d; S8-S11 (performance benchmarks and uncertainty)
#  - SM figures S12a-f (team scores by outcome)
#  - SM figure S13 (out-of-sample error estimates)
#  - SM figure S14 (alternative confidence intervals)
Rscript ./science_figures.R >> log.txt

# Matt: Generates SM figures S2a, S2b (missing data)
Rscript ./make_missing_data_graphs.R >> log.txt

# Alex: Generates SM figures S4a, S4b, S5 (participant demographics)
Rscript ./participant_analysis.R >> log.txt

# Alex: Generates SM figures S17, S18, 19 (code submission analysis)
Rscript ./code_analysis.R >> log.txt