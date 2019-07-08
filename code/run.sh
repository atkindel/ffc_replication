#!/bin/sh
# Measuring the Predictability of Life Outcomes with a Scientific Mass Collaboration   
# Script to build results (figures, tables)

# Ian: Transforms all submissions into a single data file
Rscript ./subset_submissions.R

# Ian: Generates tables/figures from main paper and supplemental materials
Rscript ./science_figures.R