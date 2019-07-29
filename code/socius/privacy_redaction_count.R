# privacy_redaction_count.R
# Author: Ian Lundberg (minor edits by Alex Kindel)
# Reads the Challenge file and raw FFCWS data files, then
#  reports proportion missing increase in Challenge.

library(readstata13)  # version 0.9.2
library(readr)  # version 1.1.1
library(tidyverse)  # version 1.2.1
library(foreign)  # version 0.8-71
library(here)

# Note: In the current version of tidyverse, rename_() is deprecated
# This produces a warning, but we want to still use it
# to keep the construction of the data exactly as it was in the Challenge 
# (at which point rename_() was not deprecated)
# This happens when we load the raw data partway through this code

# Set directories
# raw_data_location must contain all raw FF files, including some subdirectories
data.dir <- file.path(here(), "data")
private.data.dir <- file.path(data.dir, "private")
raw.data.dir <- file.path(private.data.dir, "all_public_data")
results.dir <- file.path(here(), "results", "socius")

# Write all results to a file
sink(file.path(results.dir, "privacy_redaction_count.txt"))

# Function to accept a data frame and determine columns that
# have more than one unique value (including NA as a value)
determine_number_valid <- function(data) {
  # Determine columns that are constant
  all_same <- apply(data,2,function(x) all(is.na(x)) | (!any(is.na(x)) & length(unique(x)) == 1))
  # Store the number of constant columns
  num_constant <- sum(all_same)
  # The number of valid columns is the total, minus constant columns, minus 1 for the challengeID column
  num_valid <- ncol(data) - num_constant - 1
  return(num_valid)
}

# Load the Challenge data
dta_version <- read.dta13(file.path(private.data.dir, "background.dta"), convert.factors = F)

# Load the raw files as they were when we built the Challenge file
read.dta(file.path(raw.data.dir, "ff_pub_merge2.dta"), convert.factors = F) %>%  # Load core-merged file
  # Merge year 9 data
  left_join(read.dta(file.path(raw.data.dir, "ff_y9_pub1.dta"), convert.factors = F),
            by = "idnum") %>%
  # Merge year 3 in-home interview data (add hv3 prefix)  
  left_join(read.dta(file.path(raw.data.dir, "InHome3yr.dta"), convert.factors = F) %>%
              rename_(.dots = setNames(names(.)[-1], paste0("hv3", names(.)[-1]))),  
            by = "idnum") %>%
  # Merge year 5 in-home interview data (add hv4 prefix)
  left_join(read.dta(file.path(raw.data.dir, "Inhome5yr2011_stata", "inhome5yr2011.dta"), convert.factors = F) %>%
              rename_(.dots = setNames(names(.)[-1], paste0("hv4", names(.)[-1]))),
            by = "idnum") %>%
  # Merge kindergarten teacher survey
  left_join(read.dta(file.path(raw.data.dir, "ff_kteachersurvey_fnlpub.dta"), convert.factors = F),
            by = "idnum") %>%
  # Merge several child care files (http://fragilefamilies.princeton.edu/sites/fragilefamilies/files/ff_cc_notes.pdf)
  left_join(read.dta(file.path(raw.data.dir, "Stata", "ffcc_pof_fnlpub.dta"), convert.factors = F),
            by = "idnum") %>%  # Post Observation Form
  left_join(read.dta(file.path(raw.data.dir, "Stata", "ffcc_centsurvey_fnlpub.dta"), convert.factors = F),
            by = "idnum") %>%  # Center Provider Interview
  left_join(read.dta(file.path(raw.data.dir, "Stata", "ffcc_famsurvey_fnlpub.dta"), convert.factors = F),
            by = "idnum") %>%  # Family Child Care/Kith & Kin Interview
  left_join(read.dta(file.path(raw.data.dir, "Stata", "ffcc_famobs_fnlpub.dta"), convert.factors = F),
            by = "idnum") %>%  # Family Day Care Rating Scale
  left_join(read.dta(file.path(raw.data.dir, "Stata", "ffcc_centobs_fnlpub.dta"), convert.factors = F),
            by = "idnum") %>%  # Center Scale
  # Finally, remove the two cities that were not part of the Challenge
  filter(cm1twoc != 1) ->
  raw_version

# Compare the raw and Challenge files
print("Columns in original files but not Version 2")
print(paste(sum(!colnames(raw_version) %in% colnames(dta_version)), "columns:"))
print(colnames(raw_version)[!colnames(raw_version) %in% colnames(dta_version)])

print("Columns in Version 2 but not original files")
print("These include scales that we created")
print(paste(sum(!colnames(dta_version) %in% colnames(raw_version)), "columns:"))
print(colnames(dta_version)[!colnames(dta_version) %in% colnames(raw_version)])

# Sort the raw file by challengeID, to match the Challenge file
original_in_order <- raw_version %>%
  left_join(read_csv(file.path(private.data.dir, "idLinkage.csv")),
            by = "idnum") %>%
  arrange(challengeID)

# Swiss cheese number: What proportion of entries go from non-missing to missing in redactions?
shared_columns <- intersect(colnames(raw_version), colnames(dta_version))
redacted_shared <- dta_version[,shared_columns]
original_shared <- original_in_order[,shared_columns]

# How many entries are different?
# This is more than the # redacted since some are not redacted but have noise added
num_different <- sum(redacted_shared != original_shared)

# Function to determine if an observation is missing
is_missing <- function(x) is.na(x) | x %in% c("<NA>","NA",paste0("-",1:9),"Missing")

# Apply that to produce matrices of missingness
missing_redacted <- redacted_shared %>%
  mutate_all(.funs = is_missing)
missing_original <- original_shared %>%
  mutate_all(.funs = is_missing)

# Note the number that we made missing
we_made_missing <- sum(missing_redacted & !missing_original)
num_cells <- prod(dim(dta_version %>% select(-challengeID)))
we_made_missing_prop <- we_made_missing / num_cells
print(paste("Out of",num_cells,"cells, we made",
            we_made_missing," / ",num_cells," = ",
            round(we_made_missing / num_cells,2),
            "missing in redactions."))

# Close the sink
sink()