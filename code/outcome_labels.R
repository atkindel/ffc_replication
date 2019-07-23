# Function to replace outcome variable names with English language equivalents

clean_outcome_label <- function(outcome_label) {
  pretty_outcome_label <- switch(outcome_label,
                                 materialHardship = "Material hardship",
                                 gpa = "GPA",
                                 grit = "Grit",
                                 layoff = "Layoff",
                                 eviction = "Eviction",
                                 jobTraining = "Job training",
                                 outcome_label)
  return(pretty_outcome_label)
}