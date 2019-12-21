README for ffc_replication/data

submissions.csv: This contains all predictions submitted to the Challenge.

private/background.dta: This is the background data file of predictors as shared with Challenge participants.

private/train.csv: This is the 6 outcomes for the training cases, as shared with Challenge participants.

private/test.csv: This is the 6 outcomes for the test cases, as used to evaluate Challenge submissions.

private/leaderboardUnfilled.csv: This contains the 6 outcomes for all leaderboard cases, with missing values as NA.

private/leaderboard.csv: This contains the 6 outcomes for all leaderboard cases, with missing values imputed by random draws from observed cases.

private/FF_Y15_pub.dta: This is the year 15 data file from the Fragile Families study.

private/idLinkage.csv: This links the public idnum of the Fragile Families study to the challengeID that was used in the files shared with participants.

leaderboard_nodupes.zip: This file contains all submissions on the leaderboard at the end of the Challenge, with duplicate submissions (same person but multiple accounts) removed to have no more than one submission per team.

r_squared_all.csv: This file contains R^2 for each team-outcome combination.

application_data.csv: This file contains data from applications to participate in the Challenge.

author_methods.csv: This file contains narrative descriptions that participants provided for their submissions.

methods_responses_clean.csv: This file contains participant responses to a survey about the methods they used.

ffc_code.csv: This file contains serialized function call lists from each script submitted to the Challenge.

ffc_model_functions_coded.csv: This file contains all model functions called in ffc_code.csv; functions are labeled by learning algorithm type.

ffc_symbols_coded.csv: This file contains a list of all functions called in ffc_code.csv; functions are labeled by data pipeline role (preparation, modeling, visualization)