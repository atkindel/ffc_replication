# participant_analysis.R
# Author: Alex Kindel
# Date: 17 July 2019
# Produces participant demographic plots.

require(tidyverse)
require(magrittr)
require(here)
require(reshape2)
require(stringr)

# Set directory information
data.dir <- file.path(here(), "data")
private.data.dir <- file.path(data.dir, "private")
results.dir <- file.path(here(), "results")

# Set ggplot2 theme
theme_set(theme_bw())

# Load participant survey
participants <- read.csv(file.path(data.dir, "application_data.csv")

# Pre-filter
participants %>%
  filter(Status == "IP Address") %>%
  slice(-c(1, 2, 18)) %>%  # Test responses
  filter(Finished == "TRUE") %>%
  filter(Q11 != "") ->  # Responses missing intake questions
  participants_clean

# Function to break responses out of factor jail
# For some reason this codes 0 as 2 (???)
rcde <- function(dfc) {
  levels(dfc) <- c("1", "0")
  as.integer(dfc)
}


# Extract reasons for participating data
participants_clean %>%
  select(starts_with("Q15")) %>%
  droplevels() %>%
  transmute_all(rcde) ->
  intentions
intentions <- abs(intentions - 2)  # Recoding, again

# Compute median # of options selected
it.rsum <- median(rowSums(intentions))

# Label responses
it.levels <- c("General interest in topic", "Curiosity about the challenge", "Relevant to own research",
               "Relevant to job", "Relevant to school or degree program", "Required for coursework", 
               "To improve the lives of disadvantaged children", "For fun", 
               "To experience a mass scientific collaboration", "Contributing to data science",
               "Contributing to social science", "Learning about cutting-edge research",
               "Learning/practicing data analysis skills", "To earn scholarly recognition",
               "To work on a prestigious research project", "To make the best-performing model in the challenge",
               "To create the most interesting/innovative model in the challenge",
               "Connecting with others who share my interests", "Collaborating with strangers",
               "Collaborating with colleagues/friends", "Collaborating with university researchers")
colnames(intentions) <- it.levels

# Construct bar plot
intentions %>%
  melt() %>%
  filter(value == 1) %>%
  group_by(variable) %>%
  summarize(n = n()/nrow(intentions)) %>%
  ggplot(aes(x=reorder(variable, n), y=n)) +
  geom_bar(stat="identity") + 
  scale_y_continuous(labels = scales::percent, limits=c(0, 1), expand=c(0, 0.005)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  coord_flip() +
  theme(axis.text.x = element_text(hjust=1)) +
  labs(title="Reasons for participating",
       y="Proportion",
       x="Reason") +
  ggsave(file.path(results.dir, "figures", "s5_participant_intentions.pdf"),
         height = 8, width = 6)


# Extract background data
participants_clean %>%
  select(starts_with("Q16")) %>%
  droplevels() %>%
  transmute_all(rcde) ->
  backgrounds
backgrounds <- abs(backgrounds - 2)  # Recoding, again

# Compute mean # of options selected
bg.rsum <- median(rowSums(backgrounds))

# Label responses
bg.levels <- c("Sociologist", "Economist", "Psychologist", "Political scientist", "Demographer",
               "Data scientist", "Computer scientist", "No academic background", "Other")
colnames(backgrounds) <- bg.levels

# Construct bar plot
backgrounds %>%
  melt() %>%
  filter(value == 1) %>%
  group_by(variable) %>%
  mutate(count=n()) %>%
  ggplot(aes(x=reorder(variable, -count))) +
  geom_bar() + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  labs(title="Academic background",
       y="N",
       x="Discipline") +
  ggsave(file.path(results.dir, "figures", "s4a_participant_disciplines.pdf"),
         height = 4, width = 6.5)


# Extract employment data
participants_clean %>%
  select(starts_with("Q18")) %>%
  droplevels() %>%
  transmute_all(rcde) ->
  employment
employment <- abs(employment - 2)  # Recoding, again

# Compute mean # of options selected
ep.rsum <- median(rowSums(employment))

# Label responses
ep.levels <- c("Industry", "Academia", "Government", "Nonprofit", "Undergraduate", "Graduate student", "Unemployed", "Other")
colnames(employment) <- ep.levels

# Bar plot
employment %>%
  melt() %>%
  filter(value == 1) %>%
  group_by(variable) %>%
  summarize(n = n()) %>%
  ggplot(aes(x=reorder(variable, -n), y=n)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  labs(title="Professional background",
       y="N",
       x="Type") +
  ggsave(file.path(results.dir, "figures", "s4b_participant_professions.pdf"),
         height = 4, width = 6.5)
