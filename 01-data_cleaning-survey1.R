#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from: 
# https://www.voterstudygroup.org/publication/nationscape-data-set
# Author: Elric Lazaro
# Data: 27 October 2020
# Contact: elric.lazaro@mail.utoronto.ca 
# License: MIT


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("C:/Users/elric/OneDrive - University of Toronto/University 2020-2021/Fall/STA304/Problem Sets/Problem Set 3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("inputs/ns20200625/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(vote_intention,
         vote_2020,
         gender,
         race_ethnicity,
         household_income,
         state)


# Remove NAs
reduced_data <- reduced_data %>% drop_na(vote_intention) %>%
                drop_na(vote_2020) %>%
                drop_na(gender) %>%
                drop_na(race_ethnicity) %>%
                drop_na(household_income) %>%
                drop_na(state)

# Remove those not eligible or are not planning to vote
reduced_data <- 
  reduced_data %>% 
  filter(vote_intention == "Yes, I will vote")

# Re-categorize some of Asian/pacific Islander types 
# due to census data not containing all of them
reduced_data$race_ethnicity <- as.character(reduced_data$race_ethnicity)
reduced_data$race_ethnicity[reduced_data$race_ethnicity == 'Asian (Asian Indian)'] <- 'Other Asian or Pacific Islander'
reduced_data$race_ethnicity[reduced_data$race_ethnicity == 'Asian (Vietnamese)'] <- 'Other Asian or Pacific Islander'
reduced_data$race_ethnicity[reduced_data$race_ethnicity == 'Asian (Korean)'] = 'Other Asian or Pacific Islander'
reduced_data$race_ethnicity[reduced_data$race_ethnicity == 'Asian (Filipino)'] = 'Other Asian or Pacific Islander'
reduced_data$race_ethnicity[reduced_data$race_ethnicity == 'Asian (Other)'] = 'Other Asian or Pacific Islander'
reduced_data$race_ethnicity[reduced_data$race_ethnicity == 'Pacific Islander (Native Hawaiian)'] = 'Other Asian or Pacific Islander'
reduced_data$race_ethnicity[reduced_data$race_ethnicity == 'Pacific Islander (Other)'] = 'Other Asian or Pacific Islander'
reduced_data$race_ethnicity[reduced_data$race_ethnicity == 'Pacific Islander (Samoan)'] = 'Other Asian or Pacific Islander'
reduced_data$race_ethnicity[reduced_data$race_ethnicity == 'Pacific Islander (Guamanian)'] = 'Other Asian or Pacific Islander'

# Simplify the scale for household income to reduce amount of factors
reduced_data$household_income <- as.character(reduced_data$household_income)
reduced_data$household_income[reduced_data$household_income == '$125,000 to $149,999'] <- '$125,000 and above'
reduced_data$household_income[reduced_data$household_income == '$150,000 to $174,999'] <- '$125,000 and above'
reduced_data$household_income[reduced_data$household_income == '$175,000 to $199,999'] <- '$125,000 and above'
reduced_data$household_income[reduced_data$household_income == '$200,000 to $249,999'] <- '$125,000 and above'
reduced_data$household_income[reduced_data$household_income == '$250,000 and above'] <- '$125,000 and above'
reduced_data$household_income[reduced_data$household_income == '$15,000 to $19,999'] <- '$15,000 to $49,999'
reduced_data$household_income[reduced_data$household_income == '$20,000 to $24,999'] <- '$15,000 to $49,999'
reduced_data$household_income[reduced_data$household_income == '$25,000 to $29,999'] <- '$15,000 to $49,999'
reduced_data$household_income[reduced_data$household_income == '$30,000 to $34,999'] <- '$15,000 to $49,999'
reduced_data$household_income[reduced_data$household_income == '$35,000 to $39,999'] <- '$15,000 to $49,999'
reduced_data$household_income[reduced_data$household_income == '$40,000 to $44,999'] <- '$15,000 to $49,999'
reduced_data$household_income[reduced_data$household_income == '$45,000 to $49,999'] <- '$15,000 to $49,999'
reduced_data$household_income[reduced_data$household_income == '$50,000 to $54,999'] <- '$50,000 to $74,999'
reduced_data$household_income[reduced_data$household_income == '$55,000 to $59,999'] <- '$50,000 to $74,999'
reduced_data$household_income[reduced_data$household_income == '$60,000 to $64,999'] <- '$50,000 to $74,999'
reduced_data$household_income[reduced_data$household_income == '$65,000 to $69,999'] <- '$50,000 to $74,999'
reduced_data$household_income[reduced_data$household_income == '$70,000 to $74,999'] <- '$50,000 to $74,999'
reduced_data$household_income[reduced_data$household_income == '$75,000 to $79,999'] <- '$75,000 to $99,999'
reduced_data$household_income[reduced_data$household_income == '$80,000 to $84,999'] <- '$75,000 to $99,999'
reduced_data$household_income[reduced_data$household_income == '$85,000 to $89,999'] <- '$75,000 to $99,999'
reduced_data$household_income[reduced_data$household_income == '$90,000 to $94,999'] <- '$75,000 to $99,999'
reduced_data$household_income[reduced_data$household_income == '$95,000 to $99,999'] <- '$75,000 to $99,999'

# Columns for each vote intentions  
reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))
reduced_data<-
  reduced_data %>%
  mutate(vote_Biden = 
           ifelse(vote_2020=="Joe Biden", 1, 0))
reduced_data<-
  reduced_data %>%
  mutate(vote_undecided = 
           ifelse(vote_2020=="I am not sure/don't know", 1, 0))
reduced_data<-
  reduced_data %>%
  mutate(vote_someone_else = 
           ifelse(vote_2020=="Someone else", 1, 0))


# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "outputs/survey_data.csv")

