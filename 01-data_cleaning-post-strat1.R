#### Preamble ####
# Purpose: Prepare and clean the census data downloaded from usa.ipums.rog
# Author: Elric Lazaro
# Data: 27 October 2020
# Contact: elric.lazaro@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("C:/Users/elric/OneDrive - University of Toronto/University 2020-2021/Fall/STA304/Problem Sets/Problem Set 3")
raw_data <- read_dta("inputs/usa_00003.dta")

# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% select(stateicp, sex, age, race, inctot)
# Identity of each person summarized and will be used for counting

# rm(raw_data)         

# Only ages 18 or older are allowed to vote
reduced_data$age <- as.integer(reduced_data$age)
reduced_data <- 
  reduced_data %>% 
  filter(age >= 18)

# Remove any NAs
reduced_data <- reduced_data %>% 
  drop_na(stateicp) %>%
  drop_na(sex) %>%
  drop_na(age) %>% 
  drop_na(race) %>% 
  drop_na(inctot)

# No negative income in survey data
new_data <- 
  reduced_data %>% 
  filter(inctot >= 0)
# IPUMS states that '9999999 = N/A'
new_data <- 
  new_data %>% 
  filter(inctot != 9999999)

# Add household_income catgeory that matches survey
new_data <- new_data %>% mutate(household_income = '')
new_data$household_income[new_data$inctot <= 14999] = 'Less than $14,999'
new_data$household_income[new_data$inctot >= 15000 & new_data$inctot <= 49999] = '$15,000 to $49,999'
new_data$household_income[new_data$inctot >= 50000 & new_data$inctot <= 74999] = '$50,000 to $74,999'
new_data$household_income[new_data$inctot >= 75000 & new_data$inctot <= 99999] = '$75,000 to $99,999'
new_data$household_income[new_data$inctot >= 100000 & new_data$inctot <= 124999] = '$100,000 to $124,999'
new_data$household_income[new_data$inctot >= 125000] = '$125,000 and above'


# Get state code from state name
new_data$stateicp <- str_to_title(new_data$stateicp)
new_data <- new_data %>% mutate(state = state.abb[match(new_data$stateicp, state.name)])
new_data <- new_data %>% drop_na(state) # drop district of columbia

# Capitalize gender
new_data$sex <- str_to_title(new_data$sex)

# Match race category with survey
new_data$race <- str_to_title(new_data$race)
new_data$race[new_data$race == 'Black/African American/Negro'] = 'Black, or African American'
new_data$race[new_data$race == 'Other Race, Nec'] = 'Some other race'
new_data$race[new_data$race == 'Two Major Races'] = 'Some other race'
new_data$race[new_data$race == 'Three Or More Major Races'] = 'Some other race'
new_data$race[new_data$race == 'Japanese'] = 'Asian (Japanese)'
new_data$race[new_data$race == 'American Indian Or Alaska Native'] = 'American Indian or Alaska Native'
new_data$race[new_data$race == 'Chinese'] = 'Asian (Chinese)'
new_data$race[new_data$race == 'Other Asian Or Pacific Islander'] = 'Other Asian or Pacific Islander'

# Ages 94 and over are not in survey data
new_data <- 
  new_data %>% 
  filter(age <= 93)

# Rename columns
new_data <- new_data %>% rename(race_ethnicity = race,
                    gender = sex)
# Group the data (also remove age - too expensive)
grouped_data <- new_data %>% select(state,
                                gender,
                                race_ethnicity,
                                household_income)
grouped_data <- grouped_data %>% group_by_all() %>% summarise(COUNT = n())

# Saving the census data as a csv file in my
# working directory
write_csv(grouped_data, "outputs/census_data.csv")
