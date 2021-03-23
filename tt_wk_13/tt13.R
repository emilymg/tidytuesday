library(tidytuesdayR)
library(tidyverse)

#Import tidytuesday data
tt_data <- tt_load("2021-03-23")

#Assign each table to an object
unvotes <- tt_data$unvotes
roll_calls <-tt_data$roll_calls
issues <- tt_data$issues

#join unvotes and issues by rcid: votes_issues
votes_issues <- unvotes %>%
  inner_join(issues, by = "rcid")
#join votes_issues and roll_calls by rcid: votes_issues_roll
votes_issues_roll <- votes_issues %>%
  inner_join(roll_calls, by = "rcid")

#create a year column by adding 1945 to session col: unvotes_full
#recode vote col as an integer: $vote_code
unvotes_full <- votes_issues_roll %>%
  mutate(year = session + 1945) %>%
  mutate(vote_code = recode(vote,
                            "no" = 0,
                            "yes" = 1,
                            "abstain" = 2)) 

#Summarize percent of yes votes by year and country
by_year_country <- unvotes_full %>%
  group_by(year, country) %>%
  summarize(total = n(),
            percent_yes = mean(vote_code == 1)) 

#Filter for specific countries: us_fr_india
us_fr_india <- by_year_country %>%
  filter(country %in% c("United States", "India", "France"))

#Plot percent_yes by year for the three countries
ggplot(us_fr_india, aes(x = year, y = percent_yes, color = country)) +
  geom_line() +
  ylab("% of votes that are 'Yes'") 

