library(tidytuesdayR)
library(tidyverse)

# Import tidytuesday data
tt_data <- tt_load("2021-03-23")

# Assign each table to an object
unvotes <- tt_data$unvotes
roll_calls <-tt_data$roll_calls
issues <- tt_data$issues

# Examine data structure
glimpse(unvotes)
glimpse(roll_calls)
glimpse(issues)

# Join unvotes and issues by rcid column: votes_issues
votes_issues <- unvotes %>%
  inner_join(issues, by = "rcid")
# Join votes_issues and roll_calls by rcid column: votes_issues_roll
votes_issues_roll <- votes_issues %>%
  inner_join(roll_calls, by = "rcid")

# Create a year column by adding 1945 to session column (first session was 1946): unvotes_full
# Note:  Could also have created a year column by extracting year from date
unvotes_full <- votes_issues_roll %>%
  mutate(year = session + 1945) %>%
  # Recode vote column as an integer to calculate percentage of 'yes' votes: new column 'vote_code'
    mutate(vote_code = recode(vote,
                            "no" = 0,
                            "yes" = 1,
                            "abstain" = 2)) 

#Summarize percent 'yes' votes by year and country
by_year_country <- unvotes_full %>%
  group_by(year, country) %>%
  summarize(total = n(),
            percent_yes = mean(vote_code == 1) * 100) 

#Filter for specific countries: us_fr_india
us_fr_india <- by_year_country %>%
  filter(country %in% c("United States", "India", "France"))

#Plot percent_yes by year for the three countries
p_us_fr_india <- ggplot(us_fr_india, aes(x = year, y = percent_yes, color = country)) +
  geom_line() +
  ggtitle("Percent 'Yes' Votes by US, France, and India") +
  ylab("% 'Yes' Votes") 
p_us_fr_india

# Plot percent_yes votes faceted by issue
p_yesVotes_byIssue <- unvotes_full %>%
  group_by(year, issue) %>%
  summarize(total = n(),
            percent_yes = mean(vote_code == 1) * 100) %>%
  ggplot(aes(x = year, y = percent_yes)) +
  geom_point() +
  facet_wrap(~issue) +
  geom_smooth() +
  ggtitle("UN 'Yes' Votes by Issue") +
  xlab("Year") +
  ylab("Percent 'Yes' Votes")
p_yesVotes_byIssue
