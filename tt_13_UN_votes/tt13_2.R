library(tidyverse)

# Explore the data
glimpse(unvotes_full)
unique(unvotes_full$issue)
# Change data type of issue column from character to factor
unvotes_full$issue <- as.factor(unvotes_full$issue)

# Merge using base R instead of dplyr. roll_calls and issues:  roll_issue
roll_issue <- merge(roll_calls, issues, by = "rcid", all = FALSE)
glimpse(roll_issue)

# Merge using base R instead of dplyr. roll_issue and unvotes:  unvotes_full3
unvotes_full3 <- merge(unvotes, roll_issue, by = "rcid", all = FALSE)
glimpse(unvotes_full3)

# How many votes are important according to the US State Dept? (Classification only began with session 38)
n_imp <- unvotes_full3 %>%
  group_by(importantvote) %>%
  summarise(n = n())

# Filter for important votes only
important_only <- unvotes_full3 %>%
  filter(importantvote == 1) %>%
  # Add year column by adding 1945 to session (session 1 in 1946)
  mutate(year = session + 1945) %>%
  # Add vote_code column to code yes votes as integers
  mutate(vote_code = recode(vote,
                            "no" = 0,
                            "yes" = 1,
                            "abstain" = 2)) 

glimpse(important_only)

# Plot percent_yes votes for important votes only
p_imp_by_issue <- important_only %>%
  group_by(year, issue) %>%
  filter(issue != "NA") %>%
  summarize(total = n(),
            percent_yes = mean(vote_code == 1) * 100) %>%
    ggplot(aes(x = year, y = percent_yes)) +
      geom_point() +
      geom_smooth() +
      facet_wrap(~issue) +
      ylim(0, 100) +
      ggtitle("UN 'Yes' Votes by Issue - important votes only") +
      ylab("Percent 'Yes' Votes")

p_imp_by_issue

