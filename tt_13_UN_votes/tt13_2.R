library(tidyverse)
glimpse(unvotes_full)
unique(unvotes_full$issue)
distinct(as.factor(unvotes_full$issue))

unvotes_full %>%
  group_by(year, issue) %>%
  summarize(total = n(),
            percent_yes = mean(vote_code == 1)) %>%
  ggplot(aes(x = year, y = percent_yes)) +
    geom_point() +
    facet_wrap(~issue) +
    geom_smooth()  

#Assign each table to an object
unvotes <- tt_data$unvotes
roll_calls <-tt_data$roll_calls
issues <- tt_data$issues

tail(unvotes)
tail(issues)
tail(roll_calls)

glimpse(unvotes)
glimpse(roll_calls)
glimpse(issues)

roll_issue <- merge(roll_calls, issues, by = "rcid", all = TRUE)
tail(roll_issue)

unvotes_full3 <- merge(unvotes, roll_issue, by = "rcid", all = TRUE)
unvotes_full3 <- unvotes_full3 %>% 
  mutate(year = session + 1945) %>%
  mutate(vote_code = recode(vote,
                            "no" = 0,
                            "yes" = 1,
                            "abstain" = 2))
p_by_issue <- unvotes_full3 %>%
  group_by(year, issue) %>%
  filter(issue != "NA") %>%
  mutate(percent_yes = mean(vote == "yes")) %>%
    ggplot(aes(x = year, y = percent_yes)) +
      geom_point() +
      facet_wrap(~issue) +
      geom_smooth(method = "gam")  

p_by_issue

n_imp <- unvotes_full3 %>%
  group_by(importantvote) %>%
  summarise(n = n())

important_only <- unvotes_full3 %>%
  filter(importantvote == 1)

glimpse(important_only)

p_imp_by_issue <- important_only %>%
  group_by(year, issue) %>%
  filter(country == "United States" & issue != "NA") %>%
  mutate(percent_yes = mean(vote == "yes")) %>%
    ggplot(aes(x = year, y = percent_yes)) +
      geom_() +
      facet_wrap(~issue) +
      geom_smooth(method = "gam") +
  xlim(xmin, xmax) +  
  ylim(0,1) 
p_imp_by_issue

diff_joins <- anti_join(unvotes_full3, unvotes_full)
