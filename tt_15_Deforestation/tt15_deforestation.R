library(colorRamps)
library(tidytuesdayR)
library(tidyverse)

install.packages("ggstream")
library(ggstream)

# Import and save the data
tues_15 <- tt_load("2021-04-06")

brazil_loss <- tues_15$brazil_loss
forest <- tues_15$forest
forest_area <- tues_15$forest_area
soybean <- tues_15$soybean_use
veg_oil <- tues_15$vegetable_oil

glimpse(veg_oil)

# Part 1: Plot top palm oil producers

# Change data types to factors
veg_oil$code <- as.factor(veg_oil$code)
veg_oil$crop_oil <- as.factor(veg_oil$crop_oil)
veg_oil$entity <- as.factor(veg_oil$entity)

# Filter veg_oil for all countries producing palm oil
palm <- veg_oil %>%
  group_by(entity) %>%
  filter(crop_oil == "Palm" & production != "NA") %>%
  as.data.frame()

glimpse(palm) 
  
# Filter by countries only, extract top 5
palm_top5 <- palm %>% 
  filter(!is.na(code)) %>%
  filter(entity != "World") %>%
  group_by(entity) %>%
  summarise(mean_prod = mean(production)) %>%
  slice_max(order_by = mean_prod, n = 5)

palm_top5

# Plot top 5 palm oil producers

# Join palm_top5 w/palm to extract yearly data for top 5 producers
palm_top5_yearly_prod <- right_join(palm, palm_top5, by = "entity")
# Create stream plot of production by year
p_palm <- ggplot(palm_top5_full, aes(x = year, y = production, fill = entity)) +
  geom_stream(type = "ridge") +
  scale_x_continuous(breaks = scales::breaks_extended(10)) +
  scale_y_continuous(labels = scales::comma)
# Add theme
p_palm + theme(panel.background = element_rect(fill = "grey26"),
               panel.grid = element_blank(),
               legend.title = element_blank())

# Part 2: Causes of deforestation in Brazil

# Make a long df
brazil_long <- pivot_longer(brazil_loss, cols = 4:14, names_to = "cause", values_to = "value")
glimpse(brazil_long)
# Remove hyphens in cause column, change data type to factor
brazil_long$cause <- str_replace_all(brazil_long$cause, "_", " ")
brazil_long$cause <- as.factor(brazil_long$cause)
# Rename value column to loss_hectares
names(brazil_long)[5] <- 'loss_hectares'
# Set color palette
pal <- colorRamps::matlab.like(11)

# Plot stream
p_brazil <- ggplot(brazil_long, aes(x = year, y = loss_hectares, fill = cause)) +
  geom_stream(extra_span = 0.01, type = "proportional") +
  scale_x_continuous(breaks = c(2002, 2004, 2006, 2008, 2010, 2012)) +
  scale_fill_manual(values = pal)
# Add theme and labels
p_brazil + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        legend.title = element_blank()) +
  labs(title = "Causes of Deforestation in Brazil, 2001-2013",
       caption = "Data Source: TidyTuesday 2021 week 15, Our World in Data, https://ourworldindata.org/palm-oil",
       x = NULL,
       y = "Proportion of total forest loss")



