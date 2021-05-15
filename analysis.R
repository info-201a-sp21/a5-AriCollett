
#clear history
rm(list = ls())

# load and install packages
library(dplyr)
library(ggplot2)
library(plotly)
library(lintr)
library(knitr)

# load data
mass_shooting_df <- read.csv("./data/shootings-2018.csv", stringsAsFactors = FALSE)

# summary information
number_shootings <- nrow(mass_shooting_df)

num_lives_lost <- sum(mass_shooting_df$num_killed)

most_impacted_city <- mass_shooting_df %>%
  mutate(number_impacted = num_killed + num_injured) %>%
  group_by(city) %>%
  summarize(number_impacted = sum(number_impacted)) %>%
  filter(number_impacted == max(number_impacted)) %>%
  pull(city)

most_impacted_state <- mass_shooting_df %>%
  mutate(number_impacted = num_killed + num_injured) %>%
  group_by(state) %>%
  summarize(number_impacted = sum(number_impacted)) %>%
  filter(number_impacted == max(number_impacted)) %>%
  pull(state)

avg_lives_lost <- mean(mass_shooting_df$num_killed)
 
# summary table
avg_lives_lost_city <- mass_shooting_df %>%
  group_by(city) %>%
  summarise(num_killed = mean(num_killed)) %>%
  arrange(-num_killed)
av_lives_lost_city_table <- 
  kable(avg_lives_lost_city, col.names = c("City Name", "Avg Number of Lives Lost"), 
               caption =  "The Average Number of Lives Lost Per City")

# Description of a particular incident
# provide your reader with relevant information from the dataset, such as the 
# date and location of the incident, as well as the number of people impacted (injured, killed).

max_killed <- mass_shooting_df %>%
  filter(num_killed == max(num_killed)) %>%
  pull(num_killed)

date_max_killed <- mass_shooting_df %>%
  filter(num_killed == max_killed) %>%
  pull(date)

city_max_killed <- mass_shooting_df %>%
  filter(num_killed == max_killed) %>%
  pull(city)

state_max_killed <- mass_shooting_df %>%
  filter(num_killed == max_killed) %>%
  pull(state)

number_injured_max_killed <- mass_shooting_df %>%
  filter(num_killed == max_killed) %>%
  pull(num_injured)





  