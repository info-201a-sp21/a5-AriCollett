
#clear history
rm(list = ls())

# load and install packages
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(leaflet)
library(lintr)

# load data
mass_shooting_df <- read.csv("./data/shootings-2018.csv",
                             stringsAsFactors = FALSE)

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
avg_lives_lost_city_table <-
  kable(avg_lives_lost_city, col.names = c("City Name",
                                           "Avg Number of Lives Lost"),
               caption =  "The Average Number of Lives Lost Per City")

# Description of a particular incident:
# provide your reader with relevant information from the dataset, such as the
# date and location of the incident, as well as the number of people impacted.
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

# An interactive map that shows a marker at the location of each shooting.
mass_shooting_df <- mass_shooting_df %>%
  mutate(total_impacted = num_killed + num_injured)

interactive_map <- leaflet(data = mass_shooting_df) %>%
  addTiles() %>%
  addCircleMarkers(
    lat = mass_shooting_df$lat,
    lng = mass_shooting_df$long,
    popup = ~paste(
      paste("<b>", "Date:", "</b>", date()),
      paste("<b>",  "Total Impacted:", "<b>", total_impacted),
      paste("<b>", "City:", city),
      sep = "<br/>"),
    radius = mass_shooting_df$total_impacted
  )

# bar chart that shows the number of shootings per state in 2018
shootings_per_state <- mass_shooting_df %>%
  group_by(state) %>%
  summarise(n = n())

shootings_bar_chart <- ggplot(shootings_per_state, aes(x = state, y = n,
                                                       fill = cond)) +
  geom_bar(stat = "identity", fill = "#56B4E9", colour = "white") +
  coord_flip()
shootings_bar_chart

lint("analysis.R")
