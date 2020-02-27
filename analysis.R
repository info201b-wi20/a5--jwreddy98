
library(dplyr)
library(knitr)
library(plotly)
library(tidyr)
library(leaflet)
library(ggplot2)
library(ggmap)
library(lubridate)

# Data load for shootings Summary
mass_shootings_data <- read.csv("data/shootings-2018.csv", 
                           stringsAsFactors = FALSE)

# How many shootings occured?
shootings_occured <- nrow(mass_shootings_data)

# How many lives were lost?
lives_lost_total <- sum(mass_shootings_data$num_killed)

# City impacted the most by shootings (most killed)
highest_impacted <- mass_shootings_data %>%
  group_by(city) %>%
  summarize(n = n()) %>%
  filter(n == max(n)) %>%
  pull(city)

# City with the highest number of casualties 
highest_casualties <- mass_shootings_data %>%
  mutate(casualty = sum(num_killed + num_injured)) %>%
  group_by(city) %>%
  summarize(n = n()) %>%
  filter(n == max(n)) %>%
  pull(city)

# State with the highest number of shootings
state_casualties <- mass_shootings_data %>%
  group_by(state) %>%
  summarize(n = n()) %>%
  filter(n == max(n)) %>%
  pull(state)

# Table summary, data is mutated for better data
data_shootings <- mutate(mass_shootings_data, month =
                           format(as.Date(mass_shootings_data$date,
                           format = "B% d%, Y%"),  "%B")) %>%
                          group_by(month)

summary_table <- data.frame(data_shootings$month,
                            data_shootings$num_killed,
                            data_shootings$num_injured,
                            data_shootings$state,
                            data_shootings$city)

names(summary_table) <- c("month", "num_killed", "num_injured",
                          "state", "city")

# Table cities with highest number of shootings, top 10
shootings_by_city <- summary_table %>%
  group_by(city) %>%
  count(sort = T) %>%
  rename(shootings = n) %>%
  ungroup() %>%
  top_n(10)

# Event
pompano_beach_killed <- mass_shootings_data %>%
  filter(city == "Pompano Beach (Parkland)") %>%
  pull(num_killed)
pompano_beach_injured <- mass_shootings_data %>%
  filter(city == "Pompano Beach (Parkland)") %>%
  pull(num_injured)
pompano_beach_date <- mass_shootings_data %>%
  filter(city == "Pompano Beach (Parkland)") %>%
  pull(date)
  
# Map 
color_palette <- colorFactor(palette = "Set3",
                             domain = mass_shootings_data$number_shootings)
mass_shootings_data$number_shootings <- cut(mass_shootings_data$num_killed,
              breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18),
              right = FALSE,
              labels = c(
                "[0-2)", "[2-4)", "[4-6)",
                "[6-8)", "[8-10)", "[10-12)",
                "[12-14)", "[14-16)", "[16-18)"
              )
)

shooting_map <- leaflet(data = mass_shootings_data[1:9]) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  setView(lng = -100, lat = 40, zoom = 4) %>%
  addCircles(
    lat = ~lat,
    lng = ~long,
    popup = ~paste(
      "State:", mass_shootings_data$state, "<br>",
      "City:", mass_shootings_data$city, "<br>",
      "People Killed:", mass_shootings_data$num_killed, "<br>",
      "People Injured:", mass_shootings_data$num_injured, "<br>",
      "Date:", mass_shootings_data$date
    ),
    color = ~ color_palette(number_shootings),
    radius = 15000,
    stroke = FALSE,
    fillOpacity = 1
  ) %>%
  addLegend(
    position = "bottomright",
    title = "Number of Deaths caused by Shootings",
    pal = color_palette,
    values = ~ mass_shootings_data$number_shootings,
    opacity = 1
  )

# Plot 
monthly_shooting <- mass_shootings_data %>%
  mutate(month = unlist(strsplit(mass_shootings_data$date, " "))
         [c(T, F, F)]) %>%
  group_by(month)

plot <- ggplot(data = monthly_shooting) +
  geom_bar(mapping = aes(x = month), fill = "blue") +
  coord_flip() +
  labs(
    title = "Number of Monthly Shootings Across the U.S.",
    y = "Number of Shootings",
    x = "Month"
  )


