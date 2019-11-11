library(dplyr)
library(plotly)
incident <- read.csv(file = "data/shootings-2018.csv", stringsAsFactors = FALSE)

shooting_occurs <- nrow(incident)
lives_lost <- sum(incident$num_killed)
city_impact <- incident %>%
  select(city) %>%
  group_by(city) %>%
  summarise(total = n()) %>%
  filter(total == max(total)) %>%
  pull(city)
city_most_killed <- incident %>%
  select(city, num_killed) %>%
  group_by(city) %>%
  summarise(total = sum(num_killed)) %>%
  filter(total == max(total)) %>%
  pull(city)
city_most_injured <- incident %>%
  select(city, num_injured) %>%
  group_by(city) %>%
  summarise(total = sum(num_injured)) %>%
  filter(total == max(total)) %>%
  pull(city)
  
cal_info <- incident[14, ]

table <- incident %>%
  select(num_killed, num_injured) %>%
  aggregate(by = list(incident$city), FUN = sum) %>%
  arrange(-num_killed)

names(table) <- list("city", "total_num_killed", "total_num_injured")

pie_incident <- incident %>%
  select(num_killed) %>%
  aggregate(by = list(incident$city), FUN = sum) %>%
  arrange(-num_killed) %>%
  top_n(10) %>%
  mutate(paste0(round(num_killed / sum(num_killed) * 100, 1), "%"))

names(pie_incident) <- list("city", "num_killed", "percentage")

library(ggplot2)
pie <- ggplot(pie_incident, aes(x = "", y = num_killed, fill = city)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  ggtitle("Top 10 shooting murder cities in U.S. in 2018")
  # geom_text(aes(x=1, y = cumsum(pie_incident$percentage)
  # - pie_incident$percentage/2, label = pie_incident$percentage
  # , color = "white", check_overlap = FALSE, size = 3))
  # I tried to add percentage to the graph using the code
  # above but it leaves overlapping. However, I leave the results for
  # percentage on the last colomn of pie_incident dataframe
  pie
  
library(ggmap)
library(mapproj)
library(jpeg)
library(maps)
library(plotly)

map_incident <- incident %>%
  select(city, num_killed, num_injured, lat, long) %>%
  mutate(info = paste0("City : ", city, "<br>"
                       , "Number of people being killed : ", num_killed, "<br>"
                       , "Number of people being injured : ", num_injured),
         size = num_killed + num_injured)

state_shape <- map_data("state")


maps <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "white",
    size = .1
  ) +
  geom_point(data = map_incident, mapping =
             aes(x = long, y = lat, size = size, text = info), color = "blue") +
  coord_map()
  
maps <- ggplotly(maps, tooltip = "text")
maps