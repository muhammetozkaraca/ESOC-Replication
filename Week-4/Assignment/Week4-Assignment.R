library(tidyverse)
library(maps)
library(showtext)
library(splitstackshape)
library(countrycode)
library(paletteer)
library(gameofthrones)


setwd("./Desktop")
all_flows <- read.csv("all_flow_classes.csv")
projects_sources <- read.csv("project_descriptions_and_sources.csv")

font_add_google("Noto Serif Display", family = "title")
font_add_google("Libre Caslon Display", family = "subtitle")
font_add_google("Viaoda Libre", family = "caption")
showtext_auto()


## Create a map of Chinese investment projects globally. 

world <- map_data("world")
world <- world[world$region!="Antarctica",]

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey") +
  geom_point(data = all_flows, aes(x = longitude, y = latitude), alpha = 0.3, color = "3FFF33", size = 1) +
  ggthemes::theme_map() +
  coord_fixed(1.3) +
  labs(title = "Chinese Foreign Investment Projects",
       caption = "Source: ESOC FDR Pre-Doctoral Training Curriculum | Plot: @muhammetozkrca") +
  theme(plot.title = element_text(hjust = 0.5, family = "title", size = 14),
        plot.caption = element_text(hjust = 0.5, family = "caption", size = 7))


## Then, create a few more maps visualizing details of these projects, for example playing with:

### different kinds of projects,

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.6) +
  geom_point(data = all_flows, aes(x = longitude, y = latitude, color = intent), alpha = 0.3, size = 1) +
  ggthemes::theme_map() +
  coord_fixed(1.3) +
  labs(title = "Chinese Foreign Investment Projects",
       caption = "Source: ESOC FDR Pre-Doctoral Training Curriculum | Plot: @muhammetozkrca") +
  theme(plot.title = element_text(hjust = 0.5, family = "title", size = 14),
        plot.caption = element_text(hjust = 0.5, family = "caption", size = 7))

### the density of projects per country and/or region,

flows_per_country <- all_flows %>%
  splitstackshape::cSplit(c("recipient_iso3"), sep= c(";"), "long") %>%
  group_by(recipient_iso3) %>%
  mutate(project_per_country = n()) %>%
  select(recipients, recipient_iso3, project_per_country) %>%
  distinct(recipient_iso3, project_per_country) %>%
  arrange(desc(project_per_country))

colnames(flows_per_country)[1] <- "iso3c"

world$iso3c <- countrycode(world$region, origin = "country.name", destination = "iso3c")

setdiff(flows_per_country$iso3c, world$iso3c)
world[world$region == "Micronesia", ]$iso3c <- "FSM"

world <- world %>%
  left_join(flows_per_country, by = "iso3c")

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group, fill = project_per_country)) +
  ggthemes::theme_map() +
  scale_fill_got(option = "Daenerys", direction = 1) +
  # scale_fill_distiller (palette = "Spectral") +
  labs(title = "Chinese Foreign Investment Projects per Each Country",
       caption = "Source: ESOC FDR Pre-Doctoral Training Curriculum | Plot: @muhammetozkrca") +
  coord_fixed(1.3) +
  theme(plot.title = element_text(hjust = 0.5, family = "title", size = 14),
        plot.caption = element_text(hjust = 0.5, family = "caption", size = 7))


### the monetary amount of these projects.


## Pick a country of interest and create a map of Chinese investment at the province/state level.
### Note: You will need to find your own shapefiles at the relevant unit of analysis for 
### your country of choice. It is recommended to pick a country with a good number of projects.















