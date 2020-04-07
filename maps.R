#' ---
#' title: "Some meaningful title here"
#' author: "Pieter-Jan Volders"
#' output: 
#'    html_document:
#'       toc: TRUE
#'       toc_float: TRUE
#'       theme: paper
#'       df_print: paged
#'       highlight: tango
#' ---
#' 

#' # Setup
#' ## Subtitle

library(tidyverse)
library(ggplot2)
library(dplyr)
require(maps)
library(wpp2019)
library(plotly)

#' Resolve conflicts
library(conflicted)
conflict_prefer("desc", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("View", "utils")
conflict_prefer("layout", "plotly")

dat <- read_csv("processed/covid_selection.csv")
latest_dat = dat %>%
  group_by(Country) %>%
  top_n(n = 1, wt = date) %>%
  ungroup() %>%
  mutate(Country = replace(Country, Country=="US", "USA")) %>%
  mutate(Country = replace(Country, Country=="Congo (Brazzaville)", "Republic of Congo")) %>%
  mutate(Country = replace(Country, Country=="Congo (Kinshasa)", "Democratic Republic of the Congo")) %>%
  mutate(Country = replace(Country, Country=="North Macedonia", "Macedonia")) %>%
  mutate(Country = replace(Country, Country=="UK", "United Kingdom"))

data("pop")
pop_clean = as_tibble(pop) %>%
  mutate(name = as.character(name)) %>%
  mutate(name = replace(name, name=="United States of America", "USA")) %>%
  mutate(name = replace(name, name=="Dem. Republic of the Congo", "Democratic Republic of the Congo")) %>%
  mutate(name = replace(name, name=="Congo", "Republic of Congo")) %>%
  mutate(name = replace(name, name=="United Republic of Tanzania", "Tanzania")) %>%
  mutate(name = replace(name, name=="Russian Federation", "Russia")) %>%
  mutate(name = replace(name, name=="Venezuela (Bolivarian Republic of)", "Venezuela")) %>%
  mutate(name = replace(name, name=="Bolivia (Plurinational State of)", "Bolivia")) %>%
  mutate(name = replace(name, name=="Iran (Islamic Republic of)", "Iran")) %>%
  mutate(name = replace(name, name=="Syrian Arab Republic", "Syrian")) %>%
  mutate(name = replace(name, name=="Viet Nam", "Vietnam")) %>%
  mutate(name = replace(name, name=="China, Taiwan Province of China", "Taiwan")) %>%
  select(Country = name, population = `2020`)

latest_dat_pop = pop_clean %>% inner_join(latest_dat) %>%
  mutate(cases_per_million = Confirmed/population*1e3,
         deaths_per_million = Deaths/population*1e3) # population in thousands
  
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white") +
  theme_minimal()

world_map.dat = world_map %>% left_join(latest_dat_pop, by = c('region' = 'Country'))

cases_plot = ggplot(world_map.dat, aes(x = long, y = lat, group = group, fill = cases_per_million, label = region)) +
  geom_polygon()

cases_plot

#' # Plotly maps
data("iso3166")
latest_dat_pop_iso = latest_dat_pop %>% 
  mutate(Country = replace(Country, Country=="USA", "United States")) %>%
  mutate(Country = replace(Country, Country=="Russia", "Russian Federation")) %>%
  mutate(Country = replace(Country, Country=="Tanzania", "Tanzania, United Republic of")) %>%
  mutate(Country = replace(Country, Country=="Iran", "Iran, Islamic Republic of")) %>%
  mutate(Country = replace(Country, Country=="Czechia", "Czech Republic")) %>%
  mutate(Country = replace(Country, Country=="Venezuela", "Venezuela, Bolivarian Republic of")) %>%
  mutate(Country = replace(Country, Country=="Bolivia", "Bolivia, Plurinational State of")) %>%
  mutate(Country = replace(Country, Country=="United Kingdom", "United Kingdom of Great Britain and Northern Ireland")) %>%
  right_join(iso3166, by= c('Country' = 'ISOname'))

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'world',
  showframe = FALSE,
  landcolor = 'lightgray',
  showland = TRUE,
  showcountries = TRUE,
  countrycolor = 'gray',
  countrywidth = 0.5,
  showcoastlines = FALSE,
  showlakes = FALSE,
  projection = list(type = "natural earth")
)

latest_dat_pop_iso$label = 
  with(latest_dat_pop_iso, paste(Country, '<br>', "confirmed cases", Confirmed, '<br>',
                                 "by million inhabitants", cases_per_million, '<br>',
                                 "deaths", Deaths, '<br>',
                                 "by million inhabitants", deaths_per_million))


cases_plot_geo <- plot_geo(latest_dat_pop_iso, locationmode = 'world')%>% 
  add_trace(
    z = ~round(cases_per_million, 2), text = ~Country, locations = ~a3,
    color = ~cases_per_million, colors =  'YlGnBu'#'YlOrRd'
  ) %>% 
  colorbar(title = "confirmed cases <br>per million inhabitants") %>%
  layout(
    geo = g
  )

cases_plot_geo

deaths_plot_geo <- plot_geo(latest_dat_pop_iso, locationmode = 'world')%>% 
  add_trace(
    z = ~round(deaths_per_million, 2), text = ~Country, locations = ~a3,
    color = ~deaths_per_million, colors =  'YlOrRd'
  ) %>% 
  colorbar(title = "deaths per <br>million inhabitants") %>%
  layout(
    geo = g
  )

deaths_plot_geo



sessionInfo()
