#------------------------------------
# Create a plot showing trend of confirmed cases
# Author: Joris Meys
# date last modified: 2020-03-10
#------------------------------------
# Setup

library(ggplot2)
library(patchwork)
library(dplyr)
library(gridExtra)
library(conflicted)
conflict_prefer("filter", "dplyr")
#conflict_prefer("View", "utils")

# This will either create the file covid_selection.csv
# or download the latest data and add that to covid_selection.csv
source("DownloadData.R")

country_colors = c(
  "Belgium"      = '#000000',
  "France"       = '#e41a1c',
  "Germany"      = '#377eb8',
  "Italy"        = '#4daf4a',
  "Japan"        = '#984ea3',
  "Netherlands"  = '#ff7f00',
  "South Korea"  = '#ffff33',
  "Spain"        = '#a65628',
  "UK"           = '#f781bf',
  "US"           = '#999999'
)   

# Read in the data
dat <- read_csv("processed/covid_selection.csv")
#dat = dat %>%
#  filter(Country != "Argentina")

# Calculate the delay based on reaching 100 patients.
# Look for the first date more than 100 patients were confirmed.
# Calculate the difference with Italy to determine lag.
dat %>% group_by(Country) %>%
  summarise(refdate = date[min(which(Deaths > 10))]) %>%
  ungroup() %>%
  mutate(delay = refdate - refdate[Country == "Italy"]) ->
  delays

ItalyStart <- delays[delays$Country == "Italy","refdate", drop = TRUE]

plotdata <- left_join(dat, delays, by = "Country") %>%
  mutate(alpha = ifelse(Country == "Belgium",1,0.5))

# Some plots

# Linear plot
plotdata_lin = plotdata %>%
  select(date, `confirmed cases` = Confirmed, country = Country)
plin <- ggplot(plotdata_lin, aes(x = date, y = `confirmed cases` ,
                             color = country)) +
  geom_line(size = 1, alpha = 0.5) + 
  geom_point(alpha = 0.5) + 
  geom_point(alpha = 0.5, data = plotdata_lin[plotdata_lin$country == "Belgium",]) + 
  geom_line(data = plotdata_lin[plotdata_lin$country == "Belgium",]) +
  theme_classic() +
  scale_color_manual(values = country_colors) +
  labs(y = "confirmed cases", color = "country")

fn = format(Sys.time(), "%Y_%m_%d_confirmed_cases_plot.png")
ggsave(filename = fn, plot = plin, width = 6, height = 4)


# Logarithmic plot
plog <- plin + scale_y_log10(limits = c(10, NA)) +
  labs(y = "confirmed cases (log)")
plog
# Delay plot
plotdata_delay = plotdata %>%
  mutate(`days since 10 deaths` = as.numeric((date - delay) - ItalyStart),
         deaths = Deaths,
         country = Country) %>% select(
           `days since 10 deaths`, deaths, country, date
         )
pdelay <- ggplot(plotdata_delay, aes(x = `days since 10 deaths`,
                                     y = deaths,
                                     color = country, 
                                     label = date)) +
  geom_point(alpha = 0.5) + geom_line(stat = "smooth",
                                      method = "loess",
                                      size = 1,
                                      alpha = 0.5) +
  geom_line(data = plotdata_delay[plotdata_delay$country == "Belgium",],
            stat = "smooth",
            method = "loess",
            size = 1) +
  scale_color_manual(values = country_colors) +
  labs(x = "days since 10 deaths reached",
       y = "deaths (log)", color = "country")

pdelaylog <- pdelay + 
  scale_y_log10(limits=c(1,NA)) + 
  xlim(c(-10,NA)) +
  theme_classic()
pdelaylog

fn = format(Sys.time(), "%Y_%m_%d_death_delay_plot.png")
fn
ggsave(filename = fn, plot = pdelaylog, width = 6, height = 4)
# The table
# delaytab <-tableGrob(t(delays[,c('Country','delay')]),
#                      rows = NULL,
#                      theme = ttheme_minimal(base_size = 10))

# Construct the patchwork
# (plin | plog)  /
#   (pdelay | pdelaylog) / wrap_elements(delaytab) + 
#   ggtitle("Difference with Italy in day of reaching 100 patients") +
#   plot_layout(guides = 'collect',
#               heights = c(2,2,1)) +
#   plot_annotation(title = "Evolution of total confirmed cases in 8 countries",
#                   caption = paste("Created by Joris Meys on",
#                                   Sys.Date(),
#                                   "\n Data obtained from Johns Hopkins CSSE:",
#                                   "https://github.com/CSSEGISandData/COVID-19"))

# Save the figure
#if(!dir.exists("Fig")) dir.create("Fig")
#fname <- paste0("TrendConfirmed",Sys.Date(),".png")
#ggsave(file.path("Fig",fname), width = 8, height = 8)

#' # Animated
library(gganimate)

ggplot(plotdata, aes(Confirmed, Deaths, color = Country))+
  geom_point(alpha = 0.5) + geom_line(stat = "smooth",
                                      method = "loess",
                                      size = 1,
                                      alpha = 0.5) +
  scale_x_log10() + scale_y_log10() +
  theme_classic()
  

# pdelaylog + 
#   labs(title = 'Day: {frame_time}') +
#   transition_time(date)

#' # Doubling time

doubling_time = as_tibble(plotdata) %>%
  filter(Confirmed > 10) %>%
  group_by(Country) %>%
  arrange(desc(date)) %>%
  mutate(
    nr_increase = Confirmed - c(Confirmed[2:n()], 0),
    pc_increase = Confirmed/c(Confirmed[2:n()], NA),
    pc_increase = (pc_increase-1)*100,
    doubling_time = 70/((pc_increase-1)*100)) %>%
  ungroup()
doubling_time

doubling_time_models = doubling_time %>%
  group_by(Country) %>%
  do(m = lm(pc_increase~date, data = .)) %>%
  ungroup() %>%
  mutate(intercept = map_dbl(m, function(x) x[['coefficients']][1]),
         slope = map_dbl(m, function(x) x[['coefficients']][2])) %>%
  mutate(day_zero = -1*intercept/slope) %>%
  mutate(day_zero = as.Date(day_zero, origin = "1970-01-01"))
  


increase_conf = doubling_time %>% 
  filter(Country == "Belgium") %>%
  select(date, `percentage daily increase` = pc_increase) %>%
  ggplot(aes(date, `percentage daily increase`)) +
  geom_point() + xlim(c(as.Date("2020-03-14"), NA)) + ylim(NA, 40) + stat_smooth(method = "lm") +
  labs(y = "confirmed cases (Belgium, % increase)") + theme_classic()

ggplot(filter(doubling_time, Country == "Belgium"), aes(date, doubling_time)) +
  geom_point() + xlim(c(as.Date("2020-03-14"), NA)) + stat_smooth(method = "lm") +
  labs(y = "doubling time (Belgium, days)") + theme_classic()


ggplot(doubling_time, aes(date, doubling_time, color = Country))+
  geom_point() +
  ylim(c(-5, 20)) +
  geom_line()


#' Sciensano data
sciensano_be = read_csv('COVID19BE_HOSP.csv')

be_icu = sciensano_be %>%
  group_by(DATE, REGION) %>%
  summarise(TOTAL_IN_ICU = sum(TOTAL_IN_ICU)) %>%
  mutate(`total in icu Belgium` = sum(TOTAL_IN_ICU)) %>%
  select(date = DATE, `patients in icu` = TOTAL_IN_ICU, region = REGION, `total in icu Belgium`) %>%
  ggplot(aes(date, `patients in icu`, fill=region, label = `total in icu Belgium`)) +
  geom_area(alpha = 0.7) +
  geom_hline(yintercept=1765, color = "#666666", linetype = 2) +
  annotate("text", x = as.Date('2020-03-15') , y = 1830, 
           label = "max nr of beds in ICU", hjust = 'left', color = "#666666") +
  labs(y = "nr of patients on intensive care", x = 'date', fill = "region") +
  theme_classic() + 
  scale_fill_manual(values = c("Brussels" = "black", "Flanders" = "yellow", "Wallonia" = "red"))
  # geom_line(stat = "smooth",
  #           method = "loess",
  #           size = 1,
  #           alpha = 0.5) + 
  # scale_y_log10()
be_icu

sciensano_be %>%
  group_by(DATE, REGION) %>%
  summarise(TOTAL_IN_ICU = sum(TOTAL_IN_ICU)) %>%
  group_by(REGION) %>%
  arrange(desc(DATE)) %>%
  mutate(
    nr_increase = TOTAL_IN_ICU - c(TOTAL_IN_ICU[2:n()], 0)
  ) %>%
  ggplot(aes(DATE, nr_increase, color = REGION)) +
  geom_point() + 
  stat_smooth()

nr_increase_icu = sciensano_be %>%
  group_by(DATE) %>%
  summarise(TOTAL_IN_ICU = sum(TOTAL_IN_ICU)) %>%
  arrange(desc(DATE)) %>%
  mutate(
    nr_increase = TOTAL_IN_ICU - c(TOTAL_IN_ICU[2:n()], 0)
  ) %>%
  select(date = DATE, `daily increase` = nr_increase) %>%
  ggplot(aes(date, `daily increase`)) +
  geom_point() + 
  stat_smooth() + 
  labs(y = "ICU patients (Belgium, daily increase)", x = "date") + theme_classic()

panel = (((pdelaylog + guides(color = F) + be_icu) / plin) / (increase_conf + nr_increase_icu) ) + 
  plot_layout(guides = 'collect') + 
  plot_annotation(title = format(Sys.time(), "%A %d/%m/%Y"))


fn = format(Sys.time(), "%Y_%m_%d_panel.png")
fn
ggsave(filename = fn, plot = panel, width = 10, height = 10)


