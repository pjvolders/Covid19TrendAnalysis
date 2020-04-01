#------------------------------------
# Download and selection data Belgium
# Author: Joris Meys
# date last modified: 2020-03-08
#------------------------------------
# This file will check the latest data on the repo of John Hopkins
# It will -if necessary - download those and add the new data to
# the original file. 
library(tidyverse)
library(conflicted)
library(lubridate)
conflict_prefer("filter", "dplyr")
# Setup
firstdate <- as.Date("2020-01-22")
fprocessed <- file.path("processed","covid_selection.csv")

# Choose countries to keep
countries <- c("Belgium","France","Germany","Italy","Netherlands", "UK", #"China", "Mainland China",
               "Spain",  "South Korea","Korea, South", "Japan", "US", "United Kingdom")

days <- seq(firstdate,Sys.Date() - 1,
            by = "1 day")
fnames <- paste0(format(days, "%m-%d-%Y"),".csv")

if(!dir.exists("rawdata")) dir.create("rawdata")

if(!dir.exists("processed")) dir.create("processed")

# if(!file.exists(fprocessed)){
#   tmpdf <- data.frame(date = integer(0), Confirmed = integer(0), 
#                       Deaths = integer(0), Recovered = integer(0),
#                       Country = character(0))
#   write.csv(tmpdf,
#             fprocessed,
#             row.names = FALSE
#   )
#   rm(tmpdf)
# }


#----------------------------------------
# Download files from John Hopkins (thx!)
master_url <- paste("https://raw.githubusercontent.com",
                    "CSSEGISandData/COVID-19/master",
                    "csse_covid_19_data",
                    "csse_covid_19_daily_reports",
                    sep = "/")

for(fn in fnames){
  thefile <- file.path("rawdata",fn)
  if(!file.exists(thefile))
  download.file(file.path(master_url,fn),
                dest = thefile)
}

processed_data = map_dfr(fnames, function(fn){
  f_date = mdy(str_remove(fn, '.csv'))
  read_csv(file.path("rawdata",fn)) %>%
    rename(Country = starts_with('Country')) %>%
    mutate(date = f_date) %>% 
    select(date, Confirmed, Deaths, Recovered, Country) %>%
    filter(Country %in% countries) %>%
    group_by(Country, date) %>%
    summarise_all(sum) %>%
    ungroup()
})

processed_data = processed_data %>% 
  mutate(Country = replace(Country, Country=="United Kingdom", "UK")) %>% 
  mutate(Country = replace(Country, Country=="Mainland China", "China")) %>% 
  mutate(Country = replace(Country, Country=="Korea, South", "South Korea"))

write_csv(processed_data, fprocessed)


#----------------------------------------
# Select data for Belgium




# find files to add
# presdates <- read.csv(fprocessed,
#                       colClasses = c("Date",rep("numeric",3),
#                                      "character")
# )
# latest <- max(presdates$date, firstdate - 1, na.rm = TRUE)
# id <- days > latest

#cols <- c("Country/Region","Confirmed","Deaths","Recovered")
# Loop over the necessary files, find data, add to .csv
# for(i in which(id)){
#   fn <- fnames[i]
#   tmp <- read_csv(file.path("rawdata",fn), ) %>%
#     mutate(date = days[i]) %>%
#     select(date, Confirmed, Deaths, Recovered,
#            Country = `Country_Region`) %>%
#     filter(Country %in% countries) %>%
#     group_by(Country, date) %>%
#     summarise_all(sum) %>%
#     ungroup() %>%
#     select(date, Confirmed, Deaths, Recovered, Country)
# 
#   write.table(tmp,
#             fprocessed,
#             row.names = FALSE,
#             col.names = FALSE,
#             append = TRUE,
#             sep = ",")
# }

#' Sciensano data Belgium
sciensano_data_url = 'https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv'
download.file(sciensano_data_url,
              dest = 'COVID19BE_HOSP.csv')
# Cleanup
rm(list = ls())
