

## Using USA Facts data
## Script downloads and preps case count data from: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
## Data courtesy of usa facts
## Will Doyle
## Rev: 2020-04-11

library(tidyverse)
library(lubridate)
library(httr)
setwd("/Users/doylewr/ya_covid_dash/")
library(here)

myurl <-
  "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"

## Open time of last update
last_update <-
  read_lines(here("covid_usa_facts", "last_update.txt")) %>% ymd_hms()

## Request header info
file_get <- GET(myurl)

## Check to see if it's changed
file_change_time <- cache_info(file_get)$modified %>% ymd_hms()

## If it has changed, download relevant files, otherwise post no changes

if (file_change_time > last_update) {
  write_lines(file_change_time,
              here("covid_usa_facts", "last_update.txt"))
  
  download.file(myurl, destfile = here("data", "covid_confirmed_usafacts.csv"))
  
  myurl <-
    "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"
  
  download.file(myurl, destfile = here("data", "covid_deaths_usafacts.csv"))
  
  paste("Updating, revisons from", file_change_time, "at", now())
} else {
  paste("No update necessary at", now())
}
## Open case count data
usa_data <- read_csv(here("data", "covid_confirmed_usafacts.csv"))

## Excluded county designations
not_list <- c("Statewide Unallocated", "Grand Princess Cruise Ship")

usa_data <- usa_data %>%
  rename(County = `County Name`) %>%
  rename_all(str_to_title) %>%
  gather(key = Date,
         value = "Case Count",
         -State,
         -County,
         -Statefips,
         -Countyfips) %>% # Reshape
  mutate(`Case Count` = as.numeric(`Case Count`)) %>%
  filter(!(County %in% (not_list))) %>%
  mutate(Date = mdy(Date)) %>%
  filter((Date) > as_date("2020-03-01"))

usa_deaths <- read_csv(here("data", "covid_deaths_usafacts.csv"))

usa_deaths <- usa_deaths %>%
  rename(County = `County Name`) %>%
  rename_all(str_to_title) %>%
  gather(key = Date,
         value = "Deaths",
         -State,
         -County,
         -Statefips,
         -Countyfips) %>% # Reshape
  mutate(Deaths = as.numeric(Deaths)) %>%
  filter(!(County %in% (not_list))) %>%
  mutate(Date = mdy(Date)) %>%
  filter((Date) > as_date("2020-03-01")) %>%
  select(County, State, Date, Deaths)

usa_data <-
  left_join(usa_data, usa_deaths, by = c("State" , "County", "Date")) %>%
  mutate(County = fct_reorder(
    .f = as_factor(County),
    .x = `Case Count`,
    .fun = "max",
    .desc = TRUE
  )) %>% # Ordered by prevalence
  group_by(State, County) %>%
  arrange(Date) %>%
  mutate(`Daily Increase, Cases` = `Case Count` - lag(`Case Count`)) %>%
  mutate(`Percent Change, Cases` = (((
    `Case Count` - lag(`Case Count`)
  ) / `Case Count`) * 100)) %>%
  mutate(`Daily Increase, Deaths` = `Deaths` - lag(`Deaths`)) %>%
  mutate(`Percent Change, Deaths` = (((
    `Deaths` - lag(`Deaths`)
  ) / `Deaths`) * 100)) %>%
  ungroup() %>%
  select(
    County,
    State,
    Date,
    `Case Count`,
    `Daily Increase, Cases`,
    `Percent Change, Cases`,
    Deaths,
    `Daily Increase, Deaths`,
    `Percent Change, Deaths`
  )


## Write data out to app directory
write_rds(usa_data, path = here("covid_usa_facts", "usa_data.Rds"))



