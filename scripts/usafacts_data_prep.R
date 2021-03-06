## Using USA Facts data
## Script downloads and preps case count data from: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
## Data courtesy of usa facts
## Will Doyle
## Rev: 2020-04-29

library(tidyverse)
library(tibbletime)
library(lubridate)
library(httr)
setwd("/Users/doylewr/ya_covid_dash/")
library(here)

## Define 3-day rolling average

rolling_3_mean<-rollify(~mean(.x,na.rm=TRUE),window=3)

## Open time of last update
last_update <-
  read_lines(here("covid_usa_facts", "last_update.txt")) %>% ymd_hms()


## Request header info

myurl <-
  "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"

file_get <- GET(myurl)

## Check to see if it's changed
file_change_time <- cache_info(file_get)$modified %>% ymd_hms()

## If it has changed, download relevant files, otherwise post no changes

if (file_change_time > last_update) {
  
  myurl <-
    "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"
  
  ## Update change time
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
usa_counts <- read_csv(here("data", "covid_confirmed_usafacts.csv"))

## Excluded county designations
not_list <- c("Statewide Unallocated", "Grand Princess Cruise Ship")

usa_counts_county <- usa_counts %>%
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


usa_deaths_county <- read_csv(here("data", "covid_deaths_usafacts.csv"))

usa_deaths_county <- usa_deaths_county %>%
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

usa_data_county <-
  left_join(usa_counts_county, usa_deaths_county, by = c("State" , "County", "Date")) %>%
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
  mutate(`3-Day Mean, New Cases`=rolling_3_mean(`Daily Increase, Cases`))%>%
  mutate(`Daily Increase, Deaths` = `Deaths` - lag(`Deaths`)) %>%
  mutate(`Percent Change, Deaths` = (((
    `Deaths` - lag(`Deaths`)
  ) / `Deaths`) * 100)) %>%
  mutate(`3-Day Mean, New Deaths`=rolling_3_mean(`Daily Increase, Deaths`))%>% 
  ungroup() %>%
  select(
    County,
    State,
    Date,
    `Case Count`,
    `Daily Increase, Cases`,
    `3-Day Mean, New Cases`,
    `Percent Change, Cases`,
    Deaths,
    `Daily Increase, Deaths`,
    `Percent Change, Deaths`,
    `3-Day Mean, New Deaths`
  )


## Write data out to app directory
write_rds(usa_data_county, path = here("covid_usa_facts", "usa_data_county.Rds"))



## State Level
usa_counts_state<-usa_counts_county%>%
  group_by(State,Date)%>%
  summarise(`Case Count`=sum(`Case Count`))%>%
  ungroup()

usa_deaths_state<-usa_deaths_county%>%
  group_by(State,Date)%>%
  summarise(`Deaths`=sum(`Deaths`))%>%
  ungroup()

usa_data_state<-left_join(usa_counts_state,usa_deaths_state)%>%
  mutate(State = fct_reorder(
    .f = as_factor(State),
    .x = `Case Count`,
    .fun = "max",
    .desc = TRUE
  )) %>% # Ordered by prevalence
  group_by(State) %>%
  arrange(Date) %>%
  mutate(`Daily Increase, Cases` = `Case Count` - lag(`Case Count`)) %>%
  mutate(`Percent Change, Cases` = (((
    `Case Count` - lag(`Case Count`)
  ) / `Case Count`) * 100)) %>%
  mutate(`3-Day Mean, New Cases`=rolling_3_mean(`Daily Increase, Cases`))%>%
  mutate(`Daily Increase, Deaths` = `Deaths` - lag(`Deaths`)) %>%
  mutate(`Percent Change, Deaths` = (((
    `Deaths` - lag(`Deaths`)
  ) / `Deaths`) * 100)) %>%
  mutate(`3-Day Mean, New Deaths`=rolling_3_mean(`Daily Increase, Deaths`))%>% 
  ungroup() %>%
  select(
    State,
    Date,
    `Case Count`,
    `Daily Increase, Cases`,
    `3-Day Mean, New Cases`,
    `Percent Change, Cases`,
    Deaths,
    `Daily Increase, Deaths`,
    `Percent Change, Deaths`,
    `3-Day Mean, New Deaths`
  )
  

## Write data out to app directory
write_rds(usa_data_state, path = here("covid_usa_facts", "usa_data_state.Rds"))
