
## Using USA Facts data 
## Script downloads and preps case count data from: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
## Data courtesy of usa facts
## Will Doyle
## Rev: 2020-04-10

library(tidyverse)
library(lubridate)


last_update<-date(file.info("../data/covid_confirmed_usafacts.csv")$mtime)

if (last_update<today()){
  
myurl<-"https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"

download.file(myurl,destfile = "../data/covid_confirmed_usafacts.csv")

paste("Updating for",today())

} else {
  paste("No update necessary")
}

usa_data<-read_csv("../data/covid_confirmed_usafacts.csv")

not_list<-c("Statewide Unallocated","Grand Princess Cruise Ship")

usa_data<-usa_data%>%
  rename(County=`County Name`)%>%
  rename_all(str_to_title)%>%
  gather(key=Date,value="Case Count",-State,-County,-Statefips,-Countyfips)%>%
  mutate(`Case Count`=as.numeric(`Case Count`)) %>%
  filter(!(County%in%(not_list)))%>%
  mutate(Date=mdy(Date))%>%
  filter((Date)>as_date("2020-03-01"))%>%
  mutate(County=fct_reorder(.f=as_factor(County),.x=`Case Count`,.fun = "max",.desc = TRUE))%>%
  group_by(State,County)%>%
  arrange(Date)%>%
  mutate(`Daily Increase`=`Case Count`-lag(`Case Count`))%>%
  mutate(`Percent Change`=(((`Case Count`-lag(`Case Count`))/`Case Count`)*100))%>%
  ungroup()%>%
  select(County,State,Date,`Case Count`,`Daily Increase`,`Percent Change`)

  
write_rds(usa_data,path="../covid_usa_facts/usa_data.Rds")



  