# TASK Take the covid data from the last session and map the death / 
# cases over the time. Show the trend for the entire world as well as for Germany and the USA
#LINE PLOT

library(Quandl)
library(lubridate)
library(tidyverse)
library(readxl)
library(dplyr)
library(data.table)
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)

class(covid_data_dt)

# Create a table with data we need
#First graph is for Cumulative Cases


Relevant_data_needed<- covid_data_dt %>%
  
  # Select Columns
  select(dateRep, countriesAndTerritories, deaths, cases, popData2019) %>%
 group_by(countriesAndTerritories)%>%
  mutate(dateRep = as.Date(dateRep, "%d/%m/%Y")) %>%
  
 
  arrange(dateRep) %>%
  
  # Filter  (choose a country/countries)
  group_by(countriesAndTerritories) %>% 
  filter(countriesAndTerritories %in% c("Germany","United_States_of_America")) %>% 
  ungroup() %>%

group_by(countriesAndTerritories) %>% mutate(cumCases = cumsum(cases))

frmLast <- Relevant_data_needed %>%
  slice(which.max(dateRep))

frmFirst <- Relevant_data_needed %>% 
  slice(which.min(dateRep))


# Step 2 - Visualize Cases
Relevant_data_needed %>%
  
  # Set up x, y, fill
  ggplot(aes(x = dateRep, y = cumCases, group = countriesAndTerritories)) +
  
  # Geometries
  geom_line(aes(color=countriesAndTerritories)) + 
  
  scale_y_continuous(trans = 'log10') +
annotation_logticks(sides="lr") +
  scale_x_date(date_minor_breaks = "30 day")+
  geom_text(data = frmLast, aes(x = dateRep, y = cumCases, label = cumCases),size = 4, vjust = 2.5, hjust= 1)+
  geom_point(data = frmLast, aes(x = dateRep, y = cumCases), col = "Black", shape = 20, fill = "white", size = 2, stroke = 1.7)+
  labs( title = "COVID Cumulative Cases Germany Vs. USA")

# Create a table with data we need
#Second graph is for Cumulative deaths


# Relevant_data_needed<- covid_data_dt %>%
#   
#   # Select Columns
#   select(dateRep, countriesAndTerritories, deaths, cases, popData2019) %>%
#   group_by(countriesAndTerritories)%>%
#   mutate(dateRep = as.Date(dateRep, "%d/%m/%Y")) %>%
#   
#   
#   arrange(dateRep) %>%
#   
#   # Filter  (choose a country/countries)
#   group_by(countriesAndTerritories) %>% 
#   filter(countriesAndTerritories %in% c("Germany","United_States_of_America")) %>% 
#   ungroup() %>%
  
 Deaths_Relevant_data_needed<- Relevant_data_needed %>% 
   group_by(countriesAndTerritories) %>% 
   mutate(cumDeaths = cumsum(deaths))

frmLast <- Deaths_Relevant_data_needed %>% #last data date
  slice(which.max(dateRep))

frmFirst <- Deaths_Relevant_data_needed %>% #first data date 
  slice(which.min(dateRep))


# Step 2 - Visualize Cases
Deaths_Relevant_data_needed %>%
  
  # Set up x, y, fill
  ggplot(aes(x = dateRep, y = cumDeaths, group = countriesAndTerritories)) +
  
  # Geometries
  geom_line(aes(color=countriesAndTerritories)) + 
  
  scale_y_continuous(trans = 'log10') +
  annotation_logticks(sides="lr") +
  scale_x_date(date_minor_breaks = "15 day")+
  geom_text(data = frmLast, aes(x = dateRep, y = cumDeaths, label = cumDeaths),size = 4, vjust = 2.5, hjust= 1)+
  geom_point(data = frmLast, aes(x = dateRep, y = cumDeaths), col = "Black", shape = 20, fill = "white", size = 2, stroke = 1.7)+
  labs( title = "COVID Cumulative Deaths Germany Vs. USA")


