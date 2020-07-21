library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")
library("colorspace")
library("scales")
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

covid_data_dt[,  `:=`(deaths_per_capita = deaths / popData2019,
                                          cases_per_capita = cases / popData2019,
                                          cases_per_deaths = cases / deaths)]

covid_data_dt[, cum_deaths := cumsum(deaths)]

covid_data_dt[, deaths_per_capita := (cum_deaths / popData2019)]

Relevant_data_needed<- covid_data_dt %>%
  
  # Select Columns
  select(dateRep, countriesAndTerritories, deaths_per_capita, countryterritoryCode) %>%
  group_by(countriesAndTerritories)%>%
  mutate(dateRep = as.Date(dateRep, "%d/%m/%Y")) %>%
  
  arrange(dateRep) 

Last_Data_date <- Relevant_data_needed %>%
  slice(which.max(dateRep))


Last_Data_date <- rename(Last_Data_date, adm0_a3 = countryterritoryCode )

world <- ne_countries(scale = "medium", returnclass = "sf")

World_updated <- merge(x = Last_Data_date, y = world, 
                       by    = "adm0_a3", 
                       all.x = TRUE, 
                       all.y = FALSE)

# 
# World_updated <- filter(World_updated, (cum_deaths_per_capita < 1) )

ggplot(data = World_updated) +
  geom_sf(aes(fill = deaths_per_capita, geometry = geometry)) +
  scale_fill_viridis_c(name = "Total Deaths Per Capita", 
                       option = "plasma", 
                       trans = "log", 
                       labels = comma)
