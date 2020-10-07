# -------------------------------------------
# R script for reading and processing Covid-19 cases data for England 
# Author: Roger Beecham
# -------------------------------------------

# -------------------------------------------
# L I B R A R I E S
# -------------------------------------------

# Required libraries.
library(tidyverse)
library(sf)
library(lubridate)
library(RcppRoll)
library(rmapshaper)

# -------------------------------------------
# D O W N L O A D
# -------------------------------------------

# Read in latest C A S E S data.
data <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv") %>%
  rename_all(~tolower(gsub("\\s", "_",.))) %>%
  select(area_name, area_code, area_type, date=specimen_date, cases=`daily_lab-confirmed_cases`) %>%
  filter(area_type=="utla") %>% 
  mutate(
    date=lubridate::as_date(date),
    area_type=if_else(area_type %in% c("utla", "ltla"), "la", area_type)
  ) %>% 
  unique()
# Store area name as lookups.
area_lookups <- data %>% select(area_name, area_code, area_type) %>% unique
# Work on cases data only until June  -- so first wave in cases.
data <- data %>%  filter(date<=lubridate::as_date("2020-06-01"))


# Create record of all days for which we have data and also geogs. 
# -- for populating new tibble, useful for example in animating.
temp_area_codes <- data %>% pull(area_code) %>% unique
temp_area_names <- data %>% pull(area_name) %>% unique
temp_days <- seq(min(data$date),max(data$date), by = '1 day')
temp_geog_days <- tibble(
  area_code=rep(temp_area_codes,each=length(temp_days), times=1),
  area_name=rep(temp_area_names,each=length(temp_days), times=1),
  date=rep(temp_days, each=1, times=length(temp_area_names))
) %>% 
  left_join(area_lookups)
# Merge with data -- with some calculations that should be globally useful.
data <- temp_geog_days %>%
  left_join(data) %>%
  mutate(cases=if_else(is.na(cases),0,cases)) %>%
  group_by(area_code) %>%
  mutate(
    # Cumulative cases.
    cases_cum=cumsum(cases),
    # 7-day rolling cases.
    cases_mov_avg_local=roll_mean(cases,7,align="right", fill=0),
    # Total cases in geog area.
    cases_total_local=max(cases_cum),
    # Maximum 7-day rolling new cases -- this represents the local "peak".
    cases_max_mov_local=max(cases_mov_avg_local)
  ) %>% 
  ungroup

# Clean Environment.
rm(temp_area_codes, temp_area_names, temp_days, temp_geog_days)

# Write data file to data directory
if(!dir.exists("./data")) {dir.create("./data")}
write_csv(data, "./data/data.csv")
data <- read_csv("./data/data.csv")

# -------------------------------------------

# Read in B O U N D A R Y data for LAs.

# From : https://hub.arcgis.com/datasets/f3c8abecb0144417856378122eb7025f_0
url <- "https://opendata.arcgis.com/datasets/f3c8abecb0144417856378122eb7025f_0.zip"
download.file(url, "./data/boundaries.zip")
unzip("./data/boundaries.zip", exdir="./data/boundaries")

# Files have complex names that vary on download, so record and use on reading-in.
temp_boundary_file <- list.files("./data/boundaries", pattern=".shp")
la_boundaries <- st_read(paste0("./data/boundaries/", temp_boundary_file))
# Delete directory with large shapefile.
unlink("./data/boundaries", recursive=TRUE)

# Boundary data for regions.
url <- "https://opendata.arcgis.com/datasets/324a9f3ad2cc4a049e913dddc508aeb2_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
download.file(url, "./data/regional_boundaries.zip")
unzip("./data/regional_boundaries.zip", exdir="./data/regional_boundaries")

# Files have complex names that vary on download, so record and use on reading-in.
temp_boundary_file <- list.files("./data/regional_boundaries", pattern=".shp")
regional_boundaries <- st_read(paste0("./data/regional_boundaries/", temp_boundary_file))
# Delete directory with large shapefile.
unlink("./data/regional_boundaries", recursive=TRUE)
# Set geom to GBNG.
regional_boundaries <- regional_boundaries %>% st_transform(crs=27700)
la_boundaries <- la_boundaries %>% st_transform(crs=27700)
# Join LA with regional data.
la_boundaries <- la_boundaries %>%
  st_join(regional_boundaries %>% select(region_name=rgn19nm), largest=TRUE)
# Simplify shapefile.
la_boundaries <- ms_simplify(la_boundaries, keep=0.01)
# Write out to data folder.
st_write(la_boundaries %>% select(area_code=ctyua19cd, area_name=ctyua19nm, easting=bng_e, northing=bng_n, region_name),
         append=FALSE, "./data/la_boundaries.geojson")
# Clean workspace.
rm(temp_boundary_file, url)

# Read in simplified la_boundaries.
la_boundaries <- st_read("./data/la_boundaries.geojson") %>% 
  st_transform(crs=27700)
# Define a tibble for reordering regions
regional_layout <- tibble(
  region_name=(c("London", "North West", "Yorkshire and The Humber", "North East",
                 "West Midlands", "East Midlands", "South West", "East of England", "South East")),
  grid_x=c(2,1,2,2,1,2,1,3,3),
  grid_y=c(4,2,2,1,3,3,4,3,4)
)


# Read in P O P U L A T I O N data for LAs.

temp_pop_file  <- tempfile()
url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
download.file(url, "./data/la_pop.xls")
la_pop <- readxl::read_excel("./data/la_pop.xls", sheet="MYE2-All", range="A5:D367") %>%
  transmute(area_code=Code, pop=`All ages`)
write_csv(la_pop, "./data/la_pop.csv")
# Clean workspace
rm(temp_pop_file, url)

# Read in population data.
la_pop <- read_csv("./data/la_pop.csv")
# Add to la_boundaries
la_boundaries <- la_boundaries %>% left_join(la_pop, by=c("area_code"="area_code"))
# Join pop data with data. 
data <- data %>% 
  left_join(la_pop, by=c("area_code"="area_code"))
# Clean workspace.
rm(la_pop)

