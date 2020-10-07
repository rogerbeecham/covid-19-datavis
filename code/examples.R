# -------------------------------------------
# R script demonstrating how Covid-19 glyphmaps can be generated
# and parameterised
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
library(gganimate)

# Set theme.
theme_set(theme_minimal(base_family="Avenir Book"))
# Paired back theme.
theme_spare <- theme(axis.title=element_blank(),axis.text = element_blank(),panel.grid = element_blank())
# -------------------------------------------
# P L O T S
# -------------------------------------------

# These examples depend on data being downloaded (download_data.R), staged  
# (data_staging.R) and also the associated  helper functions (helper_functions.R). 
# So if you have not already, run those now by uncommenting the lines below.
# source("./code/download_data.R")
# source("./code/data_staging.R")
# source("./code/helper_functions.R")


# T H I C K N E S S 

# Mappings 
# size (mark thickness) : relative cases counts
# colour (mark colour) : not encoded 
# alpha (mark lightness) : not encoded 

# Ridge contour example.
ridges <- 
  generate_ridge(
    # Parameter 1 is data.
    # We want to pass through a sf object depending on geog of choice.
    # In this case smwg_sf. If we wanted to use the real geog, then replace
    # smwg_sf with england_boundaries.
    data=smwg_sf %>% inner_join(plot_data) %>%  
    # Update the data frame with more generic names that are understood by
    # helper functions. Note that *size* is set to relative case counts.
    # x and y are time elapsed and cumulative cases respectively.
    mutate(is_london=if_else(region_name=="London",TRUE,FALSE),
           x=time_rescaled, y=cases_rescaled, colour=as.character(1), 
           size=current_exposure/max(current_exposure), fill=is_london, alpha=1),
    # Parameter 2 is the *current* data. Useful for animating.
    current=max(dates),
    # -- If animating, uncomment the line below and comment the line above
    # current=dates,
    # When not using smwg layour make this constant 0.
    cell_size=cell_height*.5) +
  scale_size(range=c(0,1), limits=c(0,1), guide=FALSE)+
  scale_colour_manual(values=c("1"="#636363"), guide=FALSE) +
  scale_fill_manual(values=c("#d9d9d9", "#bdbdbd"), guide=FALSE) +
  scale_alpha(range=c(1,1), guide=FALSE) +
  # Uncomment line below when animating.
  # transition_time(date) +
  theme_spare


# Lines example.
lines <- 
  generate_line(
    data=smwg_sf %>% inner_join(plot_data) %>%  ungroup %>% 
      mutate(is_london=if_else(region_name=="London",TRUE,FALSE),
             x=time_rescaled, y=cases_rescaled, colour=as.character(1), 
             size=current_exposure/max(current_exposure), fill=is_london, alpha=1),
    cell_size=cell_height*.5) +
  scale_size(range=c(0,1), limits=c(0,1), guide=FALSE)+
  scale_colour_manual(values=c("1"="#636363"), guide=FALSE) +
  scale_fill_manual(values=c("#d9d9d9", "#bdbdbd"), guide=FALSE) +
  scale_alpha(range=c(1,1), guide=FALSE) +
  # Uncomment line below when animating.
  # transition_reveal(date) +
  theme_spare

# T H I C K N E S S + H U E + V A L U E

# Mappings 
# size (mark thickness) : relative cases counts
# colour hue (mark colour) : whether has peaked
# alpha (mark lightness) : distance from local peak

# Ridge contour example.
ridges <- 
  generate_ridge(
    data=smwg_sf %>% inner_join(plot_data) %>%  
      # *size* is set to relative case counts, colour to has_peaked and
      # alpha to cases relative to local peak in 7-day moving avg
      # new cases (cases_rescaled_mov).
      mutate(is_london=if_else(region_name=="London",TRUE,FALSE),
             x=time_rescaled, y=cases_rescaled, colour=as.character(as.numeric(has_peaked)), 
             size=current_exposure/max(current_exposure), fill=is_london, alpha=cases_rescaled_mov),
    current=dates,
    # When not using smwg layour make this constant 0.
    cell_size=cell_height*.5) +
  scale_size(range=c(0,1), limits=c(0,1), guide=FALSE)+
  # Peaked colours -- red/blue.
  scale_colour_manual(values=c("0"="#b2182b","1"="#2166ac"), guide=FALSE)+
  scale_fill_manual(values=c("#d9d9d9", "#bdbdbd"), guide=FALSE) +
  scale_alpha(range=c(.1,1), guide=FALSE) +
  # Uncommented as layout only makes sense when animating.
  transition_time(date) +
  theme_spare


# Lines example.
lines <- 
  generate_line(
    data=smwg_sf %>% inner_join(plot_data) %>%  ungroup %>% 
      mutate(is_london=if_else(region_name=="London",TRUE,FALSE),
             x=time_rescaled, y=cases_rescaled, colour=as.character(as.numeric(has_peaked)), 
             size=current_exposure/max(current_exposure), fill=is_london, alpha=cases_rescaled_mov),
    cell_size=cell_height*.5) +
  scale_size(range=c(0,1), limits=c(0,1), guide=FALSE)+
  scale_colour_manual(values=c("0"="#b2182b","1"="#2166ac"), guide=FALSE)+
  scale_fill_manual(values=c("#d9d9d9", "#bdbdbd"), guide=FALSE) +
  scale_alpha(range=c(.1,1), guide=FALSE) +
  # Uncomment line below when animating.
  # transition_reveal(date) +
  theme_spare

# R E F O C U S  O N  D A I L Y  C A S E S

# Daily cases area chart
areas <- generate_area(
  data=smwg_sf %>% inner_join(plot_data) %>% group_by(area_name) %>% 
    mutate(
      # Rescale case counts locally, so that ends at same position in horizontal 
      # time. E.g. time is a semi-global scaling (in terms of disease trajectory).
      # E.g. we start counting at 100 cases rather than absolute time. 
      # But case counts are locally scaled to this horizontal position.
      y_2=scales::rescale(cases_cum, to=c(0, max(time_rescaled)), from = c(min(cases_cum), max(cases_cum)))
    ) %>% ungroup %>% 
    mutate(
      is_london=if_else(region_name=="London",TRUE,FALSE), 
      x=time_rescaled, 
      y=cases_rescaled_mov,
      colour=as.character(as.numeric(is_london)), fill=as.character(as.numeric(is_london)), size=current_exposure/max(current_exposure), height=cases_rescaled,
      width=scales::rescale(size, to=c(0, 1), from = c(min(size), max(size))),
      milestone_days=if_else((day_num %% 10 < lag(day_num %% 10,1)), as.integer(day_num/10)*10,0)
    ),
  current=max(dates),
  cell_size=cell_height*.5) +
  scale_fill_manual(values=c("0"="#bdbdbd", "1"="#bdbdbd"), guide=FALSE) +
  scale_colour_manual(values=c("0"="#bdbdbd", "1"="#bdbdbd"), guide=FALSE) +
  theme_spare

