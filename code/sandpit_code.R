library(tidyverse)
library(lubridate)
library(RcppRoll)
library(sf)
library(rmapshaper)
library(gganimate)
library(ggtext)
library(magick)


# load_data
# script for reading, processing and visually representing covid-19 cases data for England 
# Author: Roger Beecham
###############################################################################


# Read in latest cases data.
data <- read_csv(url("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv")) %>%
  rename_all(~tolower(gsub("\\s", "_",.))) %>%
  select(area_name, area_code, area_type, date=specimen_date, cases=`daily_lab-confirmed_cases`) %>%
  mutate(
    date=lubridate::as_date(date),
    area_type=if_else(area_type %in% c("Upper tier local authority", "lower tier local authority"), "la", area_type),
    area_type=tolower(area_type)
  )
# Store area name as lookups.
area_lookups <- data %>% select(area_name, area_code, area_type) %>% unique

# Create record of all days for which we have data and also geogs 
# -- for populating new tibble for animating.
temp_area_codes <- data %>% pull(area_code) %>% unique
temp_area_names <- data %>% pull(area_name) %>% unique
temp_days <- seq(min(data$date),max(data$date), by = '1 day')
temp_geog_days <- tibble(
  area_code=rep(temp_area_codes,each=length(temp_days), times=1),
  area_name=rep(temp_area_names,each=length(temp_days), times=1),
  date=rep(temp_days, each=1, times=length(temp_area_names))
  ) %>% 
  left_join(area_lookups)

data <- temp_geog_days %>%
  left_join(data) %>%
  mutate(cases=if_else(is.na(cases),0,cases)) %>%
  group_by(area_code) %>%
  mutate(cases_cum=cumsum(cases)) %>% 
  ungroup

# Write data file to data directory
if(!dir.exists("./data")) {dir.create("./data")}
write_csv(data, "./data/data.csv")
data <- read_csv("./data/data.csv")

# Boundary data from : https://hub.arcgis.com/datasets/f3c8abecb0144417856378122eb7025f_0
url <- "https://opendata.arcgis.com/datasets/f3c8abecb0144417856378122eb7025f_0.zip"
download.file(url, "./data/boundaries.zip", mode="wb")
unzip("./data/boundaries.zip", exdir="./data/boundaries")

# Files have complex names that vary on download, so record and use on reading-in.
temp_boundary_file <- list.files("./data/boundaries", pattern=".shp")
la_boundaries <- st_read(paste0("./data/boundaries/", temp_boundary_file)) 
# Delete directory with large shapefile.
unlink("./data/boundaries", recursive=TRUE)

# Boundary data for regions 
url <- "https://opendata.arcgis.com/datasets/324a9f3ad2cc4a049e913dddc508aeb2_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
download.file(url, "./data/regional_boundaries.zip", mode="wb")
unzip("./data/regional_boundaries.zip", exdir="./data/regional_boundaries")

# Files have complex names that vary on download, so record and use on reading-in.
temp_boundary_file <- list.files("./data/regional_boundaries", pattern=".shp")
regional_boundaries <- st_read(paste0("./data/regional_boundaries/", temp_boundary_file)) 
# Delete directory with large shapefile.
unlink("./data/regional_boundaries", recursive=TRUE)
# Set geom to GBNG.
regional_boundaries <- regional_boundaries %>% st_transform(crs=27700)
la_boundaries <- la_boundaries %>% st_transform(crs=27700)
# Join.
la_boundaries <- la_boundaries %>% 
  st_join(regional_boundaries %>% select(region_name=rgn19nm), largest=TRUE)
# Simplify shapefile.
la_boundaries <- ms_simplify(la_boundaries, keep=0.01) 
# Write out to data folder.
st_write(la_boundaries %>% select(area_code=ctyua19cd, area_name=ctyua19nm, easting=bng_e, northing=bng_n, region_name) , 
         "./data/la_boundaries.geojson")
# Read in.
la_boundaries <- st_read("./data/la_boundaries.geojson") %>% 
  st_transform(crs=27700)
# Define a tibble for reordering regions
regional_layout <- tibble(
  region_name=(c("London", "North West", "Yorkshire and The Humber", "North East",
                 "West Midlands", "East Midlands", "South West", "East of England", "South East")),
  grid_x=c(2,1,2,2,1,2,1,3,3),
  grid_y=c(4,2,2,1,3,3,4,3,4)
)
write_csv(regional_layout,"./data/regional_layout.csv")

# -------------------------------------------
# C O M P U T E
# -------------------------------------------

data <- data %>%
  group_by(area_code) %>% 
  mutate(
    cases_mov_avg=roll_mean(cases,7,align="right", fill=0),
    cases_total_local=max(cases_cum),
    start_count=as.numeric(cases_cum>100)) %>% 
  ungroup %>% group_by(area_type) %>% 
  mutate(
    cases_total_global=max(cases_cum), 
    cases_max_mov=max(cases_mov_avg),
    cases_prop_mov=cases_mov_avg/cases_max_mov
  ) %>% 
  ungroup
  
# -------------------------------------------
# V I S
# -------------------------------------------
# theme_void makes view composition with annotate_custom and gridExtra easier.
theme_set(theme_void(base_family="Iosevka Light"))

# Create animation for London region.
london_anim <- data %>% 
  filter(date>="2020-03-01", area_name=="London") %>% 
  mutate(
    day_num=cumsum(start_count), 
    milestone=if_else((cases_cum %% 5000 < lag(cases_cum %% 5000,1)), cases_cum,0)
  ) %>% ungroup %>% 
  mutate(
    cases_rescaled=scales::rescale(cases_cum, to=c(-0.5, 0.5), from = c(min(cases_cum), max(cases_cum))),
    time_rescaled=scales::rescale(day_num, to=c(0, 0.5), from = c(1, 67))
  ) %>%  
  filter(date>"2020-03-06")	%>% 
  ggplot()+
  geom_rect(aes(xmin=-.5, xmax=.5,ymin=-.5,ymax=.5), fill="#f0f0f0", colour="#f0f0f0", linejoin = "round")+
  # Case data.
  geom_segment(aes(x=-time_rescaled, xend=0, y=-.5, yend=cases_rescaled), size=0.25, colour="#636363", lineend="round")+
  geom_segment(aes(x=time_rescaled, xend=0, y=-.5, yend=cases_rescaled), size=0.25, colour="#636363", lineend="round")+
  # Cases at lockdown. 
  geom_segment(data=. %>% filter(date=="2020-03-23") %>% select(-date), aes(x=time_rescaled, xend=0, y=-.5, yend=cases_rescaled), size=0.05, colour="#cb181d", lineend="round")+
  geom_segment(data=. %>% filter(date=="2020-03-23") %>% select(-date), aes(x=-time_rescaled, xend=0, y=-.5, yend=cases_rescaled), size=0.05, colour="#cb181d", lineend="round")+
  # Case milestones -- 5000 cases.
  geom_segment(data=. %>% filter(milestone>0) %>% select(-date), aes(x=time_rescaled, xend=0, y=-.5, yend=cases_rescaled), size=0.03, colour="#636363", lineend="round")+
  geom_segment(data=. %>% filter(milestone>0) %>% select(-date), aes(x=-time_rescaled, xend=0, y=-.5, yend=cases_rescaled), size=0.03, colour="#636363", lineend="round")+
  geom_text(data=. %>% select(area_name) %>% unique, aes(x=0,y=0,label=area_name,hjust="centre"), colour="#252525", alpha=0.5, size=2, family="Iosevka Light")+
  xlim(-0.5,0.5)+
  ylim(-0.5,0.5)+
  labs(title="{mday(frame_time)} <span style='font-family: Iosevka Medium'>{month(frame_time, label=TRUE, abbr=FALSE)} </span>")+
  theme(plot.title = element_markdown(size=6, margin=margin(0,0,0,0, unit="pt")))+
  coord_equal()+
  transition_time(date)
  
london_gif <- animate(london_anim, duration=10, fps=10, width=500, height=500, res=500, end_pause=3)

# Create animation for London region -- comparison line.
london_anim_line <- data %>% 
  filter(date>="2020-03-01", area_name=="London") %>% 
  mutate(
    day_num=cumsum(start_count), 
    milestone=if_else((cases_cum %% 5000 < lag(cases_cum %% 5000,1)), cases_cum,0)
  ) %>%
  filter(date>"2020-03-06")	%>% 
  ggplot(aes(x=day_num, y=cases_cum))+
  geom_line(colour="#636363", size=0.1)+
  geom_point(colour="#636363", size=0.1)+
  # Lockdown.
  geom_point(data=. %>% filter(date=="2020-03-23"), colour="#cb181d", size=0.1)+
  geom_segment(data=. %>% filter(date=="2020-03-23"), aes(x=day_num, xend=day_num, y=0, yend=cases_cum), size=0.05, colour="#cb181d", lineend="round")+
  geom_segment(data=. %>% filter(date=="2020-03-23"), aes(x=0, xend=day_num, y=cases_cum, yend=cases_cum), size=0.05, colour="#cb181d", lineend="round")+
  geom_text(data=. %>% filter(date=="2020-03-23"), aes(x=day_num+2, y=cases_cum, label="lockdown"), colour="#cb181d", hjust="left", family="Iosevka Light", size=0.5)+
  # Milestones
  geom_hline(data=. %>% filter(milestone>0) %>% select(-date), aes(yintercept=milestone), size=0.05, colour="#636363")+
  geom_text(data=. %>% filter(milestone>0) %>% select(-date), aes(x=67, y=milestone+320, label=paste0(milestone," cases")), hjust="right", family="Iosevka Light", size=0.4)+
  labs(
    x="days elapsed since first 100 cases", 
    y="cumulative number of cases", 
    title="<span style='font-family:Iosevka Medium; color:white;'>d</span>")+
  theme_classic()+
  theme(
    axis.text=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill="#f0f0f0"),
    text=element_text(family="Iosevka Light"),
    axis.title=element_text(size=2),
    axis.line=element_line(size=0.1), 
    plot.title = element_markdown(size=6, margin=margin(0,0,0,0, unit="pt"))
  )+
  transition_reveal(date)

london_line_gif <- animate(london_anim_line, duration=10, fps=10, width=500, height=500, res=500, end_pause=3)

# Use package:magick for view composition of gifs.
london_mgif <- image_read(london_gif)
london_line_mgif <- image_read(london_line_gif)

london_gif <- image_append(c(london_mgif[1], london_line_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(london_mgif[i], london_line_mgif[i]))
  london_gif <- c(london_gif, combined)
}
image_write(london_gif, "./figures/london.gif")

# Legend for demonstrating mappings.
legend <-  
  ggplot()+
  # Case data.
  geom_segment(aes(x=-.4, xend=0, y=-.5, yend=.25), size=0.7, colour="#636363", lineend="round")+
  geom_segment(aes(x=.4, xend=0, y=-.5, yend=.25), size=0.7, colour="#636363", lineend="round")+
  # Cases at lockdown. 
  # Cases at lockdown. 
  geom_segment(aes(x=.11, xend=0, y=-.5, yend=-.36), size=0.4, colour="#cb181d", lineend="round")+
  geom_segment(aes(x=-.11, xend=0, y=-.5, yend=-.36), size=0.4, colour="#cb181d", lineend="round")+
  # Case milestones -- 5000 cases.
  geom_segment(aes(x=.15, xend=0, y=-.5, yend=-.3), size=0.3, colour="#636363", lineend="round")+
  geom_segment(aes(x=-.15, xend=0, y=-.5, yend=-.3), size=0.3, colour="#636363", lineend="round")+
  geom_segment(aes(x=.19, xend=0, y=-.5, yend=-.11), size=0.3, colour="#636363", lineend="round")+
  geom_segment(aes(x=-.19, xend=0, y=-.5, yend=-.11), size=0.3, colour="#636363", lineend="round")+
  geom_segment(aes(x=.23, xend=0, y=-.5, yend=.08), size=0.3, colour="#636363", lineend="round")+
  geom_segment(aes(x=-.23, xend=0, y=-.5, yend=.08), size=0.3, colour="#636363", lineend="round")+
  geom_text(aes(x=0,y=-.58,label="days since 100 cases",hjust="centre", vjust="top"), colour="#252525", family="Iosevka Light")+
  geom_segment(aes(x=-.4, xend=.4, y=-.57, yend=-.57), arrow=arrow(type="closed", ends="both", length = unit(.08, "inches")), colour="#636363")+
  geom_text(aes(x=-.55,y=-.1,label="cumulative cases"), hjust="centre", angle=90, colour="#252525", family="Iosevka Light")+
  geom_segment(aes(x=-.5, xend=-.5, y=-.5, yend=.3), arrow=arrow(type="closed", ends="last", length = unit(.08, "inches")), colour="#636363")+
  geom_text(aes(x=1,y=.3,label="current cases in animation"), hjust="right", colour="#636363", family="Iosevka-Heavy")+
  geom_text(aes(x=1,y=.2,label="case 'milestones' 5k increments"), hjust="right", colour="#636363", family="Iosevka")+
  geom_text(aes(x=1,y=.1,label="cases at lockdown"), hjust="right", colour="#cb181d", family="Iosevka")+
  #annotate("segment", x=-.4, xend=.4, y=-.5, yend=.5, colour="#636363", size=3, alpha=0.6, arrow=arrow())
  xlim(-0.6,1)+
  ylim(-0.6,0.3)+
  coord_equal()
ggsave("./figures/key.png", legend, width=5, height=3, dpi=500)

# Create animation for regions.
regions_anim <- data %>% 
  inner_join(regional_layout, by=c("area_name"="region_name")) %>%  
  filter(date>="2020-03-01", area_type=="region") %>% 
  group_by(area_name) %>%
  mutate(
         day_num=cumsum(start_count), 
         milestone=if_else((cases_cum %% 5000 < lag(cases_cum %% 5000,1)), cases_cum,0)
  ) %>% ungroup %>% 
  mutate(
    cases_rescaled=scales::rescale(cases_cum, to=c(-0.5, 0.5), from = c(min(cases_cum), max(cases_cum))),
    time_rescaled=scales::rescale(day_num, to=c(0, 0.5), from = c(1, 67)),  
    area_name=if_else(area_name=="Yorkshire and The Humber", "Yorkshire", area_name),
  ) %>%  
  filter(date>"2020-03-06")	%>% 
ggplot()+
  geom_rect(data=. %>% select(area_name, grid_x, grid_y) %>% unique, aes(xmin=-.5, xmax=.5,ymin=-.5,ymax=.5), fill="#f0f0f0", colour="#f0f0f0", linejoin = "round")+
  # additional rect -- acky solution to changing dimensions of plot area
  geom_rect(data=tibble(area_name="", grid_x=4, grid_y=1), aes(xmin=0, xmax=0,ymin=0,ymax=0))+
  geom_rect(data=tibble(area_name="", grid_x=5, grid_y=1), aes(xmin=0, xmax=0,ymin=0,ymax=0))+
  # Case data.
  geom_segment(aes(x=-time_rescaled, xend=0, y=-.5, yend=cases_rescaled, group=area_name), size=0.25, colour="#636363", lineend="round")+
  geom_segment(aes(x=time_rescaled, xend=0, y=-.5, yend=cases_rescaled, group=area_name), size=0.25, colour="#636363", lineend="round")+
  # Cases at lockdown. 
  geom_segment(data=. %>% filter(date=="2020-03-23") %>% select(-date), aes(x=time_rescaled, xend=0, y=-.5, yend=cases_rescaled), size=0.05, colour="#cb181d", lineend="round")+
  geom_segment(data=. %>% filter(date=="2020-03-23") %>% select(-date), aes(x=-time_rescaled, xend=0, y=-.5, yend=cases_rescaled), size=0.05, colour="#cb181d", lineend="round")+
  # Case milestones -- 5000 cases.
  geom_segment(data=. %>% filter(milestone>0) %>% select(-date), aes(x=time_rescaled, xend=0, y=-.5, yend=cases_rescaled), size=0.03, colour="#636363", lineend="round")+
  geom_segment(data=. %>% filter(milestone>0) %>% select(-date), aes(x=-time_rescaled, xend=0, y=-.5, yend=cases_rescaled), size=0.03, colour="#636363", lineend="round")+
  geom_text(data=. %>% select(area_name, grid_x, grid_y) %>% unique, aes(x=0,y=0,label=area_name,hjust="centre"), colour="#252525", alpha=0.5, size=2, family="Iosevka Light")+
  xlim(-0.5,0.5)+
  ylim(-0.5,0.5)+
  labs(title="{mday(frame_time)} <span style='font-family: Iosevka Medium'>**{month(frame_time, label=TRUE, abbr=FALSE)}** </span>",
        caption="<img src='./figures/key.png'height='50' />")+
  facet_grid(grid_y~grid_x)+
  theme(strip.text=element_blank(),
        plot.title = element_markdown(size=8, margin=margin(0,0,0,0, unit="pt")),
        plot.caption = element_markdown(size=4, margin=margin(t=-50,b=0, unit="pt")),
        panel.spacing=unit(-0.1, "lines")
        )+
  coord_equal()+
  transition_time(date)

animate(regions_anim, duration=10, fps=10, width=1700, height=1450, res=500, renderer=gifski_renderer("figures/regions_anim.gif"), end_pause=3)


# Calculate bounding boxes.
london_bbox <- st_bbox(la_boundaries %>% filter(region_name=="London"))
london_width <- unname(london_bbox$xmax)-unname(london_bbox$xmin)
london_height <- unname(london_bbox$ymax)-unname(london_bbox$ymin)
london_aspect <- london_width/london_height
england_bbox <- st_bbox(la_boundaries %>% filter(!is.na(region_name)))
england_width <- unname(england_bbox$xmax)-unname(england_bbox$xmin)
england_height <- unname(england_bbox$ymax)-unname(england_bbox$ymin)

# Plot data for authorities.
plot_data <- data %>% 
  filter(date>="2020-03-01", area_type=="la") %>% 
  group_by(area_name) %>%
  mutate(
    day_num=cumsum(start_count), 
    milestone=if_else((cases_cum %% 1000 < lag(cases_cum %% 1000,1)), cases_cum,0)
  ) %>% ungroup %>% 
  mutate(
    cases_rescaled=scales::rescale(cases_cum, to=c(1500, 1500*50), from = c(min(cases_cum), max(cases_cum))),
    time_rescaled=scales::rescale(day_num, to=c(500, 500*50), from = c(1, 67))
  ) %>%  
  filter(date>"2020-03-06")

# Create animation for authorities.
las_anim <- la_boundaries %>% select(-area_name) %>% 
  inner_join(plot_data) %>% 
  filter(region_name!="London") %>% 
  ggplot()+
  geom_sf(data=. %>% select(area_name) %>% unique, fill="#636363", colour="#636363", alpha=0.2, size=0.01)+
  coord_sf(crs=27700, datum=NA, xlim = c(unname(england_bbox$xmin-6*london_width), unname(england_bbox$xmax)), ylim = c(unname(england_bbox$ymin), unname(england_bbox$ymax)))+
  # Case data.
  geom_segment(aes(x=easting-time_rescaled, xend=easting, y=northing, yend=northing+cases_rescaled, group=area_name), size=0.15, colour="#636363", lineend="round")+
  geom_segment(aes(x=easting+time_rescaled, xend=easting, y=northing, yend=northing+cases_rescaled, group=area_name), size=0.15, colour="#636363", lineend="round")+
  # Cases at lockdown. 
  geom_segment(data=. %>% filter(date=="2020-03-23") %>% select(-date), aes(x=easting-time_rescaled, xend=easting, y=northing, yend=northing+cases_rescaled), size=0.03, colour="#cb181d", lineend="round")+
  geom_segment(data=. %>% filter(date=="2020-03-23") %>% select(-date), aes(x=easting+time_rescaled, xend=easting, y=northing, yend=northing+cases_rescaled), size=0.03, colour="#cb181d", lineend="round")+
  # Case milestones -- 1000 cases.
  geom_segment(data=. %>% filter(milestone>0) %>% select(-date), aes(x=easting-time_rescaled, xend=easting, y=northing, yend=northing+cases_rescaled), size=0.02, colour="#636363", lineend="round")+
  geom_segment(data=. %>% filter(milestone>0) %>% select(-date), aes(x=easting+time_rescaled, xend=easting, y=northing, yend=northing+cases_rescaled), size=0.02, colour="#636363", lineend="round")+
  labs(title="{mday(frame_time)} <span style='font-family: Iosevka Medium'>**{month(frame_time, label=TRUE, abbr=FALSE)}** </span>",
       caption="<img src='./figures/key.png'height='45' />")+
  theme(strip.text=element_blank(),
        plot.title = element_markdown(size=8, margin=margin(0,0,0,0, unit="pt")),
        plot.caption = element_markdown(size=4, margin=margin(t=-50,b=0, unit="pt"), hjust = 0)
  )+
  transition_time(date)

las_gif <- animate(las_anim, duration=10, fps=10, width=1250, height=1000, res=500, end_pause=3)

# Create animation for authorities -- London-only.
london_anim <- la_boundaries %>% 
  inner_join(plot_data) %>% 
  filter(region_name=="London") %>% 
  ggplot()+
  geom_sf(data=. %>% select(area_name) %>% unique, fill="#636363", colour="#636363", alpha=0.2, size=0.01)+
  coord_sf(crs=27700, datum=NA, xlim = c(unname(london_bbox$xmin), unname(london_bbox$xmax)), ylim = c(unname(london_bbox$ymin-1*london_height), unname(london_bbox$ymax+3*london_height)))+
  # Case data.
  geom_segment(aes(x=easting-(time_rescaled/4), xend=easting, y=northing, yend=northing+(cases_rescaled/4), group=area_name), size=0.15, colour="#636363", lineend="round")+
  geom_segment(aes(x=easting+(time_rescaled/4), xend=easting, y=northing, yend=northing+(cases_rescaled/4), group=area_name), size=0.15, colour="#636363", lineend="round")+
  # Cases at lockdown. 
  geom_segment(data=. %>% filter(date=="2020-03-23") %>% select(-date), aes(x=easting-(time_rescaled/4), xend=easting, y=northing, yend=northing+(cases_rescaled/4)), size=0.03, colour="#cb181d", lineend="round")+
  geom_segment(data=. %>% filter(date=="2020-03-23") %>% select(-date), aes(x=easting+(time_rescaled/4), xend=easting, y=northing, yend=northing+(cases_rescaled/4)), size=0.03, colour="#cb181d", lineend="round")+
  # Case milestones -- 1000 cases.
  geom_segment(data=. %>% filter(milestone>0) %>% select(-date), aes(x=easting-(time_rescaled/4), xend=easting, y=northing, yend=northing+(cases_rescaled/4)), size=0.02, colour="#636363", lineend="round")+
  geom_segment(data=. %>% filter(milestone>0) %>% select(-date), aes(x=easting+(time_rescaled/4), xend=easting, y=northing, yend=northing+(cases_rescaled/4)), size=0.02, colour="#636363", lineend="round")+
  theme(strip.text=element_blank(),
        plot.title = element_markdown(size=8, margin=margin(0,0,0,0, unit="pt"))
  )+
  transition_time(date)
london_gif <- animate(london_anim, duration=10, fps=10, width=250, height=1000, res=500, end_pause=3)


las_mgif <- image_read(las_gif)
london_mgif <- image_read(london_gif)

las_gif <- image_append(c(las_mgif[1], london_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(las_mgif[i], london_mgif[i]))
  las_gif <- c(las_gif, combined)
}
image_write(las_gif, "./figures/las.gif")

