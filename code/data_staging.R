
# -------------------------------------------
# D A T A   S T A G I N G
# -------------------------------------------

# -------------------------------------------
# L I B R A R I E S
# -------------------------------------------

# Required libraries.
library(tidyverse)
library(sf)
library(lubridate)
library(RcppRoll)
library(cartogram)

# -------------------------------------------
# S T A G E
# -------------------------------------------

plot_data <- data %>% 
  filter(date>="2020-03-01", area_type=="la") %>% 
  group_by(area_name) %>%
  mutate(
    # Start counting from first 100 cases.
    start_count=as.numeric(cases_cum>100),
    day_num=cumsum(start_count), 
    milestone=if_else((cases_cum %% 1000 < lag(cases_cum %% 1000,1)), as.integer(cases_cum/1000)*1000,0),
    # Check whether has reached daily peak new cases (7day rolling avg).
    has_peaked=!day_num < max(if_else(cases_mov_avg_local==cases_max_mov_local,day_num, 0)),
    ordinal_dist_peak=case_when(
      cases_mov_avg_local <=.8*cases_max_mov_local & !has_peaked ~ 1,
      cases_mov_avg_local >.8*cases_max_mov_local & !has_peaked ~ 2,
      cases_mov_avg_local >.8*cases_max_mov_local & has_peaked ~ 3,
      cases_mov_avg_local <=.8*cases_max_mov_local & has_peaked ~ 4,
      TRUE ~ 0
    ),
    dist_from_peak=if_else(cases_mov_avg_local <=.6*cases_max_mov_local, .3, 1),
  ) %>% ungroup %>% 
  mutate(
    #Recode Cornwall and Hackney.
    area_name=if_else(area_name=="Cornwall and Isles of Scilly", "Cornwall", area_name),
    area_name=if_else(area_name=="Hackney and City of London", "Hackney", area_name),
    relative_cases=cases_cum/pop,
    cases_mov_local_rescaled=
      scales::rescale(cases_mov_avg_local/cases_max_mov_local, to=c(0.3,1), 
                      from=range(cases_mov_avg_local/cases_max_mov_local, na.rm=TRUE))
  ) %>%  
  filter(date>"2020-03-06") %>%  group_by(area_code) %>%
  mutate(
    current_exposure=max(relative_cases),
    moving_exposure=relative_cases/current_exposure
  ) %>% ungroup

dates <- plot_data %>% pull(date) %>%  unique

# Calculate bounding boxes.
london_bbox <- st_bbox(la_boundaries %>% filter(region_name=="London"))
london_width <- unname(london_bbox$xmax)-unname(london_bbox$xmin)
london_height <- unname(london_bbox$ymax)-unname(london_bbox$ymin)
london_aspect <- london_width/london_height
england_bbox <- st_bbox(la_boundaries %>% filter(!is.na(region_name)))
england_width <- unname(england_bbox$xmax)-unname(england_bbox$xmin)
england_height <- unname(england_bbox$ymax)-unname(england_bbox$ymin)


#library(ggrepel)
# Continous area cartogram
england_boundaries <- la_boundaries %>% 
  inner_join(plot_data %>% group_by(area_code) %>% summarise(pop=first(pop)) %>% ungroup %>% add_row(area_code="E09000001", pop=8706)) 
england_cartogram <- cartogram::cartogram_cont(england_boundaries, weight="pop", itermax=30)
carto_centroids <- england_cartogram  %>% st_centroid() %>% st_coordinates() %>% as_tibble() %>% rename("easting"="X", "northing"="Y")
england_cartogram <- england_cartogram %>%
  mutate(easting=carto_centroids$easting, northing=carto_centroids$northing, type="grid")
rm(carto_centroids)

# Smwg
# Update plot_data recode Cornwall and City of London.
plot_data <- plot_data %>% 
  mutate(
    area_name=if_else(area_name=="Cornwall and Isles of Scilly", "Cornwall", area_name),
    area_name=if_else(area_name=="Hackney and City of London", "Hackney", area_name))
smwg_layout <- read_csv("./data/smwg_layout.csv") %>% select(-region_name) %>% inner_join(la_boundaries %>% st_drop_geometry %>% select(area_name, region_name))
# Make a grid over England 
bbox_england<-st_bbox(england_boundaries)
# Increase bbox by factor of 1.25 along x
offset_x <- 1.25*(bbox_england$xmax-bbox_england$xmin)-(bbox_england$xmax-bbox_england$xmin)
# Increase bbox by factor of 1.05 along y
offset_y <- 1.05*(bbox_england$ymax-bbox_england$ymin)-(bbox_england$ymax-bbox_england$ymin)
# Update bounds.
bbox_england[1] <- bbox_england[1]-.5*offset_x
bbox_england[3] <- bbox_england[3]+.5*offset_x
bbox_england[4] <- bbox_england[4]+offset_y
smwg_sf <- st_sf(geom=st_make_grid(st_as_sfc(bbox_england), n=c(20,20), what="polygons")) %>% mutate(id=row_number())
# Store gid cell locations and add as fields.
x <- rep(1:20,20)
y <- vector(length=length(x))
for(i in 1:20) {
  for(j in 1:20) {
    index <- (i-1)*20+j
    y[index] <- scales::rescale(i,c(20,1),c(1,20))
  }  
}
smwg_sf <- smwg_sf %>% add_column(x=x, y=y) %>% 
  inner_join(smwg_layout, by=c("x"="gridX", "y"="gridY")) %>% 
  st_cast(to="MULTIPOLYGON") %>% 
  rename("geometry"="geom") %>%
  arrange(id) %>% 
  inner_join(plot_data %>% select(area_name) %>% unique %>% add_row(area_name="City of London"))

smwg_regions <- smwg_sf %>% group_by(region_name) %>%  summarise()

# Calculate grid centroids.
smwg_centroids <- smwg_sf  %>% st_centroid() %>% st_coordinates() %>% as_tibble() %>% rename("easting"="X", "northing"="Y")
# Add to smwg_sf.
smwg_sf <- smwg_sf %>%
  mutate(easting=smwg_centroids$easting, northing=smwg_centroids$northing, type="grid")
rm(smwg_centroids)
# Find smwg cell size.
cell_height <- smwg_sf %>% st_drop_geometry() %>% 
  filter(case_when(x==9 & y==10 ~ TRUE,
                   x==10 & y==10 ~ TRUE,
                   TRUE ~ FALSE)) %>% 
  transmute(diff=easting-lag(easting,1)) %>% filter(!is.na(diff), diff>0) %>% pull
smwg_bounds <- st_bbox(smwg_sf)

# Update plot_data with rescaling for smwg on area chart and also according to 
# dimensions of smwg as that is our preferred arrangment.
plot_data <- plot_data %>% 
  group_by(area_name) %>% 
  mutate(
    # Local rescaling for background area chart.
    cases_rescaled_mov=scales::rescale(cases_mov_avg_local, to=c(.01*cell_height, .9*cell_height), from = c(min(cases_mov_avg_local), max(cases_mov_avg_local)))
  ) %>%  ungroup %>% 
  mutate(
    # Global rescaling cumulative cases chart.
    cases_rescaled=scales::rescale(cases_cum, to=c(0, 2*cell_height), from = c(min(cases_cum), max(cases_cum))),
    time_rescaled=scales::rescale(day_num, to=c(0, cell_height), from = c(1, max(day_num)))
  )

