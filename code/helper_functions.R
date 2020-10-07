# -------------------------------------------
# R script with helper functions for Covid-19 glyphmaps
# TODO -- generalise by fixing hard coding of size parameters.
# Author: Roger Beecham
# -------------------------------------------

generate_ridge <- function(data, current, cell_size) {
  plot <- data %>% 
    ggplot()+
    geom_sf(data=. %>% select(area_name, fill) %>% unique, aes(fill=fill), colour="#ffffff", size=0.1)+ 
    coord_sf(crs=27700, datum=NA)+
    # Case data.
    geom_segment(data=. %>% filter(date %in% current), aes(x=easting-.5*x, xend=easting, y=northing-cell_size, yend=northing+y-cell_size, group=area_name, colour=colour, size=size, alpha=alpha), lineend="round")+
    geom_segment(data=. %>% filter(date %in% current), aes(x=easting+.5*x, xend=easting, y=northing-cell_size, yend=northing+y-cell_size, group=area_name, colour=colour, size=size, alpha=alpha), lineend="round")+
    # Case milestones.
    geom_segment(data=. %>% filter(milestone>0) %>% select(-date), aes(x=easting-.5*x, xend=easting, y=northing-cell_size, yend=northing+y-cell_size), colour="#636363", size=0.15, lineend="round")+
    geom_segment(data=. %>% filter(milestone>0) %>% select(-date), aes(x=easting+.5*x, xend=easting, y=northing-cell_size, yend=northing+y-cell_size), size=0.15, colour="#636363", lineend="round")
  return(plot)
}

generate_line <- function(data, cell_size) {
  plot <- data %>% 
    ggplot()+
    geom_sf(data=. %>% select(area_name, fill) %>% unique, aes(fill=fill), colour="#ffffff", size=0.1)+ 
    coord_sf(crs=27700, datum=NA, clip="off")+
    geom_point(data=.%>% filter(day_num==1) %>% select(-date), aes(x=easting+x-cell_size, y=northing+y-cell_size), pch=21, size=.5, fill="#636363", colour="#636363", alpha=.7)+
    # Case data.
    geom_path(
      aes(x=easting+x-cell_size, y=northing+y-cell_size, group=area_name, colour=colour, size=size), 
      lineend="round" 
    )
  return(plot)
}

generate_area <- function(data, current, cell_size) {
  plot <- data %>% 
    ggplot()+
    geom_sf(data=. %>% select(area_name, fill) %>% unique, aes(fill=fill), colour="#ffffff", size=0.1, alpha=.1)+ 
    coord_sf(crs=27700, datum=NA)+
    geom_rect(data=. %>% filter(date==max(dates)) %>%  select(-date), 
              aes(xmin=easting-.95*cell_size, xmax=easting+.95*cell_size, ymin=northing-.95*cell_size, ymax=northing-.95*cell_size+height*.5, group=area_name, fill=fill), size=.1, alpha=.2)+ 
    geom_rect(data=. %>% filter(date==max(dates)) %>%  select(-date), 
              aes(xmin=easting-.95*cell_size, xmax=easting+width*(.95*cell_size), ymin=northing-.95*cell_size, ymax=northing-.95*cell_size+height*.5, group=area_name, fill=fill), size=.1, alpha=.4)+ 
    geom_path(aes(x=easting+x-.95*cell_size,y=northing+y-cell_size, group=area_name, colour=fill), fill="transparent", size=.4, alpha=1)+ 
    # Time milestones.
    geom_segment(data=. %>% filter(milestone_days>0) %>% select(-date), aes(x=easting-.95*cell_size+x, y=northing-.9*cell_size,  xend=easting-.95*cell_size+y_2, yend=northing+.9*cell_size, group=area_name, colour=fill), size=.1, lineend="round") 
  return(plot)
}
