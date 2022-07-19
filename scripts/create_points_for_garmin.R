library(tidyverse)
library(sf)
library(here)

initial_process <- readxl::read_excel(paste0(here(),  '/AIM_Field/data/raw/PlotTracking_AIM_plots_2022-COPY.xlsx'), 
                                      sheet = 3) %>% 
  janitor::clean_names() 

initial_process <- initial_process %>% 
  drop_na(long:lat) %>% 
  filter(long != 'SAMPLED IN 2018') %>% 
  st_as_sf(coords = c(x = 'long', y = 'lat'), crs = 4269, remove = F) %>% 
  select(any_of(c('plot_id'))) %>%
  rename(name = 'plot_id') %>% 
  st_transform(4326) %>% 
  as('Spatial') 


writeOGR(initial_process,
         dsn= paste0(here(),  '/AIM_Field/data/processed/AIM_2022.gpx'), layer="waypoints", driver="GPX",
         dataset_options="GPX_USE_EXTENSIONS=yes")

# we then drag and drop these data onto the garmin waypoint area! one problem is they arranged by distance from where you do the process rather than alphabetical order...