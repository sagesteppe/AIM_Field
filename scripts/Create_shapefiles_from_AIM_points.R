library(tidyverse)
library(sf)

initial_process <- readxl::read_excel(paste0(here(),  '/data/raw/PlotTracking_AIM_plots_2022-COPY.xlsx'), 
                              sheet = 3) %>% 
  janitor::clean_names() 

initial_process <- initial_process %>% 
  drop_na(long:lat) %>% 
  filter(long != 'SAMPLED IN 2018') %>% 
  st_as_sf(coords = c(x = 'long', y = 'lat'), crs = 4269, remove = F) %>% 
  select(any_of(c('plot_id', 'panel', 'long', 'lat', 'next_in_line_or_oversample'))) %>% 
  rename('Point_Type' = next_in_line_or_oversample)

st_write(initial_process, paste0(here(), '/data/raw/AIM_Points_2022.shp'), quiet = T)

rm(initial_process)
