library(tidyverse)
library(sf)
library(here)

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

# now we can create shapefiles for the SSURGO database query
aim_pts_ssurgo <- initial_process %>% 
  st_transform(26913) %>% 
  st_buffer(805) %>%
  split( ~ plot_id) %>% 
  lapply(st_bbox) %>% 
  lapply(st_as_sfc) %>%
  lapply(st_as_sf) %>% 
  data.table::rbindlist() %>% 
  bind_cols(initial_process %>% 
              dplyr::select(plot_id, Point_Type) %>% 
              st_drop_geometry(), .) %>% 
  rename('geometry' = x, 'PartName' = plot_id) %>% 
  st_as_sf()
  
ggplot(aim_pts_ssurgo) +
  geom_sf(aes(fill = Point_Type)) +
  theme_bw()

st_write(aim_pts_ssurgo %>% 
           filter(Point_Type == 'Yes'), paste0(here(), '/data/raw/AOI_AIM_22_base.shp'),
         quiet = T, append = F)

st_write(aim_pts_ssurgo %>% 
           filter(Point_Type == 'Yes'), paste0(here(), '/data/raw/AOI_AIM_22_over.shp'),
         quiet = T, append = F)

rm(initial_process)
