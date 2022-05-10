
################################
###        plot_mover        ###
################################


plot_mover <- function(x, utm_nad, ras_dem, ras_slo){
  
  # this functions serves to determine whether an aim plot
  # would be established in an area with a degree slope less
  # than 26*, if not it will determine whether the point can be shifted
  # 50 meters in the four cardinal directions, and be under the slope
  # threshold. The function will also consider the difference in elevation 
  # between the original point and the shifted points in case the 
  # shift is pushed off a cliff (thanks to the canyon country AIMers for
  # pointing this out!).
  
  # Inputs:
  # 'x' = an sf dataframe, raw AIM points accepted, function will buffer. 
  # 'utm_nad' the utm zone that best characterizes your field office in NAD not WGSS
  # 'ras_dem' your preferred raster dem (dtm) model, we run on 1/3 arc second
  # 'ras_slo' a raster of slopes values, this can be *quickly* created using terra or 
  # raster package, specify output in degrees. Should match resolution of dem. 
  
  ##################################
  ####          shifts           ###
  ##################################
  
  ## this function does the heavy listing for testing the shifted AA centers
  shifts <- function(x){
    
    if(x$Direction == "O"){
      
      geom <- c(st_coordinates(x))
      poly <- as.data.frame(t(matrix(c(
        geom[1], geom[2]),2))
      )# create two row matrix (rows transposed to columns)
      poly <- st_as_sf(poly, coords=c("V1","V2"))
      poly <- st_as_sf(poly)
      
    } else if(x$Direction == "N"){
      
      geom <- c(st_coordinates(x))
      poly <- as.data.frame(t(matrix(c(
        geom[1]+50, geom[2]),2))
      )# create two row matrix (rows transposed to columns)
      poly <- st_as_sf(poly, coords=c("V1","V2"))
      poly <- st_as_sf(poly)
    } else if(x$Direction == "E"){
      
      geom <- c(st_coordinates(x))
      poly <- as.data.frame(t(matrix(c(
        geom[1], geom[2]-50),2))
      )# create two row matrix (rows transposed to columns)
      poly <- st_as_sf(poly, coords=c("V1","V2"))
      poly <- st_as_sf(poly)
      
    } else if (x$Direction == "S"){
      
      geom <- c(st_coordinates(x))
      poly <- as.data.frame(t(matrix(c(
        geom[1]-50, geom[2]),2))
      )
      poly <- st_as_sf(poly, coords=c("V1","V2"))
      poly <- st_as_sf(poly)
      
    } else if(x$Direction == "W"){
      
      geom <- c(st_coordinates(x))
      poly <- as.data.frame(t(matrix(c(
        geom[1], geom[2]+50),2))
      )
      poly <- st_as_sf(poly, coords=c("V1","V2"))
      poly <- st_as_sf(poly)
    }
  }
  
  
  # buffer points and perform initial slope extraction of slope
  buf_pts <- x %>% 
    st_transform(26913) %>% 
    st_buffer(27.5) %>% 
    st_transform(st_crs(slope_degree))
  
  slopes <- raster::extract(ras_slo, initial_process, 
                            method = 'bilinear', fun = mean)
  initial_process <- buf_pts %>%
    mutate(mean_slope = slopes)

  possible_rejects <- initial_process %>% 
    group_by(plot_id) %>% 
    filter(mean_slope >= 25) %>% 
    slice(rep(1:nrow(.), each = 5)) %>% 
    mutate(Direction = c('O', 'N', 'E', 'S', 'W')) %>% 
    st_transform(26913) %>% 
    st_centroid() %>% 
    rownames_to_column() 
  
  out <- split(possible_rejects, f = possible_rejects$rowname) 
  out_data <- st_as_sf(data.table::rbindlist(lapply(out, shifts))) 
  possible_rejects <- possible_rejects %>% 
    st_drop_geometry() %>%
    bind_cols(., out_data) %>% 
    st_as_sf(crs = 26913) %>% 
    st_buffer(27.5) %>% 
    st_transform(st_crs(ras_slo)) 
  
  Slope <- raster::extract(ras_slo, possible_rejects, 
                            method = 'bilinear', fun = mean)
  Elevation <- raster::extract(ras_dem, possible_rejects, 
                           method = 'bilinear', fun = mean)
  
  possible_rejects <- possible_rejects %>% 
    cbind(Slope, Elevation) %>% 
    st_drop_geometry() 
  
  original_pts <- possible_rejects %>% 
    filter(Direction == 'O') %>% 
    slice(rep(1:nrow(.), each = 2)) %>% 
    mutate(ID = rep(LETTERS[1:2], times = nrow(.)/2)) %>% 
    mutate(Elevation = if_else(ID == 'A', Elevation + 30, Elevation - 30))
  
  appear_samplable <- possible_rejects %>% 
    filter(Direction != 'O', Slope <= 25) %>% 
    bind_rows(original_pts) %>% 
    group_by(plot_id) %>% 
    arrange(Elevation) %>% 
    slice(2:nrow(.)-1) %>% 
    filter(Direction != 'O') %>% 
    distinct(plot_id) %>% 
    pull(plot_id)
  
  rejection <- possible_rejects %>% 
    filter(!plot_id %in% appear_samplable) %>% 
    distinct(plot_id) %>% 
    pull(plot_id)
  
  questionable <- c(appear_samplable, rejection)
  `%notin%` <- Negate(`%in%`)
  
  rejections <- x %>% 
    mutate(Comment = case_when(
      plot_id %notin% questionable ~ '',
      plot_id %in% appear_samplable ~ 'Appears can be shifted',
      plot_id %in% rejection ~ 'Office Rejection: Slope >= 50%'
    ))
  
  return(rejections)
  rm(shifts)
}

#################################
###      KML_POINTS_DRAW      ###
#################################

KML_Points_draw <- function(x, path){
  
  # this function takes an input of the initial process sheet of an 
  # AIM sample design and outputs as many KML files as strata. 
  # Note if the number of strata exceed 8, than the excess strata will
  # share the same colour as KML points. 
  
  # Inputs an initial process datasheet as an sf object. 
  
  # section 1 generate the appropriate points for assignment to strata
  base_url <- 'http://maps.google.com/mapfiles/ms/micons/'
  url_suffix <- '.png'
  opts_base <- paste0(base_url, c('yellow', 'blue', 'green', 'lightblue', 
                                  'orange', 'pink', 'purple', 'red'), url_suffix)
  opts_oversamples <- paste0(base_url, 
                             c('yellow-dot', 'blue-dot', 'green-dot', 'ltblue-dot', 
                               'orange-dot', 'pink-dot', 'purple-dot', 'red-dot'),
                             url_suffix)
  r
  rm(base_url, url_suffix)
  
  # section 2 write path to each file and file name
  
  save_path <- paste0(here(), '/results/', path)
  
  # section 3 identify the number of strata and determine order of colours
  
  x <- x %>% 
    mutate(stratum = str_remove(plot_id, '-[0-9]{3}'))
  
  colours_req <- x %>% 
    count(stratum) %>% 
    arrange(n) %>%  
    st_drop_geometry()
  
  excess_grps <- colours_req %>% 
    slice_head(n = ( nrow(colours_req)-length(opts_base) ) + 1 ) %>% 
    pull(stratum)
  
  core_grps <- colours_req %>% 
    slice_tail(n = length(opts_base) - length(excess_grps) ) %>% 
    pull(stratum)
  
  stratum_cols <- colours_req %>% 
    arrange(-n) %>% 
    mutate(color_grp = if_else(stratum %in% excess_grps, 'Secondary', 'Primary')) %>% 
    dplyr::select(-n) %>% 
    right_join(., x, by = 'stratum')
  
  rm(excess_grps, core_grps, colours_req)
  
  # section 4 prepare all attributes of files (NOTE THIS CODE SUCKS !!!!)
  
  stratum_cols <- stratum_cols %>% 
    mutate(fname = paste0(stratum, '_',
                          str_remove(Sys.Date(), '-[0-9]{2}-[0-9]{2}'), '_',
                          if_else(next_in_line_or_oversample == 'Yes', 'Base', 'OS')
    )) %>% 
    mutate(fpath = paste0(save_path, '/', fname, '.kml')) 
  
  primary_strata_cols <- stratum_cols %>% 
    filter(next_in_line_or_oversample == 'Yes', color_grp == 'Primary') %>% 
    distinct(fname)
  secondary_strata_cols <- stratum_cols %>% 
    filter(next_in_line_or_oversample == 'Yes', color_grp == 'Secondary') %>% 
    distinct(fname)
  
  cols_base1 <- bind_cols('icon' = opts_base[1:nrow(primary_strata_cols)], primary_strata_cols)
  cols_base2 <- bind_cols('icon' = opts_base[(nrow(primary_strata_cols)+1):length(opts_base)], secondary_strata_cols)
  colors_BASE <- bind_rows(cols_base1, cols_base2)
  
  # repeat for over-samples
  
  primary_strata_cols <- stratum_cols %>% 
    filter(next_in_line_or_oversample == 'Oversample', color_grp == 'Primary') %>% 
    distinct(fname)
  secondary_strata_cols <- stratum_cols %>% 
    filter(next_in_line_or_oversample == 'Oversample', color_grp == 'Secondary') %>% 
    distinct(fname)
  
  cols_base1 <- bind_cols('icon' = opts_oversamples[1:nrow(primary_strata_cols)], primary_strata_cols)
  cols_base2 <- bind_cols('icon' = opts_oversamples[(nrow(primary_strata_cols)+1):length(opts_oversamples)], secondary_strata_cols)
  
  colors <- bind_rows(cols_base1, cols_base2, colors_BASE)
  stratum_cols <- left_join(stratum_cols, colors, by = 'fname')
  
  # 5 create files
  
  files <- stratum_cols %>% 
    split( ~ .$fname)
  
  write_files <- function(y){
    
    file <-  st_as_sf(y) %>% 
      st_transform(4326) %>% 
      as('Spatial')
    
    kmlPoints(file, 
              kmlfile = file$fpath[1], 
              name = file$plot_id,
              icon = file$icon[1], 
              kmlname = file$fname[1]
    )
  }
  
  lapply(files, write_files)
}


###################################
###     PUBLIC LANDS COLORS     ###
###################################

public_lands_colours <- setNames(
  
  # these manually transcribed from the H-1553-Publications Standards Manual
  # Handbook - hopefully no errors.
  # [H-1553](https://www.ntc.blm.gov/krc/uploads/223/Ownership_Map_Color_Reference_Sheet.pdf)
  
  c( # colours
    rgb(254, 230, 121, max = 255), # BLM
    rgb(204, 235, 197, max = 255), # USFS
    rgb(202, 189, 220, max = 255), # NPS
    rgb(127, 204, 167, max = 255), # FWS
    rgb(255, 255, 179, max = 255), # USBR
    rgb(253, 180, 108, max = 255), # TRIB
    rgb(251, 180, 206, max = 255), # DOD
    rgb(228, 196, 159, max = 255), # OTHF
    rgb(179, 227, 238, max = 255), # SLB
    rgb(255, 255, 255, max = 255), # PVT
    rgb(143, 181, 190, max = 255) # CITY CNTY
  ), 
  
  c( # names
    'BLM', 'USFS', 'NPS', 'FWS', 'USBR', 'TRIB', 'DOD', 'OTHF', 'SLB', 'PVT', 'CITY_CNTY_SDC_SDNR_SPR'
  ))


#####################################
###       KML_Polygons_draw       ###
#####################################

KML_Polygons_draw <- function(x, path){
  
  # this function takes an input of the initial process sheet of an 
  # AIM sample design and outputs as many KML files as strata. 
  # Note if the number of strata exceed 8, than the excess strata will
  # share the same colour as KML points. 
  
  # Inputs an initial process datasheet as an sf object. 
  
  # section 1 modify public lands colours to dataframe
  
  pl_colours <- data.frame(public_lands_colours) %>% 
    rownames_to_column('Mang_Name')
  
  # section 2 write path to each file and file name
  
  save_path <- paste0(here(), '/results/', path)
  
  # section 3 prep data for creating polygons
  
  data_cols <- left_join(x, pl_colours, by = 'Mang_Name') %>% 
    mutate(fname = paste0(Mang_Name, '_land_', 
                          str_remove(Sys.Date(), '-[0-9]{2}-[0-9]{2}')
    )) %>% 
    mutate(fpath = paste0(save_path, '/', fname, '.kml')) %>% 
    mutate(public_lands_colours = paste0("'", public_lands_colours, "'"))
  
  # 5 create files
  
  files <- data_cols %>% 
    split( ~ .$fname)
  
  write_files <- function(y){
    
    file <-  st_as_sf(y) %>% 
      st_transform(4326) %>% 
      as('Spatial')
    
    kmlPolygon(file,
               kmlfile = file$fpath[1],
               name = file$Mang_Name[1], 
               description = file$Mang_Name[1], 
               col = file$public_lands_cols[1],
               visibility = 1, lwd = 1, border = "black")
    
  }
  lapply(files, write_files)
  
}
