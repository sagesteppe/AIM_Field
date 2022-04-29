
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


