# input ####

# read and merge DEM tiles from IGN DBALTI or RGEALTI as a function of WGS84 coordinates
read_dem <- function(coord, buffer = 1000, dep, source="db_alti"){
  
  # define map location in IGN CRS
  location <- data.frame(
    lon = coord["lon"],
    lat = coord["lat"]) %>% 
    st_as_sf(coords = c("lon", "lat")) %>% 
    st_set_crs(4326) %>% 
    st_transform(crs = 2154) 
  
  # load grid corresponding to selected data source
  switch(
    source,
    
    db_alti = {
      grid <- read_sf(glue::glue("data/private/{source}/shp/dalles.shp"))
    },
    
    rge_alti = {
      # TODO get departement code corresponding to coordinates
      # https://geo.api.gouv.fr/adresse
      # TODO handle cases with cross departements datasets
      grid <- read_sf(glue::glue("data/private/{source}/shp/{dep}/dalles.shp"))
    }
  )
  
  # list tile containing requested location plus buffer
  list_tiles <- st_intersection(grid, location %>% st_buffer(buffer)) %>% pull(NOM_DALLE)
  
  # read corresponding tiles and merge them 
  dem_list <- tibble(path = glue::glue("data/private/{source}/data/{list_tiles}.asc")) %>%
    mutate(dem = map(path, ~ read_stars(.) %>% set_names("z") %>% st_set_crs(2154)))
  
  dem <- reduce(dem_list$dem, st_mosaic)
  
  return(dem)
  
}

# define a rectangle bounding box with a given ratio around a location (sf object of type point)
buffer_rectangle <- function(
  data, size = 60000, x_shift=0, y_shift=0,
  orientation="h", ratio = 297/210, ...){
  
  switch(
    orientation,
    h = {
      x_size = size / 2
      y_size = (size / ratio) / 2},
    v = {
      x_size = (size / ratio) / 2
      y_size = size / 2}
  )
  
  st_bbox(data) + 
    c(- x_size, - y_size, x_size, y_size) +
    c(x_shift, y_shift, x_shift, y_shift)
}


# rendering ####

# iterate to create geom_sf layers as a function of a dataframe
geom_waterline <- function(
  data, n = 5, r = 2, d0 = 50,
  color="black", scale = 1, ...) {
  
  plot <- tibble(
    buffer = accumulate(seq(d0, by=1, length.out = n), ~ . * r),
    shade = seq(0.5, 0.9, length.out = n)
  ) %>%
    mutate(
      layers = map2(
        buffer, shade,
        ~ geom_sf(
          data = data %>% st_buffer(.x), # %>% st_crop(data),
          fill = NA, color = colorspace::lighten(color, .y), size = 1/10 * scale)
      ))
  
  return(plot$layers)
  
}


# render a DEM as contour plot with waterlines
render_contour <- function(
  data, coord, area_min = 2e4, length_min = 600,
  alt_min, alt_max, alt_contour = 100,
  n_water = 5, scale = 1,
  outline = FALSE, layers = FALSE, ...) {
  
  # define a sf point object corresponding to given coordinates  
  point <- data.frame(lon = coord["lon"], lat = coord["lat"]) %>% 
    st_as_sf(coords = c("lon", "lat")) %>% st_set_crs(4326) %>% st_transform(crs = 2154) 
  
  # define map layers with contour lines
  # TODO simplify polygons to speed up plotting with rmapshaper::ms_simplify
  # TODO crop before contour
  # TODO test isoband package to speed up calculations
  
  layer_shore <- data %>%
    st_contour(breaks = c(0, alt_min), contour_lines = TRUE) %>%
    st_crop(buffer_rectangle(point, ...)) %>% 
    filter(st_length(.) > units::set_units(length_min,"m"))
  
  bb <- st_bbox(layer_shore)
  
  switch(
    as.character(outline),
    
    "FALSE" = {  
      
      breaks_major <- seq(alt_min, alt_max, by = alt_contour)
      
      breaks_minor <- seq(alt_min, alt_max, by = alt_contour / 2)
      
      # get a multipolygon object for water, used to compute waterlines
      layer_water <- data %>%
        st_contour(breaks = c(0, alt_min)) %>%
        st_buffer(dist = 0) %>% 
        st_crop(buffer_rectangle(point, ...)) %>%
        st_cast("MULTIPOLYGON") %>% slice(2) %>% 
        st_cast("POLYGON") %>%
        filter(st_area(.) > units::set_units(area_min,"m^2")) %>% 
        st_combine()
      
      layer_contour_minor <- data %>%
        st_contour(
          breaks = breaks_minor[! breaks_minor %in% breaks_major],
          contour_lines = TRUE) %>%
        st_crop(buffer_rectangle(point, ...)) %>% 
        filter(st_length(.) > units::set_units(length_min,"m"))
      
      layer_contour_major <- data %>%
        st_contour(
          breaks = breaks_major[! breaks_major %in% alt_min],
          contour_lines = TRUE) %>%
        st_crop(buffer_rectangle(point, ...)) %>% 
        filter(st_length(.) > units::set_units(length_min,"m"))
      },
    
    "TRUE" = {}
  )
  
  # plot contour lines 
  
  switch(
    as.character(outline),
    
    "FALSE" = {
      
      plot_waterline <- geom_waterline(data = layer_water, n = n_water, ...)
      
      plot_contour_minor <- geom_sf(
        data = layer_contour_minor,
        color = colorspace::lighten("black", 0.6), size=1/10 * scale)
      
      plot_contour_major <- geom_sf(
        data = layer_contour_major,
        color = colorspace::lighten("black", 0.4), size=2/10 * scale)
      
      plot_shore <- geom_sf(
        data = layer_shore,
        color="black", size = 4/10 * scale)
      
      coord_box <- coord_sf(xlim=c(bb$xmin, bb$xmax), ylim=c(bb$ymin, bb$ymax))
  
      plot <- ggplot() + plot_waterline + plot_contour_minor +
        plot_contour_major + plot_shore + theme_void() 
      
      plot_layers <- list(
        shore = plot_shore,
        contour_minor = plot_contour_minor,
        contour_major = plot_contour_major,
        waterline = plot_waterline,
        coord_box = coord_box)
      
    },
    "TRUE" = {
      plot <- ggplot() + 
        geom_sf(
          data = layer_shore,
          color="black", size = 4/10 * scale) +
        theme_void() 
    }
  )
  
  switch(
    as.character(layers),
    "TRUE" = {return(plot_layers)},
    "FALSE" = {return(plot)}
  )
  
}

# render a DEM with elevation as a function of longitude, grouped per latitude (ridge plot)
render_ridge <- function(
  data, 
  n_ridges = 200,     # number of ridges 
  n_drop = 0,         # number of ridges to drop in distance
  n_lag = 100,        # number of neighbors used to remove points of current ridge
  z_shift = 15,       # distance to shift successive ridges (m)
  z_threshold = 10   # distance threshold to remove points between successive ridges (m)
  ){
  
  # set n_ridges to max number in data if parameter is 0
  n_ridges <- ifelse(n_ridges == 0, data %>% distinct(y) %>% nrow(), n_ridges)
  
  # keep a fixed number of distinct ridges 
  data_index <- data %>%
    distinct(y) %>% arrange(y) %>% 
    slice(seq(1, (n() - n_drop), len = n_ridges) %>% as.integer()) %>%
    mutate(
      y_rank = rank(y),
      y_dist = scales::rescale(y, to=c(0,1)),
      d = 1:n() * z_shift
    )
  
  # compute z shift as a function of ridge index 
  data_shift <- data %>% 
    inner_join(data_index) %>%
    mutate(xn = x - min(x)) %>% 
    arrange(y) %>% group_by(x) %>% 
    mutate(
      zs = z + d,
      z_rank = rank(zs)
    )
  
  # define window functions for multiple lag positions
  list_distance <- map(glue::glue("~ . - lag(., n = {1:n_lag})"), ~ as.formula(.))
  list_col <- glue::glue("zs_{1:n_lag}")
  
  # remove points hidden by foreground ridges :
  # distance between successive y for each x is less than a threshold (default 0)
  data_ridge <- data_shift %>% 
    mutate(across(zs, .fns = list_distance)) %>% ungroup() %>% 
    mutate(
      zn = case_when(
        if_any(all_of(list_col), ~ . < z_threshold) ~ NA_real_,
        TRUE ~ zs)
    ) %>% select(- all_of(list_col))
  
  return(data_ridge)
  
}

filter_ridge <- function(
  data,
  length_n = 10, # length of ridge segments to be filtered (cells)
  length_x = 5/100, # proportion of ridge length to be filtered (%)
  dist_y = 0.95 # view distance to be filtered
  ) {
  
  # filter for ridge elements shorter than a threshold (cell number)
  data_cell <- data %>%
    mutate(zl = cumsum(is.na(zn))) %>%
    group_by(zl) %>% mutate(zl_n = n()) %>% ungroup() %>% 
    mutate(zn = if_else(zl_n <= length_n, NA_real_, zn)) # %>% 
    # mutate(zn = if_else((zl_n <= 2 & dist_y >= 0.90), NA_real_, zn)) # f(distance)
  
  # filter for ridge lines with a low contribution
  data_line <- data %>%
    group_by(y_rank, y_dist) %>% 
    summarise(xp = sum(!is.na(zn)) / n()) %>% 
    filter(xp < length_x, y_dist > dist_y)
  
  data_filter <- data_cell %>% anti_join(data_line)
  
  return(data_filter)
  
}







