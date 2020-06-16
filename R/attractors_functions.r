# math functions ####

# define logistic map
logistic <- function(a, xn, yn) {
  xn1 <- a[1]*xn*(1 - xn)
  yn1 <- 0
  
  c(xn1, yn1)
}

# define difference equations for {x, y} coordinates (quadratic map)
quadratic_2d <- function(a, xn, yn) {
  xn1 <- a[1] + a[2]*xn + a[3]*xn*xn + a[ 4]*xn*yn + a[ 5]*yn + a[ 6]*yn*yn
  yn1 <- a[7] + a[8]*xn + a[9]*xn*xn + a[10]*xn*yn + a[11]*yn + a[12]*yn*yn
  
  c(xn1, yn1)
}

# henon phase function from http://www.complexification.net/gallery/machines/henonPhase/
henon_phase <- function(a, xn, yn) {
  xn1 <- xn * cos(a[1]) - (yn - xn^2) * sin(a[1])
  yn1 <- xn * sin(a[1]) + (yn - xn^2) * cos(a[1])
  
  c(xn1, yn1)
}


# iterate quadratic map
iterate <- function(step_fn, a, x0, y0, iterations, ...) {
  x <- rep(x0, iterations)
  y <- rep(y0, iterations)
  
  for(n in 1:(iterations - 1)) {
    xy <- step_fn(a, x[n], y[n])
    x[n+1] <- xy[1]
    y[n+1] <- xy[2]
  }
  
  tibble(x = x, y = y) %>%
    mutate(n = row_number())
}


# simulation functions ####

# estimate occurrence of a chaotic behaviour
L <- function(step_fn, a, x0, y0, iterations = 1000) {
  # Really, put the point nearby and see what happens
  nearby_distance <- 0.000001
  
  xy <- c(x0, y0)
  xy_near <- xy + c(nearby_distance, 0)
  
  # Collect the log distance ratios as we iterate
  sum_log_distance_ratios <- 0
  
  for (n in 1:iterations) {
    xy <- step_fn(a, xy[1], xy[2])
    xy_near <- step_fn(a, xy_near[1], xy_near[2])
    
    new_distance = sqrt((xy[1] - xy_near[1])^2 + (xy[2] - xy_near[2])^2)
    
    if (new_distance == 0) {
      # The points have converged
      return (-Inf)
    }
    
    if (abs(new_distance) == Inf) {
      # The points have run away
      return (Inf)
    }
    
    # Move near point after xy
    # We put the near point just behind in the direction that they differ
    angle = atan2(xy_near[2] - xy[2], xy_near[1] - xy[1])
    xy_near <- c(xy[1] + nearby_distance * cos(angle),
                 xy[2] + nearby_distance * sin(angle))
    
    
    sum_log_distance_ratios = sum_log_distance_ratios + log2(new_distance / nearby_distance)
    
  }
  
  sum_log_distance_ratios / iterations
}

# estimate the quantile of density distribution per grid cell
density_quantile <- function(data, gridsize=20) {
  data %>%
    group_by(x = ntile(x, gridsize), y = ntile(y, gridsize)) %>%
    summarize(n = n()) %>% nrow()
}

# estimate 2D point density
density_heatmap <- function(data, gridsize=20, bw=1/10) {
  matrix <- with(data, MASS::kde2d(x, y, n=gridsize, h=bw))$z %>% scale(.)
  
  matrix_long <- matrix %>% reshape2::melt() %>%
    set_names(c("x","y","value")) %>% as_tibble()
  
  return(matrix_long)
}


# low resolution simulation of a single parameter set
compute_grid_data <- function(a, iterations, ...) {
  a %>%
    mutate(xy = future_map(a, ~ iterate(quadratic_2d, ., 0, 0, iterations))) %>%
    # Remove those who have grown very large / might run away
    filter(map_lgl(xy, function(d) with(d, all(abs(x) + abs(y) < 1e7)))) %>%
    mutate(xy = map(xy, normalize_xy))
}

# high resolution simulation of a single parameter set
# data is binned by cells with n as the density of points in this cell
compute_print_data <- function(a, iterations, gridsize, ...) {
  data <- iterate(quadratic_2d, a, x0=0, y0=0, iterations) 
  
  data_scaled <- data %>% 
    mutate(range = max(max(x) - min(x), max(y) - min(y))) %>%
    mutate(x = (x - min(x)) / range,
           y = (y - min(y)) / range)
  
  data_density <- data_scaled %>%
    group_by(x = round(x * gridsize) / gridsize,
             y = round(y * gridsize) / gridsize) %>%
    summarize(n = n())
  
  return(data_density)
}

# high resolution simulation of a single parameter set
# each core is used for a different path with the same parameter set
# data is binned by cells with n as the density of points in this cell
compute_parallel_data <- function(a, iterations, gridsize, ...) {
  CPU_cores <- parallel::detectCores()
  
  data <- tibble(thread = 1:CPU_cores) %>%
    mutate(x0 = runif(length(thread), -0.1, 0.1),
           y0 = runif(length(thread), -0.1, 0.1)) %>%
    mutate(xy = future_pmap(
      .,
      function(x0, y0, ...) iterate(quadratic_2d, a, x0, y0, iterations / CPU_cores))
    ) %>%
    unnest(xy) 
  
  data_scaled <- data %>% 
    mutate(range = max(max(x) - min(x), max(y) - min(y))) %>%
    mutate(x = (x - min(x)) / range,
           y = (y - min(y)) / range)
  
  data_density <- data_scaled %>%
    group_by(x = round(x * gridsize) / gridsize,
             y = round(y * gridsize) / gridsize) %>%
    summarize(n = n())
  
  return(data_density)
}


# plot functions ####


# normalize coordinates for each simulated graph
normalize_xy <- function(df) {
  range <- with(df, max(max(x) - min(x), max(y) - min(y)))
  
  df %>%
    mutate(x = (x - min(x)) / range,
           y = (y - min(y)) / range)
  
}

render_grid <- function(data, output="facet", color="black", size=0.5, alpha=1/10) {
  
  data <- switch(output,
                 "single" = data,
                 "facet" = unnest(data, xy))
  
  plot <- data %>%
    ggplot(aes(x, y)) +
    geom_point(color = color, size = size, alpha = alpha) +
    coord_equal() + theme_void()
  
  switch(output,
         "single" = return(plot),
         "facet" = return(plot + facet_wrap(~ pattern) + theme_void())
  )
  
}

render_print <- function(data, scale_color = scale_grey, size=0.3) {
  plot <- data %>%
    ggplot(aes(x, y)) +
    geom_point(aes(alpha = sqrt(n), color = log(n)), size = size) +
    scale_alpha_continuous(range = c(0, 1), limits = c(0, NA)) +
    scale_color + coord_equal() + theme_void() + theme(legend.position = "none")
  
  return(plot)
}

# render a single character
render_plot <- function(data, color="black", alpha=0.2, size=0.5) {
  data %>%
    ggplot(aes(x, y)) +
    geom_point(color = color, alpha = alpha, size = size, stroke = 0, shape = 16) +
    coord_equal() + theme_void()
}

# sample and render a sequence of characters in a list
render_sequence <- function(n, length, data) {
  data %>%
    sample(size=n, replace = TRUE) %>% 
    plot_grid(plotlist = ., nrow=length) + 
    theme_void() + theme(plot.margin = unit(c(1,0,1,0), "cm")) 
}

# render a paragraph from an existing text and a set of characters
render_paragraph <- function(text, data, ncol=80, scale=0.9) {
  seq <- text %>% str_to_lower() %>% str_split(., "") 
  # join characters with glyph plots, non-match creates NULL plot (blank space with cowplot)
  data <- tibble(character=seq[[1]]) %>%
    left_join(data %>% select(character, pattern, plot=plot_ld)) 
  # render plots here to allow some variability in final layout ?
  plot_grid(plotlist = data$plot, ncol=ncol, scale=scale)
}

