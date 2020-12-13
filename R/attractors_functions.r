# model and simulation ####

# define logistic map
logistic_map <- function(p, xn, yn) {
  xn1 <- p[1]*xn*(1 - xn)
  yn1 <- 0
  
  c(xn1, yn1)
}

# define difference equations for {x, y} coordinates (quadratic map)
quadratic_map <- function(p, xn, yn) {
  xn1 <- p[1] + p[2]*xn + p[3]*xn*xn + p[ 4]*xn*yn + p[ 5]*yn + p[ 6]*yn*yn
  yn1 <- p[7] + p[8]*xn + p[9]*xn*xn + p[10]*xn*yn + p[11]*yn + p[12]*yn*yn
  
  c(xn1, yn1)
}

# henon phase function from http://www.complexification.net/gallery/machines/henonPhase/
henon_phase <- function(p, xn, yn) {
  xn1 <- xn * cos(p[1]) - (yn - xn^2) * sin(p[1])
  yn1 <- xn * sin(p[1]) + (yn - xn^2) * cos(p[1])
  
  c(xn1, yn1)
}

# iterate quadratic map
iterate <- function(f, p, x0, y0, iterations, ...) {
  x <- rep(x0, iterations)
  y <- rep(y0, iterations)
  
  for(n in 1:(iterations - 1)) {
    xy <- f(p, x[n], y[n])
    x[n+1] <- xy[1]
    y[n+1] <- xy[2]
  }
  
  tibble(x = x, y = y) %>%
    mutate(n = row_number())
}


# estimate occurrence of a chaotic behaviour with Lyapunov exponent 
L <- function(f, p, x0, y0, iterations = 1000) {
  # put the point nearby and see what happens
  nearby_distance <- 0.000001
  
  xy <- c(x0, y0)
  xy_near <- xy + c(nearby_distance, 0)
  
  # Collect the log distance ratios as we iterate
  sum_log_distance_ratios <- 0
  
  for (n in 1:iterations) {
    xy <- f(p, xy[1], xy[2])
    xy_near <- f(p, xy_near[1], xy_near[2])
    
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

# normalize coordinates for simulated data
normalize_xy <- function(data) {
  
  # remove outliers (usually first iterations)
  data <- data %>% filter(n > 100)
  
  range <- with(data, max(max(x) - min(x), max(y) - min(y)))
  
  data <- data %>%
    mutate(
      x = (x - min(x)) / range,
      y = (y - min(y)) / range
    )
}

# bin coordinates to reduce resolution
bin_xy <- function(data, gridsize = 1000) {
  data %>%
    group_by(x = round(x * gridsize) / gridsize,
             y = round(y * gridsize) / gridsize) %>%
    summarize(n = n())
  
}


# sampling ####

# sample parameter space using @Sprott1993 discrete method 
sample_sequence <- function(set = LETTERS[1:25], length = 12) {
  paste0(sample(set, size = length, replace=TRUE), collapse = "")
}

# translate letter code into parameters
get_parameters <- function(
  string="FIRCDERRPVLD",
  scale = seq(from = -1.2, to = 1.2, by = 0.1)) {
  
  names(scale) <- LETTERS[1:25]
  
  scale[str_split(string, "", simplify = TRUE)]
  
}

# estimate the metrics for density distribution of points per grid cell
density_metric <- function(data, gridsize=20) {
  data %>%
    group_by(x = ntile(x, gridsize), y = ntile(y, gridsize)) %>%
    summarize(n = n()) %>% ungroup() %>% 
    summarise(
      d = n() / gridsize^2,
      m = mean(n) / sum(n) * 100,
      cv = sd(n) / mean(n)
    )
}


# rendering ####

# render a single scatterplot
render_plot <- function(data, color="black", size=0.5, alpha=1/10) {
  
  data %>%
    ggplot(aes(x, y)) +
    geom_point(color = color, size = size, alpha = alpha) +
    coord_equal() + theme_void()
}

# sample and render a sequence of plots in a list
render_sequence <- function(n, length, data) {
  data %>%
    sample(size = n, replace = TRUE) %>% 
    plot_grid(plotlist = ., nrow = length) + 
    theme_void() + theme(plot.margin = unit(c(1,0,1,0), "cm")) 
}

# render a paragraph from an existing text and a set of characters
render_paragraph <- function(text, data, ncol = 80, scale = 0.8) {
  seq <- text %>% str_to_lower() %>% str_split(., "") 
  # join characters with glyph plots, non-match creates NULL plot (blank space with cowplot)
  data <- tibble(character=seq[[1]]) %>%
    left_join(data %>% select(character, pattern, plot = plot_ld)) 
  # render plots here to allow some variability in final layout ?
  plot_grid(plotlist = data$plot, ncol=ncol, scale=scale)
}

