
# sequence generation ####

# https://en.wikipedia.org/wiki/Collatz_conjecture
# define rules for the Collatz sequence with shortcut for odd numbers
collatz <- function(n) {
  
  if (n == 1) {
    done(n)
  } else {
    case_when(
      n %% 2 == 0 ~ n/2,
      n %% 2 != 0 ~ (3*n + 1) / 2
    )
  }
  
}

# build the sequence recursively
seq_collatz <- function(i, max = 1000) {
  accumulate(1:max, ~ collatz(.), .init = i) %>% head(., -1)
}

# define functions to generate sequences of angles
seq_alt <- function(n, m=60, sd=10) {rnorm(n, m, sd)*(-1)^(1:n)}

# sample from a bimodal distribution from two normal distributions
bimodal <- function(m=c(-60, 60), sd=c(5, 5)) {
  
  dist <- distr::UnivarMixingDistribution(
    distr::Norm(mean = m[1], sd = sd[1]), 
    distr::Norm(mean = m[2], sd = sd[2]),
    mixCoeff=c(0.5, 0.5)) 
  
  return(dist)
  
}


# geometry ####


# define a recursive function to mutate vectors as a function of sequence parameters
transform_vector <- function(.x, .y) {
  .y %>% mutate(
    x = .x$xend,
    y = .x$yend,
    xend = x + .y$length * cos((90 - .y$angle) * pi/180),
    yend = y + .y$length * sin((90 - .y$angle) * pi/180)
  ) 
}

# function to transform path into splines or polygons
transform_path <- function(data, scale=1, width=10, shape="polygon") {
  
  switch(
    shape,
    
    spline = {
      plot <- data %>% ggplot(aes(x, y)) + geom_bspline()
      
      path <- layer_data(plot) %>%
        select(x,y) %>% 
        mutate(xend = lead(x), yend = lead(y)) %>% drop_na() %>%
        mutate(x = x, y = y + width) %>% 
        summarise(
          x = c(x, rev(xend), x[1]),
          y = c(y, rev(yend), y[1] - width))  
    },
    
    path = {
      path <- data %>%
        mutate(x = x, yend = yend + width) 
    },
    
    polygon = {
      path <- data %>%
        mutate(x = x, y = y + width) %>% 
        summarise(
          x = c(x, rev(xend), x[1]),
          y = c(y, rev(yend), y[1] - width)) 
    }
  )
  
  return(path %>% mutate(across(c(x,y), ~ . * scale)))
  
}

# objects creation ####
# generate a single sequence and chain individual vectors as sequence elements
gen_leaf <- function(v, a = 20, x0 = 0, y0 = 0) {
  
  # set parameters and initial value  
  init <- tibble(s = seq_collatz(v)) %>% 
    mutate(
      n = seq_along(s),
      length = s,
      angle = n * a,
      #angle = cumsum(if_else(s %% 2 == 0, a, -a)),
      x = x0, y = y0,
      xend = x + v * cos((90 - a) * pi/180),
      yend = y + v * sin((90 - a) * pi/180) 
    )
  
  # transform vector coordinates
  init %>% 
    group_by(n) %>% nest() %>% ungroup() %>% 
    mutate(
      data = accumulate(data, transform_vector)
    ) %>% unnest(data)
}

# generate a single collection of sequences .options = furrr_options(seed=TRUE)
gen_node <- function(
  n = 20, imin = 20, imax = 70, lmax = 1000,
  amin = -20, amax = 20, shift = 20,
  scale = 1, width = 15, shape="polygon", seed) {
  
  # set seed if needed
  if (!missing(seed)) set.seed(seed) 
  
  data <- tibble(
    id = seq_len(n),
    v = runif(n, imin, imax) %>% as.integer(),
    a = runif(n, amin, amax)) %>% 
    mutate(
      path = future_map2(v, a, ~ gen_leaf(v=.x, a=.y)),
      c_n = map_int(path, ~ nrow(.)),
      c_l = map_dbl(path, ~ sum(.$length))
    ) %>% 
    filter(c_l < lmax, a != 0) %>% unnest(path)
  
  # shift organs vertically
  layout <- data %>% 
    left_join(
      data %>%
        distinct(id, c_l) %>% arrange(-c_l) %>% 
        mutate(shift = 0:(n() - 1) * shift)
    ) %>% 
    mutate(across(c(y, yend), ~ . + shift))
  
  # transform path into polygons or splines 
  layout <- layout %>%
    group_by(id, c_n, c_l, a) %>% nest() %>% 
    mutate(path = map(data, ~ transform_path(., scale, width, shape))) %>% 
    select(-data) %>% unnest(path)
  
  return(layout)
}


# object rendering ####
# render nodes with different aesthetic
render_node <- function(
  data, shape = "polygon", radius = 0,
  xlim=NULL, ylim = NULL, margin = 0) {
  
  switch(
    shape,
    
    spline = {
      plot <- data %>% 
        ggplot(aes(x, y, group = id)) +
        geom_bspline(size = 0.5, alpha=0.5) 
    },
    
    path = {
      plot <- data %>% 
        ggplot(aes(x, y, xend = xend, yend = yend, group = id)) +
        geom_segment(size = 0.5, lineend = "round", alpha = 0.3) +
        geom_path(size=0.5, alpha=0.5) +
        geom_path(aes(x=xend, y=yend), size=0.5, alpha=0.5) 
    },
    
    polygon = {
      plot <- data %>% 
        ggplot(aes(x,y, group = id)) +
        geom_shape(
          color="darkgrey", fill="white",
          size = 0.5, radius = unit(radius, 'pt')) 
    }
  )
  
  return(
    plot +
      coord_fixed(xlim = xlim, ylim = ylim) +
      theme_void() + theme(plot.margin=rep(unit(margin,"pt"),4))
  )
  
}

