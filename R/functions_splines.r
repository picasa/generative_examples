
# merge character layouts into words, drop failed mappings
layout_word <- function(word, data, shift = 1) {
  
  tibble(character = str_split(word, "")[[1]]) %>% 
    left_join(data, by = "character") %>% drop_na() %>% 
    mutate(d = seq(0, by = shift, length = n())) %>% 
    mutate(layout = map2(layout, d, ~ mutate(..1, x = x + ..2))) %>% 
    unnest(layout)
}


# define a character map and randomly modify few characters in y scaling
gen_charmap <- function(n = 26, n_control = 4, n_tall = 4, size_tall = 4, ...) {
  
  data_map <- tibble(
    pattern = 1:(n + 3),
    character = c(letters[1:n], ".", ",", "?"),
    r = case_when(
      rbinom((n + 3), 1, p = n_tall/(n + 3)) == 0 ~ 1,
      TRUE ~ size_tall)
    ) %>% 
    mutate(layout = map(r, ~ layout_ellipse(n = n_control, r = .)))
  
  return(data_map)
  
}

# draw a spline curve directly on layout points
render_spline <- function(
  data, type = "clamped", n = 100, orientation = "h",
  width = 0.5, alpha = 1, coord = NULL) {
  
  switch(
    orientation,
    
    h = {
      plot <- data %>% 
        ggplot(aes(x, y)) +
        geom_bspline(
          lineend = "round", type = type, n = n, size = width, alpha = alpha) 
    }, 
    
    v = {
      plot <- data %>% 
        ggplot(aes(x = y, y = -x)) +
        geom_bspline(
          lineend = "round", type = type, n = n, size = width, alpha = alpha) 
    }
  )
  
  return(plot + coord + theme_void())
  
}


# render a paragraph from an existing text and a set of plots using script style
render_script <- function(
  text, data, ncol = 80, scale = 0.9,
  orientation = "h", align = "none") {
  # split text to characters
  seq <- text %>% str_to_lower() %>% str_split(., "") 
  
  # map characters to glyph plots, non-match creates NULL plot 
  # randomly select one variation per character. 
  glyph <- tibble(character = seq[[1]]) %>%
    mutate(position = seq_along(character)) %>% 
    left_join(data %>% select(character, pattern, variation, plot)) %>% 
    group_by(position, character) %>% 
    slice_sample(n = 1)
  
  # render the character string
  switch (
    orientation,
    
    h = {
      plot_grid(
        plotlist = glyph$plot, ncol = ncol, scale = scale, align = align)
      },
    
    v = {
      plot_grid(
        plotlist = glyph$plot, nrow = n_col,
        scale = scale, align = align, byrow = FALSE)
      }
  )
  
}


# render a line of glyphs - words
render_line <- function(data, ncol = 80, scale = 0.9) {
  plot_grid(
    plotlist = c(data$plot, list(NULL)),
    rel_widths = c(data$length, ncol - sum(data$length)),
    ncol = nrow(data) + 1, scale = scale) +
    theme(plot.margin = unit(c(2,0,2,0), "mm"))
}

# render a paragraph using cursive style
render_cursive <- function(
  text, data, ncol = 80, shift = 1,
  size = 0.5, n_points = 200, scale = 0.8) {
  # split text to words
  seq <- text %>% str_to_lower() %>% str_split(., " ") 
  
  # generate words from glyph concatenation
  # define line as groups of words of given cumulative length
  words <- tibble(word = seq[[1]]) %>%
    mutate(position = seq_along(word)) %>%
    mutate(length = str_length(word)) %>%
    mutate(cl = accumulate(length, ~ if_else(.x > ncol, .y, .x + .y))) %>% 
    mutate(line = ifelse(lag(cl, default = 0) > ncol, 1, 0) %>% cumsum()) %>% 
    mutate(
      glyph = map(word, ~ layout_word(., data = data, shift = shift)),
      plot = map(glyph, ~ render_spline(., width = size, n = n_points, type = "open"))
    )
  
  lines <- words %>% 
    group_by(line) %>% nest() %>% 
    mutate(plot = map(data, ~ render_line(., ncol, scale = scale)))
  
  plot_grid(plotlist = lines$plot, nrow = nrow(lines), scale = scale)
}
