# generate glyph shapes by using graph visualization
# https://en.wikipedia.org/wiki/Relative_neighborhood_graph

# draw n points uniformly in in x and y
layout_square <- function(n = 7, method="uniform", xlim = c(-1,1), ylim = c(-1,1)) {
  
switch(
  method,
  
  uniform = {
    tibble(
      n=1:n,
      x=runif(n, xlim[1], xlim[2]),
      y=runif(n, ylim[1], ylim[2])
    ) 
  },
  
  sobol = {
    xlim[1] + (xlim[2] - xlim[1]) *
      randtoolbox::sobol(n, dim = 2, scrambling = 1, seed = 1) %>% 
      as_tibble() %>% 
      mutate(n = seq_along(V1)) %>% 
      select(n, x=V1, y=V2)
  }
  
  )
    
}

# draw n points in a unit circle 
# https://mathworld.wolfram.com/DiskPointPicking.html
layout_ellipse <- function(n = 7, r = 1, a = -pi/6) {
  r = sqrt(runif(n, 0, r))
  theta = runif(n, 0, 2*pi)
  
  # scale and rotate layout
  layout <- tibble(
    n = 1:n,
    x0 = r * cos(theta),
    y0 = r * sin(theta)) %>% 
    mutate(x0 = x0 * 0.5) %>% 
    mutate(
      x = x0*cos(a) - y0*sin(a),
      y = x0*sin(a) + y0*cos(a)) %>% 
    select(n,x,y)
  
  return(layout)
}

# render layouts with different graphs (relative neighborhood, knn) and aesthetics 
render_graph <- function(
  data, vars = c(x,y),
  graph = "rng", aes = "default", k = 3, 
  width = 0.5, alpha=0.5, coord=NULL) {
  
  data <- data %>% select({{vars}}) %>% rename(x = 1, y = 2)  
  
  # compute graph from point layout
  switch(
    graph,
    
    rng = {
      graph <- as.data.frame(data) %>%
        cccd::rng() %>% as_tbl_graph()
    },
    
    knn = {
      graph <- as.data.frame(data) %>%
        cccd::nng(k = k) %>% igraph::as.undirected(.) %>% as_tbl_graph()
    }
  )
  
  
  # render graph
  switch(
    aes,
    
    # edges as straight lines and nodes as points
    default = {
      
      graph %>%
        ggraph(layout = data) +
        geom_edge_link(edge_width = width, alpha = alpha) +
        geom_node_point(size = width * 1.5) +
        coord + theme_void()
    },
    
    # edges as straight lines
    line = {
      
      graph %>%
        ggraph(layout = data) +
        geom_edge_link0(
          edge_width = width,
          lineend = "round") +
        coord + theme_void()
    },
    
    # edges as Bezier curves  
    bezier = {
      
      graph %>%
        ggraph(layout = data) +
        geom_edge_diagonal(
          edge_width = width, alpha = alpha, strength = 1, n = 100,
          lineend = "round", linejoin = "round") +
        coord + theme_void()
    }
  )
  
  
}


