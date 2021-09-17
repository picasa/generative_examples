
# render layouts with different graphs (relative neighborhood, knn) and aesthetics 
# https://en.wikipedia.org/wiki/Relative_neighborhood_graph

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
        cccd::rng(k = k) %>% as_tbl_graph()
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


