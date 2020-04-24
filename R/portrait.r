# source https://github.com/aschinchon/travelling-salesman-portrait
library(imager)
library(tidyverse)
library(scales)
library(TSP)
library(furrr)

future::plan(multiprocess)

# input image
set.seed(1)
name <- "pierre"
file <- glue::glue("./R/figures/input_{name}.jpg")
data <- load.image(file) %>% grayscale() %>% as.data.frame()

# repeated weighted sampling
sample_path <- function(data, n=400, weight=5, xlim=range(data$x)) {

  # sample n points more frequently in dark values
  data_sampled <- data %>%
    filter(x > xlim[1], x < xlim[2]) %>% 
    sample_n(n, weight = (1 - value)^weight) %>%
    select(x, y) 
  
  # compute distances and get shortest path
  path <- as.TSP(dist(data_sampled)) %>% 
    solve_TSP(method = "arbitrary_insertion") %>% 
    as.integer() 
  
  data_path <- data_sampled[path,] %>% as_tibble()
  
  return(data_path)
}

# generate data
sample_path(data, n=3000, weight=3) %>% ggplot(aes(x, -y)) + geom_path(alpha=0.1) + coord_fixed() 

# parameters | jean : 3000, 5, c(250,1100) | arthur : 4000, 3, c(200,1100) | pierre : 5000, 3
n_sample <- 1:16
n_point <- 5000
value_weight <- 3
xlim <- c(200,1100)

data_plot <- future_map(
  n_sample,
  ~ sample_path(data, n=n_point, weight=value_weight),
  .progress = TRUE) %>%
  bind_rows(.id = "id")

# save output
saveRDS(data_plot, file=glue::glue("data/tsp_{name}.rds"), compress="gzip")

# plot (jean, id=16)
# data_plot <- readRDS("data/tsp_jean.rds")

plot_portrait <- data_plot %>% 
  filter(id %in% 4) %>% 
  #filter(id %in% sample(n_sample, 2)) %>% 
  ggplot(aes(x, -y, group=id)) +
  #geom_point(alpha=0.1, size=0.5) +
  geom_path(alpha=1) +
  #facet_wrap(vars(id)) +
  coord_fixed() + theme_void()

# export
ggsave(plot_portrait, file=glue::glue("./R/figures/ouput_{name}.png"), dpi=200, width = 4, height = 6, scale = 2)

# debug
plot_debug <- data %>%
  ggplot(aes(x, -y)) +
  geom_raster(aes(fill=value)) +
  geom_point(data=data_sampled, color="white", alpha=0.1) +
  geom_path(data=data_path, color="white", alpha=0.5) +
  coord_fixed()

