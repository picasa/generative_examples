# source : https://github.com/aschinchon/reaction-diffusion
# libraries
library(Rcpp) #to iterate fast
library(tidyverse) #to plot
library(colourlovers) #to color drawings with nice colors

# Import C++ code
sourceCpp('R/reaction_diffusion.cpp')

# Default aesthetics of the ggplot
opt <-  theme(panel.border = element_rect(color="black", fill = NA),
              legend.position = "none",
              axis.ticks       = element_blank(),
              panel.grid       = element_blank(),
              axis.title       = element_blank(),
              axis.text        = element_blank())

# The plot will be a 400x400 raster
pixels <- 400

# Initialization of two matrix A and B that will form the tensor. Changing the way
# of initializing B you will obtain different patterns. Try yourself!
A <- matrix(1, pixels, pixels) #A is a zero matrix
B <- matrix(
  sample(c(0, 1), size = pixels^2, replace = TRUE, prob = c(99,1)), 
  ncol = pixels) #B is a binary one with much more 0's than 1's

# Matrix L to perform convolutions
L <- matrix(c(0.05, 0.2, 0.05, 
              0.2,  -1, 0.2, 
              0.05, 0.2, 0.05), nrow = 3)

# DA and DB parameters
DA <- 1
DB <- 0.5

# f and k parameters: play with them to obtain different patterns
f <- 0.0545
k <- 0.062

# Create the tensor X
X <- array(c(A, B) , dim = c(pixels, pixels, 2))

# Perform iterations of Gray-Scott algorithm (it may take a while)
X <- iterate_Gray_Scott(X, L, DA, DB, f, k, 5000)

# Convert matrix B into  data frame preserving indexes
df <- reshape2::melt(X[,,2]) %>% set_colnames(c("x","y","B"))

# Pick a random palette from colourlovers
palette <- sample(clpalettes('top'), 1)[[1]] %>% 
  swatch %>% .[[1]] %>% unique() %>% colorRampPalette()

# Do the plot
plot <- ggplot(data = df, aes(x = x, y = y, fill= B)) + 
  geom_raster(interpolate = T) +
  coord_equal() +
  scale_fill_gradient(low="black", high="white") +
  theme_void() + theme(legend.position = "none")
  
ggsave(plot, file="./R/figures/reaction_diffusion.png", height = 4, width = 4, scale=1)

