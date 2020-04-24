library(Rcpp)
library(ggplot2)
library(dplyr)

opt = theme(legend.position  = "none",
            panel.background = element_rect(fill="#FEFAEE"),
            axis.ticks       = element_blank(),
            panel.grid       = element_blank(),
            axis.title       = element_blank(),
            axis.text        = element_blank())

# https://fronkonstin.com/2017/11/07/drawing-10-million-points-with-ggplot-clifford-attractors/
cppFunction('DataFrame f_clifford_c(int n, double x0, double y0, 
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])+c*cos(a*x[i-1]);
            y[i] = sin(b*x[i-1])+d*cos(b*y[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

f_clifford_r <- function(n, x0, y0, a, b, c, d) {
  x <- NULL
  y <- NULL
  x[1] <- x0
  y[1] <- y0
  for (i in 2:n) {
    x[i] <- sin(a*y[i-1]) + c*cos(a*x[i-1]);
    y[i] <- sin(b*x[i-1]) + d* cos(b*y[i-1]);
  }
  return(data.frame(x,y))
}

# default example
a <- -1.24458046630025
b <- -1.25191834103316 
c <- -1.81590817030519 
d <- -1.90866735205054

n <- 1E6
df <- f_clifford_c(n, 0, 0, a, b, c, d)
plot_clifford <- ggplot(df, aes(x, y)) + geom_point(color="black", shape=46, alpha=.01) + opt
ggsave(plot_clifford, file="R/figures/clifford_01.png", width=20, height=20, unit="cm", dpi=300)

# waves up
a <- -1.5
b <- -4 
c <- -1.5 
d <- -4

n <- 20E6
df <- f_clifford_c(n, 0, 0, a, b, c, d) %>% filter(between(y, 3, 5), between(x, a, -a))
plot_clifford <- ggplot(df, aes(x, y)) + geom_point(color="black", shape=46, alpha=.01) + opt
ggsave(plot_clifford, file="R/figures/clifford_02.png", width=20, height=20, unit="cm", dpi=300)

# smoke
a <- -1.24458046630025
b <- -1.25191834103316 
c <- -1.81590817030519 
d <- -1.90866735205054

n <- 10E6
df <- f_clifford_c(n, 0, 0, a, b, c, d) %>% mutate(x=log(x))
plot_clifford <- ggplot(df, aes(y, -x)) + geom_point(color="black", shape=46, alpha=.01) + opt
ggsave(plot_clifford, file="R/figures/clifford_03.png", width=20, height=20, unit="cm", dpi=300)
