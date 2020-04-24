library(tidyverse)

# https://en.wikipedia.org/wiki/Lissajous_curve

lissajous <- function(t, a, b, delta, A=1, B=1) {
  x = A * sin(a*t + delta)
  y = B * sin(b*t)
  
  return(data.frame(x,y))
}

plot_lissajous <- lissajous(t=seq(0,2*pi,1/100), a=3, b=2, delta=pi/2) %>% 
  ggplot(aes(x, y)) +
  geom_point(alpha=0.5) +
  theme_void()
