library(dplyr)
library(ggplot2)
library(knitr)

# compute seed positions using Vogel's model

phi <- (1+sqrt(5))/2
phi_angle <- 360/phi^2

head <- data_frame(n=1:1000) %>%
  mutate(
    r=sqrt(n),
    a=(n*phi_angle)%%360,
    size=sqrt(n)
  )

# plot
plot_head <- head %>%
  ggplot(aes(r, a)) +
  geom_point(aes(size=size), alpha=0.6) +
  coord_polar(theta="y") +
  scale_y_continuous(breaks=seq(0, 360, by=30), expand=c(0,0), lim=c(0, 360)) +
  labs(x=NULL, y=NULL) + theme_void() + guides(size=FALSE) 

ggsave(plot_head, file="R/figures/sunflower.png", width=20, height=20, units="cm", dpi=300)


