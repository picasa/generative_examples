
# geometry ####

# functions to translate or rotate shapes
translate <- function(data, x0, y0) {
  tr <- linear_trans(translate(x0, y0))
  tibble(id = data$id, tr$transform(data$x, data$y, x0, y0)) 
}

rotate <- function(data, a) {
  tr <- linear_trans(rotate(a))
  tibble(id = data$id, tr$transform(data$x, data$y, a)) 
}

r_t <- function(data, x0, y0, a) { data %>% rotate(., a) %>% translate(., x0, y0)}


# spatial ####
# convert dataframe to sf object, NULL if empty
as_sf <- function(data) {
  
  if (nrow(data) == 0) {return(NULL)}
  
  else {
    data %>%
      select(one_of(c("id", "x", "y"))) %>%
      st_as_sf(coords = c("x","y"))
  }
}


# curves ####

# paper ####

ratio <- list(
  tv = 4/3,
  a = 297/210,
  photo = 15/10,
  golden = (1 + sqrt(5)) / 2,
  cinema = 16/9,
  uni = 2, 
  wide = 21/9
)


# from https://github.com/thomasp85/fawkes/blob/master/R/aaa.R
format <- list(
  a0 = c(841, 1189),
  a1 = c(594, 841),
  a2 = c(420, 594),
  a3 = c(297, 420),
  a4 = c(210, 297),
  a5 = c(148, 210),
  a6 = c(105, 148),
  a7 = c(74, 105),
  a8 = c(52, 74),
  a9 = c(37, 52),
  a10 = c(26, 37),
  b0 = c(1000, 1414),
  b1 = c(707, 1000),
  b2 = c(500, 707),
  b3 = c(353, 500),
  b4 = c(250, 353),
  b5 = c(176, 250),
  b6 = c(125, 176),
  b7 = c(88, 125),
  b8 = c(62, 88),
  b9 = c(44, 62),
  b10 = c(31, 44),
  c0 = c(917, 1297),
  c1 = c(648, 917),
  c2 = c(458, 648),
  c3 = c(324, 458),
  c4 = c(229, 324),
  c5 = c(162, 229),
  c6 = c(114, 162),
  c7 = c(81, 114),
  c8 = c(57, 81),
  c9 = c(40, 57),
  c10 = c(28, 40),
  d1 = c(545, 771),
  d2 = c(385, 545),
  d3 = c(272, 385),
  d4 = c(192, 272),
  d5 = c(136, 192),
  d6 = c(96, 136),
  d7 = c(68, 96),
  e3 = c(400, 560),
  e4 = c(280, 400),
  e5 = c(200, 280),
  e6 = c(140, 200),
  quarto = c(254, 203.2),
  foolscap = c(330.2, 203.2),
  letter = c(215.9, 279.4),
  legal = c(215.9, 355.6),
  ledger = c(279.4, 431.8),
  tabloid = c(279.4, 431.8),
  executive = c(184.15, 266.7),
  post = c(393.7, 488.95),
  crown = c(381, 508),
  'large post' = c(419.1, 533.4),
  demy = c(444.5, 571.5),
  medium = c(457.2, 584.2),
  royal = c(508, 635),
  elephant = c(584.2, 711.2),
  'double demy' = c(596.9, 889),
  'quad demy' = c(889, 1143),
  a = c(215.9, 279.4),
  b = c(279.4, 431.8),
  c = c(431.8, 558.8),
  d = c(558.8, 863.6),
  e = c(863.6, 1117.6)
)