library(tidyverse)
## script to create a Type 1 - Type 2 demo plot 01/17/2024

tibble(x = seq(-3.5, 6.5, length.out = 200)) %>%
  ggplot(aes(x)) +
  stat_function(
    fun = dnorm,
    args = list(0, 1),
    col = 'red',
    xlim = c(-3.5, 3.5)
  ) +
  stat_function(
    fun = dnorm,
    args = list(3, 1),
    col = 'blue',
    xlim = c(-0.5, 6.5),
    lty = 2
  ) +
  stat_function(
    geom = "area",
    fun = dnorm,
    args = list(3, 1),
    fill = 'blue',
    xlim = c(-0.5, 2),
    lty = 2,
    alpha = 0.25
  ) +
  stat_function(
    geom = "area",
    fun = dnorm,
    args = list(0, 1),
    fill = 'red',
    xlim = c(2, 3.5),
    lty = 2,
    alpha = 0.25
  ) +
  annotate(
    "text",
    x = 1.5,
    y = 0.015,
    label = expression(beta),
    size = 8,
    col = 'blue'
  ) +
  annotate(
    "text",
    x = 2.25,
    y = 0.015,
    label = expression(alpha),
    size = 8,
    col = 'red'
  ) +
  geom_segment(
    x = 2,
    xend = 2,
    y = 0 ,
    yend = 0.35,
    col = 'black',
    linewidth = 0.75
  )+
  geom_segment(
    y = 0.35,
    yend = 0.35,
    x = 1.0,
    xend = 3.0,
    arrow = arrow(
      ends = "both",
      type = "closed",
      angle = 20,
      length = unit(0.15, "inches")
    )
  )+
  geom_point(
    aes(x = 2, y = 0.35),
    col = 'blue',
    shape = 1,
    size = 3,
    stroke = 1
  ) +
  annotate(
    "text",
    x = 0,
    y = 0.2,
    label = expression(H[0]),
    size = 7
  ) +
  annotate(
    "text",
    x = 3,
    y = 0.2,
    label = expression(H[A]),
    size = 7
  ) +
  annotate(
    "text",
    x = 2,
    y = 0.28,
    label = "Decision Point",
    angle = 90,
    vjust = -0.5,
    size = 6
  )+
  theme_void()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  labs(x = "Z", y = NULL)
