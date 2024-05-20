library(tidyverse)
theme_set(theme_minimal(14))
set.seed(501)

# Coin Flip Sim -----------------------------------------------------------

# hdtl <- tibble(ht=c(1,0))

nsamp <- 2e3

coin <- tibble(f = sample(
  size = nsamp,
  x = c(1, 0),
  replace = TRUE
)) %>%
  mutate(out  = ifelse(f == 1, "H", "T"),
         sm = cummean(f),
         n = 1:n())

coin %>%
  group_by(out) %>%
  summarise(n() / nsamp)

coin %>%
  ggplot(aes(n, sm))+
  geom_line(col='blue')+
  xlim(1,1000)+
  geom_hline(yintercept = 0.5,
             col='red',
             lty = 2,
             lwd=0.8)+
  annotate("text",
           x= 125,
           y = 0.5,
           vjust = 1.5,
           label="True Value = 0.5",
           size = 6)

