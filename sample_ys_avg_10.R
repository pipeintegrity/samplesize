library(tidyverse)
library(rriskDistributions)
library(evd)
library(tidymodels)

loc <- 1.13
scale <- 0.096
shape <- 0.099

qgev(p = 0.975, loc, scale, shape)

smys <- 42

pipe_pop <-
  tibble(ys = rgev(
    n = 1e4,
    loc = loc,
    scale = scale,
    shape = shape
  )) %>%
  mutate(ys = ys * smys) %>%
  filter(ys < 1.75 * smys)

samp <-
  rep_sample_n(
    tbl = pipe_pop,
    size = 5,
    replace = TRUE,
    reps = 1e4
  )

# even if you consider the confidence interval you only have a 20% chance of including 42 ksi
poe <- samp %>%
  summarise(mys = mean(ys),
            cilo =t.test(ys)$conf.int[[1]],
            cihi = t.test(ys)$conf.int[[2]],
            hilo = between(42, cilo, cihi)) %>%
  ungroup() %>%
  summarise(p42 = sum(hilo[hilo==TRUE]) / 1e4)
  

samp %>% 
  summarise(mys = mean(ys)) %>% 
  ggplot(aes(mys))+
  geom_density(fill='lightblue', alpha = 0.75)+
  geom_vline(lty =2,xintercept = 52)+
  annotate("text",
           x = 53,
           y = 0.15,
           label = glue::glue("Repeated samples of size 10\nfrom population of X42\n{round(poe$p42*100,0)}% probability mean YS is >= 52 ksi"),
           hjust=0)+
  theme_bw()

samp %>% 
  summarise(mys = mean(ys)) %>% 
  ungroup() %>% 
  summarise(sum(mys>=46)/1e4)
 