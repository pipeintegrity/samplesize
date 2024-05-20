
library(tidymodels)
library(extRemes)
library(evd)
set.seed(032912)

n <- 1e3

smp <-
  tibble(x = rgev(1e5, 1.13193611, 0.09261727 , 0.11349718)) %>%
  rep_slice_sample(n = 10, replace = TRUE, reps = n) %>%
  nest(data = -replicate) %>%
  mutate(tt = map(data, ~ t.test(.$x, mu = 1.13)),
         tided = map(tt, tidy)) %>%
  unnest(tided)

smp %>%
  ungroup() %>%
  filter(p.value < 0.05) %>%
  summarise(pct = n() / n)


sds <- sd(smp$estimate)
ms <- mean(smp$estimate)

smp %>%
  ggplot(aes(estimate)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40,
    fill = 'slateblue1',
    col = 'black',
    alpha = 0.65
  ) +
  stat_function(
    fun = dnorm,
    args = list(ms, sds),
    col = 'red',
    linewidth = 1
  ) +
  geom_vline(xintercept = ms,
             lty = 2,
             linewidth = 1) +
  theme_minimal(14)+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5,"cm"))+
  labs(title = "Repeated Sampling From a Distribution",
       y = NULL)



tibble(x = seq(0.95, 2.0, length.out = 200)) %>% 
  ggplot(aes(x))+
  stat_function(fun = dgev, args = list(1.13193611 , 0.09261727 ,0.11349718 ), col ='red', linewidth = 1)
