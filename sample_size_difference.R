library(tidyverse)
library(rriskDistributions)
library(infer)
library(patchwork)

bp <- tibble(x = 3:20, n = 3:20) %>%
  mutate(p = dbinom(x, n, 0.69))

bp %>% 
  ggplot(aes(n,p))+
  geom_line(linewidth = 1)+
  annotate("text", x = 10, y = 0.2, 
           label = "Probability of success/trial = 0.69", 
           size = 5)


get.beta.par(q = c(0.05,0.25, 0.95))
defect <- tibble(d = rbeta(1e4, 2.5,6.5))

n1 <- 10
n2 <- 30

sample1 <-
  infer::rep_slice_sample(
    n = n1,
    .data = defect,
    replace = FALSE,
    reps = 1e4
  ) %>% 
  mutate(n = n1)

sample2 <-
  infer::rep_slice_sample(
    n = n2,
    .data = defect,
    replace = FALSE,
    reps = 1e4
  ) %>% 
  mutate(n = n2)

sample1 %>% 
  summarise( mx = max(d)) %>% 
  ggplot(aes(mx))+
  geom_histogram(fill='purple', col='black')+
  theme_bw()

combo <- bind_rows(sample1, sample2) %>% 
  ungroup() %>% 
  group_by(n,replicate) %>% 
  summarise(mx = max(d))

sampling <- combo %>%
  ggplot(aes(mx)) +
  geom_density(aes(fill = factor(n)), alpha = 0.5) +
  theme_bw(14) +
  ggsci::scale_fill_aaas() +
  labs(title = "Distribution of Maximum PoE Detected",
       x = "Maximum PoE",
       y = NULL,
       fill = "N") +
  theme(plot.margin = margin(0.6,0.6,0.6,0.6, "cm"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

pop <- defect %>% 
  ggplot(aes(d))+
  geom_density(fill='purple', alpha = 0.75)+
  theme_bw(14)+
  labs(title = "Defect Poplulation", x = "PoE", y = NULL)


pop + sampling + plot_layout(widths = c(1,2))
