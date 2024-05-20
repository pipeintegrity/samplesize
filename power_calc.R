library(pwr)
library(tidyverse)


sigma <-15
c<-100
mu<-seq(95,125,l=100)
d<-(mu-c)/sigma
d <- 0.5
n <- 2:50

# use pwr package to plot varying power
plot(n,pwr.norm.test(d=0.5,n=n,sig.level=0.05,alternative="greater")$power,
     type="l",ylim=c(0,1), col='red')

plot(n,pwr.norm.test(d=0.3,n=n,sig.level=0.05,alternative="greater")$power,
     type="l",ylim=c(0,1), col='blue', add=TRUE)


abline(h=0.05)
abline(h=0.80)

# power plot for different mean differences -------------------------------

tibble(
  n = 1:100,
  d1 = 0.5,
  d2 = 1 / 3,
  d3 = 1 / 6,
  p1 = pwr.norm.test(d = d1, n = n, alternative = "greater")$power,
  p2 = pwr.norm.test(d = d2, n = n, alternative = "greater")$power,
  p3 = pwr.norm.test(d = d3, n = n, alternative = "greater")$power
) %>%
  pivot_longer(p1:p3, names_to = "d", values_to = "power") %>%
  mutate(d = case_when(d == "p1" ~ "3",
                       d == "p2" ~ "2",
                       TRUE ~ "1")) %>%
  rename(difference = d) %>%
  ggplot(aes(n, power)) +
  geom_line(aes(col = difference), linewidth = 0.8) +
  geom_hline(yintercept = 0.8,
             lty = 2,
             linewidth = 0.8) +
  annotate(
    "text",
    x = 80,
    y = 0.8,
    label = "80% Power",
    vjust = -0.5,
    size = 7
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  labs(x = "No. Samples",
       y = "Power",
       color = "Difference") +
  theme_bw(16)


tibble(
  n = 1:300,
  d1 = 0.5,
  d2 = 1 / 3,
  d3 = 1 / 6,
  p1 = pwr.norm.test(d = d1, n = n, alternative = "greater")$power,
  p2 = pwr.norm.test(d = d2, n = n, alternative = "greater")$power,
  p3 = pwr.norm.test(d = d3, n = n, alternative = "greater")$power
) %>%
  pivot_longer(p1:p3, names_to = "d", values_to = "power") %>%
  group_by(d) %>%
  summarise(s = which.min(abs(power - 0.8)))
