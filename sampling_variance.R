
# repeated sampling from a population -------------------------------------
library(tidyverse)
library(pwr)
library(rriskDistributions)
library(infer)

sampSize <- 12 # size of sample
nsamps <- 5e3 # number of samples we will take

# adult population heights
mnh <- 169 # mean
sdh <- 8 # sd

pop <- tibble(h =rnorm(1e6, mnh,sdh))


adult_samp <-
  rep_sample_n(
    tbl = pop,
    size = sampSize,
    replace = FALSE,
    reps = nsamps
  ) %>%
  summarise(mh = mean(h), sh = sd(h))


adults <- adult_samp %>%
 summarise(stdev = sd(mh), # this approx. equal to population SD/sqrt(n)
           mhs = mean(mh)) # mean of all the sample means = population mean

# plot results ------------------------------------------------------------

# Now let's plot the sampling distribution. We will also overlay the sampling
# distribution of the mean predicted on the basis of the population mean and
# standard deviation, to show that it properly describes the actual sampling
# distribution.

adult_samp %>%
  summarise(minh = min(mh), mxh = max(mh))

adults %>%
  summarise(uci = mhs + qt(0.995, df = sampSize - 1) * stdev / sampSize)

# pipe the sample Means data frame into ggplot

adult_samp %>%
  ggplot(aes(mh)) +
  # create histogram using density rather than count
  geom_histogram(aes(y = after_stat(density)),
                 bins = 50,
                 col = "gray",
                 fill = "gray") +
  # add a vertical line for the population mean
  geom_vline(xintercept = mnh,
             linewidth = 1.5) +
  # add a label for the line
  annotate(
    "text",
    x = mnh,
    y = dnorm(mnh, mnh,
              sdh / sqrt(sampSize)),
    label = "Population Mean",
    size = 6,
    hjust = -0.25
  ) +
  # label the x axis
  labs(x = "Height (cm)") +
  # add normal based on population mean/sd
  stat_function(
    fun = dnorm,
    args = list(mean = mnh,
                sd = sdh / sqrt(sampSize)),
    linewidth = 1.5,
    color = "black",
    linetype = 'dotted'
  ) +
  theme_minimal()


# Power Curves ------------------------------------------------------------

# We can also create plots that can show us how the power to find an effect
# varies as a function of effect size and sample size. We will use the
# crossing() function from the tidyr package to help with this. This function
# takes in two vectors, and returns a tibble that contains all possible
# combinations of those values.

effect_sizes <- c(0.2, 0.5, 0.8)
sample_sizes = seq(10, 450, 5) #sequence from, to, by

input_df <- crossing(effect_sizes,sample_sizes)
glimpse(input_df)

# Using this, we can then perform a power analysis for each combination of
# effect size and sample size to create our power curves. In this case, let's
# say that we wish to perform a two-sample t-test.

# create a function get the power value and
# return as a tibble
get_power <- function(df){
  power_result <- pwr.t.test(n=df$sample_sizes,
                             d=df$effect_sizes,
                             type='two.sample')
  df$power=power_result$power
  return(df)
}

# run get_power for each combination of effect size
# and sample size

power_curves <- input_df %>%
  do(get_power(.)) %>%
  mutate(effect_sizes = as.factor(effect_sizes))

ggplot(
  power_curves,
  aes(
    x = sample_sizes,
    y = power,
    linetype = effect_sizes,
    col = effect_sizes
  )
) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0.8,
             linetype = 'dotdash',
             linewidth = 1) +
  theme_minimal(14) +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     labels = scales::percent)+
  theme(
    legend.position = c(0.85, 0.25),
    legend.background = element_rect(fill = 'white',
                                     colour = 'black')
  )


# CLT Demonstration -------------------------------------------------------

# The central limit theorem (CLT) is a statistical principle that explains how
# the sampling distribution of a population will tend towards a normal
# distribution, regardless of the shape of the underlying population
# distribution.

# In simpler terms, the CLT states that when a large number of random samples
# are taken from a population, the means of those samples will be normally
# distributed even if the underlying population distribution is not. This is
# true for many types of data, including data that is skewed or not symmetric.

dist <- get.lnorm.par(q=c(10,75, 250)) # about 4, 1

curve(dlnorm(x,dist[[1]], dist[[2]]), from = 0,to = 1000, n=501)

lnorm_samp <- 1:2e4 %>%
  map(~ rlnorm(n = sampSize, dist[[1]], dist[[2]])) %>%
  map_dbl(mean) %>%
  tibble(mh = .)

lnorm_sum <- lnorm_samp %>%
  summarise(stdev = sd(mh), # this approx. equal to population SD/sqrt(n)
            mhs = mean(mh)) # mean of all the sample means = population mean

lnorm_samp %>%
  ggplot(aes(mh)) +
  # create histogram using density rather than count
  geom_histogram(aes(y = after_stat(density)),
                 bins = 50,
                 col = "gray",
                 fill = "gray") +
  # add a vertical line for the population mean
  geom_vline(xintercept = lnorm_sum$mhs,
             size = 1.5) +
  # add a label for the line
  # annotate(
  #   "text",
  #   x = exp(dist[[1]]),
  #   y = dnorm(mnh, mnh,
  #             sdh / sqrt(sampSize)),
  #   label = "Population Mean",
  #   size = 6,
  #   hjust = -0.25
  # ) +
  # # label the x axis
  # labs(x = "Height (cm)") +
  # add normal based on population mean/sd
  stat_function(
    fun = dnorm,
    # n = sampSize,
    args = list(mean = lnorm_sum$mhs,
                sd = lnorm_sum$stdev),
    linewidth = 1.5,
    color = "black",
    linetype = 'dotted'
  ) +
  theme_minimal()+
  xlim(50,150)
