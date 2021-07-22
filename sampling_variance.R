
# repeated sampling from a populaiton -------------------------------------
library(tidyverse)
library(pwr)

sampSize <- 30 # size of sample
nsamps <- 5000 # number of samples we will take

adult = tibble(height =rnorm(10000,169,7.5))


# set up variable to store all of the results
sampMeans <- tibble(meanHeight=rep(NA,nsamps))

# Loop through and repeatedly sample and compute the mean
for (i in 1:nsamps) {
  sampMeans$meanHeight[i] <- adult %>%
    sample_n(sampSize,replace = T) %>%
    summarize(meanHeight=mean(height)) %>%
    pull(meanHeight)
}


# plot results ------------------------------------------------------------
# Now let's plot the sampling distribution. We will also overlay the sampling
# distribution of the mean predicted on the basis of the population mean and
# standard deviation, to show that it properly describes the actual sampling
# distribution.

# pipe the sampMeans data frame into ggplot
sampMeans %>%
  ggplot(aes(meanHeight)) +
  # create histogram using density rather than count
  geom_histogram(
    aes(y = ..density..),
    bins = 50,
    col = "gray",
    fill = "gray"
  ) +
  # add a vertical line for the population mean
  geom_vline(xintercept = mean(adult$height),
             size=1.5) +
  # add a label for the line
  annotate(
    "text",
    x = mean(adult$height),
    y = dnorm(mean(adult$height),mean(adult$height),
              sd(adult$height)/sqrt(sampSize)),
    label = "Population Mean",
    size=6,
    hjust=-0.25
  ) +
  # label the x axis
  labs(x = "Height (cm)") +
  # add normal based on population mean/sd
  stat_function(
    fun = dnorm, n = sampSize,
    args = list(
      mean = mean(adult$height),
      sd = sd(adult$height) / sqrt(sampSize)
    ),
    size = 1.5,
    color = "black",
    linetype='dotted'
  ) +
  theme_minimal()


# Power Curves ------------------------------------------------------------

# We can also create plots that can show us how the power to find an effect
# varies as a function of effect size and sample size. We will use the
# crossing() function from the tidyr package to help with this. This function
# takes in two vectors, and returns a tibble that contains all possible
# combinations of those values.

effect_sizes <- c(0.2, 0.5, 0.8)
sample_sizes = seq(10, 450, 5)

input_df <- crossing(effect_sizes,sample_sizes)
glimpse(input_df)

# Using this, we can then perform a power analysis for each combination of
# effect size and sample size to create our power curves. In this case, let's
# say that we wish to perform a two-sample t-test.

# create a function get the power value and
# return as a tibble
get_power <- function(df){
  power_result <- pwr.p.test(n=df$sample_sizes,
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
  geom_line(lwd = 1) +
  geom_hline(yintercept = 0.8,
             linetype = 'dotdash',
             lwd = 1) +
  theme_minimal(14) +
  theme(
    legend.position = c(0.85, 0.25),
    legend.background = element_rect(fill = 'white', colour = 'black')
  )
