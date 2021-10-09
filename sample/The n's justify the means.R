# Generate a simple random sample of 10 rows 
attrition_srs10 <- attrition_pop %>% 
  slice_sample(n = 10)

# Calculate the proportion of employee attrition in the sample
mean_attrition_srs10 <- attrition_srs10 %>% 
  summarize(mean_attrition = mean(Attrition == "Yes")) %>% 
  pull(mean_attrition)

# Calculate the relative error percentage
rel_error_pct10 <- 100 * abs(mean_attrition_pop - mean_attrition_srs10) / mean_attrition_pop

# See the result
rel_error_pct10

# Calculate the relative error percentage again with a 
# sample of 100 rows

attrition_srs100 <- attrition_pop %>% 
  slice_sample(n = 100)

# Calculate the proportion of employee attrition in the sample
mean_attrition_srs100 <- attrition_srs100 %>% 
  summarize(mean_attrition = mean(Attrition == "Yes")) %>% 
  pull(mean_attrition)

# Calculate the relative error percentage
rel_error_pct100 <- 100 * abs(mean_attrition_pop - mean_attrition_srs100) / mean_attrition_pop

# See the result
rel_error_pct100

# Replicate this code 500 times
mean_attritions <- replicate (
  n = 500,
  attrition_pop %>% 
    slice_sample(n = 20) %>% 
    summarize(mean_attrition = mean(Attrition == "Yes")) %>% 
    pull(mean_attrition)
)


# See the result
head(mean_attritions)

# Store mean_attritions in a tibble in a column named sample_mean
sample_means <- tibble(
  sample_mean = mean_attritions
)

# Plot a histogram of the `sample_mean` column, binwidth 0.05
ggplot(sample_means, aes(x = sample_mean)) +
  geom_histogram(binwidth = 0.05)

# Expand a grid representing 5 8-sided dice
dice <- expand_grid(
  die1 = 1 : 8,
  die2 = 1 : 8,
  die3 = 1 : 8,
  die4 = 1 : 8,
  die5 = 1 : 8
)

# See the result
dice

dice <- expand_grid(
  die1 = 1:8,
  die2 = 1:8,
  die3 = 1:8,
  die4 = 1:8,
  die5 = 1:8
) %>% 
  # Add a column of mean rolls
  mutate(mean_roll = (die1 + die2 + die3 + die4 + die5) / 5)

# Using dice, draw a bar plot of mean_roll as a factor
ggplot(dice, aes(factor(mean_roll))) +
  geom_bar()

# Sample one to eight, five times, with replacement
five_rolls <- sample(
  1:8, size = 5, replace = TRUE
)

# Calculate the mean of five_rolls
mean(five_rolls)

# Replicate the sampling code 1000 times
sample_means_1000 <- replicate(
  n = 1000,
  expr = {
    five_rolls <- sample(1:8, size = 5, replace = TRUE)
    mean(five_rolls)
  }
)

# See the result
sample_means_1000

# Wrap sample_means_1000 in the sample_mean column of a tibble
sample_means <- tibble(sample_mean = sample_means_1000)

# See the result
sample_means

# Using sample_means, draw a bar plot of sample_mean as a factor
ggplot(sample_means, aes(factor(sample_mean))) +
  geom_bar()

# Calculate the mean across replicates of the mean 
# attritions in sampling_distribution_5
mean_of_means_5 <- sampling_distribution_5 %>%
  summarize(mean_mean_attrition = mean(mean_attrition))

# Do the same for sampling_distribution_50
mean_of_means_50 <- sampling_distribution_50 %>%
  summarize(mean_mean_attrition = mean(mean_attrition))

# ... and for sampling_distribution_500
mean_of_means_500 <- sampling_distribution_500 %>%
  summarize(mean_mean_attrition = mean(mean_attrition))

# See the results
mean_of_means_5
mean_of_means_50
mean_of_means_500

# For comparison: the mean attrition in the population
attrition_pop %>% 
  summarize(mean_attrition = mean(Attrition == "Yes"))

# Calculate the standard deviation across replicates of the 
# mean attritions in sampling_distribution_5
sd_of_means_5 <- sampling_distribution_5 %>%
  summarize(sd_sd_attrition = sd(mean_attrition))

# Do the same for sampling_distribution_50
sd_of_means_50 <- sampling_distribution_50 %>%
  summarize(sd_sd_attrition = sd(mean_attrition))

# ... and for sampling_distribution_500
sd_of_means_500 <- sampling_distribution_500 %>%
  summarize(sd_sd_attrition = sd(mean_attrition))

# See the results
sd_of_means_5
sd_of_means_50
sd_of_means_500
