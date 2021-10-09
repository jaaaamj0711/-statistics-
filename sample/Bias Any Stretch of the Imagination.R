# View the whole population dataset
View(spotify_population)

# Sample 1000 rows from spotify_population
spotify_sample <- slice_sample( spotify_population, n = 1000)

# See the result
spotify_sample

# From previous step
spotify_sample <- spotify_population %>% 
  slice_sample(n = 1000)

# Calculate the mean duration in mins from spotify_population
mean_dur_pop <- spotify_population %>% 
  summarize(mean_dur = mean(duration_minutes))

# Calculate the mean duration in mins from spotify_sample
mean_dur_samp <- spotify_sample %>% 
  summarize(mean_dur = mean(duration_minutes))

# See the results
mean_dur_pop
mean_dur_samp

# Get the loudness column of spotify_population
loudness_pop <- spotify_population$loudness

# Sample 100 values of loudness_pop
loudness_samp <- sample(loudness_pop, size = 100)

# See the results
loudness_samp

# Calculate the standard deviation of loudness_pop
sd_loudness_pop <- sd(loudness_pop)

# Calculate the standard deviation of loudness_samp
sd_loudness_samp <- sd(loudness_samp)

# See the results
sd_loudness_pop
sd_loudness_samp

# Visualize the distribution of acousticness as a histogram
# Set a binwidth of 0.01
ggplot(spotify_population, aes(x = acousticness)) +
  geom_histogram(binwidth = 0.01)

# Update the histogram to use spotify_mysterious_sample
# Set the x-axis limits from 0 to 1
ggplot(spotify_mysterious_sample, aes(acousticness)) +
  geom_histogram(binwidth = 0.01) +
  xlim(0, 1)

# Visualize the distribution of duration_minutes as a histogram
# Set a binwidth of 0.5
ggplot(spotify_population, aes(x = duration_minutes)) +
  geom_histogram(binwidth = 0.5)

# Update the histogram to use spotify_mysterious_sample2
# Set the x-axis limits from 0 to 15
ggplot(spotify_mysterious_sample2, aes(duration_minutes)) +
  geom_histogram(binwidth = 0.01) + xlim(0, 15)

# Generate random numbers from ...
randoms <- data.frame(
  # a uniform distribution from -3 to 3
  uniform = runif(n_numbers, min = -3, max = 3),
  # a normal distribution with mean 5 and sd 2
  normal = rnorm(n_numbers, mean = 5, sd = 2)
)

# Plot a histogram of uniform values, binwidth 0.25
ggplot(randoms, aes(x = uniform)) +
  geom_histogram(binwidth = 0.25)

# Plot a histogram of normal values, binwidth 0.5
ggplot(randoms, aes(x = normal)) +
  geom_histogram(binwidth = 0.5)
