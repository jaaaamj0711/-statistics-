# Histogram of amount with 10 bins
ggplot(amir_deals, aes(x = amount)) + geom_histogram(bins = 10)

# Probability of deal < 7500
pnorm(7500, mean(amir_deals$amount), sd(amir_deals$amount))

# Probability of deal < 1000
pnorm(1000, mean(amir_deals$amount), sd(amir_deals$amount))

# Probability of deal between 3000 and 7000
pnorm(7000, mean(amir_deals$amount), sd(amir_deals$amount)) - pnorm(3000, mean(amir_deals$amount), sd(amir_deals$amount))

# Calculate amount that 75% of deals will be more than
qnorm(0.25, mean(amir_deals$amount), sd(amir_deals$amount))

# Calculate new average amount
new_mean <- 5000 + (5000 * 0.2)
new_mean

# Calculate new standard deviation
new_sd <- 2000 + (2000 * 0.3)
new_sd

new_sales <- data.frame(sales_num = 1:36)

# Simulate 36 sales
new_sales <- new_sales %>% mutate(amount = rnorm(36, new_mean, new_sd))

# Create histogram with 10 bins
ggplot(new_sales, aes(x = amount)) + geom_histogram(bins = 10)

# Central Limit Theorm

# Create a histogram of num_users
ggplot(amir_deals, aes(x = num_users)) + geom_histogram(bins = 10)

# Set seed to 104
set.seed(104)

# Sample 20 num_users with replacement from amir_deals
sample(amir_deals$num_users, 20, replace = TRUE) %>%
  # Take mean
  mean()

sample_means <- replicate(100, sample(amir_deals$num_users, 20, replace = TRUE) %>% mean())
sample_means

# Create data frame for plotting
samples <- data.frame(mean = sample_means)

# Histogram of sample means
ggplot(samples, aes(x = mean)) +
  geom_histogram(bins = 10)

# Set seed to 321
set.seed(321)

# Take 30 samples of 20 values of num_users, take mean of each sample
sample_means <- replicate(30, sample(all_deals$num_users, 20) %>% mean())

# Calculate mean of sample_means
mean(sample_means)

# Calculate mean of num_users in amir_deals
mean(amir_deals$num_users)

# Possion Distribution

# Probability of 5 responses
dpois(5, 4)

# Probability of 5 responses from coworker
dpois(5, 5.5)

# Probability of 2 or fewer responses
ppois(2, 4)

# Probability of > 10 responses
ppois(10, 4, lower.tail = FALSE)

# Exponential Distribution

# rate = 1 / lambda

# Probability response takes < 1 hour
pexp(1, rate = 0.4)

# Probability response takes > 4 hours
pexp(4, rate = 0.4, lower.tail = FALSE)

# Probability response takes 3-4 hours
pexp(4, rate = 0.4) - pexp(3, rate = 0.4)
