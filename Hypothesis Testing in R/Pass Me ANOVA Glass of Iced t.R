# Calculate the numerator of the test statistic
numerator <- xbar_no - xbar_yes

# Calculate the denominator of the test statistic
denominator <- sqrt (
  s_yes ^ 2 / n_yes + s_no ^ 2 / n_no
)

# Calculate the test statistic
t_stat <- numerator / denominator

# See the result
t_stat

# Calculate the degrees of freedom
degrees_of_freedom <- n_no + n_yes - 2

# Calculate the p-value from the test stat
p_value <- pt(t_stat, df = degrees_of_freedom, lower.tail = TRUE)

# See the result
p_value

# View the dem_votes_potus_12_16 dataset
View(dem_votes_potus_12_16)

# Calculate the differences from 2012 to 2016
sample_dem_data <- dem_votes_potus_12_16 %>%
  mutate(diff = dem_percent_12 - dem_percent_16)

# See the result
sample_dem_data

# Find mean and standard deviation of differences
diff_stats <- sample_dem_data %>%
  summarize(xbar_diff = mean(diff),
            s_diff = sd(diff))

# See the result
diff_stats

# Using sample_dem_data, plot diff as a histogram
ggplot(sample_dem_data, aes(x = diff))
  + geom_histogram(binwidth = 1)

# Conduct a t-test on diff
test_results <- t.test(
  # Vector of differences
  sample_dem_data$diff,
  # Choose between between 'two.sided', 'less', 'greater'
  alternative = 'greater',
  # Null hypothesis population parameter
  mu = 0
)

# See the results
test_results

# Conduct a paired t-test on dem_percent_12 and dem_percent_16
test_results <- t.test(
  sample_dem_data$dem_percent_12,
  sample_dem_data$dem_percent_16,
  alternative = 'greater',
  mu = 0,
  paired = TRUE
)

# See the results
test_results

# Using late_shipments, group by shipment mode, and 
# calculate the mean and std dev of pack price
late_shipments %>%
  group_by(shipment_mode) %>%
  summarize(xbar_pack_price = mean(pack_price),
            s_pack_price = sd(pack_price))

# Using late_shipments, plot pack_price vs. shipment_mode
# as a box plot with flipped x and y coordinates
late_shipments %>%
  ggplot(aes(x = shipment_mode, y = pack_price)) +
  geom_boxplot() +
  coord_flip()

# Run a linear regression of pack price vs. shipment mode 
mdl_pack_price_vs_shipment_mode <- lm(pack_price ~ shipment_mode, data = late_shipments)

# See the results
summary(mdl_pack_price_vs_shipment_mode)

# Perform ANOVA on the regression model
anova(mdl_pack_price_vs_shipment_mode)

# Perform pairwise t-tests on pack price, grouped by
# shipment mode, no p-value adjustment
test_results <- pairwise.t.test(
  late_shipments$pack_price,
  late_shipments$shipment_mode,
  alternative = 'two.sided',
  p.adjust.method = 'none'
)

# See the results
test_results

# Modify the pairwise t-tests to use Bonferroni p-value adjustment
test_results <- pairwise.t.test(
  late_shipments$pack_price,
  late_shipments$shipment_mode,
  p.adjust.method = "bonferroni"
)

# See the results
test_results
