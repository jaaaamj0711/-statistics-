# Create scatterplot with regression line
ggplot(house_prices_transform, aes(x = bedrooms, y = log10_price)) +
  geom_point() +
  labs(x = "Number of bedrooms", y = "log10 price") +
  geom_smooth(method = "lm", se = FALSE)

# Remove outlier
house_prices_transform <- house_prices %>%
  filter(bedrooms != 33)

# Create scatterplot with regression line
ggplot(house_prices_transform, aes(x = bedrooms, y = log10_price)) +
  geom_point() +
  labs(x = "Number of bedrooms", y = "log10 price") +
  geom_smooth(method = "lm", se = FALSE)

# Fit model
model_price_2 <- lm(log10_price ~ log10_size + bedrooms, 
                    data = house_prices)

# Get regression table
get_regression_table(model_price_2)

# Make prediction in log10 dollars
2.69 + 0.941 * log10(1000) - 0.033 * 3

# Make prediction dollars
10^5.414

# Automate prediction and residual computation
get_regression_points(model_price_2)

# Automate prediction and residual computation
get_regression_points(model_price_2) %>%
  mutate(sq_residuals = residual^2) %>%
  summarize(sum_sq_residuals = sum(sq_residuals))

# Fit model
model_price_4 <- lm(log10_price ~ log10_size + waterfront, 
                    data = house_prices)

# Get regression table
get_regression_table(model_price_4)

# Get regression table
get_regression_table(model_price_4)

# Prediction for House A
10^(2.96 + 0.322 + 0.825 * 2.9)

# Prediction for House B
10^(2.96 + 0.825 * 3.1)

# View the "new" houses
new_houses_2

# Get predictions price_hat in dollars on "new" houses
get_regression_points(model_price_4, newdata = new_houses_2)

# View the "new" houses
new_houses_2

# Get predictions price_hat in dollars on "new" houses
get_regression_points(model_price_4, newdata = new_houses_2) %>% 
  mutate(price_hat = 10^log10_price_hat)
