# Model 2
model_price_2 <- lm(log10_price ~ log10_size + bedrooms, 
                    data = house_prices)

# Calculate squared residuals
get_regression_points(model_price_2) %>%
  mutate(sq_residuals = residual^2) %>%
  summarize(sum_sq_residuals = sum(sq_residuals))

# Model 4
model_price_4 <- lm(log10_price ~ log10_size + waterfront, 
                    data = house_prices)

# Calculate squared residuals
get_regression_points(model_price_4) %>%
  mutate(sq_residuals = residual^2) %>%
  summarize(sum_sq_residuals = sum(sq_residuals))

# Fit model
model_price_2 <- lm(log10_price ~ log10_size + bedrooms,
                    data = house_prices)
                    
# Get fitted/values & residuals, compute R^2 using residuals
get_regression_points(model_price_2) %>%
  summarize(r_squared = 1 - var(residual) / var(log10_price))

# Get all residuals, square them, take the mean and square root
get_regression_points(model_price_2) %>%
  mutate(sq_residuals = residual^2) %>%
  summarize(mse = mean(sq_residuals)) %>% 
  mutate(rmse = sqrt(mse))

# Training the model in order to predict
# Set random number generator seed value for reproducibility
set.seed(76)

# Randomly reorder the rows
house_prices_shuffled <- house_prices %>% 
  sample_frac(size = 1, replace = FALSE)

# Train/test split
train <- house_prices_shuffled %>%
  slice(1:10000)
test <- house_prices_shuffled %>%
  slice(10001:21613)

# Fit model to training set
train_model_2 <- lm(log10_price ~ log10_size + bedrooms, data = train)

# Make predictions on test set
get_regression_points(train_model_2, newdata = test)

# Compute RMSE
get_regression_points(train_model_2, newdata = test) %>% 
  mutate(sq_residuals = residual^2) %>%
  summarize(rmse = sqrt(mean(sq_residuals)))
