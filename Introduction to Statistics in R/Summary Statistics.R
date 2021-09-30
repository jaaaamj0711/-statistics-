# Filter for Belgium
belgium_consumption <- food_consumption %>% filter(country == "Belgium")
belgium_consumption

# Filter for USA
usa_consumption <- food_consumption %>% filter(country == "USA")
usa_consumption

# Calculate mean and median consumption in Belgium
mean(belgium_consumption$consumption)
median(belgium_consumption$consumption)

# Calculate mean and median consumption in USA
mean(usa_consumption$consumption)
median(usa_consumption$consumption)

food_consumption %>%
  # Filter for Belgium and USA
  filter(country %in% c("Belgium", "USA")) %>%
  # Group by country
  group_by(country) %>%
  # Get mean_consumption and median_consumption
  summarize(mean_consumption = mean(consumption),
            median_consumption = median(consumption))


food_consumption %>%
  # Filter for rice food category
  filter(food_category == "rice") %>%
  # Create histogram of co2_emission
  ggplot(aes(x = co2_emission)) + geom_histogram()

food_consumption %>%
  # Filter for rice food category
  filter(food_category == "rice") %>% 
  # Get mean_co2 and median_co2
  summarize(mean_co2 = mean(co2_emission),
            median_co2 = median(co2_emission))


# Calculate the quartiles (4 pieces) of co2_emission
quantile(food_consumption$co2_emission)

# Calculate the quintiles (5 pieces) of co2_emission
quantile(food_consumption$co2_emission, probs = seq(0, 1, 0.2))

# Calculate the deciles (10 pieces) of co2_emission
quantile(food_consumption$co2_emission, probs = seq(0, 1, 0.1))


# Calculate variance and sd of co2_emission for each food_category
food_consumption %>% 
  group_by(food_category) %>%
  summarize(var_co2 = var(co2_emission), mean_co2 = mean(co2_emission))

# Plot food_consumption with co2_emission on x-axis
ggplot(food_consumption, aes(x = co2_emission))+
  # Create a histogram
  geom_histogram() +
  # Create a separate sub-graph for each food_category
  facet_wrap(~ food_category)


# Calculate total co2_emission per country: emissions_by_country
emissions_by_country <- food_consumption %>%
  group_by(country) %>%
  summarize(total_emission = sum(co2_emission))

# Compute the first and third quantiles and IQR of total_emission
q1 <- quantile(emissions_by_country$total_emission, 0.25)
q3 <- quantile(emissions_by_country$total_emission, 0.75)
iqr <- q3 - q1

# Calculate the lower and upper cutoffs for outliers
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

# Filter emissions_by_country to find outliers
emissions_by_country %>%
  filter(total_emission < lower | total_emission > upper)
