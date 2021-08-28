## Create two data frames: one that holds the rows of food_consumption for "Belgium" and the another that holds rows for "USA". Call these belgium_consumption and usa_consumption.##

# Filter for Belgium
belgium_consumption <- food_consumption %>%
  filter(country=="Belgium")

# Filter for USA
usa_consumption <- food_consumption %>%
  filter(country=="USA")

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
  group_by(country)%>%
  # Get mean_consumption and median_consumption
  summarize(mean_consumption = mean(consumption),
      median_consumption = median(consumption))

food_consumption %>%
  # Filter for rice food category
  filter(food_category == "rice") %>%
  # Create histogram of co2_emission
  ggplot(aes(x= co2_emission)) +
  geom_histogram()


# Calculate the quartiles of co2_emission
quantile(food_consumption$co2_emission)

# Calculate variance and sd of co2_emission for each food_category
food_consumption %>% 
  group_by(food_category) %>% 
  summarize(var_co2 = var(co2_emission),
     sd_co2 = sd(co2_emission))

# Plot food_consumption with co2_emission on x-axis
ggplot(food_consumption, aes(x=co2_emission)) +
  # Create a histogram
  geom_histogram() +
  # Create a separate sub-graph for each food_category
  facet_wrap(~food_category)

# Calculate total co2_emission per country: emissions_by_country
emissions_by_country <- food_consumption %>%
  group_by(country) %>%
  summarize(total_emission = sum(co2_emission))

emissions_by_country

# Count the deals for each product
amir_deals %>%
  count(product)

# Set random seed to 31
set.seed(31)

# Sample 5 deals without replacement
amir_deals %>%
  sample_n(5)

# Create a histogram of group_size
restaurant_groups %>%
ggplot(aes(x=group_size)) +
  geom_histogram(bins=5)

# Expected number won with 30% win rate
won_30pct <- 3 * 0.3
won_30pct

# Expected number won with 25% win rate
won_25pct <- 3 * 0.25
won_25pct

# Expected number won with 35% win rate
won_35pct <- 3 * 0.35
won_35pct



