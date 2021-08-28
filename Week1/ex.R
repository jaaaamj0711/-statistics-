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


