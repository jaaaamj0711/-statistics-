setwd("/Users/seominji/Downloads/")

train<- read.csv("train.csv")
test<- read.csv("test.csv")

train<- train[c('ram','four_g','battery_power','price_range')]
test<- test[c('ram','four_g','battery_power')]

train$four_g<- as.factor(train$four_g)
test$four_g<- as.factor(test$four_g)

ml<- lm(price_range~ram+battery_power, data = train)


head(train)

str(train)
library(tidyr)
prediction_data <- test %>% 
  mutate(price_range = predict(ml, test))

# Extend the plot
ggplot(
  train, 
  aes(ram, battery_power, color = price_range)
) +
  geom_point() +
  scale_color_viridis_c(option = "plasma") +
  facet_wrap(vars(four_g)) +
  # Add points from prediction data, size 3, shape 15
  geom_point(data = prediction_data, size = 1, shape = 5,
             alpha = 1 )

head(prediction_data)
