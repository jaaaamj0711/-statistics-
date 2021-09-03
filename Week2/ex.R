
library(ggplot2)
ggplot(cars,aes(speed, medv)) +
  geom_point() +
  geom_smooth(method = "lm",se = FALSE)

m<- lm(Ozone~I(Temp ^ 3), data = train)

coefficients(m)
fitted(m)

residuals(m)
     
summary(m)
library(broom)
tidy(m)
augment(m)
glance(m)

library(dplyr)
library(tibble)
explanatory_data<- tibble(speed = 5:20)
predict(m, explanatory_data)

m

prediction_data <- test %>%   
  mutate(Ozone = predict(m, test))

data[c('rm','medv')]


m<- lm(Ozone~Temp, data = data)

ex<-tibble(Temp = seq(60,90,4))
prediction_data <- ex %>%   
  mutate(Ozone = predict(m, ex))

ggplot(data,aes(Temp , Ozone)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_point(data = prediction_data, color = "blue")

ggplot(cars,aes(speed, dist)) + geom_point(data = prediction_data, color = "blue")

Regression
library(MASS)
data<- airquality2
data<-data[c('Temp','Ozone')]

set.seed(1234)
n<- nrow(data)
idx<- 1:n
training_idx<- sample(idx, n*.8)
train<- data[training_idx,]
test_idx<- setdiff(idx, training_idx)
test<- data[test_idx,]
test<- data["Temp"]



head(train)



library(car)


head(Prestige)
newdata <- Prestige[,c(1:4)]
summary(model)
model <- lm(income ~ prestige, data=newdata)
ggplot(newdata,aes(prestige, income)) +
  geom_point() +
  geom_smooth(method = "lm",se = FALSE)

m <- lm(dist ~ speed, cars)
head(fitted(m))
head(residuals(m))
summary(m)

fitted(m)[1:4] + residuals(m)[1:4]
cars$dist[1:4]

library(broom)
head(augment(m))
glance(m)

predict(m, newdata = data.frame(speed=3))
install.packages("ggfortify")
library(ggfortify)
autoplot(m, which = 4:6,nrow = 4,ncol = 1)

autoplot(m, which=1, nrow=1, ncol=1)

autoplot(m, which=2, nrow=1, ncol=1)
autoplot(m, which=3, nrow=1, ncol=1)

m %>% 
  # Augment the model
  augment() %>% 
  # Arrange rows by descending leverage
  arrange(desc(.hat)) %>% 
  # Get the head of the dataset
  head()

m %>% 
  # Augment the model
  augment() %>% 
  # Arrange rows by descending Cook's distance
  arrange(desc(.cooksd)) %>% 
  # Get the head of the dataset
  head()

data_select <- cars %>%
    filter(speed != 24)
car
ggplot(cars, aes(speed, dist)) +  
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE) +  
  geom_smooth(method = "lm", se = FALSE,data = data_select, color = "red")
autoplot(m, which=4:6, nrow=3, ncol=1)

