#Downloading libraries
library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(ggplot2)
library(DataExplorer)

#Bringing in Data
train <- vroom("train.csv")
test <- vroom("test.csv")

#Wrangling Data
train <- train |>
  select(-casual, -registered)

#EDA
#Barplot of Weather
plot1 <- ggplot(data = train, aes(x = weather)) +
  geom_bar(fill = "blue3") +
  labs(x = "Weather Type",
       y = "Total",
       title = "Barplot of Weather")

#Boxplot of Temperature
plot2 <- ggplot(data = train, aes(x = temp)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Temperature (°C)", 
       title = "Boxplot of Temperature")

#Scatterplot of Temperature and "Feels Like" Temperature
plot3 <- ggplot(data = train, aes(x = temp, y = atemp)) +
  geom_point(color = "red") +
  geom_smooth(method = lm, se = F) + 
  labs(x = "Temperature (°C)",
       y = "'Feels Like' Temperature (°C)",
       title = "Scatterplot of Temperatures")

#Barplot of Missing Data
plot4 <- plot_missing(train) + 
  labs(title = "Missing Data")

#Putting plots together
(plot1 + plot2) / (plot3 + plot4)

#Running Linear Regression
#Fitting Linear Model
bike_lm <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |>
  fit(formula = count ~ . - datetime, data=train)

#Generating Predictions
bike_pred <- predict(bike_lm,
                     new_data=test)

#Formatting Predictions for Kaggle
kaggle <- bike_pred |>
  bind_cols(test) |> 
  select(datetime, .pred) |> 
  rename(count = .pred) |>
  mutate(count = pmax(0, count)) |> 
  mutate(datetime = as.character(format(datetime)))

#Making CSV File
vroom_write(x=kaggle, file="./LinearPreds.csv", delim=",")
