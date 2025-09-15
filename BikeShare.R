### SET UP ###
#Downloading libraries
library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(ggplot2)
library(DataExplorer)
library(glmnet)

#Bringing in Data
train <- vroom("train.csv")
test <- vroom("test.csv")

#Cleaning Data
train <- train |>
  select(-casual, -registered) |>
  mutate(count = log(count))

### EDA ###
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

### FEATURE ENGINEERING ###
#Making Recipe
bike_recipe <- recipe(count ~ ., data = train) |>
  step_mutate(weather = ifelse(weather == 4, 3, weather)) |>
  step_mutate(weather = factor(weather, levels = c(1,2,3), 
                               labels = c("Clear", "Cloudy", "Severe"))) |>
  step_time(datetime, features = "hour") |>
  step_rm(datetime) |>
  step_mutate(season = factor(season, levels = c(1,2,3,4), 
                              labels = c("Spring", "Summer", "Fall", "Winter"))) |>
  step_zv(all_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())
prepped_recipe <- prep(bike_recipe)
baked_dataset <- bake(prepped_recipe, new_data = test)
head(baked_dataset)

### WORK FLOW ###
#Defining Linear Regression Model
bike_rlm <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

#Creating Workflow
bike_workflow <- workflow() |>
  add_recipe(bike_recipe) |>
  add_model(bike_rlm) |>
  fit(data = train)

#Running on Test Data
bike_wpred <- exp(predict(bike_workflow, new_data = test))

#Formatting Predictions for Kaggle
kaggle_work <- bike_wpred |>
  bind_cols(test) |> 
  select(datetime, .pred) |> 
  rename(count = .pred) |>
  mutate(count = pmax(0, count)) |> 
  mutate(datetime = as.character(format(datetime)))

#Making CSV File
vroom_write(x=kaggle_work, file="./Workflow.csv", delim=",")

#Defining Penalized Regression Models
preg_model1 <- linear_reg(penalty = 0.2, mixture = 0.5) |> #Penalty = 0.2
  set_engine("glmnet") 
preg_model2 <- linear_reg(penalty = 0.5, mixture = 0.5) |> #Penalty = 0.5
  set_engine("glmnet")
preg_model3 <- linear_reg(penalty = 1, mixture = 0.5) |> #Penalty = 1
  set_engine("glmnet")
preg_model4 <- linear_reg(penalty = 2, mixture = 0.5) |> #Penalty = 2
  set_engine("glmnet")
preg_model5 <- linear_reg(penalty = 5, mixture = 0.5) |> #Penalty = 5
  set_engine("glmnet")

#Creating a Workflow
preg_wf1 <- workflow() |>
  add_recipe(bike_recipe) |>
  add_model(preg_model1) |>
  fit(data = train)
preg_wf2 <- workflow() |>
  add_recipe(bike_recipe) |>
  add_model(preg_model2) |>
  fit(data = train)
preg_wf3 <- workflow() |>
  add_recipe(bike_recipe) |>
  add_model(preg_model3) |>
  fit(data = train)
preg_wf4 <- workflow() |>
  add_recipe(bike_recipe) |>
  add_model(preg_model4) |>
  fit(data = train)
preg_wf5 <- workflow() |>
  add_recipe(bike_recipe) |>
  add_model(preg_model5) |>
  fit(data = train)

#Running Predictions
bike_pred1 <- exp(predict(preg_wf1, new_data = test))
bike_pred2 <- exp(predict(preg_wf2, new_data = test))
bike_pred3 <- exp(predict(preg_wf3, new_data = test))
bike_pred4 <- exp(predict(preg_wf4, new_data = test))
bike_pred5 <- exp(predict(preg_wf5, new_data = test))

#Formatting Predictions for Kaggle
kaggle_preg1 <- bike_pred1 |>
  bind_cols(test) |> 
  select(datetime, .pred) |> 
  rename(count = .pred) |>
  mutate(count = pmax(0, count)) |> 
  mutate(datetime = as.character(format(datetime)))
kaggle_preg2 <- bike_pred2 |>
  bind_cols(test) |> 
  select(datetime, .pred) |> 
  rename(count = .pred) |>
  mutate(count = pmax(0, count)) |> 
  mutate(datetime = as.character(format(datetime)))
kaggle_preg3 <- bike_pred3 |>
  bind_cols(test) |> 
  select(datetime, .pred) |> 
  rename(count = .pred) |>
  mutate(count = pmax(0, count)) |> 
  mutate(datetime = as.character(format(datetime)))
kaggle_preg4 <- bike_pred4 |>
  bind_cols(test) |> 
  select(datetime, .pred) |> 
  rename(count = .pred) |>
  mutate(count = pmax(0, count)) |> 
  mutate(datetime = as.character(format(datetime)))
kaggle_preg5 <- bike_pred5 |>
  bind_cols(test) |> 
  select(datetime, .pred) |> 
  rename(count = .pred) |>
  mutate(count = pmax(0, count)) |> 
  mutate(datetime = as.character(format(datetime)))

#Making CSV Files
vroom_write(x=kaggle_preg1, file="./Preg1.csv", delim=",")
vroom_write(x=kaggle_preg2, file="./Preg2.csv", delim=",")
vroom_write(x=kaggle_preg3, file="./Preg3.csv", delim=",")
vroom_write(x=kaggle_preg4, file="./Preg4.csv", delim=",")
vroom_write(x=kaggle_preg5, file="./Preg5.csv", delim=",")

### STANDARD LINEAR REGRESSION ###
#Fitting Linear Model
bike_lm <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |>
  fit(formula = count ~ . - datetime, data=train)

#Generating Predictions
bike_pred <- exp(predict(bike_lm,
                     new_data=test))

#Formatting Predictions for Kaggle
kaggle <- bike_pred |>
  bind_cols(test) |> 
  select(datetime, .pred) |> 
  rename(count = .pred) |>
  mutate(count = pmax(0, count)) |> 
  mutate(datetime = as.character(format(datetime)))

#Making CSV File
vroom_write(x=kaggle, file="./LinearPreds.csv", delim=",")

