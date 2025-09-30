### SET UP ###
#Downloading libraries
library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(ggplot2)
library(DataExplorer)
library(glmnet)
library(ranger)
library(bonsai)
library(dbarts)
library(agua)
library(beepr)

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
  step_mutate(
    hour = factor(datetime_hour),
    wday = wday(datetime, label = T),
    is_weekend = wday %in% c("Sat", "Sun")) |>
  step_rm(datetime, datetime_hour) |>
  step_mutate(season = factor(season, levels = c(1,2,3,4), 
                              labels = c("Spring", "Summer", "Fall", "Winter"))) |>
  step_zv(all_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors())
prepped_recipe <- prep(bike_recipe)
baked_dataset <- bake(prepped_recipe, new_data = test)
maxNumXs <- ncol(baked_dataset)
head(baked_dataset)

### WORK FLOW ###
# (1) Defining Linear Regression Model
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

# (2) Defining Penalized Regression Models
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

# (3) Running Cross Validation on Penalized Regression Models
cross_model <- linear_reg(penalty = tune(),
                          mixture = tune()) |>
  set_engine("glmnet")

#Creating a Workflow
cross_wf <- workflow() |>
  add_recipe(bike_recipe)|>
  add_model(cross_model)

#Defining Grid of Values
L <- 5
grid <- grid_regular(penalty(),
                     mixture(),
                     levels = L)

#Splitting Data
folds <- vfold_cv(train, v = 5, repeats = 1)

#Run Cross Validation
CV_results <- cross_wf |>
  tune_grid(resamples = folds,
            grid = grid,
            metrics = metric_set(rmse))

#Plotting Cross Validation Results
collect_metrics(CV_results) |>
  filter(.metric == "rmse") |>
  ggplot(aes(x = penalty, y = mean, color = factor(mixture))) +
  geom_line()

#Find Best Tuning Parameters
best <- CV_results |>
  select_best(metric = "rmse")

#Finalizing Workflow
final_wf <- cross_wf |>
  finalize_workflow(best) |>
  fit(data = train)

#Prediction
cross_pred <- predict(final_wf, new_data = test) |>
  mutate(.pred = exp(.pred))

#Formatting Predictions for Kaggle
kaggle_cross <- cross_pred |>
  bind_cols(test) |> 
  select(datetime, .pred) |> 
  rename(count = .pred) |>
  mutate(count = pmax(0, count)) |> 
  mutate(datetime = as.character(format(datetime)))

#Making CSV Files
vroom_write(x=kaggle_cross, file="./Cross.csv", delim=",")

# (4) Running Regression Trees
tree_model <- decision_tree(tree_depth = tune(),
                            cost_complexity = tune(),
                            min_n = tune()) |>
  set_engine("rpart") |>
  set_mode("regression")

#Creating a Workflow
tree_wf <- workflow() |>
  add_recipe(bike_recipe)|>
  add_model(tree_model)

#Defining Grid of Values
L <- 6
tree_grid <- grid_regular(tree_depth(),
                     cost_complexity(),
                     min_n(),
                     levels = L)

#Splitting Data
tree_folds <- vfold_cv(train, v = 6, repeats = 1)

#Run Cross Validation
tree_results <- tree_wf |>
  tune_grid(resamples = tree_folds,
            grid = tree_grid,
            metrics = metric_set(rmse))

#Find Best Tuning Parameters
tree_best <- tree_results |>
  select_best(metric = "rmse")

#Finalizing Workflow
final_twf <- tree_wf |>
  finalize_workflow(tree_best) |>
  fit(data = train)

#Prediction
tree_pred <- predict(final_twf, new_data = test) |>
  mutate(.pred = exp(.pred))

#Formatting Predictions for Kaggle
kaggle_tree <- tree_pred |>
  bind_cols(test) |> 
  select(datetime, .pred) |> 
  rename(count = .pred) |>
  mutate(count = pmax(0, count)) |> 
  mutate(datetime = as.character(format(datetime)))

#Making CSV Files
vroom_write(x=kaggle_tree, file="./Tree.csv", delim=",")

# (5) Running Random Forests
forest_model <- rand_forest(mtry = tune(),
                            min_n = tune(),
                            trees = 1000) |>
  set_engine("ranger") |>
  set_mode("regression")

#Creating a Workflow
forest_wf <- workflow() |>
  add_recipe(bike_recipe)|>
  add_model(forest_model)

#Defining Grid of Values
L <- 3
forest_grid <- grid_regular(mtry(range = c(1, maxNumXs)),
                          min_n(),
                          levels = L)

#Splitting Data
forest_folds <- vfold_cv(train, v = 3, repeats = 1)

#Run Cross Validation
forest_results <- forest_wf |>
  tune_grid(resamples = forest_folds,
            grid = forest_grid,
            metrics = metric_set(rmse))

#Find Best Tuning Parameters
forest_best <- forest_results |>
  select_best(metric = "rmse")

#Finalizing Workflow
final_fwf <- forest_wf |>
  finalize_workflow(forest_best) |>
  fit(data = train)

#Prediction
forest_pred <- predict(final_fwf, new_data = test) |>
  mutate(.pred = exp(.pred))

#Formatting Predictions for Kaggle
kaggle_forest <- forest_pred |>
  bind_cols(test) |> 
  select(datetime, .pred) |> 
  rename(count = .pred) |>
  mutate(count = pmax(0, count)) |> 
  mutate(datetime = as.character(format(datetime)))

#Making CSV Files
vroom_write(x=kaggle_forest, file="./Forest.csv", delim=",")

# (6) Running BART
bart_model <- parsnip::bart() |>
  set_engine("dbarts") |>
  set_mode("regression")

#Creating a Workflow
bart_wf <- workflow() |>
  add_recipe(bike_recipe)|>
  add_model(bart_model)

#Defining Grid of Values
bart_grid <- tibble(ntree = c(50, 200, 500))

#Splitting Data
bart_folds <- vfold_cv(train, v = 10, repeats = 2)

#Run Cross Validation
bart_results <- bart_wf |>
  tune_grid(resamples = bart_folds,
            grid = bart_grid,
            metrics = metric_set(rmse))
beepr::beep()

#Find Best Tuning Parameters
bart_best <- bart_results |>
  select_best(metric = "rmse")

#Finalizing Workflow
final_bwf <- bart_wf |>
  finalize_workflow(bart_best) |>
  fit(data = train)

#Prediction
bart_pred <- predict(final_bwf, new_data = test) |>
  mutate(.pred = exp(.pred))

#Formatting Predictions for Kaggle
kaggle_bart <- bart_pred |>
  bind_cols(test) |> 
  select(datetime, .pred) |> 
  rename(count = .pred) |>
  mutate(count = pmax(0, count)) |> 
  mutate(datetime = as.character(format(datetime)))

#Making CSV Files
vroom_write(x=kaggle_bart, file="./Bart.csv", delim=",")

# (7) Initialize H2O Session
h2o::h2o.init()

#Define the Model
auto_model <- auto_ml() |>
  set_engine("h2o", max_runtime_secs = 30*60) |>
  set_mode("regression")

#Combine into Workflow
automl_wf <- workflow() |>
  add_recipe(bike_recipe) |>
  add_model(auto_model) |>
  fit(data = train)
beepr::beep()

#Prediction
auto_pred <- predict(automl_wf, new_data = test) |>
  mutate(.pred = exp(.pred))

#Formatting Predictions for Kaggle
kaggle_auto <- auto_pred |>
  bind_cols(test) |> 
  select(datetime, .pred) |> 
  rename(count = .pred) |>
  mutate(count = pmax(0, count)) |> 
  mutate(datetime = as.character(format(datetime)))

#Making CSV Files
vroom_write(x=kaggle_auto, file="./Auto.csv", delim=",")

# (8) Data Robot
#Preparing Data for Data Robot
baked_dr <- bake(prepped_recipe, new_data = train)
vroom_write(x=baked_dr, file="./Recipe.csv", delim=",")
vroom_write(x=baked_dataset, file="./Test_Recipe.csv", delim=",")

#Bringing in Data Robot Predictions
dbot <- vroom("RoboPreds.csv")

#Formatting Predictions for Kaggle
kaggle_dbot <- dbot |>
  bind_cols(test) |> 
  select(datetime, count_PREDICTION) |> 
  rename(count = count_PREDICTION) |>
  mutate(count = pmax(0, exp(count))) |> 
  mutate(datetime = as.character(format(datetime)))

#Making CSV Files
vroom_write(x=kaggle_dbot, file="./Data_Robot.csv", delim=",")

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
