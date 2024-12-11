# The following line clears the cache/environment so when the whole script is
# executed, it will do exactly what is supposed to (instead of remembering
# variable from last run)
rm(list = ls())

# PART 0: USER INPUT -----------------------------------------------------------
# Here you can input the number (integer) of months you want to forecast forward
n_forecast <- 12

# PART 1: LIBRARIES ------------------------------------------------------------
# Here you can find the libraries that this script use
# If unavailable, use install.packages("name_of_library") to install it
library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(lubridate)
library(ggplot2)
library(readxl)
library(writexl)

# Set the working directories here. You want to set it to the folder where the
# processed data file is in. At the time of submission, it will be cleared.
setwd("C:/Users/Kydo/Desktop/BAM/Q3 Workshop/Data/Submission_Part4&5")
#setwd("INPUT YOUR WORKING DIRECTORIES HERE")

# Part 2: INPUT DATA -----------------------------------------------------------

# Import the processed data file
df <- read_excel('Processed_Data.xlsx')

# The data comes with two columns Year and Month, this is to combine them to
# create a Date column to use as inputs for the model (essential for timeseries)
df$Date <-  as.Date(paste(df$Year, df$Month, "01", sep = "-"))

# Select the necessary predictors. The processed data file comes with more
# predictors from the sources. For the time being, only these predictors are
# proven to have significant predictive power but in the future, other variables
# may show improvement (or for different usage/predictions)
predictors_lst <- c(#Date
                    'Year',    'Month',
                    
                    #Target Variable
                    'JetFuelPrice_Europe_USton',
                    
                    #Crude Oil Price Family
                    'CrudeOilPrice_EU_Brent',
                    'CrudeOilPrice_US_WTI',
                    
                    #Stock Price Family
                    'SP500_Price',
                    'STOXX600_Price',
                    
                    #Gasoline and Diesel Family
                    'EU_Gasoline',
                    'EU_diesel',
                    'NA_Gasoline',
                    'NA_diesel',
                    
                    #Search Trend, only Google for now
                    'Google_SearchTrend',
                    
                    #Supply family
                    'EU_GasDieselSupply',
                    'EU_JetFuelSupply',
                    
                    #Air Activity family
                    'Africa_RPK_Billion',
                    'AsiaPacific_RPK_Billion',
                    'Europe_RPK_Billion',
                    'LatinAmerica_RPK_Billion',
                    'MiddleEast_RPK_Billion',
                    'NorthAmerica_RPK_Billion',
                    'Africa_CTK_Billion',
                    'AsiaPacific_CTK_Billion',
                    'Europe_CTK_Billion',
                    'LatinAmerica_CTK_Billion',
                    'MiddleEast_CTK_Billion',
                    'NorthAmerica_CTK_Billion',
                    
                    #Market Sentiment Family
                    'EUR_USD_Rate',
                    'Gold_USD',
                    'Inflation_World',
                    'Inflation_EuropeanUnion',
                    'Inflation_G7',
                    'GDP_World',
                    'GDP_EuropeanUnion',
                    'GDP_G7',
                    'GDPChange_World',
                    'GDPChange_EuropeanUnion',
                    'GDPChange_G7')

# Select the chosen variables from the list. This line usually comes with a
# warning from the library but it does not affect the model at all.
df <- df %>% select(Date, all_of(predictors_lst))

# Convert the dataframe into timetable object and double-check Date
df <- df |> tk_tbl() |> mutate(Date = as.Date(Date))

# Plot any variables against Date. It is originally set as the target variable
df |> plot_time_series(Date, JetFuelPrice_Europe_USton,
                   .facet_ncol  = 4,
                   .smooth      = FALSE, 
                   .interactive = TRUE)

# Part 3: RECIPE AND DATAPREP --------------------------------------------------

# Create splits for training/testing. For this purpose, it is recommended to
# keep the test set as 60 months so the models can be trained using the normal
# period. Unexpected events like COVID or the Russian-Ukrainian War are then
# used to test the model (which is better to tell the performance.)
splits <- df |> time_series_split(Date, assess = "60 months", cumulative = TRUE)
splits

# Plot the Train/Test Split
splits |> tk_time_series_cv_plan() |> 
  plot_time_series_cv_plan(.date_var = Date, 
                           .value = JetFuelPrice_Europe_USton, 
                           .title = paste0("Split for Training/Testing"))

# Here you can specify the recipe/formula for the models. When submitting,
# it is kept as ~. which means all of the available predictors in the data
# will be used. You can explicitly change the target variable, as well as the
# predictors

# Overall recipe
recipe_spec <- recipe(JetFuelPrice_Europe_USton ~ ., data = training(splits)) |>
  # Change the role of Year and Month to indicator (will not be used) because
  # they were combined into Date
  update_role(Year, Month, new_role = "indicator") |>
  # Let the model know to use Date as a feature (and automatically extract
  # more feature from Date)
  step_timeseries_signature(Date) |>
  # Step_rm removes features. Because the data is collected on a monthly basis,
  # these data is not usable (daily/weekly/hourly)
  step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(day)|(week)|(am.pm)")) |>
  # Data came from different sources and measurement unit, so they are all
  # normalize (0-1) to have equal effects on the models.
  step_normalize(all_numeric_predictors())
recipe_spec

# Double-check the recipe using the training data (see if it is what you want
# to feed into the model)
recipe_spec %>%prep() %>%juice() %>% glimpse()

# Part 4: WORKFLOWS ------------------------------------------------------------

# Workflow is the signature of tidyverse/tidymodel which makes Machine Learning
# very intuitive. In principle, you create a work flow, then add the model and
# the recipe, then you can fit the workflow against the data.

# It is assumed that users have basic understanding of machine learning models so
# the following lines are not specifically explained.

# Lasso Workflow
workflow_lasso <- workflow() |>
  add_model(spec = linear_reg(penalty = 0.01, mixture = 1) |>
              set_engine("glmnet")) |>
  add_recipe(recipe_spec |> step_rm(Date, Date_month.lbl)) |>
  fit(training(splits))

# Random Forest
workflow_rf <- workflow() |>
  add_model(spec = rand_forest(mode = "regression", trees = 1000) |>
            set_engine("ranger")) |>
  add_recipe(recipe_spec |> step_rm(Date, Date_month.lbl)) |>
  fit(training(splits))

# XGBoost (Boosting)
workflow_xgb <- workflow() |>
  add_model(spec = boost_tree(mode = "regression") |>
            set_engine("xgboost")) |>
  add_recipe(recipe_spec |>
             step_rm(Date, Date_month.lbl)) |>
  fit(training(splits))
workflow_xgb

# Prophet Boost (combines prophet model to predict trend and XGBoost to predict
# seasonality by modeling the residual errors)
workflow_ppb <- workflow() |>
  add_model(
    spec = prophet_boost(
      seasonality_daily  = FALSE, 
      seasonality_weekly = FALSE, 
      seasonality_yearly = TRUE
    ) |> 
      set_engine("prophet_xgboost")
  ) |>
  add_recipe(recipe_spec) |>
  fit(training(splits))
workflow_ppb

# Model pre-evaluation by adding all workflows to a single modeltime table
submodels_tbl <- modeltime_table(
  workflow_lasso,
  workflow_rf,
  workflow_xgb,
  workflow_ppb)
submodels_tbl

# Calibration: computing predictions and residuals from the test set
calibrated_tbl <- submodels_tbl |>
  modeltime_calibrate(new_data = testing(splits))
calibrated_tbl

# Print the output table with the assessment metrics
calibrated_tbl |> modeltime_accuracy(testing(splits)) |> arrange(rmse)

# If you want to write the raw output of the train set, uncomment the following
#lasso_output <- calibrated_tbl[[5]][[1]]
#write_xlsx(lasso_output, 'Lasso_Output.xlsx')
#xgb_output <- calibrated_tbl[[5]][[3]]
#write_xlsx(xgb_output, 'xgb_Output.xlsx')

# Forecast plot of all candidates model
calibrated_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = df,
    keep_data   = TRUE 
  ) %>%
  plot_modeltime_forecast(
    #.facet_ncol         = 4, 
    .conf_interval_show = FALSE,
    .interactive        = TRUE,
    .title = 'Models Performance')


# PART 4.5: GRAPHS OF CANDIDATES -----------------------------------------------

# Extract variable importance measure from the lasso train/test set
workflow_lasso |>
  extract_fit_parsnip() |>
  tidy() |>
  # only non-zero coefficients
  filter(estimate != 0) |>
  arrange(desc(abs(estimate))) |>
  print(n = 10)

# Extract variable importance measure from the XGB train/test set
workflow_xgb |>
  extract_fit_parsnip() |>
  vip::vi() |>
  print(n=10)

# Candidate graph

# Extract numerical predictions
candi_lasso <- calibrated_tbl[[5]][[1]]
candi_rf <- calibrated_tbl[[5]][[2]]
candi_xgb <- calibrated_tbl[[5]][[3]]
candi_pp <- calibrated_tbl[[5]][[4]]

# Dataframe
candi_graph <- data.frame(
  Date = c(df$Date,
           candi_lasso$Date,
           candi_rf$Date,
           candi_pp$Date,
           candi_xgb$Date
           ),
  Value = c(df$JetFuelPrice_Europe_USton,
            candi_lasso$.prediction,
            candi_rf$.prediction,
            candi_pp$.prediction,
            candi_xgb$.prediction
            ))

# Sources
candi_graph$Model <- c(rep("Actual", nrow(df)),
                       rep("Lasso", nrow(candi_lasso)),
                       rep("Random Forest", nrow(candi_rf)),
                       rep("Prophet", nrow(candi_pp)),
                       rep("XGB", nrow(candi_xgb))
                       )

# Graph
ggplot(candi_graph, aes(x = Date, y = Value, color = Model)) +
  geom_line(size = 1.1, alpha = 1) +  # Increase line thickness and adjust transparency
  labs(x = "Date [Year]", y = "Jet Fuel Price (Europe) [US$/ton]",
       title = "Actual and Prediction Jet Fuel Price (Europe)") +
  scale_color_manual(values = c("Actual" = "black",
                                "Lasso" = "red",
                                "Random Forest" = "#009E73",
                                "Prophet" = "#0072B2",
                                "XGB" = "#E69F00"
                                )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        legend.title = element_text(size = 14),  # Increase legend title size
        legend.text = element_text(size = 14),   # Increase legend text size
        legend.key.size = unit(1.5, "lines"),   # Increase legend key size
        axis.title = element_text(size = 14),    # Increase axis title size
        plot.title = element_text(size = 18)) +  # Increase title size
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")  # Set breaks to one year

# PART 5: FORECAST PREDICTORS USING PROPHET ------------------------------------

# Import the processed data file
df <- read_excel('Processed_Data.xlsx')

# The data comes with two columns Year and Month, this is to combine them to
# create a Date column to use as inputs for the model (essential for timeseries)
df$Date <-  as.Date(paste(df$Year, df$Month, "01", sep = "-"))

# Select the predictors (from Part 2) that needs to be projected into the future
# and only uses Date (solely based on historical data)
tmp <- df %>% select(Date, all_of(predictors_lst))

# Split the dataframe into train/test set. The logic is similar to Part 2
splits <- tmp %>% time_series_split(assess = "60 months", cumulative = TRUE)

# Create a loop to project all of the predictors into the future (loop through
# the pre-specified predictors_lst in Part 2)
for (predictor in predictors_lst) {
  # Prophet model and feed the training set
  model_fit_prophet <- suppressWarnings(prophet_boost() %>%
                                          set_engine("prophet_xgboost") %>%
                                          fit(as.formula(paste(predictor, "~ Date")), training(splits)))
  # Summarize and fit to Modeltime Object
  model_table <- modeltime_table(model_fit_prophet)
  # Calibration and making prediction on test set
  calibration_table <- model_table %>% modeltime_calibrate(testing(splits))
  # Forecast using the test set, the duration follow the user input from part 0
  output <- calibration_table %>%
    modeltime_refit(tmp) %>%
    modeltime_forecast(h = paste0(n_forecast, " months"), actual_data = tmp)
  names(output)[names(output) == '.index'] <- "Date"
  # Output the forecast into a new dataframe/excel file if needed
  if (exists("pred_data") && is.data.frame(pred_data)) {
    pred_data$.value <- output$.value
  } else {
    pred_data <- output[c("Date", ".value")]
  }
  names(pred_data)[names(pred_data) == '.value'] <- paste(predictor)
}
#write_xlsx(pred_data, "Pred_Data.xlsx")


# PART 6: FORECAST JET FUEL PRICE ----------------------------------------------

# When forecasting the whole predictors_lst into the future, naturally, the 
# target variable Jet Fuel Price is also included. That is the prediction of the
# Prophet model using only Date data. Usually, it is not good enough as it does
# not use any other predictors, but it can serve as a good reference assuming
# normalities (no randomness or sudden events)

# However, to perform the better forecast, the name has to be changed
names(pred_data)[names(pred_data) == 'JetFuelPrice_Europe_USton'] <- 'JetFuelPrice_Europe_USton_Baseline'

# Re-input the original Jet Fuel Price, because now the dataframe also has the
# future values of the predictors (while the original JFP only has 108 values),
# a number of NA has to be used to fill in the column of the dataframe
tmp<- c(df$JetFuelPrice_Europe_USton, rep(NA, n_forecast))
pred_data$JetFuelPrice_Europe_USton <- tmp
pred_data$Month <- as.factor(month(pred_data$Date))

# At this stage (and by the time of the submission), the Lasso (GLMNET) and the
# Boosting (XGB) are the two best performing model. These models are not 
# timeseries-sensitive so the procedures can follow normal practice

# Set seed number for reproducing outcomes
set.seed(123)

# Split the dataframe into train/test set. This time, the original data (108
# months) will be used for training, the rest (wanted forecast duration) is the
# "test" set.
splits <- initial_time_split(pred_data, prop = (108)/(108+n_forecast))
splits

# Double check if the train/test has the correct duration (otherwise later it
# could pop an error of not having the true value)
df_train <- training(splits)
df_test <- testing(splits)
range(df_train$Date)
range(df_test$Date)

# Set seed number for reproducing purposes
set.seed(456)

# Perform CV-fold validation on the training set
cv_folds <- vfold_cv(df_train, v = 10)
cv_folds

# New recipes for the predictions. At the time of the submission, the Lasso and
# the XGB are the two best performing models. They do not rely on the timetable
# features so they can follow normal practice of tidyverse/tidymodel.
recipe_new <-
  recipe(JetFuelPrice_Europe_USton ~., data = df_train) |>
  # The baseline prediction from Prophet is excluded to avoid leakage
  update_role(Date, JetFuelPrice_Europe_USton_Baseline, new_role = "metadata") |>
  # Dummy the month
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  # Uncomment if you want to see the interactions between each pair of predictors
  #step_interact(~ all_predictors():all_predictors()) |>
  # Normalize as explained earlier
  step_normalize(all_predictors()) 

# Double-check the recipe using the training data (see if it is what you want
# to feed into the model)
recipe_new %>%prep() %>%juice() %>% glimpse()

# Lasso Model. This includes specify the model, the workflow, and the tuning process
lasso_linear_reg <- linear_reg(penalty = tune(), mixture = 1) |> set_engine("glmnet")
lasso_wf <- workflow() |> add_recipe(recipe_new) |> add_model(lasso_linear_reg)
grid_lasso <- grid_regular(penalty(c(-2, 2), trans = log10_trans()), levels = 50)
lasso_tune <-
  lasso_wf |>
  tune_grid(
    resamples = cv_folds,
    grid = grid_lasso,
    metrics = metric_set(rmse, rsq_trad, mae))
lasso_1se_model <- lasso_tune |> select_by_one_std_err(metric = "rmse", desc(penalty))
lasso_wf_tuned <- lasso_wf |> finalize_workflow(lasso_1se_model)
lasso_last_fit <- lasso_wf_tuned |>
  last_fit(splits, metrics = metric_set(rmse, mae, rsq_trad))

# XGB Model.  This includes specify the model, the workflow, and the tuning process
xgb_model_tune <-
  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    stop_iter = 500
  ) |>
  set_mode("regression") |>
  set_engine("xgboost")

xgb_tune_wf <-
  workflow() |>
  add_recipe(recipe_new) |>
  add_model(xgb_model_tune)

class_metrics <- metric_set(rmse, mae, rsq_trad)

# This forces R to use all of the PC's processor to parallelly tune the XGB
num_cores <- parallel::detectCores()
num_cores
doParallel::registerDoParallel(cores = num_cores - 1L)

# Grid to tune the XGB
xgb_grid <- crossing(
  trees = 500 * 1:20,
  learn_rate = c(0.1, 0.01, 0.001),
  tree_depth = c(1, 2, 3))

# Actual tuning, this could take a while
#xgb_tune_res <- tune_grid(xgb_tune_wf,resamples = cv_folds,grid = xgb_grid,metrics = class_metrics)

# Because the previous step (tune_res) can be very time-consuming, the tuning
# results is saved and submitted with the script. The actual tuning is commented
# out. If there are big changes in the model/predictors, it is recommended to
# perform the tuning again. For now, the script will load the previous tune_res
#save(xgb_tune_res, file = './xgb_tune_res.RData')
load(file = './xgb_tune_res.RData')

# Extract the metrics
xgb_tune_metrics <- xgb_tune_res |> collect_metrics()


# Select the best model (for this specific setup)
xgb_best <-
  xgb_tune_metrics |>
  filter(tree_depth == 1, learn_rate == 0.001, trees == 10000) |>
  distinct(trees, tree_depth, learn_rate)
xgb_final_wf <-
  finalize_workflow(xgb_tune_wf, xgb_best)
xgb_final_wf
# Cast prediction
xgb_final_fit <-
  xgb_final_wf |>
  last_fit(splits)


# PART 6.5: GRAPHS OF PREDICTIONS ----------------------------------------------

# Extract variable importance measure from the lasso
lasso_last_fit |>
  extract_fit_parsnip() |>
  tidy() |>
  # only non-zero coefficients
  filter(estimate != 0) |>
  arrange(desc(abs(estimate))) |>
  print(n = 100)

# Extract variable importance measure from the XGB
xgb_final_fit |>
  extract_fit_parsnip() |>
  vip::vi() |>
  print(n=10)

# Extract numeric predictions
lasso_pred <- lasso_last_fit[[5]][[1]]
lasso_pred <- lasso_pred$.pred
xgb_pred <- xgb_final_fit[[5]][[1]]
xgb_pred <- xgb_pred$.pred
#print(length(lasso_pred))

# LASSO PREDICTION GRAPH
# Dataframe
lasso_graph <- data.frame(
  Date = c(pred_data$Date),
  Value = c(df_train$JetFuelPrice_Europe_USton, lasso_pred))

# Sources
lasso_graph$Source <- c(rep("Actual", nrow(df_train)),
                 rep("Predictions", length(lasso_pred)))

# Graph
ggplot(lasso_graph, aes(x = Date, y = Value, color = Source)) +
  geom_line(size = 1.1, alpha = 0.8) +  # Increase line thickness and adjust transparency
  labs(x = "Date [Year]", y = "Jet Fuel Price (Europe) [US$/ton]",
       title = "Actual and Prediction Jet Fuel Price (Europe)") +
  scale_color_manual(values = c("Actual" = "black", "Predictions" = "red")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        legend.title = element_text(size = 14),  # Increase legend title size
        legend.text = element_text(size = 14),   # Increase legend text size
        legend.key.size = unit(1.5, "lines"),   # Increase legend key size
        axis.title = element_text(size = 14),    # Increase axis title size
        plot.title = element_text(size = 18)) +  # Increase title size
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")  # Set breaks to one year

# XGB PREDICTION GRAPH
# Dataframe
xgb_graph <- data.frame(
  Date = c(pred_data$Date),
  Value = c(df_train$JetFuelPrice_Europe_USton, xgb_pred))

# Sources
xgb_graph$Source <- c(rep("Actual", nrow(df_train)),
                        rep("Predictions", length(xgb_pred)))

# Graph
ggplot(xgb_graph, aes(x = Date, y = Value, color = Source)) +
  geom_line(size = 1.1, alpha = 0.8) +  # Increase line thickness and adjust transparency
  labs(x = "Date [Year]", y = "Jet Fuel Price (Europe) [US$/ton]",
       title = "Actual and Prediction Jet Fuel Price (Europe)") +
  scale_color_manual(values = c("Actual" = "black", "Predictions" = "red")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        legend.title = element_text(size = 14),  # Increase legend title size
        legend.text = element_text(size = 14),   # Increase legend text size
        legend.key.size = unit(1.5, "lines"),   # Increase legend key size
        axis.title = element_text(size = 14),    # Increase axis title size
        plot.title = element_text(size = 18)) +  # Increase title size
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")  # Set breaks to one year
