library(tidymodels)
library(tidyverse)
library(NbClust)
library(GGally)
library(ggdendro)
library(purrr)


#Load the processed data
load(here::here("data_retrieval", "process_data.RData"))


#Set seed for reproducability
set.seed(2503)


#Recipe
data_processing <-
  recipe(~., data = df_loans) |>
  step_rm(loan_id) |>
  step_impute_knn(all_predictors(), neighbors = 5) |>
  step_range(sentiment, lenderRepaymentTerm, daysSinceFundraising, loanAmount, researchScore) |>
  step_dummy(all_nominal(), -all_outcomes())


#Function to activate recipe
cook_it <- function(recipe) {
  baked <- recipe |> prep() |> bake(new_data = NULL)
  return(baked)
}


#Test for best k
multiple_k_means <- function(recipe, max_k) {
  baked <- cook_it(recipe)

  tibble(k = seq_len(max_k)) |>
    mutate(
      kmc = map(k, ~ kmeans(baked,
                            centers = .x,
                            nstart = 1000
      )),
      kmc_glance = map(kmc, glance),
      kmc_tidy = map(kmc, tidy),
      kmc_augment = map(kmc, augment, baked)
    )
}


#Graph
km_fit <-
  data_processing |>
  multiple_k_means(20) |>
  unnest(cols = c(kmc_glance)) |>
  select(-starts_with("kmc")) |>
  ggplot() +
  aes(x = k, y = tot.withinss / totss) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::percent)


#Chosen k is 5. Run the code and categorize observations into clusters
set.seed(2503)
final <- kmeans(cook_it(data_processing), centers = 5, nstart = 1000)
final <- final |>
  augment(df_loans)
final <- final |>
  rename(cluster = .cluster)

km_fit

