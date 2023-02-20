# Predicting charges
source('global.R')

library(xgboost)
library(tidymodels)
library(ggplot2)
library(vip)
library(bundle)

# Get airports info
airports_df <- read.delim("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
                          sep = ",",
                          header = FALSE,
                          col.names = c("airport_id",
                                        "airport_name",
                                        "city",
                                        "country",
                                        "iata",
                                        "icao",
                                        "lat",
                                        'lon',
                                        "alt",
                                        'tz',
                                        'dst',
                                        'tz_db',
                                        'type',
                                        'source')) %>%
  as_tibble()

# Get data
combined_df <- readRDS('data/oneworld_combined_sample_with_charges.rds') %>%
  left_join(select(airports_df, source_airport = iata, source_country = country)) %>%
  left_join(select(airports_df, dest_airport = iata, dest_country = country))

# Prepare the data ----------------------------------------------

# Get columns for prediction
model_df <- combined_df %>%
  mutate(international = dest_country != source_country) %>%
  select(charges, source_airport, source_country, dest_country, gc_distance,
         airline, source_lat, source_lon, dest_lat, dest_lon) %>%
  mutate(across(.cols = c(source_airport, source_country, dest_country, airline), .fns = as.factor))

set.seed(123)
data_split <- initial_split(model_df, prop = 4/5, strata = charges)

df_train <- training(data_split)
df_test  <- testing(data_split)

# Create recipe - just turning categorical into dummies
df_rec <- recipe(charges ~ ., data = df_train) %>% 
  step_dummy(all_nominal(), -all_outcomes())

# Create CV folds
cv_folds <- vfold_cv(df_train, strata = charges, repeats = 1, v = 3)

# grid specification
xgboost_params <- 
  parameters(
    trees(range = c(10, 200)),
    tree_depth(range = c(1,4))
  )

# Set up the grid
xgboost_grid <- 
  dials::grid_max_entropy(
    xgboost_params, 
    size = 20
  )

# Xgboost - run model -----------------------------------------------------------------

# We run an xgboost with default parameter values

# Set up model
xgb_model <- boost_tree(
  trees = tune(),
  min_n = 3,
  tree_depth = tune()
) %>%
  set_mode("regression") %>%
  set_engine("xgboost", importance = "permutation")

xgb_wf <- workflow() %>%
  add_model(xgb_model) %>% 
  add_recipe(df_rec)

# Takes about 5 minutes
# hyperparameter tuning
xgboost_tuned <- tune_grid(
  object = xgb_wf,
  resamples = cv_folds,
  grid = xgboost_grid,
  metrics = metric_set(rmse, rsq, mae),
  control = control_grid(verbose = TRUE)
)

# ---- Results ----

# Plot results!
autoplot(xgboost_tuned) + theme_light(base_family = "IBMPlexSans")

# Top result
show_best(xgboost_tuned, metric = "rmse")
show_best(xgboost_tuned, metric = "mae")
show_best(xgboost_tuned, metric = "rsq")

# See performance on test set
best_fit <- xgb_wf %>%
  finalize_workflow(select_best(xgboost_tuned, "mae")) %>%
  last_fit(data_split)

# Performance on test data
collect_metrics(best_fit)

# Variable importance

## use this fitted workflow `extract_workflow(stopping_fit)` to predict on new data
extract_workflow(best_fit) %>%
  extract_fit_parsnip() %>%
  vip(num_features = 15, geom = "point")

# Get predictions on test set
collect_predictions(best_fit) %>%
  ggplot(aes(x=.pred, y = charges)) +
  geom_point() +
  geom_abline()

df_test %>%
  mutate(pred = collect_predictions(best_fit)$.pred) %>%
  mutate(res = pred - charges) %>%
  arrange(desc(res)) %>%
  select(pred, charges, everything())

# Confidence intervals
df_preds <- df_test %>%
  mutate(pred = collect_predictions(best_fit)$.pred) %>%
  mutate(res = pred - charges,
         res_pct = res/pred) %>%
  mutate(ci_low = pred * (1 + quantile(res_pct, c(0.2))),
         ci_high = pred * (1 + quantile(res_pct, c(0.8)))) 

# Plot CIs
df_preds %>%
  ggplot(aes(x=charges, y=pred)) +
  geom_point(alpha = 0.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), alpha = 0.5) +
  geom_abline() +
  theme_minimal()

# ---- Final model ----

# Fit the final model on all data for new predictions
xgb_final <- xgb_wf %>%
  finalize_workflow(select_best(xgboost_tuned, "mae")) %>%
  fit(model_df)

# Bundle the model for saving
mod_bundle <- bundle(xgb_final)
saveRDS(mod_bundle, file = "models/xgb_model.rds")

# Save confidence intervals
conf_ints <- quantile(df_preds$res_pct, c(0.2, 0.8))
saveRDS(conf_ints, 'models/xgb_conf_ints.rds')

