base_out_run_exp <- read.csv("base_out_run_exp.csv", stringsAsFactors = TRUE, colClasses = c("character", "numeric"))
#base_out_run_exp$state_clean <- c("0000", "0001", "0002", "1000", "1001", "1002", "0100", "0101", "0102", "1100", "1101", "1102", "")

# situational_run_values <- bb_type_model_data %>%
#   group_by(outs, runner_on_first, runner_on_second, runner_on_third) %>%
#   mutate(base_out_state = paste0(if_else(!runner_on_first, "1", "0"), if_else(!runner_on_second, "1", "0"), if_else(!runner_on_third, "1", "0"), outs)) %>%
#   group_by(base_out_state, bb_type)
#   summarise(average_RE = mean(delta_run_exp))

overall_average_swing_metrics

first_model <- bat_tracking %>%
  mutate(
    bb_cat = case_when(
      bb_type == "ground_ball" ~ "GB",
      bb_type == "line_drive" ~ "LD",
      bb_type %in% c("fly_ball", "popup") ~ "FB",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(bb_cat)) %>%
  mutate(across(
    c(bat_speed, swing_length,
      attack_angle, attack_direction,
      swing_path_tilt),
    scale
  )) %>% na.omit()

first_model$bb_cat <- factor(first_model$bb_cat,
                             levels = c("GB", "LD", "FB"))

library(nnet)

mod <- multinom(
  bb_cat ~ bat_speed + swing_length + attack_angle +
    attack_direction + swing_path_tilt,
  data = first_model
)

summary(mod)

1 - as.numeric(logLik(mod) / logLik(null_mod))

null_mod <- multinom(bb_cat ~ 1, data = first_model)

anova(null_mod, mod, test = "Chisq")

train_control <- trainControl(method = "cv", number = 5)

library(caret)

cv_model <- train(
  bb_cat ~ bat_speed + swing_length + attack_angle +
    attack_direction + swing_path_tilt,
  data = first_model,
  method = "multinom",
  trControl = trainControl(method = "cv", number = 5)
)


savant2025 %>% head(100) %>% View()

# swing probability - filter to pitches in or near the strike zone? this is not as pertinent to the project as the next two points


# Model contact (In Play) probability given swing
library(xgboost)

# Make sure target is 0/1 numeric
bat_tracking$contact <- as.numeric(bat_tracking$contact)

# Remove target from predictors
X <- as.matrix(bat_tracking[, c("bat_speed", "swing_length", "attack_angle", "attack_direction", "swing_path_tilt", "plate_x", "plate_z", "release_speed", "induced_vert_break", "horz_break")])
#X <- model.matrix(contact ~ bat_speed + swing_length + attack_angle + attack_direction + swing_path_tilt + plate_x + plate_z + induced_vert_break + horz_break - 1, data = bat_tracking)
y <- bat_tracking$contact

train_idx <- sample(seq_len(nrow(bat_tracking)), size = 0.8 * nrow(bat_tracking))

X_train <- X[train_idx, ]
y_train <- y[train_idx]

X_test  <- X[-train_idx, ]
y_test  <- y[-train_idx]

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test, label = y_test)

params <- list(
  objective = "binary:logistic",  # key for classification
  eval_metric = "logloss",        # or "auc"
  max_depth = 8,
  eta = 0.01,
  subsample = 0.8,
  colsample_bytree = 0.8
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 300,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 20,
  verbose = 1
)

pred_prob <- predict(xgb_model, dtest)

pred_class <- ifelse(pred_prob > 0.5, 1, 0)

mean(pred_class == y_test) # Model Accuracy

bat_tracking$contact_prob <- X %>% xgb.DMatrix() %>% predict(object = xgb_model, .)

# Model batted ball distribution given contact: Grounder vs Fly ball vs Line drive vs popup vs foulball

library(brms)

bb_type_model_data <- bat_tracking %>% mutate(bb_type = replace_na(bb_type, "miss")) %>% filter(contact_prob > .5)

get_prior(bb_type ~ bat_speed + swing_length + attack_angle + attack_direction + swing_path_tilt,
          data = bb_type_model_data,
          family = categorical())

bat_tracking_scaled <- bb_type_model_data
bat_tracking_scaled[, c(
  "bat_speed",
  "swing_length",
  "attack_angle",
  "attack_direction",
  "swing_path_tilt"
)] <- scale(bat_tracking_scaled[, c(
  "bat_speed",
  "swing_length",
  "attack_angle",
  "attack_direction",
  "swing_path_tilt"
)])

fit <- brm(
  bb_type ~ bat_speed + swing_length + attack_angle + attack_direction + swing_path_tilt, #+ (1 | batter_id),
  data = bat_tracking_scaled,
  family = categorical(),
  iter = 1000,
  warmup = 500,
  prior = c(
    prior(normal(0, 1), class = "b", dpar = "mugroundball"),
    #prior(exponential(2), class = "sd", dpar = "mugroundball"),
    prior(normal(0, 1), class = "b", dpar = "mulinedrive"),
    #prior(exponential(2), class = "sd", dpar = "mulinedrive"),
    prior(normal(0, 1), class = "b", dpar = "mupopup"),
    #prior(exponential(2), class = "sd", dpar = "mupopup"),
    prior(normal(0, 1), class = "b", dpar = "mumiss")
    #prior(exponential(2), class = "sd", dpar = "mumiss")
  ),
  chains = 4,
  cores = 4,
  threads = threading(4),
  backend = "cmdstanr"
)
# 
posterior_summary(fit)
pred_class <- predict(fit)

chunk_size <- 1000

pred_probs <- lapply(seq(1, nrow(bat_tracking_scaled), by = chunk_size), function(i) {
  
  idx <- i:min(i + chunk_size - 1, nrow(bat_tracking_scaled))
  
  fitted(
    fit,
    newdata = bat_tracking_scaled[idx, ],
    summary = TRUE
  )
})

pred_probs_with_row_structure <- lapply(pred_probs, function(x) {
  x[, "Estimate", ]   # 1000 × 5
})

pred_probs_mat <- do.call(rbind, pred_probs_with_row_structure)
dim(pred_probs_mat)
colnames(pred_probs_mat) <- c("prob_fly_ball", "prob_ground_ball", "prob_line_drive", "prob_miss", "prob_popup")

colnames(pred_probs_with_row_structure)
bb_levels <- levels(bat_tracking_scaled$bb_type)

#dimnames(pred_probs[[1]])

bat_tracking_with_preds <- bb_type_model_data %>%
  bind_cols(pred_probs_mat)
