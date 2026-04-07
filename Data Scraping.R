library(tidyverse)
library(jsonlite)

url <- "https://baseballsavant.mlb.com/gf?game_pk=776481"


raw <- jsonlite::fromJSON(url)
View(raw$exit_velocity)

install.packages("devtools")
devtools::install_github(repo = "saberpowers/sabRmetrics")
library(sabRmetrics)

setwd("~/Desktop/Rice Classes/SMGT 490/smgt490capstone")
savant2025 <- sabRmetrics::download_baseballsavant(
  start_date = "2025-03-27",
  end_date = "2025-09-28")

# Dates ensure we only have data for the 2025 MLB regular season

colnames(savant2025)

bat_tracking <- savant2025 %>%
  mutate(in_zone = plate_x > -8.5/12 & plate_x < 8.5/12 & plate_z > strike_zone_bottom & plate_z < strike_zone_top) %>%
  # Filter to only include swings (no balls/strike called)
  filter(description %in% c("hit_into_play", "foul", "swinging_strike", "foul_tip", "swinging_strike_blocked")) %>%
    mutate(
      contact = case_when(description %in% c("hit_into_play", "foul", "foul_tip") ~ TRUE, .default = FALSE), # "foul", "foul_tip"
      runner_on_first = !is.na(pre_runner_1b_id),
      runner_on_second = !is.na(pre_runner_2b_id),
      runner_on_third = !is.na(pre_runner_3b_id)
      ) %>%
  get_quadratic_coef(source = "baseballsavant") %>%
  get_trackman_metrics() %>%
  dplyr::select(game_id, game_date, year, event_index, home_team, away_team, batter_id, bat_side, batter_name, pitcher_id, pitch_hand, pitcher_id,
              pre_runner_1b_id, pre_runner_2b_id, pre_runner_3b_id, inning, half_inning, outs, balls, strikes, pitch_type, release_speed, plate_x, plate_z,
              strike_zone_top, strike_zone_bottom, bat_speed, swing_length, launch_speed, launch_angle, bb_type, type, description, events, home_score, 
              away_score, post_home_score, post_away_score, attack_angle, attack_direction, swing_path_tilt, delta_run_exp, induced_vert_break, horz_break, contact, 
              runner_on_first, runner_on_second, runner_on_third)
  
  

ggplot(bat_tracking, mapping = aes(plate_x, plate_z)) +
  geom_point(mapping = aes(color = in_zone)) +
  ylim(0, 6) + 
  xlim(-5, 5)

total_swings <- bat_tracking %>%
  filter(type != "ball") %>%
  group_by(batter_id, batter_name) %>%
  mutate(swings = n()) %>%
  ungroup() %>%
  filter(swings > 750) %>%
  mutate(runner_on_first = !is.na(pre_runner_1b_id),
         runner_on_second = !is.na(pre_runner_2b_id),
         runner_on_third = !is.na(pre_runner_3b_id)) %>%
  group_by(batter_id, outs, runner_on_first, runner_on_second, runner_on_third) %>%
  summarise(sitiational_swings = n())


# Should this eventually follow statcast qualifications?
overall_average_swing_metrics <- bat_tracking %>%
  group_by(batter_id, batter_name, bat_side) %>%
  summarise(
    bat_speed = mean(bat_speed, na.rm = T),
    swing_length = mean(swing_length, na.rm = T),
    attack_angle = mean(attack_angle, na.rm = T),
    attack_direction = mean(attack_direction, na.rm = T),
    swing_path_tilt = mean(swing_path_tilt, na.rm = T),
    gbrate = mean(bb_type == "ground_ball", na.rm = T),
    ldrate = mean(bb_type == "line_drive", na.rm = T),
    fbrate = mean(bb_type == "fly_ball" | bb_type == "popup", na.rm = T),
    miss_rate = mean(bb_type == "NA")
  )

situational_average_swing_metrics <- bat_tracking %>%
  group_by(batter_id, outs, runner_on_first, runner_on_second, runner_on_third) %>%
  summarise(
    bat_speed = mean(bat_speed, na.rm = T),
    swing_length = mean(swing_length, na.rm = T),
    attack_angle = mean(attack_angle, na.rm = T),
    attack_direction = mean(attack_direction, na.rm = T),
    swing_path_tilt = mean(swing_path_tilt, na.rm = T)
  )

situational_run_values <- bb_type_model_data %>%
  group_by(outs, runner_on_first, runner_on_second, runner_on_third) %>%
  mutate(base_out_state = paste0(if_else(!runner_on_first, "1", "0"), if_else(!runner_on_second, "1", "0"), if_else(!runner_on_third, "1", "0"), outs)) %>%
  group_by(base_out_state, bb_type) %>% 
  summarise(average_RE = mean(delta_run_exp), .groups = "drop_last") %>%
  pivot_wider(names_from = bb_type, values_from = average_RE, names_prefix = "RE_")


### ATTEMPTING TO GET POSTERIOR DRAWS ###
probs_draws <- lapply(seq(1, nrow(bat_tracking_scaled), by = chunk_size), function(i) {
  
  idx <- i:min(i + chunk_size - 1, nrow(bat_tracking_scaled))
  
  fitted(
    fit,
    newdata = bat_tracking_scaled[idx, ],
    summary = FALSE
  )
})

RE_array <- array(NA, dim = dim(probs_draws)[1:2])  # iter × obs

for (k in 1:dim(probs_draws)[3]) {
  RE_array <- RE_array + probs_draws[, , k] * RE_mat[, k]
}
