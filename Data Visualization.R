player_totals <- bat_tracking_with_preds %>%
  mutate(base_out_state = paste0(if_else(!runner_on_first, "1", "0"), if_else(!runner_on_second, "1", "0"), if_else(!runner_on_third, "1", "0"), outs)) %>%
  left_join(situational_run_values, by = "base_out_state") %>%
  mutate(
    RE_projection = prob_fly_ball*RE_fly_ball + prob_ground_ball*RE_ground_ball + prob_line_drive*RE_line_drive + prob_miss*0 + prob_popup*RE_popup,
    actual_RE = case_when(
      bb_type == "ground_ball" ~ RE_ground_ball,
      bb_type == "miss" ~ 0,
      bb_type == "fly_ball" ~ RE_fly_ball,
      bb_type == "popup" ~ RE_popup,
      bb_type == "line_drive" ~ RE_line_drive,
      .default = NA
    )
  ) %>%
  group_by(batter_id, batter_name) %>%
  summarise(swing_based_expected_RE = sum(RE_projection, na.rm = T),
            swing_based_actual_2025_RE = sum(actual_RE, na.rm = T),
            swings = n()) %>%
  mutate(expectedperPA = swing_based_expected_RE/swings,
         actualperPA = swing_based_actual_2025_RE/swings) %>%
  filter(batter_id %in% total_swings$batter_id)

g <- ggplot(player_totals, mapping = aes(expectedperPA, actualperPA)) +
  geom_point(aes(color = batter_name)) +
  geom_smooth(method = "lm", se = T) 

ggplotly(g)

fe <- fixef(fit, summary = TRUE)
coef_df <- as.data.frame(fe)
coef_df$term <- rownames(coef_df)
rownames(coef_df) <- NULL

#coef_df <- coef_df[grepl("^b_", coef_df$term), ]
coef_df <- coef_df[!grepl("Intercept", coef_df$term), ]
coef_df$outcome <- str_extract(coef_df$term, "(?<=mu)[^_]+")
#coef_df$variable <- str_remove(coef_df$term, "^b_mu[^_]+_")

coef_df_done <- coef_df %>%
  mutate(variable = str_remove(term, paste0("mu", outcome, "_"))) %>%
  dplyr::rename(
    estimate = Estimate,
    lower = Q2.5,
    upper = Q97.5
  ) %>%
  dplyr::select(variable, outcome, estimate, lower, upper) %>%
  dplyr::mutate(variable = str_to_title(gsub("_", " ", variable)),
                outcome = case_when(
                  outcome == "groundball" ~ "Ground Ball",
                  outcome == "linedrive" ~ "Line Drive",
                  outcome == "miss" ~ "Foul Ball",
                  outcome == "popup" ~ "Popup"
                ))

ggplot(coef_df_done,
       aes(x = estimate, y = variable)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ outcome) +
  theme_minimal() +
  labs(x = "Estimated Effect", y = "Variable", title = "Batted Ball Model Substantive Effects")

ggplot(coef_df_done,
       aes(x = estimate, y = variable)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ outcome) +
  theme_minimal() +
  labs(x = "Estimated Effect", y = "Variable", title = "Batted Ball Model Substantive Effects",
       subtitle = "Relative to Fly Balls")

ggplot(coef_df_done,
       aes(x = estimate, y = outcome)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ variable) +
  theme_minimal() +
  labs(
    x = "Estimated Effect",
    y = "Batted Ball Type",
    title = "Batted Ball Model Substantive Effects",
    subtitle = "Relative to Fly Balls"
  )


ggplot(coef_df_done, aes(x = outcome, y = variable, fill = estimate)) +
  geom_tile() +
  scale_fill_gradient2() +
  theme_minimal()

heatmap_data <- bat_tracking %>%
  filter(!is.na(plate_x), !is.na(plate_z), !is.na(contact_prob)) %>%
  mutate(
    plate_x_bin = cut(plate_x, breaks = seq(-2, 2, by = 0.1)),
    plate_z_bin = cut(plate_z, breaks = seq(0, 5, by = 0.1))
  ) %>%
  group_by(plate_x_bin, plate_z_bin) %>%
  summarise(
    avg_contact_prob = mean(contact_prob, na.rm = TRUE),
    plate_x = mean(plate_x, na.rm = TRUE),
    plate_z = mean(plate_z, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
ggplot(heatmap_data, aes(x = plate_x, y = plate_z)) +
  geom_tile(aes(fill = avg_contact_prob)) +
  scale_fill_viridis_c(name = "Contact Prob") +
  coord_fixed() +
  # Strike zone (black box)
  geom_rect(
    xmin = -8.5/12, xmax = 8.5/12,
    ymin = 1.5, ymax = 3.5,
    color = "black", fill = NA, linewidth = 1
  ) +
  labs(
    x = "Plate X",
    y = "Plate Z",
    title = "Contact Probability Heatmap"
  ) +
  theme_minimal()

ggplot(bat_tracking, aes(plate_x, plate_z, z = contact_prob)) +
  stat_summary_2d(bins = 100, fun = mean) +
  scale_fill_viridis_c(option = "magma") +
  coord_fixed() +
  geom_rect(
    xmin = -0.83, xmax = 0.83,
    ymin = 1.5, ymax = 3.5,
    color = "black", fill = NA, linewidth = 1.5
  ) + 
  xlim(-2, 2) + 
  ylim(0, 5) +
  labs(x = "Horizontal Location (feet)", y = "Pitch Height", fill = "Contact Prob", 
       title = "Contact Probability by Pitch Location")

ggplot(bat_tracking, aes(-horz_break, induced_vert_break, z = contact_prob)) +
  stat_summary_2d(bins = 100, fun = mean) +
  scale_fill_viridis_c(option = "magma") +
  coord_fixed() +
  facet_grid(~pitch_hand) + 
  labs(x = "Horizontal Break", y = "Induced Vertical Break", fill = "Contact Prob",
       title = "Contact Probability by Pitcher Hand and Pitch Movement")
# xlim(-20, 20) + 
# ylim(-20, 20)

coef_df <- posterior_summary(fit, pars = "^b_") %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term")

ggplot(coef_df, aes(x = Estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5))
