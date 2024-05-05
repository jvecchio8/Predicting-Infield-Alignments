library(baseballr)
library(tidyverse)
library(cowplot)
library(caret)
library(randomForest)
library(ranger)
library(vip)


savant_data_Mar_Apr <- read.csv("./DATA/savant_data_March_April.csv")
savant_data_May <- read.csv("./DATA/savant_data_May.csv")
savant_data_June <- read.csv("./DATA/savant_data_June.csv")
savant_data_July <- read.csv("./DATA/savant_data_July.csv")
savant_data_Aug <- read.csv("./DATA/savant_data_Aug.csv")
savant_data_Sep_Oct <- read.csv("./DATA/savant_data_Sep_Oct.csv")

SavantHitterEventData2023 <- rbind(savant_data_Mar_Apr, 
                                   savant_data_May, 
                                   savant_data_June, 
                                   savant_data_July, 
                                   savant_data_Aug, 
                                   savant_data_Sep_Oct)




SavantData2023 <- subset(SavantHitterEventData2023, select = -c(spin_dir, 
                                                                spin_rate_deprecated, 
                                                                break_angle_deprecated, 
                                                                break_length_deprecated,
                                                                tfs_deprecated,
                                                                tfs_zulu_deprecated,
                                                                umpire,
                                                                sv_id,
                                                                description,
                                                                des,
                                                                game_type,
                                                                type,
                                                                game_year,
                                                                game_pk,
                                                                pitcher.1,
                                                                fielder_2.1,
                                                                player_name,
                                                                release_speed,
                                                                release_pos_x,
                                                                release_pos_z,
                                                                game_date,
                                                                pfx_x,
                                                                pfx_z,
                                                                plate_x,
                                                                plate_z,
                                                                vx0,
                                                                vy0,
                                                                vz0,
                                                                ax,
                                                                ay,
                                                                az,
                                                                release_spin_rate,
                                                                release_extension,
                                                                effective_speed,
                                                                release_pos_y,
                                                                spin_axis
                                                                ))


SavantData2023 <- SavantData2023 %>%
  mutate(
    on_3b = ifelse(is.na(on_3b), 0, 1),
    on_2b = ifelse(is.na(on_2b), 0, 1),
    on_1b = ifelse(is.na(on_1b), 0, 1),
    hit_location = ifelse(is.na(hit_location), 10, hit_location)
  )

SavantData2023 <- SavantData2023 %>%
  mutate(hit_location = case_when(
    hit_location == 1 ~ "P",
    hit_location == 2 ~ "C",
    hit_location == 3 ~ "1B",
    hit_location == 4 ~ "2B",
    hit_location == 5 ~ "3B",
    hit_location == 6 ~ "SS",
    hit_location == 7 ~ "LF",
    hit_location == 8 ~ "CF",  
    hit_location == 9 ~ "RF",
    events == "home_run" & hit_location == 10 ~ "HR",
    events == "double" & hit_location == 10 ~ "GRD",
    TRUE ~ as.character(hit_location)  
  ))

SavantData2023$NumberOnBase <- rowSums(SavantData2023[, c("on_3b", "on_2b", "on_1b")])


SavantData2023$if_fielding_alignment[SavantData2023$if_fielding_alignment == ""] <- NA
SavantData2023$of_fielding_alignment[SavantData2023$of_fielding_alignment == ""] <- NA

SavantData2023 <- SavantData2023 %>%
  drop_na(if_fielding_alignment) %>%
  drop_na(of_fielding_alignment)


columns_to_convert <- c("if_fielding_alignment", "of_fielding_alignment", "batter", 
                        "pitcher", "fielder_2","fielder_3", "fielder_4", "fielder_5", 
                        "fielder_6", "fielder_7", "fielder_8", "fielder_9", "zone", 
                        "pitch_type", "events", "stand", "p_throws", "home_team",
                        "away_team", "hit_location", "bb_type", "inning_topbot",
                        "pitch_name")

for (col in columns_to_convert) {
  SavantData2023[[col]] <- as.factor(SavantData2023[[col]])
}


SavantData2023 <- drop_na(SavantData2023)

SavantData2023 <- SavantData2023 %>%
  rename(
    catcher = fielder_2,
    first_baseman = fielder_3,
    second_baseman = fielder_4,
    third_baseman = fielder_5,
    shortstop = fielder_6,
    left_fielder = fielder_7,
    center_fielder = fielder_8,
    right_fielder = fielder_9,
    hit_coordinate_x = hc_x,
    hit_coordinate_y = hc_y,
    batter_strike_zone_top = sz_top,
    batter_strike_zone_bottom = sz_bot
  )

set.seed(12388)
train_indices <- sample(nrow(SavantData2023), 0.8 * nrow(SavantData2023))
train_data <- SavantData2023[train_indices, ]
test_data <- SavantData2023[-train_indices, ]


model <- ranger(if_fielding_alignment ~ ., data = train_data, num.trees = 1000 ,importance = 'impurity')

print(model)

predictions <- predict(model, data = test_data)

predicted_classes <- as.numeric(predictions$predictions)
actual_classes <- as.numeric(test_data$if_fielding_alignment)

accuracy <- mean(predicted_classes == actual_classes)
cat("Accuracy:", accuracy, "\n")

conf_matrix <- confusionMatrix(factor(predicted_classes), factor(actual_classes))
print(conf_matrix)

overall_precision <- mean(conf_matrix$byClass[ , "Precision"])
overall_recall <- mean(conf_matrix$byClass[ , "Recall"])
overall_f1_score <- mean(conf_matrix$byClass[ , "F1"])

cat("Overall Precision:", overall_precision, "\n")
cat("Overall Recall:", overall_recall, "\n")
cat("Overall F1 Score:", overall_f1_score, "\n")


importance(model)

importance_values <- c(
  pitch_type = 554.55405, batter = 1348.46839, pitcher = 1069.20290, events = 282.79693,
  zone = 703.86563, stand = 5599.81076, p_throws = 276.30639, home_team = 1007.10950,
  away_team = 949.26184, hit_location = 467.13479, bb_type = 225.34374, balls = 353.88799,
  strikes = 382.73999, on_3b = 447.41681, on_2b = 526.92724, on_1b = 304.06117,
  outs_when_up = 560.73456, inning = 593.63480, inning_topbot = 166.06424, hit_coordinate_x = 1263.82992,
  hit_coordinate_y = 1082.06796, catcher = 935.94498, batter_strike_zone_top = 1127.48308, batter_strike_zone_bottom = 1075.80746,
  hit_distance_sc = 969.55735, launch_speed = 1163.67239, launch_angle = 992.56932,
  first_baseman = 1095.42269, second_baseman = 1070.21489, third_baseman = 1163.81255, shortstop = 1205.59283,
  left_fielder = 1050.38726, center_fielder = 1090.04330, right_fielder = 1115.87907,
  estimated_ba_using_speedangle = 942.70561, estimated_woba_using_speedangle = 957.34953,
  woba_value = 162.03459, woba_denom = 42.45732, babip_value = 92.12694,
  iso_value = 80.26334, launch_speed_angle = 285.77089, at_bat_number = 1003.22354,
  pitch_number = 624.44197, pitch_name = 471.31198, home_score = 458.82060,
  away_score = 468.74418, bat_score = 498.82817, fld_score = 469.99796,
  post_away_score = 476.97893, post_home_score = 463.70738, post_bat_score = 509.53968,
  post_fld_score = 474.02535, of_fielding_alignment = 101.62863, delta_home_win_exp = 1077.97709,
  delta_run_exp = 1104.05862, NumberOnBase = 506.17392
)


sorted_importance_values <- sort(importance_values, decreasing = TRUE)


importance_list <- list()
for (i in seq_along(names(sorted_importance_values))) {
  importance_list[[names(sorted_importance_values)[i]]] <- sorted_importance_values[i]
}


print(importance_list)

vip(model)

conf_matrix_df <- data.frame(conf_matrix$table)


class_levels <- c("Infield shade", "Standard", "Strategic")
conf_matrix_df$Reference <- factor(conf_matrix_df$Reference, levels = 1:length(class_levels), labels = class_levels)
conf_matrix_df$Prediction <- factor(conf_matrix_df$Prediction, levels = 1:length(class_levels), labels = class_levels)


plot_confusion_matrix <- ggplot(data = conf_matrix_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 0.5) +
  labs(title = "Confusion Matrix",
       x = "Observed Class",
       y = "Predicted Class",
       fill = "Frequency") +
  theme_minimal() +
  scale_fill_gradient(low = "blue", high = "red")

print(plot_confusion_matrix)


geom_spray <- function(...) {
  ggplot(...) +
    geom_curve(x = 33, xend = 223, y = -100, yend = -100, curvature = -.65) +
    geom_segment(x = 128, xend = 33, y = -208, yend = -100) +
    geom_segment(x = 128, xend = 223, y = -208, yend = -100) +
    geom_curve(x = 83, xend = 173, y = -155, yend = -156, curvature = -.65, linetype = "dotted") +
    coord_fixed() +
    scale_x_continuous(NULL, limits = c(25, 225)) +
    scale_y_continuous(NULL, limits = c(-225, -25))
}

sample_spray_data %>% 
  geom_spray(aes(x = hc_x, y = -hc_y)) +
  geom_point(aes(color = pitch_name), alpha = .5) +
  labs(title = "Spray Chart With Field Lines", 
       subtitle = "All MLB Games July 2 & July 3, 2022", 
       color = "Pitch") +
  theme_void()

matt_olsen <- SavantData2023 %>%
  filter(batter == 621566)


matt_olsen %>% 
  geom_spray(aes(x = hit_coordinate_x, y = -hit_coordinate_y)) +
  geom_density_2d_filled(alpha = 0.5, show.legend = FALSE) +
  #geom_point() +
  labs(title = "Contour Plot of Full Field",  subtitle = "Matt Olsen 2023 season") +
  theme_void() +
  theme(text = element_text(family = "serif")) 


high_importance_model <- SavantData2023 %>%
  select(stand, batter, hit_coordinate_x, hit_coordinate_y, first_baseman, second_baseman, 
         third_baseman, shortstop, left_fielder, center_fielder, 
         right_fielder, launch_speed, pitcher, if_fielding_alignment)  
  
set.seed(456)
train_indices_second_model <- sample(nrow(high_importance_model), 0.8 * nrow(high_importance_model))
train_data_second_model <- high_importance_model[train_indices_second_model, ]
test_data_second_model <- high_importance_model[-train_indices_second_model, ]


model2 <- ranger(if_fielding_alignment ~ ., data = train_data_second_model, importance = 'impurity')

print(model2)

predictions_second_model <- predict(model2, data = test_data_second_model)

predicted_classes_second_model <- as.numeric(predictions_second_model$predictions)
actual_classes_second_model <- as.numeric(test_data_second_model$if_fielding_alignment)

accuracy_second_model <- mean(predicted_classes_second_model == actual_classes_second_model)
cat("Accuracy:", accuracy_second_model, "\n")

conf_matrix_second_model <- confusionMatrix(factor(predicted_classes_second_model), factor(actual_classes_second_model))
print(conf_matrix_second_model)

overall_precision_second_model <- mean(conf_matrix_second_model$byClass[ , "Precision"])
overall_recall_second_model <- mean(conf_matrix_second_model$byClass[ , "Recall"])
overall_f1_score_second_model <- mean(conf_matrix_second_model$byClass[ , "F1"])

cat("Overall Precision:", overall_precision_second_model, "\n")
cat("Overall Recall:", overall_recall_second_model, "\n")
cat("Overall F1 Score:", overall_f1_score_second_model, "\n")


input_data <- data.frame(
  batter = as.factor(621566),
  pitcher = as.factor(543135),
  stand = as.factor("L"),
  hit_coordinate_x = 131.5875,
  hit_coordinate_y = 111.8128,
  launch_speed = 93.62873,
  first_baseman = as.factor(663993),
  second_baseman = as.factor(543760),
  third_baseman = as.factor(669701),
  shortstop = as.factor(608369),
  left_fielder = as.factor(677649),
  center_fielder = as.factor(665750),
  right_fielder = as.factor(666969),
  if_fielding_alignment = as.factor("Infield shade")
)


predictions_input_data <- predict(model2, data = input_data)


predicted_alignment_input_data <- predictions_input_data$predictions

cat("Predicted Fielding Alignment:", predicted_alignment_input_data, "\n")


predicted_actual_df <- data.frame(
  Predicted = factor(predicted_classes_second_model, labels = c("Infield shade", "Standard", "Strategic")),
  Actual = factor(actual_classes_second_model, labels = c("Infield shade", "Standard", "Strategic"))
)


predicted_actual_plot <- ggplot(data = predicted_actual_df, aes(x = Actual, fill = Predicted)) +
  geom_bar(position = "dodge") +
  labs(title = "Predicted vs. Actual Defensive Alignments",
       x = "Actual Alignment",
       y = "Frequency",
       fill = "Predicted Alignment") +
  theme_minimal()

print(predicted_actual_plot)



team_analysis <- SavantData2023 %>%
  group_by(home_team, if_fielding_alignment) %>%
  summarise(avg_woba_value = mean(woba_value, na.rm = TRUE)) %>%
  arrange(desc(avg_woba_value))

print(team_analysis)




