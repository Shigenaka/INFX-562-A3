setwd("/Users/MasonShigenaka/Desktop/INFX 562/INFX-562-A3")
path <- "./data/raw/"

library(dplyr)

all_data <- data.frame()

file_names <- dir(path, pattern=".csv")

for(i in 1:length(file_names)) {
  file <- read.csv(file.path(path, file_names[i]))
  team <- substr(file_names[i],1,3)
  file <- file %>%
    filter(events != "null") %>%
    mutate(team = team)
  len <- nrow(file)
  print(len)
  write.csv(file, file.path(path, file_names[i]), row.names = F)
  all_data <- rbind(all_data, file)
}

out_data <- all_data %>%
  select(-batter,
         -pitcher,
         -spin_dir,
         -spin_rate_deprecated,
         -break_angle_deprecated,
         -break_length_deprecated,
         -on_3b,
         -on_2b,
         -on_1b,
         -tfs_deprecated,
         -tfs_zulu_deprecated
         -pos2_person_id,
         -umpire,
         -sv_id,
         -game_pk,
         -pos1_person_id,
         -pos2_person_id,
         -pos3_person_id,
         -pos4_person_id,
         -pos5_person_id,
         -pos6_person_id,
         -pos7_person_id,
         -pos8_person_id,
         -pos9_person_id
         -launch_speed_angle,
         -at_bat_number,
         -pitch_number,
         -home_score,
         -away_score,
         -bat_score,
         -fld_score,
         -post_away_score,
         -post_home_score,
         -post_bat_score,
         -post_fld_score) %>%
  mutate(events = ifelse((events=="field_out" & bb_type == "fly_ball"), 
                         "Fly Out", 
                         as.character(events))) %>%
  mutate(events = ifelse((events=="field_out" & bb_type == "line_drive"), 
                         "Line Out", 
                         as.character(events))) %>%
  mutate(events = ifelse((events=="field_out" & bb_type == "ground_ball"), 
                         "Ground Out", 
                         as.character(events))) %>%
  mutate(events = ifelse((events=="field_out" & bb_type == "popup"), 
                         "Popup", 
                         as.character(events))) %>%
  mutate(events = ifelse(events == "sac_fly", "Fly Out",
                         as.character(events))) %>%
  mutate(events = ifelse(events == "double", "Double",
                         as.character(events))) %>%
  mutate(events = ifelse(events == "walk", "Walk",
                         as.character(events))) %>%
  mutate(events = ifelse(events == "single", "Single",
                         as.character(events))) %>%
  mutate(events = ifelse(events == "strikeout", "Strikeout",
                         as.character(events))) %>%
  mutate(events = ifelse(events == "grounded_into_double_play" |
                           events == "double_play" |
                           events == "strikeout_double_play" |
                           events == "sac_fly_double_play",
                         "Double/Triple Play", as.character(events))) %>%
  mutate(events = ifelse(events == "force_out", "Fielder's Choice",
                         as.character(events))) %>%
  mutate(events = ifelse(events == "hit_by_pitch", "Hit by Pitch",
                         as.character(events))) %>%
  mutate(events = ifelse(events == "fielders_choice_out" |
                           events == "fielders_choice", 
                         "Fielder's Choice", as.character(events))) %>%
  mutate(events = ifelse(events == "home_run", "Home Run",
                         as.character(events))) %>%
  mutate(events = ifelse(events == "field_error", "Error",
                         as.character(events))) %>%
  mutate(events = ifelse(events == "intent_walk", "Intentional Walk",
                         as.character(events))) %>%
  mutate(events = ifelse(events == "sac_bunt", "Sacrifice",
                         as.character(events))) %>%
  mutate(events = ifelse(events == "triple", "Triple",
                         as.character(events))) %>%
  mutate(events = ifelse(events == "caught_stealing_2b" |
                           events == "caught_stealing_3b" |
                           events == "caught_stealing_home",
                         "Caught Stealing", as.character(events))) %>%
  mutate(events = ifelse(events == "pickoff_caught_stealing_2b" |
                           events == "pickoff_2b" |
                           events == "pickoff_caught_stealing_home" |
                           events == "pickoff_1b" |
                           events == "pickoff_caught_stealing_3b",
                         "Pickoff", as.character(events))) %>%
  mutate(events = ifelse(events == "triple_play", "Double/Triple Play",
                         as.character(events))) %>%
  mutate(events = ifelse(events == "catcher_interf", "Catcher's Interference",
                         as.character(events))) %>%
  mutate(events = ifelse(events == "other_out" |
                           events == "run",
                         "Other", as.character(events))) %>%
  select(-description,
         -game_type) %>%
  mutate(team = ifelse(team == "bal", "Baltimore Orioles", as.character(team))) %>%
  mutate(team = ifelse(team == "bos", "Boston Red Sox", as.character(team))) %>%
  mutate(team = ifelse(team == "cle", "Cleveland Indians", as.character(team))) %>%
  mutate(team = ifelse(team == "cws", "Chicago Whitesox", as.character(team))) %>%
  mutate(team = ifelse(team == "det", "Detroit Tigers", as.character(team))) %>%
  mutate(team = ifelse(team == "hou", "Houston Astros", as.character(team))) %>%
  mutate(team = ifelse(team == "kc.", "Kansas City Royals", as.character(team))) %>%
  mutate(team = ifelse(team == "laa", "Los Angeles Angels", as.character(team))) %>%
  mutate(team = ifelse(team == "min", "Minnesota Twins", as.character(team))) %>%
  mutate(team = ifelse(team == "nyy", "New York Yankees", as.character(team))) %>%
  mutate(team = ifelse(team == "oak", "Oakland Athletics", as.character(team))) %>%
  mutate(team = ifelse(team == "sea", "Seattle Mariners", as.character(team))) %>%
  mutate(team = ifelse(team == "tb.", "Tampa Bay Rays", as.character(team))) %>%
  mutate(team = ifelse(team == "tex", "Texas Rangers", as.character(team))) %>%
  mutate(team = ifelse(team == "tor", "Toronto Blue Jays", as.character(team))) %>%
  mutate(count = paste0(balls, "-", strikes)) %>%
  mutate(pitch_supertype = ifelse(pitch_name == "Knuckle Curve","Curveball",
                                  as.character(pitch_name)))

out_data <- out_data %>%
  mutate(pitch_supertype = ifelse(pitch_name == "4-Seam Fastball" |
                                    pitch_name == "2-Seam Fastball",
                                  "Fastball", as.character(pitch_supertype))) %>%
  mutate(pitch_supertype = ifelse(pitch_name == "Curveball" |
                                    pitch_name == "Knuckle Curve",
                                  "Curveball", as.character(pitch_supertype))) %>%
  filter(pitch_supertype != "") %>%
  filter(!(events %in% c("Intentional Walk", 
                         "Catcher's Interference",
                         "Pickoff",
                         "Other",
                         "Caught Stealing",
                         "Hit by Pitch",
                         "Sacrifice",
                         "Error",
                         "Fielder's Choice"))) %>%
  filter(!(pitch_supertype %in% c("Eephus",
                                  "Screwball",
                                  "Knuckle Ball",
                                  "Unknown",
                                  "Automatic Ball"))) %>%
  filter(count != "4-2")

first_half <- out_data %>%
  mutate(source = count,
         target = pitch_supertype) %>%
  group_by(source, target) %>%
  summarise(freq = n())

second_half <- out_data %>%
  mutate(source = pitch_supertype,
         target = events) %>%
  group_by(source, target) %>%
  summarise(freq = n())

first_half$source_type <- "Count"
first_half$target_type <- "Pitch"

second_half$source_type <- "Pitch"
second_half$target_type <- "Outcome"

out_data_grouped <- rbind(first_half, second_half)

#write.csv(out_data, "./data/prepped/prepped_data.csv", row.names = F)
write.csv(out_data_grouped, "./data/prepped/prepped_data_grouped.csv", row.names = F)
