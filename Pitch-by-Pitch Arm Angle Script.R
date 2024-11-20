
library(tidyverse)

MLB_24 <- read_csv("data/MLB_24.csv")

MLB_ID <- read_csv("data/MLB_ID.csv")

FF_24 = list(
  FF_1 = MLB_24 %>% filter(pitch_type == "FF", Month != 10),
  FF_2 = ArmAngleSpinDirection
) %>% reduce(merge, by = c("pitcher", "pitch_type"), all = T) %>%
  filter(pitcher != 681799) %>%
  mutate(pfx_x_adj = case_when(p_throws == "R" & pfx_x < 0 ~ abs(pfx_x),
                               p_throws == "R" & pfx_x >= 0 ~ -abs(pfx_x),
                               p_throws == "L" & pfx_x >= 0 ~ abs(pfx_x),
                               p_throws == "L" & pfx_x < 0 ~ -abs(pfx_x),
                               .default = pfx_x),
         Zone_loc = abs(sz_top - plate_z),
         vy_f = -sqrt(vy0^2-(2*ay*(50-17/12))), 
         t = (vy_f-vy0)/ay, vz_f = vz0+(az*t), VAA = -atan(vz_f/vy_f)*(180/pi),
         Whiff = ifelse(description %in% c("swinging_strike", "swinging_strike_blocked", "foul_tip"), 1, 0))

ArmAngle_vModel = lm(log(pfx_z) ~ arm_angle * active_spin + I(arm_angle^2) + I(active_spin^2), FF_24)

ArmAngle_vPredict = predict(ArmAngle_vModel, newdata = FF_24, type = "response")

ArmAngle_hModel = lm(pfx_x_adj ~ arm_angle * active_spin, FF_24)

ArmAngle_hPredict = predict(ArmAngle_hModel, newdata = FF_24, type = "response")

ArmAngleMaster = cbind(FF_24, ArmAngle_vPredict, ArmAngle_hPredict) %>%
  rename(vMove_exp = ArmAngle_vPredict, hMove_exp = ArmAngle_hPredict) %>%
  mutate(vMove_exp = exp(vMove_exp), vMove_diff = pfx_z - vMove_exp, 
         hMove_exp = hMove_exp, hMove_diff = pfx_x_adj - hMove_exp)

ArmAngle_whiffModel = glm(Whiff ~ vMove_diff + release_speed + VAA + Zone_loc, family = "binomial", ArmAngleMaster)

ArmAngle_whiffPredict = predict(ArmAngle_whiffModel, newdata = ArmAngleMaster, type = "response")

ArmAngleMaster2 = list(
  ArmAngleSpinDirection <- read_csv("data/spin-direction-pitches (1).csv") %>%
    rename(pitcher_name = `last_name, first_name`, pitch_type = api_pitch_type) %>%
    select(pitcher_name, pitch_type, player_id, diff2, active_spin) %>%
    rename(pitcher = player_id) %>% filter(pitch_type == "FF"),
  ArmAngleMaster %>%
    filter(Month != 10) %>%
    mutate(vy_f = -sqrt(vy0^2-(2*ay*(50-17/12))), 
           t = (vy_f-vy0)/ay, vz_f = vz0+(az*t), VAA = -atan(vz_f/vy_f)*(180/pi),
           release_angle_x = atan(vx0/vy0) * 180/pi, 
           release_angle_z = atan(vz0/sqrt(vx0^2+vy0^2)) * 180/pi,
           Zone_loc = abs(sz_top - plate_z)) %>%
    group_by(pitcher, pitch_type) %>%
    reframe(ball_angle = mean(arm_angle, na.rm = T), vMove = mean(pfx_z, na.rm = T) * 12, hMove = mean(pfx_x_adj, na.rm = T) * 12, Pitches = n(),
            vMove_exp = mean(vMove_exp, na.rm = T) * 12, hMove_exp = mean(hMove_exp, na.rm = T) * 12, vMove_diff = mean(vMove_diff, na.rm = T) * 12, 
            hMove_diff = mean(hMove_diff, na.rm = T) * 12,
            Whiff = length(which(description %in% c("swinging_strike", "swinging_strike_blocked", "foul_tip"))),
            Swing = length(which(description %in% c("swinging_strike", "swinging_strike_blocked", "foul_tip",
                                                    "foul", "foul_bunt", "bunt_foul_tip", "foul_pitchout",
                                                    "hit_into_play", "missed_bunt"))), 
            Whiff_p = Whiff/Pitches,
            GB_p = length(which(bb_type %in% c("ground_ball")))/length(which(!is.na(bb_type))),
            FB_p = length(which(bb_type %in% c("fly_ball", "popup")))/length(which(!is.na(bb_type))),
            Velocity = mean(release_speed, na.rm = T), RP_z = mean(release_pos_z, na.rm = T),
            RP_x = mean(release_pos_x, na.rm = T), VAA = mean(VAA, na.rm = T),
            release_angle_x = mean(release_angle_x, na.rm = T), release_angle_z = mean(release_angle_z, na.rm = T), 
            Spin = mean(release_spin_rate, na.rm = T), bat_speed_diff = mean(bat_speed_diff, na.rm = T),
            Zone_loc = mean(Zone_loc, na.rm = T), Extension = mean(release_extension, na.rm = T)) %>%
    filter(pitch_type == "FF", Pitches >= 10),
  MLB_24 %>% group_by(pitcher) %>% reframe(TotalPitches = n())
) %>% reduce(merge, by = c("pitcher")) %>% mutate(Pitch_p = Pitches/TotalPitches) %>%
  select(-c(pitch_type.y)) %>% rename(pitch_type = pitch_type.x)

ArmAngleWhiff = list(
  ArmAngleWhiff1 = MLB_ID %>% select(player_name, mlbid) %>% rename(pitcher = mlbid),
  ArmAngleWhiff2 = cbind(ArmAngleMaster, ArmAngle_whiffPredict) %>%
    rename(xWhiff = ArmAngle_whiffPredict) %>%
    select(pitcher, release_speed, VAA, vMove_diff, Zone_loc, Whiff, xWhiff, arm_angle, active_spin)
) %>% reduce(merge, by = "pitcher")

ArmAngleWhiff2 = ArmAngleWhiff %>%
  group_by(player_name) %>%
  reframe(Whiff_p = mean(Whiff, na.rm = T), xWhiff_p = mean(xWhiff, na.rm = T)) %>%
  arrange(desc(xWhiff_p)) %>%
  mutate(xWhiff_rank = row_number(), xWhiff_pct = 1 - xWhiff_rank/max(xWhiff_rank), Whiff_diff = Whiff_p - xWhiff_p)