
library(tidyverse)

MLB_24 <- read_csv("data/MLB_24.csv")

MLB_ID <- read_csv("data/MLB_ID.csv")

ArmAngleData = list(
  ArmAngle = read_csv("data/pitcher_arm_angles (1).csv") %>%
    select(pitcher, pitcher_name, ball_angle),
  ArmAngleData2 = list(
    ArmAngleSpinDirection = read_csv("data/spin-direction-pitches (1).csv") %>%
      select(pitcher_name, pitch_type, player_id, diff2, active_spin) %>%
      rename(pitcher = player_id) %>% filter(pitch_type == "FF"),
    PitchMovement2 = MLB_24 %>%
      filter(Month != 10) %>%
      mutate(pfx_x_adj = case_when(p_throws == "R" & pfx_x < 0 ~ abs(pfx_x),
                                   p_throws == "R" & pfx_x >= 0 ~ -abs(pfx_x),
                                   p_throws == "L" & pfx_x >= 0 ~ abs(pfx_x),
                                   p_throws == "L" & pfx_x < 0 ~ -abs(pfx_x),
                                   .default = pfx_x),
             vy_f = -sqrt(vy0^2-(2*ay*(50-17/12))), 
             t = (vy_f-vy0)/ay, vz_f = vz0+(az*t), VAA = -atan(vz_f/vy_f)*(180/pi),
             release_angle_x = atan(vx0/vy0) * 180/pi, 
             release_angle_z = atan(vz0/sqrt(vx0^2+vy0^2)) * 180/pi,
             Zone_loc = abs(sz_top - plate_z)) %>%
      group_by(pitcher, pitch_type) %>%
      reframe(vMove = mean(pfx_z, na.rm = T) * 12, hMove = mean(pfx_x_adj, na.rm = T) * 12, Pitches = n(),
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
              Zone_loc = mean(Zone_loc, na.rm = T)) %>%
      filter(pitch_type == "FF", Pitches >= 50),
    LaunchAngle = MLB_24 %>%
      filter(!is.na(bb_type)) %>%
      group_by(pitcher, pitch_type) %>%
      reframe(EV = mean(launch_speed, na.rm = T), LA = mean(launch_angle, na.rm = T))
  ) %>% reduce(merge, by = c("pitcher", "pitch_type")),
  Height = list(
    Height1 = read_csv("data/SwStr Model (VBOE) - Sheet1.csv") %>%
      mutate(Height = Ft. * 12 + In.) %>%
      select(PlayerID, Height),
    Height2 = MLB_ID %>%
      select(mlbid, key_bbref) %>%
      rename(pitcher = mlbid, PlayerID = key_bbref)
  ) %>% reduce(merge, by = "PlayerID") %>% select(pitcher, Height),
  TotalPitches = MLB_24 %>%
    filter(Month != 10) %>%
    group_by(pitcher) %>%
    reframe(Pitches_total = n())
) %>% reduce(merge, by = "pitcher") %>%
  select(-c(pitcher_name.y)) %>% rename(pitcher_name = pitcher_name.x)

ArmAngle_vModel = lm(log(vMove) ~ ball_angle * active_spin + I(ball_angle^2) + I(active_spin^2), ArmAngleData)

ArmAngle_vPredict = predict(ArmAngle_vModel, newdata = ArmAngleData, type = "response")

ArmAngle_hModel = lm(hMove ~ ball_angle * active_spin, ArmAngleData)

ArmAngle_hPredict = predict(ArmAngle_hModel, newdata = ArmAngleData, type = "response")

ArmAngleMaster = cbind(ArmAngleData, ArmAngle_vPredict, ArmAngle_hPredict) %>%
  rename(vMove_exp = ArmAngle_vPredict, hMove_exp = ArmAngle_hPredict) %>%
  mutate(vMove_exp = exp(vMove_exp), vMove_diff = vMove - vMove_exp, hMove_diff = hMove - hMove_exp,
         Pitch_p = Pitches/Pitches_total)

ArmAngle_whiffModel = lm(Whiff_p ~ vMove_diff + Velocity + VAA + Zone_loc, ArmAngleMaster)

ArmAngle_whiffPredict = predict(ArmAngle_whiffModel, newdata = ArmAngleMaster, type = "response")

ArmAngleWhiff = cbind(ArmAngleMaster, ArmAngle_whiffPredict) %>%
  rename(xWhiff_p = ArmAngle_whiffPredict) %>%
  select(pitcher_name, pitcher, Pitches, Velocity, VAA, vMove_diff, Zone_loc, Whiff_p, xWhiff_p) %>%
  arrange(desc(xWhiff_p)) %>%
  mutate(xWhiff_rank = row_number(), Whiff_diff = Whiff_p - xWhiff_p)
