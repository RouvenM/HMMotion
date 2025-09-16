tracking_data2 = tracking_data |> group_by(uniId) |> 
  mutate(frame_mim = frameId[which(event == "man_in_motion")]) |> 
  mutate(period = case_when(
  frameId < frame_mim & frameId >= frame_mim - 5 ~ "before",
  frameId > frame_mim & frameId <= frame_mim + 5 ~ "after",
  TRUE ~ "other"
)) %>%
  filter(period %in% c("before", "after")) %>%
  group_by(uniId, period) %>%
  summarise(mean_speed = mean(s, na.rm = TRUE), .groups = "drop")

# Breite Darstellung: Vorher/Nachher pro Spieler
result_wide <- tracking_data2 %>%
  tidyr::pivot_wider(names_from = period, values_from = mean_speed)

# Ausgabe
print(result_wide)

# Optional: t-Test über alle Spieler
t_test <- t.test(result_wide$before, result_wide$after, paired = TRUE)
print(t_test)
