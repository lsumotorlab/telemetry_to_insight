library(tidyverse)

real_file <- r"(C:\Users\galzi\OneDrive - Lietuvos sporto universitetas\Research\Telemetry Methodology\paper\frontiers_sport_active_living\telemetry_example.csv)"

read_csv(real_file, col_select = c(lapCount, lapTime, steer, accGVertical, accGHorizontal, accGFrontal),
         show_col_types = FALSE) |>
  filter(lapCount == 13) |>
  mutate(time_s = lapTime / 1000, steer_scaled = steer / 30) |>
  pivot_longer(c(steer_scaled, accGVertical, accGHorizontal, accGFrontal),
               names_to = "channel", values_to = "value") |>
  ggplot(aes(time_s, value)) +
  geom_line(linewidth = 0.3) +
  facet_wrap(~channel, ncol = 1, scales = "free_y") +
  labs(x = "Lap time (s)", y = NULL, title = "Lap 13 (steering divided by 30)") +
  theme_classic(base_family = "Times New Roman", base_size = 12)