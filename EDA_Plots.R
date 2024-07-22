pacman::p_load(data.table, ggplot2)
sex <- fread("CRC_sex.csv")
race <- fread("CRC_race.csv")
age <- fread("CRC_age.csv")

ggplot(sex[, sex := stringr::str_to_title(sex)], aes(x = year)) +
  geom_line(aes(y = total_est, color = "Estimated Total")) +
  facet_wrap(~sex) +
  geom_line(aes(y = N_ID, color = "Observed")) +
  geom_ribbon(aes(ymin = total_li, ymax = total_ui, fill = "Estimated Total"), alpha = 0.1) +
  theme_bw() +
  labs(x = "Year", y = "N") +
  scale_color_manual(name = "Legend", values = c("Observed" = "blue", "Estimated Total" = "darkorange")) +
  scale_fill_manual(name = "Legend", values = c("Estimated Total" = "darkorange"), guide = "none") +
  geom_point(aes(y = total_est, col = "Estimated Total"), alpha = 0.5) +
  geom_point(aes(y = N_ID, col = "Observed"), alpha = 0.5)
ggsave("CRC_sex.png")

ggplot(race[, race := stringr::str_to_title(race)], aes(x = year)) +
  geom_line(aes(y = total_est, color = "Estimated Total")) +
  facet_wrap(~race, scales = "free") +
  geom_line(aes(y = N_ID, color = "Observed")) +
  geom_ribbon(aes(ymin = total_li, ymax = total_ui, fill = "Estimated Total"), alpha = 0.1) +
  theme_bw() +
  labs(x = "Year", y = "N") +
  scale_color_manual(name = "Legend", values = c("Observed" = "blue", "Estimated Total" = "darkorange")) +
  scale_fill_manual(name = "Legend", values = c("Estimated Total" = "darkorange"), guide = "none") +
  geom_point(aes(y = total_est, col = "Estimated Total"), alpha = 0.5) +
  geom_point(aes(y = N_ID, col = "Observed"), alpha = 0.5)
ggsave("CRC_race.png")

ggplot(age[, agegrp := gsub("_", "-", agegrp)], aes(x = year)) +
  geom_line(aes(y = total_est, color = "Estimated Total")) +
  facet_wrap(~agegrp, scales = "free") +
  geom_line(aes(y = N_ID, color = "Observed")) +
  geom_ribbon(aes(ymin = total_li, ymax = total_ui, fill = "Estimated Total"), alpha = 0.1) +
  theme_bw() +
  labs(x = "Year", y = "N") +
  scale_color_manual(name = "Legend", values = c("Observed" = "blue", "Estimated Total" = "darkorange")) +
  scale_fill_manual(name = "Legend", values = c("Estimated Total" = "darkorange"), guide = "none") +
  geom_point(aes(y = total_est, col = "Estimated Total"), alpha = 0.5) +
  geom_point(aes(y = N_ID, col = "Observed"), alpha = 0.5)
ggsave("CRC_age.png")
