library(doFuture)
library(doRNG)
library(future)
library(future.apply)
library(data.table)
library(CRCSim)
library(tidyverse)

captures <- fread("Data/OUDOrigin_26JUN2024.csv")
caps <- captures[N_ID != -1,
                 ][year == 2021,
                   ]
total <- sum(caps$N_ID)
apcd <- sum(caps[APCD == 1, ]$N_ID)
bsas <- sum(caps[BSAS == 1, ]$N_ID)
pmp <- sum(caps[PMP == 1, ]$N_ID)
matris <- sum(caps[Matris == 1, ]$N_ID)
casemix <- sum(caps[Casemix == 1, ]$N_ID)
death <- sum(caps[Death == 1, ]$N_ID)


apcd <- apcd/total
bsas = bsas/total
casemix = casemix/total
death = death/total
matris = matris/total
pmp = pmp/total
# (apcd = .87, bsas = .19, casemix = 0.06, death = 0.02, matrix = 0.11, pmp = 0.42)

race  <- fread("Data/OUDCount_Race_Yearly_26JUN2024.csv")
r <- race[year == 2021,
          ][, prob := N_ID / sum(N_ID)]

# (1 = 0.792, 2 = 0.067, 3 = 0.004, 4 = 0.124, 5 = 0.012)
future::plan(future::multisession())
registerDoFuture()
registerDoRNG()

seeds <- future.apply::future_lapply(1:1e2, function(x) .Random.seed, future.seed = 2024)
list10 <- future.apply::future_lapply(1:1e2, function(x){
  CRCSim::analyze(3e5, 6, 5, c(0.87, 0.19, 0.06, 0.02, 0.11, 0.42), c(0.792, 0.067, 0.004, 0.124, 0.012), suppress = 10)
}, future.seed = seeds)
dat10 <- rbindlist(list10, idcol = "run")


list3 <- future.apply::future_lapply(1:1e2, function(x){
  CRCSim::analyze(3e5, 6, 5, c(0.87, 0.19, 0.06, 0.02, 0.11, 0.42), c(0.792, 0.067, 0.004, 0.124, 0.012), suppress = 3)
}, future.seed = seeds)
dat3 <- rbindlist(list3, idcol = "run")

write.csv(dat3, "dat3.csv")
write.csv(dat10, "dat10.csv")

dat10 <- fread("dat10.csv")
dat3 <- fread("dat3.csv")
test10 <- unique(dat10[, .(se = sd(estimate)/10,
                           mu = mean(estimate),
                           ground = mean(ground)), by = "group"])[, `:=` (lb = mu - qnorm(0.975) * se,
                                                                     ub = mu + qnorm(0.975) * se), by = "group"]
test3 <- unique(dat3[, .(se = sd(estimate)/10,
                          mu = mean(estimate),
                          ground = mean(ground)), by = "group"])[, `:=` (lb = mu - qnorm(0.975) * se,
                                                                         ub = mu + qnorm(0.975) * se), by = "group"]


test3 <- test3[, group := factor(group, levels = c("1", "2", "3", "4", "5"),
                                 labels = c("White", "Black", "Asian/PI", "Hispanic", "Other"))]
test10 <- test10[, group := factor(group, levels = c("1", "2", "3", "4", "5"),
                                 labels = c("White", "Black", "Asian/PI", "Hispanic", "Other"))]

ggplot(test3, aes(y = mu, x = group, group = group)) +
  geom_col(alpha = 0) +
  labs(x = "Stratification", y = "Estimate (Black)\n X marks Ground Truth (Blue)",
       title = "CRC: Estimated vs Ground Truth",
       subtitle = "1:3 Supression") +
  guides(fill = "none") +
  geom_point(aes(y = ground), shape = 4, size = 2, col = "blue") +
  geom_errorbar(aes(ymin = lb, ymax = ub)) +
  theme_bw() +
  geom_text(aes(y = 7500, label = round(mu))) +
  geom_text(aes(y = 7000, label = round(ground)), col = "blue")
ggsave("1_3Suppression.png", dpi = 300)

ggplot(test10, aes(y = mu, x = group, group = group)) +
  geom_col(alpha = 0) +
  labs(x = "Stratification", y = "Estimate (Black)\n X marks Ground Truth (Blue)",
       title = "CRC: Estimated vs Ground Truth",
       subtitle = "1:10 Supression") +
  guides(fill = "none") +
  geom_point(aes(y = ground), shape = 4, size = 2, col = "blue") +
  geom_errorbar(aes(ymin = lb, ymax = ub)) +
  theme_bw() +
  geom_text(aes(y = 7500, label = round(mu))) +
  geom_text(aes(y = 7000, label = round(ground)), col = "blue")
ggsave("1_10Suppression.png")

#====================================
od_total <- fread("Data/Overdose_Monthly_26JUN2024.csv")[fod == 1,
                                                         ][,age_grp_five := NULL
                                                           ] |>
  unique()
od_total <- od_total[, date := lubridate::make_date(year, month, 1L)]
od_race <- fread("Data/OverdoseRace_Monthly_26JUN2024.csv")
od_race_pct <- od_race[fod == 1 & N_ID != -1,
                ][, date := make_date(year, month, 1L)
                  ][, .(resum = sum(N_ID)), by = "date"
                    ][od_total, on = "date"
                      ][, lost := N_ID - resum]
lostODsmean <- mean(od_race_pct$lost)
lostODssd <- sd(od_race_pct$lost)

ggplot(od_race_pct, aes(y = N_ID, x = date)) +
  geom_point(col = "black", shape = 4, size = 3) +
  geom_point(aes(y = resum), col = "red", shape = 1) +
  theme_bw() +
  labs(x = "Month", y = "Total Fatal Overdoses (Black)\nResummed FODs after Race Stratification (Red)",
       title = "Lost Overdoses after Resummation from Racial Stratification",
       subtitle = paste0("Average: ", round(lostODsmean, 2), "\nSD: ", round(lostODssd, 2))) +
  guides(col = "none")
ggsave("fods.png")

od_race_plot <- od_race[, N_ID := ifelse(N_ID == -1, 1, 0)
                        ][fod == 1,
                          ][, .(pct_suppressed = sum(N_ID) / .N * 100), by = "final_re"
                            ][, final_re := factor(final_re, labels = c("White", "Black", "Asian/PI", "Hispanic", "Other"),
                                                   levels = c("1", "2", "3", "4", "5"))]
ggplot(od_race_plot, aes(y = pct_suppressed, x = final_re)) +
  geom_col(fill = "white", col = "black") +
  theme_bw() +
  labs(x = "Race", y = "Percent", title = "Percent of Monthly FOD Cells Suppressed by Race", subtitle = "2013 and 2021 (Inclusive)") +
  guides(fill = "none") +
  geom_text(aes(label = round(pct_suppressed, 2), vjust = -.5))
ggsave("percentLost.png")


ggplot(od_race_pct[, pct := round(lost/N_ID*100,2)], aes(x = date, y = pct)) +
  geom_col() +
  theme_bw() +
  labs(y = "Percent FODs Lost When Stratifying by Race", x = "month")
ggsave("pctlostbar.png")
