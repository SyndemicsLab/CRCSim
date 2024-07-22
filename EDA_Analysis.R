pacman::p_load(CRCSim, data.table, doFuture, future, future.apply, doRNG, devtools)
devtools::install_github("SyndemicsLab/Syndemics")

nboot <- 1000

doFuture::registerDoFuture()
future::plan(multisession())
setDTthreads(1)
doRNG::registerDoRNG()

boot.lsit <- c()
boot.list <- future_lapply(1:nboot, function(x) {
  race_mapping <- data.table(final_re = 1:5, race = c("white", "black", "asianpi", "hispanic", "aiother"))
  race <- fread("Raw/OUDOrigin_Race_26JUN2024.csv")[N_ID == -1, N_ID := sample(1:10, 1)
                                                     ][race_mapping, on = "final_re"
                                                      ][, final_re := NULL]
  out.list <- c()
  inner.list <- c()
  for(i in c(2015:2021)){
    out <- CRCSim::simulate(race[year == i, ], groups = "race", suppress = FALSE)
    names(out) <- c(sort(race_mapping$race))

    for(j in unique(race$race)){
      inner.list[[j]] <- list(estimate = out[[j]]$estimate,
                              ui = out[[j]]$upper_ci,
                              li = out[[j]]$lower_ci)
    }
    out.list[[paste(i)]] <- rbindlist(inner.list, idcol = c("race", names(inner.list)))
  }
  race_estimates <- rbindlist(out.list, idcol = c("year", 2015:2021))[, year := as.numeric(year)]
  race_obs <- fread("Raw/OUDCount_Race_Yearly_26JUN2024.csv")[race_mapping, on = "final_re"
                                                              ][, final_re := NULL]
  race_totals <- race_estimates[race_obs, on = c("race", "year")
                                ][, `:=` (total_est = N_ID + estimate,
                                          total_ui = N_ID + ui,
                                          total_li = N_ID + li)]

  age_mapping <- data.table(age_grp_twenty = 1:5, agegrp = c("1_20", "21_40", "41_60", "61_80", "81+"))
  age <- fread("Raw/OUDOrigin_Twenty_26JUN2024.csv")[N_ID == -1, N_ID := sample(1:10, 1)
                                                     ][age_mapping, on = "age_grp_twenty"
                                                      ][, age_grp_twenty := NULL]
  out.list <- c()
  inner.list <- c()
  for(i in c(2015:2021)){
    out <- CRCSim::simulate(age[year == i, ], groups = "agegrp", suppress = FALSE)
    names(out) <- c(sort(age_mapping$agegrp))

    for(j in unique(age$agegrp)){
      inner.list[[j]] <- list(estimate = out[[j]]$estimate,
                              ui = out[[j]]$upper_ci,
                              li = out[[j]]$lower_ci)
    }
    out.list[[paste(i)]] <- rbindlist(inner.list, idcol = c("agegrp", names(inner.list)))
  }
  age_estimates <- rbindlist(out.list, idcol = c("year", 2015:2021))[, year := as.numeric(year)]
  age_obs <- fread("Raw/OUDCount_Twenty_Yearly_26JUN2024.csv")[age_mapping, on = "age_grp_twenty"
                                                               ][, age_grp_twenty := NULL]
  age_totals <- age_estimates[age_obs, on = c("agegrp", "year")
                              ][, `:=` (total_est = N_ID + estimate,
                                        total_ui = N_ID + ui,
                                        total_li = N_ID + li)]

  sex_mapping <- data.table(final_sex = 1:2, sex = c("male", "female"))
  sex <- fread("Raw/OUDOrigin_Sex_26JUN2024.csv")[N_ID == -1, N_ID := sample(1:10, 1)
                                                  ][sex_mapping, on = "final_sex"
                                                      ][, final_sex := NULL]
  out.list <- c()
  inner.list <- c()
  for(i in c(2015:2021)){
    out <- CRCSim::simulate(sex[year == i, ], groups = "sex", suppress = FALSE)
    names(out) <- c(sort(sex_mapping$sex))

    for(j in unique(sex$sex)){
      inner.list[[j]] <- list(estimate = out[[j]]$estimate,
                              ui = out[[j]]$upper_ci,
                              li = out[[j]]$lower_ci)
    }
    out.list[[paste(i)]] <- rbindlist(inner.list, idcol = c("sex", names(inner.list)))
  }
  sex_estimates <- rbindlist(out.list, idcol = c("year", 2015:2021))[, year := as.numeric(year)]
  sex_obs <- fread("Raw/OUDCount_Sex_Yearly_26JUN2024.csv")[sex_mapping, on = "final_sex"
                                                            ][, final_sex := NULL]
  sex_totals <- sex_estimates[sex_obs, on = c("sex", "year")
                              ][, `:=` (total_est = N_ID + estimate,
                                        total_ui = N_ID + ui,
                                        total_li = N_ID + li)]

  total <- fread("Raw/OUDOrigin_26JUN2024.csv")[N_ID == -1, N_ID := sample(1:10, 1)]
  for(i in c(2015:2021)){
    out <- CRCSim::simulate(total[year == i, ], suppress = FALSE)
    out.list[[paste(i)]] <- list(estimate = out$estimate,
                                 ui = out$upper_ci,
                                 li = out$lower_ci)
  }
  total_estimates <- rbindlist(out.list, idcol = c("year", 2015:2021))[, year := as.numeric(year)]
  total_obs <- fread("Raw/OUDCount_Yearly_26JUN2024.csv")
  total_totals <- total_estimates[total_obs, on = "year"
                                  ][, `:=` (total_est = N_ID + estimate,
                                            total_ui = N_ID + ui,
                                            total_li = N_ID + li)]
  return(list(total = total_totals,
              age = age_totals,
              race = race_totals,
              sex = sex_totals))
})

write.csv(sex_totals, "CRC_sex.csv", row.names = FALSE)
write.csv(age_totals, "CRC_age.csv", row.names = FALSE)
write.csv(race_totals, "CRC_race.csv", row.names = FALSE)
write.csv(total_totals, "CRC_total.csv", row.names = FALSE)
