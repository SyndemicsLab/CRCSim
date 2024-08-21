library(CRCSim)

unevenCaptures_Strata <- runCRC(100, 1,
                                p_captures = c(0.9, 0.2, 0.05, 0.02, 0.1, 0.4),
                                p_strata = c(.8, .05, 0.05, 0.15, 0.01))
write.csv(unevenCaptures_Strata, "../Data/unevenCaptures_Strata.csv", row.names = FALSE)

unevenCaptures_noStrata <- runCRC(100, 1,
                                  p_captures = c(0.9, 0.2, 0.05, 0.02, 0.1, 0.4),
                                  1)
write.csv(unevenCaptures_Strata, "../Data/unevenCaptures_noStrata.csv", row.names = FALSE)

evenCaptures_Strata <- runCRC(100, 1,
                              p_captures = rep(0.1, 6),
                              p_strata = c(.8, .05, 0.05, 0.15, 0.01))
write.csv(evenCaptures_Strata, "../Data/evenCaptures_Strata.csv", row.names = FALSE)

evenCaptures_noStrata <- runCRC(100, 1,
                                p_captures = rep(0.1, 6),
                                p_strata = 1)
write.csv(evenCaptures_noStrata, "../Data/evenCaptures_noStrata.csv", row.names = FALSE)
