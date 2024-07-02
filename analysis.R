if(!require(pacman)) install.packages(pacman)
pacman::p_load(CRCSim, data.table)

DT <- create.data(300000, 2024, groups = "sex")
groundTruth <- extract.groundTruth(data, group = "sex")

test <- Syndemics::crc(DT, "N_ID", c("APCD", "BSAS", "Casemix", "Death", "Matris", "PMP"))
