if(!require(pacman)) install.packages(pacman)
pacman::p_load(CRCSim, data.table)

data <- create.data(300000, 2024)
groundTruth <- extract.groundTruth(data)

test <- Syndemics::crc(DT, "N_ID", c("APCD", "BSAS", "Casemix", "Death", "Matris", "PMP"))
