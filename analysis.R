if(!require(pacman)) install.packages(pacman)
pacman::p_load(CRCSim, data.table)

DT <- create.data(300000, 2024, groups = "sex")
groundTruth <- extract.groundTruth(DT, group = "sex")

test <- simulate(DT, capture = c("APCD", "BSAS", "Casemix", "Death", "Matris", "PMP"), group = "sex")
