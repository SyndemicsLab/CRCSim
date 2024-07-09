if(!dir.exists("lib")) dir.create("lib")
# CLUSTER CUSTOM PACKAGE INSTALL: .libPaths(c("lib", .libPaths()))
if(!require(pacman)) install.packages(pacman)
pacman::p_load(CRCSim, data.table, doFuture, future, future.apply, doRNG, devtools)
devtools::install_github("SyndemicsLab/Syndemics")

ncores <- 2 # Number of cores to use
nparticipants <- 1e5 # Population size to simulate
niter <- 2 # Number of bootstraps (iterations)
start_seed <- 2024
set.seed(2024)
#Parallel Setup ===============================================================
doFuture::registerDoFuture()
doRNG::registerDoRNG()
future::plan(future::multisession(workers = ncores), gc = TRUE)
data.table::setDTthreads(1)

# Execution ===================================================================
result <- list()
result <- future_lapply(1:niter, function(x) {
  sex <- CRCSim::analyze(nparticipants, FALSE, FALSE, "sex")
  race <- CRCSim::analyze(nparticipants, FALSE, FALSE, "race")
  age <- CRCSim::analyze(nparticipants, FALSE, FALSE, "agegrp")
  base <- CRCSim::analyze(nparticipants, FALSE, FALSE)

  l1 <- list(sex, race, age, base)
  l1 <- lapply(l1, function(x) {
    DT <- x[, `:=` (corr = FALSE, obf = FALSE)]
    return(DT)
  })

  sex_obf <- CRCSim::analyze(nparticipants, FALSE, TRUE, "sex")
  race_obf <- CRCSim::analyze(nparticipants, FALSE, TRUE, "race")
  age_obf <- CRCSim::analyze(nparticipants, FALSE, TRUE, "agegrp")
  base_obf <- CRCSim::analyze(nparticipants, FALSE, TRUE)

  l2 <- list(sex_obf, race_obf, age_obf, base_obf)
  l2 <- lapply(l2, function(x) {
    DT <- x[, `:=` (corr = FALSE, obf = TRUE)]
    return(DT)
  })

  sex_corr <- CRCSim::analyze(nparticipants, TRUE, FALSE, "sex")
  race_corr <- CRCSim::analyze(nparticipants, TRUE, FALSE, "race")
  age_corr <- CRCSim::analyze(nparticipants, TRUE, FALSE, "agegrp")
  base_corr <- CRCSim::analyze(nparticipants, TRUE, FALSE)

  l3 <- list(sex_corr, race_corr, age_corr, base_corr)
  l3 <- lapply(l3, function(x) {
    DT <- x[, `:=` (corr = TRUE, obf = FALSE)]
    return(DT)
  })

  sex_corr_obf <- CRCSim::analyze(nparticipants, TRUE, TRUE, "sex")
  race_corr_obf <- CRCSim::analyze(nparticipants, TRUE, TRUE, "race")
  age_corr_obf <- CRCSim::analyze(nparticipants, TRUE, TRUE, "agegrp")
  base_corr_obf <- CRCSim::analyze(nparticipants, TRUE, TRUE)

  l4 <- list(sex_corr_obf, race_corr_obf, age_corr_obf, base_corr_obf)
  l4 <- lapply(l4, function(x) {
    DT <- x[, `:=` (corr = TRUE, obf = TRUE)]
    return(DT)
  })
  out.list <- data.table::rbindlist(c(l1, l2, l3, l4))
}, future.seed = start_seed)
