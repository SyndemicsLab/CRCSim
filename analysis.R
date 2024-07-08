if(!dir.exists("lib")) dir.create("lib")
# CLUSTER CUSTOM PACKAGE INSTALL: .libPaths(c("lib", .libPaths()))
if(!require(pacman)) install.packages(pacman)
pacman::p_load(CRCSim, data.table, doFuture, future, future.apply, doRNG, devtools)
devtools::install_github("SyndemicsLab/Syndemics")

ncores <- 12 # Number of cores to use
nparticipants <- 1e5 # Population size to simulate
niter <- 5 # Number of bootstraps (iterations)

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

  sex_obf <- CRCSim::analyze(nparticipants, FALSE, TRUE, "sex")
  race_obf <- CRCSim::analyze(nparticipants, FALSE, TRUE, "race")
  age_obf <- CRCSim::analyze(nparticipants, FALSE, TRUE, "agegrp")
  base_obf <- CRCSim::analyze(nparticipants, FALSE, TRUE)

  sex_corr <- CRCSim::analyze(nparticipants, TRUE, FALSE, "sex")
  race_corr <- CRCSim::analyze(nparticipants, TRUE, FALSE, "race")
  age_corr <- CRCSim::analyze(nparticipants, TRUE, FALSE, "agegrp")
  base_corr <- CRCSim::analyze(nparticipants, TRUE, FALSE)

  sex_corr_obf <- CRCSim::analyze(nparticipants, TRUE, TRUE, "sex")
  race_corr_obf <-


})

