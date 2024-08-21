#' Function to compare between simulation results and ground truth
#'
#' @param sim list of simulations done by \code{simulate}
#' @param groundTruth list of ground truths by \code{extract.groundTruth}
#'
#' @import data.table
#' @keywords internal

compare <- function(sim, groundTruth) {
  compare.list <- c()
  if(length(groundTruth) > 1){
    for(i in seq_along(sim)) {
      aic <- sim[[i]]$AIC
      est <- sim[[i]]$estimate
      est.lci <- sim[[i]]$lower_ci
      est.uci <- sim[[i]]$upper_ci
      form <- sim[[i]]$formula

      group <- sort(names(groundTruth))[i]
      gt <- as.integer(groundTruth[[group]])

      out.list <- list(Formula = as.character(form)[3], AIC = aic, Estimate = est, LowerCI = est.lci, UpperCI = est.uci,
                       GroundTruth = gt, Group = group)
      compare.list[[i]] <- out.list
    }
    out <- rbindlist(compare.list)
  } else {
    aic <- sim[[1]]$AIC
    est <- sim[[1]]$estimate
    est.lci <- sim[[1]]$lower_ci
    est.uci <- sim[[1]]$upper_ci
    form <- sim[[1]]$formula
    group <- "base"
    gt <- as.integer(groundTruth[1])

    out <- data.table(Formula = as.character(form)[3], AIC = aic, Estimate = est, LowerCI = est.lci, UpperCI = est.uci,
                      GroundTruth = gt, Group = group)
  }
  return(out)
}
