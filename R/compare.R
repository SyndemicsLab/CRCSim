#' Function to compare between simulation results and ground truth
#'
#' @param sim list of simulations done by \code{simulate}
#' @param groundTruth list of ground truths by \code{extract.groundTruth}
#'
#' @import data.table
#' @export

compare <- function(sim, groundTruth) {
  compare.list <- c()
  for(i in seq_along(sim)) {
    aic <- sim[[i]]$AIC
    est <- sim[[i]]$estimate
    est.lci <- sim[[i]]$lower_ci
    est.uci <- sim[[i]]$upper_ci
    gt <- groundTruth[[i]]
    group <- names(groundTruth)[i]

    out.list <- list(aic = aic, estimate = est, lower_ci = est.lci, upper_ci = est.uci,
                     ground = gt, group = group)
    compare.list[[i]] <- out.list
  }
  out <- rbindlist(compare.list)
  return(out)
}
