#' Function to run the analysis
#'
#' @param n int: total number entering the simulation
#' @param correlate bool: attempts to correlate captures (default is analagous to MA PHD data)
#' @param suppress bool: mimic required data suppression (N_ID in 1:10 becomes obfuscated)
#' @param groups string: by-group to estimate
#'
#' @import data.table
#' @export

analyze <- function(n, n_captures = 6, n_strata, p_captures, p_strata, suppress){

  DT <- create.data(n, n_captures, n_strata, p_captures, p_strata)
  groundTruth <- extract.groundTruth(DT, capture = paste0("capture_", 1:n_captures), group = "strata")
  model <- simulate(DT, capture = paste0("capture_", 1:n_captures), group = "strata", suppress = suppress)

  out <- compare(model, groundTruth)
  return(out)
}
