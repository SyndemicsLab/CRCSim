#' Function to run the analysis
#'
#' @param n int: total number entering the simulation
#' @param correlate bool: attempts to correlate captures (default is analagous to MA PHD data)
#' @param suppress bool: mimic required data suppression (N_ID in 1:10 becomes obfuscated)
#' @param groups string: by-group to estimate
#'
#' @import data.table
#' @export

analyze <- function(n, correlate, suppress, groups){
  if(!missing(groups)){
    DT <- create.data(n, seed, correlate, collapse = TRUE, suppress, groups)
    groundTruth <- extract.groundTruth(DT, groups = groups)
    model <- simulate(DT, groups = groups)
  } else {
    DT <- create.data(n, seed, correlate, collapse = TRUE, suppress)
    groundTruth <- extract.groundTruth(DT)
    model <- simulate(DT)
  }
  out <- compare(model, groundTruth)
  return(out)
}
