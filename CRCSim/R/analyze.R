#' Function to run the analysis
#'
#' @param n int: total number entering the simulation
#' @param correlate bool: attempts to correlate captures (default is analagous to MA PHD data)
#' @param suppress bool: mimic required data suppression (N_ID in 1:10 becomes obfuscated)
#' @param groups string: by-group to estimate
#' #' @param method string: selection for spatial capture recapture model - either "poisson" or "negbin"
#' @param formula.selection string: selection for formula decision - either "aic", "corr", or "stepwise"
#' @param opts.stepwise list: list containing 'direction' of 'forward' 'backward' or 'both', and 'threshold': p value threshold for stepwise selection
#'
#' @import data.table
#' @export

analyze <- function(n, n_captures = 6, n_strata, p_captures, p_strata, suppress,
                    method = "poisson", formula.selection = "stepwise", opts.stepwise = list(direction = "forward",
                                                                                             threshold = 0.5,
                                                                                             verbose = FALSE)){

  DT <- create.data(n, n_captures, n_strata, p_captures, p_strata)
  groundTruth <- extract.groundTruth(DT, capture = paste0("capture_", 1:n_captures), group = "strata")
  model <- simulate(DT, capture = paste0("capture_", 1:n_captures), group = "strata", suppress = suppress,
                    method, formula.selection, opts.stepwise)

  out <- compare(model, groundTruth)
  return(out)
}
