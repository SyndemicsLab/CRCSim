#' Function to run the analysis
#'
#' @param n int: total number entering the simulation
#' @param p_captures list: list of capture probabilities to emulate
#' @param p_strata list: list of extra 'stratification' probabilities to emulate
#' @param suppress int: supression cap, defaults to 10
#' @param method string: selection for spatial capture recapture model - either "poisson" or "negbin"
#' @param formula.selection string: selection for formula decision - either "aic", "corr", or "stepwise"
#' @param opts.stepwise list: list containing 'direction' of 'forward' 'backward' or 'both', and 'threshold': p value threshold for stepwise selection
#'
#' @import data.table
#' @keywords internal

analyze <- function(n, p_captures, p_strata, suppress,
                    method = "poisson", formula.selection = "stepwise", opts.stepwise = list(direction = "forward",
                                                                                             threshold = 0.5,
                                                                                             verbose = FALSE)){
  n_captures <- length(p_captures)
  n_strata <- length(p_strata)

  DT <- create.data(n, n_captures, n_strata, p_captures, p_strata)
  groundTruth <- extract.groundTruth(DT, capture = paste0("capture_", 1:n_captures), group = "strata")
  model <- simulate(DT, capture = paste0("capture_", 1:n_captures), group = "strata", suppress = suppress,
                    method, formula.selection, opts.stepwise)

  rm(DT)
  gc()

  out <- compare(model, groundTruth)
  return(out)
}
