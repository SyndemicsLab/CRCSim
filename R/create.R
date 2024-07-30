#' Data generation tool
#'
#' @param n int: number of participants to simulate
#' @param correlate bool: attempts correlation between 'captures'
#' @param collapse bool: collapse on groups given
#' @param groups list: strings of additional cols to group on, supports "agegrp", "sex", and "race"
#'
#' @import data.table
#' @returns a data.table
#' @export

create.data <- function(n_individuals, n_captures, n_strata, p_captures, p_strata){
  out <- future.apply::future_lapply(1:n_individuals, function(x){
    captures <- create.capture(n_captures, p_captures)
    strata <- create.strata(n_strata, p_strata)
    out <- data.table(t(captures), strata = strata)

    return(out)
  }, future.seed = 2024)

  DT <- data.table::rbindlist(out)
  DT <- DT[, list(N_ID = .N), by = c(names(DT))]
  return(DT)
}

#' A function to sample from 1:n along probability \code{prob}
#'
#' @param n number of 'strata' to simulate
#' @param prob probabilities of n - expects summation to 1
#'
#' @keywords internal

create.strata <- function(n, prob) {
  if(length(prob) != n) stop("Probability length differs from n")
  if(sum(prob) != 1) warning("Probability does not sum to 1")

  out <- sample(1:n, 1, prob = prob)

  return(out)
}

#' A function to generate binomial flips along the probability specified
#'
#' @param n number of 'captures' to simulate
#' @param prob list of probabilities respective to each \code{n}
#'
#' @importFrom stats rbinom
#' @keywords internal
create.capture <- function(n, prob){
  if(length(prob) != n) stop("Probability length differs from n")

  out <- vector(length = n)
  for(i in seq_along(1:n)){
    out[i] <- rbinom(1, 1, prob[[i]])
  }
  names(out) <- paste0("capture_", 1:n)

  return(out)
}











