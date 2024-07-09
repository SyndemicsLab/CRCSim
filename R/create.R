#' Data generation tool
#'
#' @param n int: number of participants to simulate
#' @param correlate bool: attempts correlation between 'captures'
#' @param collapse bool: collapse on groups given
#' @param groups list: strings of additional cols to group on, supports "agegrp", "sex", and "race"
#' @param suppress bool: mimic suppression (recode cells in 1:10 as -1)
#'
#' @import data.table
#' @returns a data.table
#' @export

create.data <- function(n, correlate = FALSE, collapse = TRUE, suppress = FALSE, groups){

  stats <- list()
  for(i in 1:n){
    race <- estimate.race()
    sex <- estimate.sex()
    age <- estimate.age()
    capture <- estimate.captures(correlate = correlate)

    stats[[i]] <- c(race = race, sex = sex, agegrp = age, capture)

  }
  DT <- rbindlist(stats)
  if(!collapse) return(DT)

  cols <- colnames(DT)[!colnames(DT) %in% c("race", "sex", "agegrp")]
  if(!missing(groups)) cols <- c(cols, groups)

  DT <- DT[, .(N_ID = .N), by = cols]
  if(suppress) DT <- DT[, N_ID := ifelse(N_ID %in% 1:10, sample.int(10, 1), N_ID)]

  return(DT)
}

#' Draws race from a sample
#'
#' @param race list: races to sample from
#' @param prob list: sampling probabilities relative to \code{race}
#' @param seed int: seeding
#'
#' @keywords internal
estimate.race <- function(race = c("White", "Black", "Asian/PI", "Hispanic", "Other"),
                          prob = c(.777, .0509, .00629, .0942, .0109)){
  if(length(race) != length(prob)) stop("race and prob have different lengths")
  out <- sample(race, 1, prob, replace = FALSE)
  return(out)
}

#' Draws sex from a sample
#'
#' @param sex list: sexes to sample from
#' @param prob list: sampling probabilities relative to \code{sex}
#' @param seed int: seeding
#'
#' @keywords internal
estimate.sex <- function(sex = c("Male", "Female"),
                         prob = c(.583, .386)){
  if(length(sex) != length(prob)) stop("sex and prob have different lengths")
  out <- sample(sex, 1, prob, replace = FALSE)
  return(out)
}

#' Draws age from a sample of bins
#'
#' @param age list: age bins to sample from
#' @param prob list: probabilities relative to \code{age}
#' @param seed int: seeding
#'
#' @keywords internal
estimate.age <- function(age = c("1_10", "11_20", "21_30", "31_40",
                                 "41_50", "51_60", "61_70", "71_80",
                                 "81_90", "91_100"),
                         prob = c(.00295, .0271, .273,
                                  .274, .194, .154, .0512,
                                  .0129, .00568, .000681)){
  if(length(age) != length(prob)) stop("age and prob have different lengths")
  out <- sample(age, 1, prob, replace = FALSE)
  return(out)
}

#' Draws captures as a series of binomial flips
#'
#' @param capture list: captures to sample from
#' @param prob list: binomial probabilities relative to \code{capture}
#' @param correlate bool: attempts correlation between captures
#' @param corr.matrix matrix: correlation between \code{capture}
#' @param seed int: seeding
#'
#' @importFrom MASS mvrnorm
#'
#' @keywords internal
estimate.captures <- function(capture = c("APCD", "BSAS", "Casemix", "Death", "Matris", "PMP"),
                              prob = c(.4887, .1467, .096, .0108, .0527, .205),
                              correlate = FALSE,
                              corr.matrix = matrix(c(
                                1.00000000, 0.03464933, -0.09139326, -0.06406634, -0.10679959, -0.11561070,
                                0.03464933, 1.00000000,  0.21020500, -0.06738847,  0.33085861,  0.05487056,
                                -0.09139326, 0.21020500,  1.00000000, -0.08669687,  0.09550053, -0.00245505,
                                -0.06406634, -0.06738847, -0.08669687,  1.00000000, -0.08148882, -0.04910034,
                                -0.10679959, 0.33085861,  0.09550053, -0.08148882,  1.00000000,  0.11940396,
                                -0.11561070, 0.05487056, -0.00245505, -0.04910034,  0.11940396,  1.00000000),
                                ncol=6, byrow=TRUE)){
  if(length(capture) != length(prob)) stop("capture and prob have different lengths")
  out <- list()
  if(correlate){
    mv <- MASS::mvrnorm(1, mu = rep(0, length(capture)), Sigma = corr.matrix)
    uni <- pnorm(mv)
    for(i in seq_along(capture)){
      out[[i]] <- rbinom(1, 1, uni[[i]])
    }
  } else {
    for(i in seq_along(capture)){
      out[[i]] <- rbinom(1, 1, prob[[i]])
    }
  }
  names(out) <- capture
  return(out)
}














