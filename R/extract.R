#' Function to extract the ground truth from data created through the \code{create.data} method
#'
#' @param DT data.table from \code{create.data}
#' @param capture list: strings of captures
#' @param groups list: strings of groups
#'
#' @import data.table
#' @returns list the same length of \code{group}
#'
#' @export

extract.groundTruth <- function(DT,
                                capture = c("APCD", "BSAS", "Casemix", "Death", "Matris", "PMP"),
                                groups){

  DT <- DT[, tmp := rowSums(.SD), .SDcols = capture
           ][tmp == 0,
             ][, tmp := NULL]

  out <- as.list(DT[["N_ID"]])
  names <- as.list(DT[[group]])
  if(!missing(groups)) names(out) <- names else names(out) <- "base"

  return(out)
}
