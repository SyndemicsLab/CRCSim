#' Attempts recovery of the ground truth - fundamentally a wrapper for Syndemics::crc
#'
#' @importFrom Syndemics crc
#' @param DT data.table from the \code{create.data} step
#' @param capture list: strings of captures
#' @param group list: strings of stratifications to group by
#' @param ... other options, as passed to Syndemics::crc
#'
#' @export
simulate <- function(DT, capture = c("APCD", "BSAS", "Casemix", "Death", "Matris", "PMP"), group, ...){

  DT <- DT[, tmp := rowSums(.SD), .SDcols = capture
           ][tmp != 0,
             ][, tmp := NULL]

  if(!missing(group)) {
    DT.list <- c()

  }

}
