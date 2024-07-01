#' Function to extract the ground truth from data created through the \code{create.data} method
#'
#' @param DT data.table from \code{create.data}
#' @param capture list: strings of captures
#' @param group list: strings of groups
#'
#' @import data.table
#' @returns list the same length of \code{group}
#'
#' @export
extract.groundTruth <- function(DT,
                                capture = c("APCD", "BSAS", "Casemix", "Death", "Matris", "PMP"),
                                group){
  if(!missing(group)) cols <- c(capture, group) else cols <- capture

  DT <- DT[, tmp := rowSums(.SD), .SDcols = cols
           ][tmp == 0,
             ][, tmp := NULL]

  out <- as.list(DT$N_ID)
  if(!missing(group)) names(out) <- group else names(out) <- "base"

  return(out)
}
