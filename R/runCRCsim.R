#' Function to run CRC sim - returns a list of data.tables resulting from \code{analyze}
#'
runCRC <- function(nboot){
  n <- 100
  n_captures <- 6

  even_captures <- future.apply::future_lapply(1:nboot, function(x){
    ?seq
    for(i in 1:10){

    }
  })
}
