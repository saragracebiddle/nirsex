#' Create data.frame of monoexponential y values at time x based on coefficients
#'
#' @param Amp amplitude
#' @param Baseline baseline y
#' @param TD time delay
#' @param tau time constant
#' @param xlen number of output x values
#'
#' @return data.frame
#' @export
monoexp <- function(Amp, Baseline, TD, tau, xlen){
  data.frame(
    x = seq_len(xlen),
    y = Baseline + Amp*( 1 - exp( -(seq_len(xlen) - TD) / tau))
  )
}


posNls <- function(){
  purrr::possibly(nls, otherwise  = NA)
}
