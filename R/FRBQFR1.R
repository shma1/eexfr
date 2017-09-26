
#' French base quarter front month trade signal
#'
#' @param frq an xts object. this should be the day closing prices of the front
#'   quarter contract of the french base quarter
#'   contract.https://www.eex.com/en/market-data/power/futures/french-futures#!/2017/09/26
#'
#'
#' @param gas an xts object. this should be the day closing prices of the front
#'   quarter contract of the gasoil  month contract.LOW SULPHUR GASOIL FUTURES
#'   https://www.theice.com/products/34361119/Low-Sulphur-Gasoil-Futures/specs
#'
#' @return 1 or -1 if the signal is buy or sell resp.
#' @export
#'
#' @import tidyverse
#' @import xts
#' @import lubridate
#' @import ranger
#' @import TTR
#' @importFrom magrittr "%>%"
#'
#' @examples
#' frbqfr1_model (frq, gas)
frbqfr1_model <- function (frq=p,gas=p1){
  source("functions.R")
  load("~/Documents/Research/FR/package/EEXFR/.RData", verbose = F)
  p1 <- tecA2(p1,"gas")
  p <- tecA2(p,"frq")
  p <- dplyr::left_join(p,p1,by="Day")
  p <- tail(p,10)


  p$R23 <- stats::predict(model23,p)

  if (tail(p$R23,1)==1) {
    print("EEXBFRQ1 will close higher the next trading day")
    return(1)
  } else {
    print("EEXBFRQ1 will close lower the next trading day")
    return(-1)
  }

}
