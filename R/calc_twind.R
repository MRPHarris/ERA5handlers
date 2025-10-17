#' Calculate total wind speed from zonal and meridional components
#'
#' @description Obtain total wind speed using the zonal (u) and meridonal (v) wind components.
#'
#' @param uwind numeric; the zonal wind component
#' @param vwind numeric; the meridional wind component
#'
#' @export
#'
calc_twind <- function(uwind,vwind){
  twind <- sqrt(uwind^2 + vwind^2)
  twind
}
