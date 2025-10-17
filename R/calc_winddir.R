#' Calculate wind direction from zonal and meridional components
#'
#' @description Obtain wind direction in degrees using the zonal (u) and meridonal (v) wind components.
#'    See https://confluence.ecmwf.int/pages/viewpage.action?pageId=133262398 or equations in e.g., https://doi.org/10.3390/geosciences9070289
#'
#' @param uwind numeric; the zonal wind component
#' @param vwind numeric; the meridional wind component
#'
#' @export
#'
calc_winddir <- function(uwind,vwind){
  winddir <- (atan2(-uwind,-vwind) * 180/pi) %% 360
  winddir
}
