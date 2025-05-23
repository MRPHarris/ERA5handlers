#' Derive monthly means of ERA5 data.
#'
#' @description Derive monthly means of a given era5 file.
#'
#' @param collated_era5 a set of collated era5 netcdf4 files, returned by collate_era5()
#' @param field NULL or a character string specifying the field to target. e.g., 't2m' for surface temperature.
#' @param total_months TRUE/FALSE to derive the means of individual months across all years, or each month type from all years.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr summarise_at
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @export
#'
monthmeans_era5 <- function(collated_era5, field = NULL, total_months = FALSE){
  if(!isTRUE(total_months)){
    if(!is.null(field)){
      collated_era5 <- collated_era5 %>%
        filter(name = field)
    }
    collated_era5 <- collated_era5 %>%
      group_by(month_seq) %>%
      mutate(month_varmean = mean(value, na.rm = T)) %>%
      mutate(month_varsd = sd(value, na.rm = T))
    monthmeans <- collated_era5 %>%
      group_by(month_seq) %>%
      summarise_at(vars(month_varmean,month_varsd,year), mean) %>%
      'colnames<-'(c('month','month_varmean','month_varsd','year'))
  } else {
    if(!is.null(field)){
      collated_era5 <- collated_era5 %>%
        filter(name = field)
    }
    collated_era5 <- collated_era5 %>%
      group_by(month) %>%
      mutate(month_varmean = mean(value, na.rm = T)) %>%
      mutate(month_varsd = sd(value, na.rm = T))
    monthmeans <- collated_era5 %>%
      group_by(month) %>%
      summarise_at(vars(month_varmean,month_varsd,year), mean) %>%
      'colnames<-'(c('month','month_varmean','month_varsd','year'))
  }
  monthmeans
}
