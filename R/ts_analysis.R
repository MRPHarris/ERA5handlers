# Functions for time series analysis

#' Remove the seasonal cycle from a time series
#'
#' @description Remove annual-scale seasonal cycle using daily, weekly or monthly-resolution data. Calculates a per-point (day-in-year, week-in-year, or month-in-year) mean and subtracts it from the time series.
#'
#' @param x time in date-coercible format.
#' @param y data at matching intervals to x.
#' @param resolution one of either 'daily', 'weekly', or 'monthly' corresponding to the temporal resolution of x and y.
#' @param type one of either 'mean' or 'sum' for calculating the seasonal cycle.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom lubridate yday
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr left_join
#' @importFrom lubridate isoweek
#' @importFrom lubridate month
#'
#' @export
#'
remove_seasonal_cycle <- function(x, y, resolution = 'daily', type = 'mean'){
  ## test vars
  # x = PH_ERA5_daily_data$timestamp
  # y = PH_ERA5_daily_data$totwind_ms
  # type = 'mean'
  # resolution = 'daily'
  ##
  frame <- data.frame(x,y)
  if(resolution == 'daily'){
    frame <- frame %>%
      mutate(x_ydays = yday(as_date(x)))
    szn <- frame %>%
      group_by(x_ydays) %>%
      summarise(dayval = ifelse(type == 'mean',
                                yes = mean(y),
                                no = ifelse(type == 'sum',
                                            yes = sum(y),
                                            no = NA)))
    frame_dszn <- frame %>%
      left_join(szn, by = 'x_ydays') %>%
      mutate(y_dszn = y - dayval)
  } else if(resolution == 'weekly'){
    frame <- frame %>%
      mutate(x_yweeks = isoweek(as_date(x)))
    szn <- frame %>%
      group_by(x_yweeks) %>%
      summarise(weekval = ifelse(type == 'mean',
                                 yes = mean(y),
                                 no = ifelse(type == 'sum',
                                             yes = sum(y),
                                             no = NA)))
    frame_dszn <- frame %>%
      left_join(szn, by = 'x_yweeks') %>%
      mutate(y_dszn = y - weekval)
  } else if(resolution == 'monthly'){
    frame <- frame %>%
      mutate(x_month = month(as_date(x)))
    szn <- frame %>%
      group_by(x_month) %>%
      summarise(monthval = ifelse(type == 'mean',
                                  yes = mean(y),
                                  no = ifelse(type == 'sum',
                                              yes = sum(y),
                                              no = NA)))
    frame_dszn <- frame %>%
      left_join(szn, by = 'x_month') %>%
      mutate(y_dszn = y - monthval)
  } else {
    stop('resolution param not recognised.')
  }
  return(frame_dszn$y_dszn)
}
