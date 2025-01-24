#' Fetch a single ERA5 file. Option to extract specific coordinates.
#'
#' @description A fetcher/parser for single netcdf4-formatted ERA5 downloaded from the CDS.
#'
#' @param nc_filepath full file path to the target netcdf4 era5 file.
#' @param coords a character string containing an individual lat, lon grid position e.g. c(-80.25, -81.25)
#'
#' @importFrom ncdf4 ncvar_get
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 ncatt_get
#' @importFrom ncdf4 nc_close
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_cols
#' @importFrom dplyr pull
#' @importFrom magrittr %>%
#'
#' @export
#'
extract_era5 <- function(nc_filepath, coords = NULL){
  ## nc handling heavily adapted from https://renewable-analytics.netlify.app/2018/06/25/using-era-interim-in-r/#open-the-file
  # Note: late 2024 updates to CDS and ERA5 have changed the GRIB to ncdf4 conversion, leading to altered netcdf file format.
  nc_file <- nc_open(nc_filepath)
  # What's the format?
  file_format = nc_file$format
  if(file_format == "NC_FORMAT_NETCDF4"){
    # Post-2024 ERA5 netcdf4 file format
    # Extract coordinate information
    lat = ncvar_get(nc_file,'latitude')
    lon = ncvar_get(nc_file,'longitude')
    # time handling
    t <- ncvar_get(nc_file, "valid_time") # time changed to 'valid_time'
    tunits <- ncatt_get(nc_file, "valid_time")
    tustr <- strsplit(tunits$units, " ")#[[1]][3]# %>% lapply(.,'[',3)
    # init_date = as.Date(tustr[[1]][3])
    time_steps = tustr[[1]][1]
    if(time_steps == 'hours'){
      timestamp = as.POSIXct(t * 3600, tz = "GMT", origin = tustr[[1]][3]) # tustr[[1]][3] is the init date
    } else if(time_steps == 'seconds'){
      timestamp = as.POSIXct(t, tz = "GMT", origin = tustr[[1]][3])
    }
    # VAR now has three elements not one.
    # This code now assumes you're after not the number and expver vars.
    data <- tibble(name = attributes(nc_file$var)$names[which(attributes(nc_file$var)$names != "number" & attributes(nc_file$var)$names != "expver")]) %>%
      bind_cols(purrr::map_df(.$name, ncatt_get, nc = nc_file)) %>%
      mutate(values = purrr::map(name, ncvar_get, nc = nc_file))
  } else {
    # Pre-late 2024 ERA5 netcdf4 file format
    # Extract coordinate information
    lat = ncvar_get(nc_file,'latitude')
    lon = ncvar_get(nc_file,'longitude')
    # time handling
    t <- ncvar_get(nc_file, "time")
    tunits <- ncatt_get(nc_file,'time')
    tustr <- strsplit(tunits$units, " ")
    timestamp = as.POSIXct(t * 3600, tz = 'GMT', origin = tustr[[1]][3])
    # Basic parsing of NC data. Frame with all attributes.
    data<- tibble(name = attributes(nc_file$var)$names) %>%
      bind_cols(purrr::map_df(.$name, ncatt_get, nc = nc_file)) %>% # Get basic names
      mutate(values = purrr::map(name, ncvar_get, nc = nc_file)) # Acquire values

  }
  nc_close(nc_file)
  # pre-allocate DF for desired grid square
  df_int <-expand.grid(lon = lon, lat = lat, timestamp = timestamp, name = data$name) %>%
    mutate(coord = factor(paste(lon,lat,'/')))
  # Pull data.
  df <- data %>%
    pull(values) %>%
    unlist() %>%
    as_tibble() %>%
    bind_cols(df_int)
  # Optional coord selection
  if(!is.null(coords)){
    df <- df %>%
      filter(lat == coords[1]) %>%
      filter(lon == coords[2])
  }
  # single-var y,m,d,h mutations
  df <- df %>%
    mutate(year = as.numeric(format(df$timestamp, "%Y"))) %>%
    mutate(month = as.numeric(format(df$timestamp, "%m"))) %>%
    mutate(day = as.numeric(format(df$timestamp, "%d"))) %>%
    mutate(hour = as.numeric(format(df$timestamp, "%H")))
  return(df)
}
