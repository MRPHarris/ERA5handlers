#' Collate a set of ERA5 files given as a data frame of filenames.
#'
#' @description A fetcher/parser for single netcdf4-formatted ERA5 downloaded from the CDS.
#'
#' @param filenames A data frame containing a single column of filenames.
#' @param string NULL or a single character string used to identify target file types within the filenames.
#' @param coords NULL or a character string containing an individual lat, lon grid position e.g. c(-80.25, -81.25)
#'
#' @importFrom dplyr filter
#' @importFrom rlist list.rbind
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @export
#'
collate_era5 <- function(filenames, string = NULL, coords = NULL){
  if(!is.data.frame(filenames)){
    filenames <- filenames %>% data.frame() %>% 'colnames<-'(c('name'))
  }
  if(!is.null(string)){
    var_fnames <- filenames %>%
      filter(grepl(string, name))
  } else {
    var_fnames <- filenames
  }
  short_names <- trim_path_int(as.character(as.matrix(var_fnames)))
  # List of all the files, parsed to -80.25, -81.25
  var_list <- vector('list', length = nrow(var_fnames))
  for(fv in seq_along(var_list)){
    fname_it <- var_fnames[fv,1]
    var_list[[fv]] <- extract_era5(fname_it, coords = coords)
    message("ERA5 file ",short_names[fv]," added to list. ",fv,"/",length(var_list)," complete.")
  }
  var_data <- list.rbind(var_list) %>%
    mutate(month_seq = cumsum(c(0, as.numeric(diff(month)) != 0)) + 1) %>%
    mutate(day_seq = cumsum(c(0, as.numeric(diff(day)) != 0)) + 1)
  var_data
}
