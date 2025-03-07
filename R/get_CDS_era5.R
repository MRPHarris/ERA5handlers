#' Fetch ERA-5 nc files
#'
#' @description Use ecmwfr to interface with the Copernicus API and obtain ERA-5 netcdf4 files for use by `accumulate()`
#'
#' @param key Copernicus CDS API key. Register with the ECMWF then go to https://cds.climate.copernicus.eu/profile.
#' @param user email address associated with the above API key.
#' @param coords a vector of latitude and longitude for a bounding box to use when fetching. Refer to the web request form. Order is c(N,W,S,E).
#' @param by individual request size. Only "month" supported currently, as it is recommended for being the most efficient.
#' @param variables strings denoting variables to get. Refer to [ecmwfr::wf_request()].
#' @param start_YYYYMM A string for starting year-month in the format "YYYYMM"
#' @param end_YYYYMM A string for ending year-month in the format "YYYYMM"
#' @param identifier A string to identify the files. E.g., project/site name, variables, etc. Files will always start with "ERA5-".
#' @param download_directory a filepath to the folder where files will be downloaded to.
#' @param archive TRUE/FALSE to archive existing files within the supplied download_directory
#'
#' @importFrom ecmwfr wf_set_key
#' @importFrom stringr str_pad
#' @importFrom ecmwfr wf_request
#'
#' @export
#'
get_CDS_era5 <- function(key,
                         user,
                         coords = c(-79.25, -161.75, -79.5, -161.5),
                         by = "month",
                         variables = c("2m_temperature","total_precipitation"),
                         start_YYYYMM = "197901",
                         end_YYYYMM = "202412",
                         identifier = "SITE",
                         download_directory,
                         archive = TRUE){
  ## Archiving
  if(archive){
    archive_folder(dir = download_directory, archive_dir = paste0(download_directory,"archive/"))
  }
  ## Set key
  wf_set_key(key, user)
  ## Convert dates
  start_date = as.Date(paste0(start_YYYYMM,"01"), format = "%Y%m%d")
  edays = as.character(days_in_month(as.numeric(substr(end_YYYYMM,5,6))))
  end_date = as.Date(paste0(end_YYYYMM,edays), format = "%Y%m%d")
  ## Form requests in loop prior to execution
  if(by == "month"){
    # message("Formatting monthly requests between ",start_date," and ",end_date)
    dateseq <- seq(start_date, end_date, by = "1 month")
    reqs <- vector('list',length(dateseq)) %>% 'names<-'(c(paste0(year(dateseq),"-",unlist(lapply(str_split(dateseq,"-"),"[[",2)))))
    for(n in seq_along(reqs)){
      yr = as.character(year(dateseq[n]))
      mth = str_pad(month(dateseq[n]),2,'left',0)
      day_seq = str_pad(seq(1,days_in_month(month(dateseq[n])),1),2,'left',0)
      reqs[[n]] <- request_month(year = yr,
                                 month = mth,
                                 vars = variables,
                                 destination = paste0("ERA5-",identifier,"-",yr,"-",mth),
                                 days_seq = day_seq,
                                 coordinates = coords)
    }
  } else if(by == "year"){
    message("Go for months instead :)")
  }
  ## Now execute the requests
  for(r in seq_along(reqs)){
    wf_request(request  = reqs[[r]],  # the request
               transfer = TRUE,     # download the file
               path     = download_directory,
               user = user,
               time_out = 3600*3)
    zipcheck(filename = reqs[[r]]$target,
             directory = download_directory)
  }
}

#' Create monthly request for ECMWFR
#'
#' @description Create a list in the format used by ecmwfr for the Copernicus CDS API, for a specific month.
#'
#' @param year A year
#' @param month A month as a zero-padded string, e.g. "01" for january
#' @param destination A full filepath to the target for download.
#' @param days_seq A sequence of days in the month, zero-padded as with month.
#' @param coordinates A set of lat/lon coordinates for the DL box, in the order c(N,W,S,E)
#'
#' @noRd
#'
request_month <- function(year,
                          month,
                          destination,
                          vars = c("2m temperature","total_precipitation"),
                          days_seq,
                          coordinates){
  req <- list(
    product_type = "reanalysis",
    variable = vars,
    year = year,
    month = month,
    day = days_seq,
    time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
    area = coordinates,
    data_format = "netcdf",
    download_format =  'unarchived',
    dataset_short_name = "reanalysis-era5-single-levels",
    target = paste0(destination,".nc")
  )
  req
}

#' Archive existing files in the target directory
#'
#' @description Creates a folder within the specified archive directory, and moves all non-default files currently in the
#'    cluster endpt directory to that folder.
#'
#' @param dir Full file path to the target directory.
#' @param archive_dir Full file path to the desired archive directory.
#' @param archive_name character vector to be added to the folder containing the archived endpoints.
#' @param stopifempty TRUE or FALSE to stop the operation if there are no endpoints to archive.
#' @param append_timestamp TRUE or FALSE to add a timestamp of the archive operation.
#'
#' @noRd
#'
archive_folder <- function(dir,
                           archive_dir,
                           archive_name = "ERA5",
                           stopifempty = FALSE,
                           append_timestamp = TRUE){
  # First, list all files in the endpts folder.
  filelist <- list.files(path = dir, full.names = TRUE)
  # Files to keep.
  keep_files <- substr(c(archive_dir),1,nchar(archive_dir)-1)
  filelist <- filelist[-match(keep_files,filelist)] # remove the files to keep from the file list.
  # Are there endpoints to archive?
  if(length(filelist) == 0){
    # No.
    if(isTRUE(stopifempty)){
      stop("No files in ",dir," to archive.")
    } else {
      message("No files in ",dir," to archive.")
      # pass_stop = FALSE
    }
  } else if(length(filelist) != 0){
    # Yes: proceed.
    # Folder renaming.
    if(!is.null(append_timestamp)){
      hour <- substr(Sys.time(),12,13) # get current time
      min <- substr(Sys.time(),15,16)
      sec <- substr(Sys.time(),18,19)
      archive_folder_name <- paste0("archive-",archive_name,"-date-",Sys.Date(),"-hms-",hour,"-",min,"-",sec)
    } else {
      archive_folder_name <- paste0("archive-",archive_name)
    }
    dir.create(path = paste0(archive_dir,archive_folder_name)) # create archive folder
    file.copy(filelist,paste0(archive_dir,archive_folder_name)) # move all files in endpts_filelist to that directory.
    file.remove(filelist) # remove files from endpts directory
    if(!file.exists(filelist[1])){
      message("Archiving successful. Endpoint files in ",dir," archived to ",paste0(archive_dir,archive_folder_name))
    } else{
      message("Oops! There may have been an issue with the archiving process. Check directory paths and file status.")
    }
  }# check that there are actually endpoint files present. If not, archive them.
}


#' Unzip netcdf files and move them into the parent directory
#'
#' @description Hunts for a filename and, if the filename has a .zip extension, unzips and renames the files. Cleans up afterwards.
#'
#' @param filename a filename without path.
#' @param directory path for the above filename.
#' @param verbose TRUE/FALSE to print message when complete
#'
#' @noRd
#'
zipcheck <- function(filename,
                     directory,
                     verbose = T){
  # Check file
  fname <- unlist(lapply(strsplit(filename,"[.]"),"[[",1))
  # Check if it exists
  fs <- list.files(directory)
  fs_long <- list.files(directory, full.names = T)
  fs_it <- fs_long[which(grepl(fname,fs))]
  if(return_extn(fs_it) == 'zip'){
    dir.create(paste0(directory,"zip-in-progress/"))
    unzip(fs_it,
          exdir = paste0(directory,"zip-in-progress/"))
    # new files
    zfs <- list.files(paste0(directory,"zip-in-progress/"))
    zfs_long <- list.files(paste0(directory,"zip-in-progress/"), full.names = T)
    to_files = paste0(fname,"-f",seq(1,length(zfs),1),".nc")
    to_files_long = paste0(get_path(zfs_long),to_files)
    file.rename(zfs_long,to_files_long)
    file.copy(to_files_long, paste0(directory,to_files))
    file.remove(fs_it)
    if(all(file.exists(to_files_long))){
      if(verbose){message("Multiple files unzipped and renamed: \n",paste0(to_files,"\n"))}
    }
    unlink(paste0(directory,"zip-in-progress/"), force = T, recursive = T)
  } else if(return_extn(fs_it) == 'nc'){
    # Nothing to do!
  }
}
