
This package provides a set of simple handling functions for importing,
parsing, and summarising ERA5 netcdf4 files downloaded from the
copernicus climate data store (CDS). It has been tested using single
levels and pressure levels data, with files downloaded both manually and
through API methods such as
[ecmwfr](https://github.com/bluegreen-labs/ecmwfr).

A simple workflow is outlined below.

1)  Create an account on the Copernicus CDS

-   Navigate to <https://cds.climate.copernicus.eu/cdsapp#!/home> and
    create an account.

2)  Download ERA5 netcdf4 data using manual or API methods

-   ERA5 data can be fetched manually from the CDS using the browser
    interface:
    <https://cds.climate.copernicus.eu/cdsapp#!/search?type=dataset&text=ERA5>.

3)  Name files in a local store

-   Give the files a suitable name that conforms to your chosen file
    naming convention. If you are working with individual field types
    (for example, 2m temperature), but are storing those files alongside
    other file of different field types, files of a common field type
    should be named to reflect this.

4)  Install and load the ERA5handlers package

-   This package and its dependencies can be fetched with the devtools
    package, using `devtools::install_github("MRPHarris/ERA5handlers")`

5)  Collate desired files

-   The `collate_era5()` function imports and parses ERA5 netcdf4 files.
    The package ships with a small netcdf4 file containing 2m
    temperature data from 1999, used in line with the Copernicus
    [license](https://cds.climate.copernicus.eu/api/v2/terms/static/licence-to-use-copernicus-products.pdf).
    The data includes the grid points lat = c(-80.00 -80.25 -80.50) and
    lon = c(-81.50 -81.25 -81.00 -80.75 -80.50), covering a small
    portion of the southern Ellsworth Mountains in Antarctica.

In this case, we use `extract_era5` to read in the single file,
narrowing the coordinates slightly.

``` r
# # Load package
# library(pacman)
# library(ERA5handlers)
# # Specify local data store
# dat_store_era5 <- "D:/DATA/General data/ERA5/"
# # Get all the files in the directory
# era5_fnames <- list.files(dat_store_era5, full.names = TRUE)
# # Get target era5 file
# temp2m_2000_PH <- collate_era5(era5_fnames, string = 'temp2m_1999', coords = c(-80.25, -81.25))
```
