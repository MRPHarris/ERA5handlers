
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

5)  Import desired files into R

-   The `collate_era5()` function imports and parses ERA5 netcdf4 files.
    The package ships with a small netcdf4 file containing 2m
    temperature data from 1999, used in line with the Copernicus
    [license](https://cds.climate.copernicus.eu/api/v2/terms/static/licence-to-use-copernicus-products.pdf).
    The data includes the grid points lat = c(-80.00 -80.25 -80.50) and
    lon = c(-81.50 -81.25 -81.00 -80.75 -80.50), covering a small
    portion of the southern Ellsworth Mountains in Antarctica.

In this case, we use `collate_era5()` to read in the single file, and
narrow the coordinates to a single grid point.

``` r
# Load package
library(ERA5handlers)
# Specify local data store
dat_store_era5 <- "D:/DATA/General data/ERA5/"
# Get filenames in store
era5_fnames <- list.files(dat_store_era5, full.names = TRUE)
# Get target era5 file
temp2m_1999_PH <- collate_era5(era5_fnames, string = 'temp2m_1999', coords = c(-80.25, -81.25))
head(temp2m_1999_PH)
```

    #> # A tibble: 6 x 12
    #>   value   lon   lat timestamp           name  coord       year month   day  hour
    #>   <dbl> <dbl> <dbl> <dttm>              <fct> <fct>      <dbl> <dbl> <dbl> <dbl>
    #> 1  258. -81.2 -80.2 1999-01-01 00:00:00 t2m   -81.25 -8~  1999     1     1     0
    #> 2  257. -81.2 -80.2 1999-01-01 01:00:00 t2m   -81.25 -8~  1999     1     1     1
    #> 3  257. -81.2 -80.2 1999-01-01 02:00:00 t2m   -81.25 -8~  1999     1     1     2
    #> 4  256. -81.2 -80.2 1999-01-01 03:00:00 t2m   -81.25 -8~  1999     1     1     3
    #> 5  255. -81.2 -80.2 1999-01-01 04:00:00 t2m   -81.25 -8~  1999     1     1     4
    #> 6  255. -81.2 -80.2 1999-01-01 05:00:00 t2m   -81.25 -8~  1999     1     1     5
    #> # ... with 2 more variables: month_seq <dbl>, day_seq <dbl>

Imported data can now be analysed freely. A simple monthly value
aggregator is provided in `monthmeans_era5()`, which will determine the
monthly mean or standard deviation for the value in a given given field
within an ERA5 file. Months can be extracted from a single year, or
across multiple years.

``` r
# Derive month means. Single year.
PH_temp_monthmeans <- monthmeans_era5(temp2m_1999_PH, total_months = TRUE) %>%
  mutate(month_varmean = month_varmean - 273.15) # K to C conversion
# Plot the monthly temperature means at this grid point for 1999, with standard devations
sd_polygon_avtemp <- data.frame(x = c(rep(PH_temp_monthmeans$month), 
                                      rev(rep(PH_temp_monthmeans$month))), 
                                y = c(PH_temp_monthmeans$month_varmean + PH_temp_monthmeans$month_varsd, 
                                      rev(PH_temp_monthmeans$month_varmean - PH_temp_monthmeans$month_varsd)))
month_labs <- c("J","F","M","A","M","J","J","A","S","O","N","D")
ggplot() +
  # SD polygon
  geom_polygon(data = sd_polygon_avtemp, aes(x = x, y = y), fill = "grey60", colour = "NA", alpha = 0.2) +
  # Avlat
  geom_line(data = PH_temp_monthmeans, aes(x = month, y = month_varmean)) +
  geom_point(data = PH_temp_monthmeans, aes(x = month, y = month_varmean)) +
  # SD: upper
  geom_line(data = PH_temp_monthmeans, aes(x = month, y = month_varmean + month_varsd), linetype = 'dashed') +
  geom_point(data = PH_temp_monthmeans, aes(x = month, y = month_varmean + month_varsd), shape = 4) +
  # SD: lower
  geom_line(data = PH_temp_monthmeans, aes(x = month, y = month_varmean - month_varsd), linetype = 'dashed') +
  geom_point(data = PH_temp_monthmeans, aes(x = month, y = month_varmean - month_varsd), shape = 4) +
  # themes etc.
  scale_x_continuous(breaks = seq(1,12,1), labels = month_labs, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(-40,-5), breaks = seq(-40,-5,5)) +
  labs(x = "Month", y = "Temp in C") +
  ggtitle('ERA5 mean monthly temperature during 1999 for 80.25S 81.25W') +
  theme_cowplot(12)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
