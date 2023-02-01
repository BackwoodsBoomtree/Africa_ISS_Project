
## This script merges the nc files I downloaded from AppEEARS.
## I used the Africa ecoregions shapefile to extract MCD43A4 data,
## but the shapefile had 37 polygons for 17 ecoregions. So here
## we merge them by ecoregion and rename them.

library(ncdf4)
library(lubridate)

dir_list     <- list.dirs("G:/MCD43A4/africa", full.names = TRUE, recursive = FALSE)

# Check to make sure the time series data matches the directory name
for (i in 1:length(dir_list)) {
  file_list   <- list.files(dir_list[i], pattern = "*.nc", full.names = TRUE)
  
  f  <- nc_open(file_list[1])
  ts <- ncvar_get(f, varid = "time")
  t  <- as.Date(ts[1], origin = "2000-01-01")
  nc_close(f)

  print(t)
}

# If check works lets merge
for (i in 1:length(dir_list)) {
  file_list   <- list.files(dir_list[i], pattern = "*.nc", full.names = TRUE)
  
  f  <- nc_open(file_list[1])
  ts <- ncvar_get(f, varid = "time")
  t  <- as.Date(ts[1], origin = "2000-01-01")
  nc_close(f)
  
  print(t)
}

# system("cdo")
