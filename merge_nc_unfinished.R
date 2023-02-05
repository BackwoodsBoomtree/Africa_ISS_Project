
##### THIS SCRIPT IS UNFINISHED AND MIGHT BE DELETED ###

## This script merges the nc files I downloaded from AppEEARS.
## I used the Africa ecoregions shapefile to extract MCD43A4 data,
## but the shapefile had 37 polygons for 17 ecoregions. So here
## we merge them by ecoregion and rename them.

library(ncdf4)
library(terra)
library(lubridate)
library(stringr)

dir_list <- list.dirs("/mnt/g/MCD43A4/africa", full.names = TRUE, recursive = FALSE)
out_dir  <- "/mnt/g/MCD43A4/africa_combined"

## Get ecoregion names
roi_file  <- "/mnt/g/Africa/Tropical_Africa_Ecoregions/Tropical_Africa_Ecoregions1.shp"
roi_all   <- vect(roi_file)
eco_names <- unique(roi_all$ECO_NAME)
eco_names <- str_to_title(eco_names)
eco_names <- gsub(" ", "_", eco_names)

mergeNC <- function(files) {
  
  t_data <- nc_open(input_file)
  
  # Get spatial and time
  coords           <- cbind(ncvar_get(t_data, "PRODUCT/longitude"), ncvar_get(t_data, "PRODUCT/latitude"))
  colnames(coords) <- c("lon", "lat")
  t                <- basename(input_file)
  t                <- substr(t, 14, 23)
  
  # Get variables and transform to vect for clipping to ROI
  df_var                  <- data.frame(lon = ncvar_get(t_data, "PRODUCT/longitude"))
  df_var$lat              <- ncvar_get(t_data, "PRODUCT/latitude")
  df_var$SIF_Corr_743     <- ncvar_get(t_data, "PRODUCT/SIF_Corr_743")
  df_var$SIF_ERROR_743    <- ncvar_get(t_data, "PRODUCT/SIF_ERROR_743")
  df_var$phase_angle      <- ncvar_get(t_data, "PRODUCT/SUPPORT_DATA/GEOLOCATIONS/phase_angle")
  df_var$cloud_fraction   <- ncvar_get(t_data, "PRODUCT/SUPPORT_DATA/INPUT_DATA/cloud_fraction_L2")
  df_var$LC_MASK          <- ncvar_get(t_data, land_cover_var)
  if (!is.null(land_cover_perc)) {
    df_var$LC_PERC <- ncvar_get(t_data, land_cover_perc)
  }
  nc_close(t_data)
  
}

## Check to make sure the time series data matches the directory name,
## and get list of dates for file names
for (i in 1:1) {
  file_list   <- list.files(dir_list[i], pattern = "*.nc", full.names = TRUE)

  f  <- nc_open(file_list[1])
  ts <- ncvar_get(f, varid = "time")
  t  <- as.Date(ts[1], origin = "2000-01-01")
  t  <- format(t, "%Y_%m")
  nc_close(f)

  if (i == 1) {
    date_list <- t
  } else {
    date_list <- c(date_list, t)
  }

  print(date_list)
}

# If check works lets merge
for (i in 1:1) {
  file_list <- list.files(dir_list[i], pattern = "*.nc", full.names = TRUE)

  out_name  <- paste0("MCD43A4.061_", eco_names[1], "_", date_list, ".nc")
  out_name  <- paste0(out_dir, "/", out_name)

  system(paste0("cdo cat ", file_list[1], " ", file_list[5], " ", out_name))

  # system(paste0("cdo merge ", file_list[1], file_list[1], file_list[1], file_list[1], file_list[1], file_list[1], file_list[1], file_list[1])
}
