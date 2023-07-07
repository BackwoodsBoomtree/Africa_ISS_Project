library(terra)

ecos         <- vect("G:/Africa/Tropical_Africa_Ecoregions/eco_regions_of_interest.shp")
evi_max      <- rast("G:/MCD43A4/vis/africa/min_max_monthly/max_monthly_2019-2021_EVI.nc")
lc_mask      <- vect("G:/Africa/Forest_Masks/dissolved/Africa_merged_2019_2.5km_Buffer.shp")
precip_files <- list.files("G:/CHIRPS/africa_monthly/original", pattern = "*.tif", full.names = TRUE)

precip             <- rast(precip_files)
precip[precip < 0] <- NA
precip             <- crop(precip, evi_max)
precip             <- mask(precip, ecos)
precip             <- mask(precip, lc_mask)

# Get mean of all three years
for (j in 1:12) {
  y1_month <- precip[[j]]
  y2_month <- precip[[j + 12]]
  y3_month <- precip[[j + 24]]
  
  # Get mean across all three years
  monthly_mean  <- mean(y1_month, y2_month, y3_month, na.rm = TRUE)
  
  if (j == 1) {
    precip_means <- monthly_mean
  } else {
    precip_means <- c(precip_means, monthly_mean)
  }
}

max_precip <- which.max(precip_means)
min_precip <- which.min(precip_means)

# Min-Max normalize the series
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

precip_norm <- app(precip_means, fun = min_max_norm)


writeCDF(max_precip, "G:/CHIRPS/africa_monthly/min_max/max_monthly_2019-2021_precip.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(min_precip, "G:/CHIRPS/africa_monthly/min_max/min_monthly_2019-2021_precip.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(precip_means, "G:/CHIRPS/africa_monthly/min_max/monthly_2019-2021_precip.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(precip_norm, "G:/CHIRPS/africa_monthly/min_max/monthly_2019-2021_precip_norm.nc",
         missval = -9999, compression = 4, overwrite = TRUE)
