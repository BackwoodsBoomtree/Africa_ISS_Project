library(terra)

ecos         <- vect("G:/Africa/Tropical_Africa_Ecoregions/eco_regions_of_interest.shp")
evi_max      <- rast("G:/MCD43A4/vis/africa/min_max_monthly/max_monthly_2019-2021_EVI.nc")
lc_mask      <- vect("G:/Africa/Forest_Masks/dissolved/Africa_merged_2019_2.5km_Buffer.shp")
soil_files   <- "G:/ERA5/tropical_africa/era5_tropical_africa_soil.nc"

soil             <- sds(soil_files)$swvl1
soil[soil < 0]   <- NA
soil             <- crop(soil, evi_max)
soil             <- mask(soil, ecos)
soil             <- mask(soil, lc_mask)

# Get mean of all three years
for (j in 1:12) {
  y1_month <- soil[[j]]
  y2_month <- soil[[j + 12]]
  y3_month <- soil[[j + 24]]
  
  # Get mean across all three years
  monthly_mean  <- mean(y1_month, y2_month, y3_month, na.rm = TRUE)
  
  if (j == 1) {
    soil_means <- monthly_mean
  } else {
    soil_means <- c(soil_means, monthly_mean)
  }
}

max_soil <- which.max(soil_means)
min_soil <- which.min(soil_means)

# Min-Max normalize the series
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

soil_norm <- app(soil_means, fun = min_max_norm)


writeCDF(max_soil, "G:/ERA5/min_max/max_monthly_2019-2021_soil.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(min_soil, "G:/ERA5/min_max/min_monthly_2019-2021_soil.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(soil_means, "G:/ERA5/min_max/monthly_2019-2021_soil.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(soil_norm, "G:/ERA5/min_max/monthly_2019-2021_soil_norm.nc",
         missval = -9999, compression = 4, overwrite = TRUE)
