library(terra)

ecos         <- vect("G:/Africa/Tropical_Africa_Ecoregions/eco_regions_of_interest.shp")
evi_max      <- rast("G:/MCD43A4/vis/africa/min_max_monthly/max_monthly_2019-2021_EVI.nc")
lc_mask      <- vect("G:/Africa/Forest_Masks/dissolved/Africa_merged_2019_2.5km_Buffer.shp")
vpd_files    <- list.files("G:/ERA5/tropical_africa_ecoregions", pattern = "*.nc", full.names = TRUE, recursive = TRUE)

# Combine ecoregions
for (i in 1:length(vpd_files)) {
  eco_vpd <- sds(vpd_files[i])$vpd
  
  if (i ==1) {
    vpd <- eco_vpd
  } else {
    vpd <- mosaic(vpd, eco_vpd, fun = "max")
  }
}

# remove 2022
vpd <- vpd[[1:36]]

# Get mean of all three years
for (j in 1:12) {
  y1_month <- vpd[[j]]
  y2_month <- vpd[[j + 12]]
  y3_month <- vpd[[j + 24]]
  
  # Get mean across all three years
  monthly_mean  <- mean(y1_month, y2_month, y3_month, na.rm = TRUE)
  
  if (j == 1) {
    vpd_means <- monthly_mean
  } else {
    vpd_means <- c(vpd_means, monthly_mean)
  }
}

max_vpd <- which.max(vpd_means)
min_vpd <- which.min(vpd_means)

# Min-Max normalize the series
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

vpd_norm <- app(vpd_means, fun = min_max_norm)


writeCDF(max_vpd, "G:/ERA5/min_max/max_monthly_2019-2021_vpd.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(min_vpd, "G:/ERA5/min_max/min_monthly_2019-2021_vpd.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(vpd_means, "G:/ERA5/min_max/monthly_2019-2021_vpd.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(vpd_norm, "G:/ERA5/min_max/monthly_2019-2021_vpd_norm.nc",
         missval = -9999, compression = 4, overwrite = TRUE)
