library(terra)

ecos         <- vect("G:/Africa/Tropical_Africa_Ecoregions/eco_regions_of_interest.shp")
evi_max      <- rast("G:/MCD43A4/vis/africa/min_max_monthly/max_monthly_2019-2021_EVI.nc")
lc_mask      <- vect("G:/Africa/Forest_Masks/dissolved/Africa_merged_2019_2.5km_Buffer.shp")
sif_files    <- list.files("G:/TROPOMI/esa/gridded/50km", pattern = "*.nc", full.names = TRUE, recursive = TRUE)

# remove 2018
sif_files <- sif_files[2:4]

for (i in 1:length(sif_files)) {
  sif_year <- sds(sif_files[1])$SIF_Corr_743
  
  if (i == 1) {
    sif <- sif_year
  } else {
    sif <- c(sif, sif_year)
  }
}

sif             <- crop(sif, evi_max)
sif             <- mask(sif, ecos)
sif             <- mask(sif, lc_mask)

# Get mean of all three years
for (j in 1:12) {
  y1_month <- sif[[j]]
  y2_month <- sif[[j + 12]]
  y3_month <- sif[[j + 24]]
  
  # Get mean across all three years
  monthly_mean  <- mean(y1_month, y2_month, y3_month, na.rm = TRUE)
  
  if (j == 1) {
    sif_means <- monthly_mean
  } else {
    sif_means <- c(sif_means, monthly_mean)
  }
}

max_sif <- which.max(sif_means)
min_sif <- which.min(sif_means)

# Min-Max normalize the series
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

sif_norm <- app(sif_means, fun = min_max_norm)


writeCDF(max_sif, "G:/TROPOMI/esa/gridded/50km/max_monthly_2019-2021_sif.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(min_sif, "G:/TROPOMI/esa/gridded/50km/min_monthly_2019-2021_sif.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(sif_means, "G:/TROPOMI/esa/gridded/50km/monthly_2019-2021_sif.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(sif_norm, "G:/TROPOMI/esa/gridded/50km/monthly_2019-2021_sif_norm.nc",
         missval = -9999, compression = 4, overwrite = TRUE)
