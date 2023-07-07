library(terra)

precip_files <- list.files("G:/CHIRPS/africa_monthly", pattern = "*.tif", full.names = TRUE)

precip <- rast(precip_files)
precip[precip < 0] <- NA

max_precip <- which.max(precip)
min_precip <- which.min(precip)
