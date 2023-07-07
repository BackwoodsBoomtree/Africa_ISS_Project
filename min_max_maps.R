library(terra)

evi_max      <- rast("G:/MCD43A4/vis/africa/min_max_monthly_2020/max_monthly_2020_EVI.nc")
evi_min      <- rast("G:/MCD43A4/vis/africa/min_max_monthly_2020/min_monthly_2020_EVI.nc")
precip_files <- list.files("G:/CHIRPS/africa_monthly", pattern = "*.tif", full.names = TRUE)
ecos         <- vect("G:/Africa/Tropical_Africa_Ecoregions/eco_regions_of_interest.shp")

precip <- rast(precip_files)
precip[precip < 0] <- NA

max_precip <- which.max(precip)
min_precip <- which.min(precip)

max_precip <- crop(max_precip, evi_max)
min_precip <- crop(min_precip, evi_min)

max_precip <- mask(max_precip, ecos)
min_precip <- mask(min_precip, ecos)

plot(max_precip, col = topo.colors(50))
plot(min_precip, col = topo.colors(50))
