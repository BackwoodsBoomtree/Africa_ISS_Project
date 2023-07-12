library(terra)
library(viridis)

ecos         <- vect("G:/Africa/Tropical_Africa_Ecoregions/eco_regions_of_interest.shp")
evi_max      <- rast("G:/MCD43A4/vis/africa/min_max_monthly/max_monthly_2019-2021_EVI.nc")
lc_mask      <- vect("G:/Africa/Forest_Masks/dissolved/Africa_merged_2019_2.5km_Buffer.shp")
topo_files   <- list.files("G:/Topography/africa/median", pattern = "*.tif", full.names = TRUE)

for (i in 1:length(topo_files)) {
  tile <- rast(topo_files[i])
  
  if (i == 1) {
    topo <- tile
  } else {
    topo <- mosaic(topo, tile, fun = "max")
  }
}

topo <- crop(topo, evi_max)

topo <- resample(topo, evi_max, method = "bilinear")

topo <- mask(topo, evi_max)

test <- na.omit(as.vector(topo))
test2 <- na.omit(as.vector(evi_max))

vs <- cor(test, test2, method = "spearman")

# for visualization
topo[topo > 1000] <- 1000

plot(topo, col = rev(viridis(10)))
