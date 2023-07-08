library(terra)
library(viridis)

sif_max      <- rast("G:/TROPOMI/esa/gridded/50km/max_monthly_2019-2021_sif.nc")
sif_min      <- rast("G:/TROPOMI/esa/gridded/50km/min_monthly_2019-2021_sif.nc")
evi_max      <- rast("G:/MCD43A4/vis/africa/min_max_monthly/max_monthly_2019-2021_EVI.nc")
evi_min      <- rast("G:/MCD43A4/vis/africa/min_max_monthly/min_monthly_2019-2021_EVI.nc")
precip_max   <- rast("G:/CHIRPS/africa_monthly/min_max/max_monthly_2019-2021_precip.nc")
precip_min   <- rast("G:/CHIRPS/africa_monthly/min_max/min_monthly_2019-2021_precip.nc")
vpd_max      <- rast("G:/ERA5/min_max/max_monthly_2019-2021_vpd.nc")
vpd_min      <- rast("G:/ERA5/min_max/min_monthly_2019-2021_vpd.nc")
sm_max       <- rast("G:/ERA5/min_max/max_monthly_2019-2021_soil.nc")
sm_min       <- rast("G:/ERA5/min_max/min_monthly_2019-2021_soil.nc")
coastlines   <- vect("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")


cairo_pdf("G:/Africa/figs/min_max_maps.pdf", width = 7.5, height = 7.5)

par(mfrow = c(5, 2), oma=c(0,0,1,0))


    plot(sif_max, main = "Maximum SIF", col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(sif_max, col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()

    plot(sif_min, main = "Minimum SIF", col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(sif_min, col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()

    plot(evi_max, main = "Maximum EVI", col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(evi_max, col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
    
    plot(evi_min, main = "Minimum EVI", col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(evi_min, col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()

    plot(precip_max, main = "Maximum Precipitation", col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(precip_max, col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
    
    plot(precip_min, main = "Minimum Precipitation", col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(precip_min, col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
    
    plot(sm_max, main = "Maximum Volumetric Soil Water", col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(sm_max, col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
    
    plot(sm_min, main = "Minimum Volumetric Soil Water", col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(sm_min, col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
    
    plot(vpd_max, main = "Maximum VPD", col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(vpd_max, col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
    
    plot(vpd_min, main = "Minimum VPD", col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(vpd_min, col = viridis(10), range = c(0,12),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()



mtext("SIF", side = 3, outer = TRUE, line = -1)
mtext("EVI", side = 3, outer = TRUE, line = -12)
mtext("Precipitation", side = 3, outer = TRUE, line = -23.25)
mtext("Volumetric Soil Water", side = 3, outer = TRUE, line = -34.25)
mtext("VPD", side = 3, outer = TRUE, line = -45.5)

dev.off()