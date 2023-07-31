library(terra)
library(viridis)

options(scipen = 999)

sif_max      <- rast("G:/TROPOMI/esa/gridded/50km/min_max/max_monthly_2019-2021_sif.nc")
sif_min      <- rast("G:/TROPOMI/esa/gridded/50km/min_max/min_monthly_2019-2021_sif.nc")
evi_max      <- rast("G:/MCD43A4/vis/africa/min_max_monthly/max_monthly_2019-2021_EVI.nc")
evi_min      <- rast("G:/MCD43A4/vis/africa/min_max_monthly/min_monthly_2019-2021_EVI.nc")
precip_max   <- rast("G:/CHIRPS/africa_monthly/min_max/max_monthly_2019-2021_precip.nc")
precip_min   <- rast("G:/CHIRPS/africa_monthly/min_max/min_monthly_2019-2021_precip.nc")
vpd_max      <- rast("G:/ERA5/min_max/max_monthly_2019-2021_vpd.nc")
vpd_min      <- rast("G:/ERA5/min_max/min_monthly_2019-2021_vpd.nc")
sm_max       <- rast("G:/ERA5/min_max/max_monthly_2019-2021_soil.nc")
sm_min       <- rast("G:/ERA5/min_max/min_monthly_2019-2021_soil.nc")
coastlines   <- vect("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")

vpd_max      <- crop(vpd_max, sm)
vpd_min      <- crop(vpd_min, sm)

# labs         <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
labs         <- c("J", "M", "M", "J", "S", "N")

cairo_pdf("G:/Africa/figs/min_max_maps.pdf", width = 7.5, height = 7.5)

par(mfrow = c(5, 4), oma=c(0,0,1,0))

plot(sif_min, main = "Min SIF", col = viridis(12), range = c(1,13),
     xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3), legend = FALSE)
plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
plot(sif_min, col = viridis(12), range = c(1,13), xlim = c(8,30), add = TRUE, type = "continuous",
     plg = list("topright", labels = labs, at = seq(1.5,11.5,2)))
axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

hist(na.omit(as.vector(sif_min)), main = "Min SIF", col = viridis(12), axes = FALSE, breaks = 12, xlim = c(0,12), yaxs="i")
axis(1, labels = labs, tck = 0, mgp=c(3, 0.2, 0), at = seq(0.5, 11.5, by = 2))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

plot(sif_max, main = "Max SIF", col = viridis(12), range = c(1,13),
     xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3), legend = FALSE)
plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
plot(sif_max, col = viridis(12), range = c(1,13), xlim = c(8,30), add = TRUE, type = "continuous",
     plg = list("topright", labels = labs, at = seq(1.5,11.5,2)))
axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

hist(na.omit(as.vector(sif_max)), main = "Max SIF", col = viridis(12), axes = FALSE, breaks = 12, xlim = c(0,12), yaxs="i")
axis(1, labels = labs, tck = 0, mgp=c(3, 0.2, 0), at = seq(0.5, 11.5, by = 2))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

plot(evi_min, main = "Min EVI", col = viridis(12), range = c(1,13),
     xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3), legend = FALSE)
plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
plot(evi_min, col = viridis(12), range = c(1,13), xlim = c(8,30), add = TRUE, type = "continuous",
     plg = list("topright", labels = labs, at = seq(1.5,11.5,2)))
axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

hist(na.omit(as.vector(evi_min)), main = "Min EVI", col = viridis(12), axes = FALSE, breaks = 12, xlim = c(0,12), yaxs="i")
axis(1, labels = labs, tck = 0, mgp=c(3, 0.2, 0), at = seq(0.5, 11.5, by = 2))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

plot(evi_max, main = "Max EVI", col = viridis(12), range = c(1,13),
     xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3), legend = FALSE)
plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
plot(evi_max, col = viridis(12), range = c(1,13), xlim = c(8,30), add = TRUE, type = "continuous",
     plg = list("topright", labels = labs, at = seq(1.5,11.5,2)))
axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

hist(na.omit(as.vector(evi_max)), main = "Max EVI", col = viridis(12), axes = FALSE, breaks = 12, xlim = c(0,12), yaxs="i")
axis(1, labels = labs, tck = 0, mgp=c(3, 0.2, 0), at = seq(0.5, 11.5, by = 2))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

plot(precip_min, main = "Min Precipitation", col = viridis(12), range = c(1,13),
     xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3), legend = FALSE)
plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
plot(precip_min, col = viridis(12), range = c(1,13), xlim = c(8,30), add = TRUE, type = "continuous",
     plg = list("topright", labels = labs, at = seq(1.5,11.5,2)))
axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

hist(na.omit(as.vector(precip_min)), main = "Min Precipitation", col = viridis(12), axes = FALSE, breaks = 12, xlim = c(0,12), yaxs="i")
axis(1, labels = labs, tck = 0, mgp=c(3, 0.2, 0), at = seq(0.5, 11.5, by = 2))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

plot(precip_max, main = "Max Precipitation", col = viridis(12), range = c(1,13),
     xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3), legend = FALSE)
plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
plot(precip_max, col = viridis(12), range = c(1,13), xlim = c(8,30), add = TRUE, type = "continuous",
     plg = list("topright", labels = labs, at = seq(1.5,11.5,2)))
axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

hist(precip_max, main = "Max Precipitation", col = viridis(12), axes = FALSE, breaks = 12, xlim = c(0,12), yaxs="i")
axis(1, labels = labs, tck = 0, mgp=c(3, 0.2, 0), at = seq(0.5, 11.5, by = 2))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

plot(sm_min, main = "Min VSM", col = viridis(12), range = c(1,13),
     xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3), legend = FALSE)
plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
plot(sm_min, col = viridis(12), range = c(1,13), xlim = c(8,30), add = TRUE, type = "continuous",
     plg = list("topright", labels = labs, at = seq(1.5,11.5,2)))
axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

hist(na.omit(as.vector(sm_min)), main = "Min VSW", col = viridis(12), axes = FALSE, breaks = 8, xlim = c(0,12), yaxs="i")
axis(1, labels = labs, tck = 0, mgp=c(3, 0.2, 0), at = seq(0.5, 11.5, by = 2))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

plot(sm_max, main = "Max VSM", col = viridis(12), range = c(1,13),
     xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3), legend = FALSE)
plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
plot(sm_max, col = viridis(12), range = c(1,13), xlim = c(8,30), add = TRUE, type = "continuous",
     plg = list("topright", labels = labs, at = seq(1.5,11.5,2)))
axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

hist(na.omit(as.vector(sm_max)), main = "Max VSW", col = viridis(12), axes = FALSE, breaks = 12, xlim = c(0,12), yaxs="i")
axis(1, labels = labs, tck = 0, mgp=c(3, 0.2, 0), at = seq(0.5, 11.5, by = 2))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

plot(vpd_max, main = "Max VPD", col = viridis(12), range = c(1,13),
     xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3), legend = FALSE)
plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
plot(vpd_max, col = viridis(12), range = c(1,13), xlim = c(8,30), add = TRUE, type = "continuous",
     plg = list("topright", labels = labs, at = seq(1.5,11.5,2)))
axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

hist(na.omit(as.vector(vpd_max)), main = "Max VPD", col = viridis(12), axes = FALSE, breaks = 10, xlim = c(0,12), yaxs="i")
axis(1, labels = labs, tck = 0, mgp=c(3, 0.2, 0), at = seq(0.5, 11.5, by = 2))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

plot(vpd_min, main = "Min VPD", col = viridis(12), range = c(1,13),
     xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3), legend = FALSE)
plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
plot(vpd_min, col = viridis(12), range = c(1,13), xlim = c(8,30), add = TRUE, type = "continuous",
     plg = list("topright", labels = labs, at = seq(1.5,11.5,2)))
axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

hist(na.omit(as.vector(vpd_min)), main = "Min VPD", col = viridis(12), axes = FALSE, breaks = 10, xlim = c(0,12), yaxs="i")
axis(1, labels = labs, tck = 0, mgp=c(3, 0.2, 0), at = seq(0.5, 11.5, by = 2))
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
box()

# mtext("SIF", side = 3, outer = TRUE, line = -1)
# mtext("EVI", side = 3, outer = TRUE, line = -12)
# mtext("Precipitation", side = 3, outer = TRUE, line = -23.25)
# mtext("Volumetric Soil Water", side = 3, outer = TRUE, line = -34.25)
# mtext("VPD", side = 3, outer = TRUE, line = -45.5)

dev.off()
