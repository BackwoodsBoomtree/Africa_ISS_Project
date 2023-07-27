library(terra)
library(viridis)
library(RColorBrewer)

sif        <- rast("G:/TROPOMI/esa/gridded/50km/min_max/monthly_2019-2021_sif_norm.nc")
evi        <- rast("G:/MCD43A4/vis/africa/min_max_monthly/monthly_2019-2021_EVI_norm.nc")
precip     <- rast("G:/CHIRPS/africa_monthly/min_max/monthly_2019-2021_precip_norm.nc")
vpd        <- rast("G:/ERA5/min_max/monthly_2019-2021_vpd_norm.nc")
sm         <- rast("G:/ERA5/min_max/monthly_2019-2021_soil_norm.nc")
coastlines <- vect("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")
diff.col   <- brewer.pal(n = 8, name = "RdBu")
vpd        <- crop(vpd, sm)

cairo_pdf("G:/Africa/figs/norm_maps_diff.pdf", width = 7.5, height = 7.5)

par(mfrow = c(5, 4), oma=c(0,0,1,0.5))

for (i in 1:4) {
  if (i == 1) {
    plot(sif[[4]] - sif[[1]], main = "April - January", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(sif[[4]] - sif[[1]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  } else if (i == 2) {
    plot(sif[[7]] - sif[[4]], main = "July - April", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(sif[[7]] - sif[[4]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  } else if (i == 3) {
    plot(sif[[10]] - sif[[7]], main = "October - July", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(sif[[10]] - sif[[7]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  } else if (i == 4) {
    plot(sif[[1]] - sif[[10]], main = "January - October", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(sif[[1]] - sif[[10]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  }
}

for (i in 1:4) {
  if (i == 1) {
    plot(evi[[4]] - evi[[1]], main = "April - January", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(evi[[4]] - evi[[1]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  } else if (i == 2) {
    plot(evi[[7]] - evi[[4]], main = "July - April", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(evi[[7]] - evi[[4]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  } else if (i == 3) {
    plot(evi[[10]] - evi[[7]], main = "October - July", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(evi[[10]] - evi[[7]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  } else if (i == 4) {
    plot(evi[[1]] - evi[[10]], main = "January - October", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(evi[[1]] - evi[[10]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  }
}

for (i in 1:4) {
  if (i == 1) {
    plot(precip[[4]] - precip[[1]], main = "April - January", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(precip[[4]] - precip[[1]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  } else if (i == 2) {
    plot(precip[[7]] - precip[[4]], main = "July - April", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(precip[[7]] - precip[[4]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  } else if (i == 3) {
    plot(precip[[10]] - precip[[7]], main = "October - July", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(precip[[10]] - precip[[7]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  } else if (i == 4) {
    plot(precip[[1]] - precip[[10]], main = "January - October", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(precip[[1]] - precip[[10]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  }
}

for (i in 1:4) {
  if (i == 1) {
    plot(sm[[4]] - sm[[1]], main = "April - January", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(sm[[4]] - sm[[1]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  } else if (i == 2) {
    plot(sm[[7]] - sm[[4]], main = "July - April", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(sm[[7]] - sm[[4]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  } else if (i == 3) {
    plot(sm[[10]] - sm[[7]], main = "October - July", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(sm[[10]] - sm[[7]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  } else if (i == 4) {
    plot(sm[[1]] - sm[[10]], main = "January - October", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(sm[[1]] - sm[[10]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  }
}

for (i in 1:4) {
  if (i == 1) {
    plot(vpd[[4]] - vpd[[1]], main = "April - January", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(vpd[[4]] - vpd[[1]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  } else if (i == 2) {
    plot(vpd[[7]] - vpd[[4]], main = "July - April", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(vpd[[7]] - vpd[[4]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  } else if (i == 3) {
    plot(vpd[[10]] - vpd[[7]], main = "October - July", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(vpd[[10]] - vpd[[7]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  } else if (i == 4) {
    plot(vpd[[1]] - vpd[[10]], main = "January - October", col = diff.col, range = c(-1,1),
         xlim = c(8,30), box = FALSE, axes = FALSE, mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, add = TRUE)
    plot(vpd[[1]] - vpd[[10]], col = diff.col, range = c(-1,1), xlim = c(8,30), add = TRUE)
    axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0))
    box()
  }
}

mtext("SIF", side = 3, outer = TRUE, line = -1)
mtext("EVI", side = 3, outer = TRUE, line = -12)
mtext("Precipitation", side = 3, outer = TRUE, line = -23.25)
mtext("Volumetric Soil Water", side = 3, outer = TRUE, line = -34.25)
mtext("VPD", side = 3, outer = TRUE, line = -45.5)

dev.off()
