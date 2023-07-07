library(terra)
library(viridis)

sif        <- rast("G:/TROPOMI/esa/gridded/50km/monthly_2019-2021_sif_norm.nc")
evi        <- rast("G:/MCD43A4/vis/africa/min_max_monthly/monthly_2019-2021_EVI_norm.nc")
precip     <- rast("G:/CHIRPS/africa_monthly/min_max/monthly_2019-2021_precip_norm.nc")
vpd        <- rast("G:/ERA5/min_max/monthly_2019-2021_vpd_norm.nc")
sm         <- rast("G:/ERA5/min_max/monthly_2019-2021_soil_norm.nc")
coastlines <- vect("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")

i_seq <- seq(1, 10, 3)

cairo_pdf("G:/Africa/figs/norm_maps.pdf", width = 7.5, height = 7.5)

par(mfrow = c(5, 4), oma=c(0,0,1,0))

for (i in i_seq) {
  if (i == 1) {
    plot(sif[[i]], main = "January", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(sif[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  } else if (i == 4) {
    plot(sif[[i]], main = "April", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(sif[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  } else if (i == 7) {
    plot(sif[[i]], main = "July", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(sif[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  } else if (i == 10) {
    plot(sif[[i]], main = "October", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(sif[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  }
}

for (i in i_seq) {
  if (i == 1) {
    plot(evi[[i]], main = "January", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(evi[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  } else if (i == 4) {
    plot(evi[[i]], main = "April", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(evi[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  } else if (i == 7) {
    plot(evi[[i]], main = "July", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(evi[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  } else if (i == 10) {
    plot(evi[[i]], main = "October", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(evi[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  }
}

for (i in i_seq) {
  if (i == 1) {
    plot(precip[[i]], main = "January", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(precip[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  } else if (i == 4) {
    plot(precip[[i]], main = "April", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(precip[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  } else if (i == 7) {
    plot(precip[[i]], main = "July", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(precip[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  } else if (i == 10) {
    plot(precip[[i]], main = "October", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(precip[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  }
}

for (i in i_seq) {
  if (i == 1) {
    plot(sm[[i]], main = "January", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(sm[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  } else if (i == 4) {
    plot(sm[[i]], main = "April", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(sm[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  } else if (i == 7) {
    plot(sm[[i]], main = "July", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(sm[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  } else if (i == 10) {
    plot(sm[[i]], main = "October", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(sm[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  }
}

for (i in i_seq) {
  if (i == 1) {
    plot(vpd[[i]], main = "January", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(vpd[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  } else if (i == 4) {
    plot(vpd[[i]], main = "April", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(vpd[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  } else if (i == 7) {
    plot(vpd[[i]], main = "July", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(vpd[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  } else if (i == 10) {
    plot(vpd[[i]], main = "October", col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), mar = c(2,2,2,3))
    plot(coastlines, border = NA, col = rgb(0.90,0.90,0.90), axes = FALSE, ad = TRUE)
    plot(vpd[[i]], col = viridis(10), range = c(0,1),
         pax = list(side = c(1:2), tick = 0, mgp=c(3, 0.2, 0)), add = TRUE)
    axis(1, labels = FALSE, tck = 0.03)
    axis(2, labels = FALSE, tck = 0.03)
    box()
  }
}



mtext("SIF", side = 3, outer = TRUE, line = -1)
mtext("EVI", side = 3, outer = TRUE, line = -12)
mtext("Precipitation", side = 3, outer = TRUE, line = -23.25)
mtext("Volumetric Soil Water", side = 3, outer = TRUE, line = -34.25)
mtext("VPD", side = 3, outer = TRUE, line = -45.5)

dev.off()
