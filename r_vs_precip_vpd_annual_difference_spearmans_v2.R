
library(stringr)

#### Output PDF name ####
out_name  <- "G:/Africa/figs/r_vs_precip_vpd_annual_difference_spearmans_v2.pdf"
trop_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/TROPOMI", pattern = "*.csv", full.names = TRUE)
evi_list  <- list.files("G:/Africa/csv/ecoregions/mask_Dans/MCD43A4_EVI/monthly", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
nirv_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/MCD43A4_NIRv/monthly", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
ndvi_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/MCD43A4_NDVI/monthly", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
lswi_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/MCD43A4_LSWI/monthly", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
df_precip <- read.csv("G:/Africa/csv/precip/TropicalAfricaMonthlyPrecipPerEcoregionESAMask04252023.csv", header = TRUE)
era5_csv_list <- list.files("G:/ERA5/tropical_africa_ecoregions_stats", pattern = "*.csv", full.names = TRUE)

# Remove Niger forest and reorder VIs to match precip and SIF
trop_list <- c(trop_list[1:6], trop_list[8], trop_list[10:13])
evi_list  <- c(evi_list[1], evi_list[3], evi_list[2], evi_list[4:6], evi_list[9], evi_list[8], evi_list[10:12])
nirv_list <- c(nirv_list[1], nirv_list[3], nirv_list[2], nirv_list[4:6], nirv_list[9], nirv_list[8], nirv_list[10:12])
ndvi_list <- c(ndvi_list[1], ndvi_list[3], ndvi_list[2], ndvi_list[4:6], ndvi_list[9], ndvi_list[8], ndvi_list[10:12])
lswi_list <- c(lswi_list[1], lswi_list[3], lswi_list[2], lswi_list[4:6], lswi_list[9], lswi_list[8], lswi_list[10:12])
df_precip <- df_precip[df_precip$ecoregion != "Nigerian lowland forests",]
df_precip <- df_precip[df_precip$ecoregion != "Niger Delta swamp forests",]
df_precip <- df_precip[df_precip$ecoregion != "Cross-Niger transition forests",]
df_precip <- df_precip[df_precip$ecoregion != "Mount Cameroon and Bioko montane forests",]
df_precip$ecoregion[df_precip$ecoregion == "Niger Delta swamp and Nigerian lowland forests"] <- "Nigerian Lowland and Niger Delta Swamp Forest"
row.names(df_precip) <- 1:nrow(df_precip)

# Alphabetize the precip data to match TROPOMI
df_precip <- df_precip[order(df_precip$ecoregion),]

### Can be used for pvalues
round2 = function(x, n, p) {
  posneg = sign(x)
  z <- abs(x)*10^n
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10^n
  z <- z*posneg
  
  if (p == TRUE) {
    if (z < 0.05 && z >= 0.01) {
      z <- "p < 0.05"
    } else if (z < 0.01) {
      z <- "p < 0.01"
    } else {
      z <- paste0("p = ", z)
    }
  }
  return(z)
}

# Index for precip data
p_start <- seq(1, 396, 36)
p_end   <- seq(36, 396, 36)

# Get annual total means
for (i in 1:length(unique(df_precip$ecoregion))) {
  precip_eco <- sum(df_precip$mean[(p_start[i]):(p_end[i])]) / 3
  
  if (i == 1) {
    precip_ann <- precip_eco
  } else {
    precip_ann <- c(precip_ann, precip_eco)
  }
}

# Get sd
for (i in 1:length(unique(df_precip$ecoregion))) {
  precip_eco <- sd(df_precip$mean[(p_start[i]):(p_end[i])])
  
  if (i == 1) {
    precip_sd <- precip_eco
  } else {
    precip_sd <- c(precip_sd, precip_eco)
  }
}

# Get correlations
for (i in 1:length(trop_list)) {
  
  # Read Data
  t_sif  <- read.csv(trop_list[i])$Mean
  precip <- df_precip$mean[p_start[i]:p_end[i]]
  vpd    <- read.csv(era5_csv_list[i])$vpd_mean[1:36]
  df_p   <- data.frame(precip, t_sif)
  df_v   <- data.frame(vpd, t_sif)
  colnames(df_p) <- c("x", "y")
  colnames(df_v) <- c("x", "y")
  
  # Run regression
  spear_p <- cor.test(precip, t_sif, method = "spearman")
  spear_v <- cor.test(vpd, t_sif, method = "spearman")
  reg_r_precip <- spear_p$estimate
  reg_p_precip <- round2(spear_p$p.value, 2, TRUE)
  reg_r_vpd    <- spear_v$estimate
  reg_p_vpd    <- round2(spear_v$p.value, 2, TRUE)
  
  if (i == 1) {
    r_tro_precip <- reg_r_precip
    p_tro_precip <- reg_p_precip
    r_tro_vpd    <- reg_r_vpd
    p_tro_vpd    <- reg_p_vpd
  } else {
    r_tro_precip <- c(r_tro_precip, reg_r_precip)
    p_tro_precip <- c(p_tro_precip, reg_p_precip)
    r_tro_vpd    <- c(r_tro_vpd, reg_r_vpd)
    p_tro_vpd    <- c(p_tro_vpd, reg_p_vpd)
  }
}

for (i in 1:length(evi_list)) {
  
  # Read Data
  evi   <- read.csv(evi_list[i])$EVI / 10000
  evi   <- evi[13:48]
  precip <- df_precip$mean[p_start[i]:p_end[i]]
  vpd    <- read.csv(era5_csv_list[i])$vpd_mean[1:36]
  df_p   <- data.frame(precip, evi)
  df_v   <- data.frame(vpd, evi)
  colnames(df_p) <- c("x", "y")
  colnames(df_v) <- c("x", "y")
  
  # Run regression
  spear_p <- cor.test(precip, evi, method = "spearman")
  spear_v <- cor.test(vpd, evi, method = "spearman")
  reg_r_precip <- spear_p$estimate
  reg_p_precip <- round2(spear_p$p.value, 2, TRUE)
  reg_r_vpd    <- spear_v$estimate
  reg_p_vpd    <- round2(spear_v$p.value, 2, TRUE)
  
  if (i == 1) {
    r_evi_precip <- reg_r_precip
    p_evi_precip <- reg_p_precip
    r_evi_vpd    <- reg_r_vpd
    p_evi_vpd    <- reg_p_vpd
  } else {
    r_evi_precip <- c(r_evi_precip, reg_r_precip)
    p_evi_precip <- c(p_evi_precip, reg_p_precip)
    r_evi_vpd    <- c(r_evi_vpd, reg_r_vpd)
    p_evi_vpd    <- c(p_evi_vpd, reg_p_vpd)
  }
}

for (i in 1:length(ndvi_list)) {
  
  # Read Data
  ndvi   <- read.csv(ndvi_list[i])$NDVI / 10000
  ndvi   <- ndvi[13:48]
  precip <- df_precip$mean[p_start[i]:p_end[i]]
  vpd    <- read.csv(era5_csv_list[i])$vpd_mean[1:36]
  df_p   <- data.frame(precip, ndvi)
  df_v   <- data.frame(vpd, ndvi)
  colnames(df_p) <- c("x", "y")
  colnames(df_v) <- c("x", "y")
  
  # Run regression
  spear_p <- cor.test(precip, ndvi, method = "spearman")
  spear_v <- cor.test(vpd, ndvi, method = "spearman")
  reg_r_precip <- spear_p$estimate
  reg_p_precip <- round2(spear_p$p.value, 2, TRUE)
  reg_r_vpd    <- spear_v$estimate
  reg_p_vpd    <- round2(spear_v$p.value, 2, TRUE)
  
  if (i == 1) {
    r_ndvi_precip <- reg_r_precip
    p_ndvi_precip <- reg_p_precip
    r_ndvi_vpd    <- reg_r_vpd
    p_ndvi_vpd    <- reg_p_vpd
  } else {
    r_ndvi_precip <- c(r_ndvi_precip, reg_r_precip)
    p_ndvi_precip <- c(p_ndvi_precip, reg_p_precip)
    r_ndvi_vpd    <- c(r_ndvi_vpd, reg_r_vpd)
    p_ndvi_vpd    <- c(p_ndvi_vpd, reg_p_vpd)
  }
}

# Put it all in a df
df_r_precip <- data.frame(r_tro_precip, r_evi_precip, r_ndvi_precip)
df_p_precip <- data.frame(p_tro_precip, p_evi_precip, p_ndvi_precip)
df_r_vpd    <- data.frame(r_tro_vpd, r_evi_vpd, r_ndvi_vpd)
df_p_vpd    <- data.frame(p_tro_vpd, p_evi_vpd, p_ndvi_vpd)

df_r_diff  <- df_r_vpd + df_r_precip

# Labels
lab_r      <- bquote("Difference in Spearman's Corrleation Coefficient (VPD - Precip)")
lab_annual <- bquote("Mean Annual Total Precipitation (mm)")
lab_sd     <- bquote("Standard Deviation of Monthly Precipitation (mm)")
# titles     <- c("TROPOMI SIF", "EVI", "NIRv", "NDVI", "LSWI")
titles     <- c("TROPOMI SIF", "EVI", "NIRv", "NDVI", "LSWI")

#### Plot ####
cairo_pdf(out_name, width = 7.5, height = 5.5)

par(mfrow = c(2, 3), oma=c(3.5,2,2,0.5))

# Plot diff
for (i in 1:3) {
  
  # Read Data
  df_p     <- data.frame(precip_ann, df_r_diff[,i])
  colnames(df_p) <- c("x", "y")
  
  # Run regression
  spear <- cor.test(precip_ann, df_r_diff[,i], method = "spearman")
  reg   <- lm(y ~ x, data = df_p)
  reg_r <- bquote(r[s]~" = "~.(round2(spear$estimate, 2, FALSE)))
  reg_p <- round2(spear$p.value, 2, TRUE)
  
  # predicts + interval
  newx  <- seq(min(df_p$x), max(df_p$x), length.out = 100)
  preds <- predict(reg, newdata = data.frame(x = newx), interval = 'confidence')
  
  # Get title
  tit <- paste0(titles[i])
  
  # Plot
  op <- par(mar = c(3,2,0,0.5))
  
  plot(precip_ann, df_r_diff[,i], axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, ylim = c(-0.5, 0.5), pch = NA)
  polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)
  par(new=TRUE)
  plot(precip_ann, df_r_diff[,i], axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, ylim = c(-0.5, 0.5))
  abline(reg)
  abline(h = 0, lty = 2)
  axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
  axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
  
  mtext(3, text = tit, line = 0.5, cex = 0.85)
  legend("topleft", bty = "n", legend = c(as.expression(reg_r), as.expression(reg_p)))
  
  box()
}

mtext(lab_annual, side = 1, outer = TRUE, line = -19)

# For VPD
for (i in 1:3) {
  
  # Read Data
  df_p     <- data.frame(precip_sd, df_r_diff[,i])
  colnames(df_p) <- c("x", "y")
  
  # Run regression
  spear <- cor.test(precip_sd, df_r_diff[,i], method = "spearman")
  reg   <- lm(y ~ x, data = df_p)
  reg_r <- bquote(r[s]~" = "~.(round2(spear$estimate, 2, FALSE)))
  reg_p <- round2(spear$p.value, 2, TRUE)
  
  # predicts + interval
  newx  <- seq(min(df_p$x), max(df_p$x), length.out = 100)
  preds <- predict(reg, newdata = data.frame(x = newx), interval = 'confidence')
  
  # Get title
  tit <- paste0(titles[i], "")
  
  # Plot
  op <- par(mar = c(0,2,3,0.5))
  
  plot(precip_sd, df_r_diff[,i], axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, ylim = c(-0.5, 0.5), pch = NA)
  polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)
  par(new=TRUE)
  plot(precip_sd, df_r_diff[,i], axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, ylim = c(-0.5, 0.5))
  abline(reg)
  abline(h = 0, lty = 2)
  axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
  axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
  
  mtext(3, text = tit, line = 0.5, cex = 0.85)
  legend("topleft", bty = "n", legend = c(as.expression(reg_r), as.expression(reg_p)))
  
  box()
}

mtext(lab_sd, side = 1, outer = TRUE, line = 2)
mtext(lab_r, side = 2, outer = TRUE, line = 0)

dev.off()
