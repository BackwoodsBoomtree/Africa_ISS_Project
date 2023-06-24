
library(stringr)

#### Output PDF name ####
out_name  <- "G:/Africa/figs/r_vs_precip_annual_v2.pdf"
trop_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/TROPOMI", pattern = "*.csv", full.names = TRUE)
evi_list  <- list.files("G:/Africa/csv/ecoregions/mask_Dans/MCD43A4_EVI/monthly", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
nirv_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/MCD43A4_NIRv/monthly", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
ndvi_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/MCD43A4_NDVI/monthly", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
lswi_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/MCD43A4_LSWI/monthly", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
df_precip <- read.csv("G:/Africa/csv/precip/TropicalAfricaMonthlyPrecipPerEcoregionESAMask04252023.csv", header = TRUE)

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

# Get correlations
for (i in 1:length(trop_list)) {
  
  # Read Data
  t_sif  <- read.csv(trop_list[i])$Mean
  precip <- df_precip$mean[p_start[i]:p_end[i]]
  df     <- data.frame(precip, t_sif)
  colnames(df) <- c("x", "y")
  
  # Run regression
  spear <- cor.test(precip, t_sif, method = "spearman")
  reg_r <- spear$estimate
  reg_p <- round2(spear$p.value, 2, TRUE)
  
  if (i == 1) {
    r_tro <- reg_r
    p_tro <- reg_p
  } else {
    r_tro <- c(r_tro, reg_r)
    p_tro <- c(p_tro, reg_p)
  }
  
}
for (i in 1:length(evi_list)) {
  
  # Read Data
  evi   <- read.csv(evi_list[i])$EVI / 10000
  evi   <- evi[13:48]
  precip <- df_precip$mean[p_start[i]:p_end[i]]
  df     <- data.frame(precip, evi)
  colnames(df) <- c("x", "y")
  
  # Run regression
  spear <- cor.test(precip, evi, method = "spearman")
  reg_r <- spear$estimate
  reg_p <- round2(spear$p.value, 2, FALSE)
  
  if (i == 1) {
    r_evi <- reg_r
    p_evi <- reg_p
  } else {
    r_evi <- c(r_evi, reg_r)
    p_evi <- c(p_evi, reg_p)
  }
}
for (i in 1:length(nirv_list)) {
  
  # Read Data
  nirv   <- read.csv(nirv_list[i])$NIRv / 10000
  nirv   <- nirv[13:48]
  precip <- df_precip$mean[p_start[i]:p_end[i]]
  df     <- data.frame(precip, nirv)
  colnames(df) <- c("x", "y")
  
  # Run regression
  spear <- cor.test(precip, nirv, method = "spearman")
  reg_r <- spear$estimate
  reg_p <- round2(spear$p.value, 2, FALSE)
  
  if (i == 1) {
    r_nirv <- reg_r
    p_nirv <- reg_p
  } else {
    r_nirv <- c(r_nirv, reg_r)
    p_nirv <- c(p_nirv, reg_p)
  }
}
for (i in 1:length(ndvi_list)) {
  
  # Read Data
  ndvi   <- read.csv(ndvi_list[i])$NDVI / 10000
  ndvi   <- ndvi[13:48]
  precip <- df_precip$mean[p_start[i]:p_end[i]]
  df     <- data.frame(precip, ndvi)
  colnames(df) <- c("x", "y")
  
  # Run regression
  spear <- cor.test(precip, ndvi, method = "spearman")
  reg_r <- spear$estimate
  reg_p <- round2(spear$p.value, 2, FALSE)
  
  if (i == 1) {
    r_ndvi <- reg_r
    p_ndvi <- reg_p
  } else {
    r_ndvi <- c(r_ndvi, reg_r)
    p_ndvi <- c(p_ndvi, reg_p)
  }
}
for (i in 1:length(lswi_list)) {
  
  # Read Data
  lswi   <- read.csv(lswi_list[i])$LSWI / 10000
  lswi   <- lswi[13:48]
  precip <- df_precip$mean[p_start[i]:p_end[i]]
  df     <- data.frame(precip, lswi)
  colnames(df) <- c("x", "y")
  
  # Run regression
  spear <- cor.test(precip, lswi, method = "spearman")
  reg_r <- spear$estimate
  reg_p <- round2(spear$p.value, 2, FALSE)
  
  if (i == 1) {
    r_lswi <- reg_r
    p_lswi <- reg_p
  } else {
    r_lswi <- c(r_lswi, reg_r)
    p_lswi <- c(p_lswi, reg_p)
  }
}

# Put it all in a df
df_r <- data.frame(r_tro, r_evi, r_nirv, r_ndvi, r_lswi)
df_p <- data.frame(p_tro, p_evi, p_nirv, p_ndvi, p_lswi)

# Labels
lab_r      <- bquote("Spearman's Corrleation Coefficient")
lab_precip <- bquote("Mean Annual Total Precipitation (mm)")
titles     <- c("TROPOMI SIF", "EVI", "NIRv", "NDVI", "LSWI")

#### Plot ####
cairo_pdf(out_name, width = 7.5, height = 5.5)

par(mfrow = c(2, 3), oma=c(3.5,3,0,1.5))

for (i in 1:5) {
  
  # Read Data
  df     <- data.frame(precip_ann, df_r[,i])
  colnames(df) <- c("x", "y")
  
  # Run regression
  spear <- cor.test(precip_ann, df_r[,i], method = "spearman")
  reg   <- lm(y ~ x, data = df)
  reg_r <- bquote(r[s]~" = "~.(round2(spear$estimate, 2, FALSE)))
  reg_p <- round2(spear$p.value, 2, TRUE)
  
  # predicts + interval
  newx  <- seq(min(df$x), max(df$x), length.out = 100)
  preds <- predict(reg, newdata = data.frame(x = newx), interval = 'confidence')
  
  # Get title
  tit <- titles[i]
  
  # Plot
  op <- par(mar = c(0,2,4,0.5))
  
  plot(precip_ann, df_r[,i], axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, ylim = c(0, 1), pch = NA)
  polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)
  par(new=TRUE)
  plot(precip_ann, df_r[,i], axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, ylim = c(0, 1))
  abline(reg)
  axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
  axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
  
  mtext(3, text = tit, line = 0.5, cex = 0.85)
  legend("topleft", bty = "n", legend = c(as.expression(reg_r), as.expression(reg_p)))
  
  box()
}

mtext(lab_precip, side = 1, outer = TRUE, line = 2.5)
mtext(lab_r, side = 2, outer = TRUE, line = 1)

dev.off()
