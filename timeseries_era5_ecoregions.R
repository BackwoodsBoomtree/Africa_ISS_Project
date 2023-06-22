library(stringr)

in_dir           <- "G:/Africa/csv/ecoregions/mask_Dans"
tropomi_csv_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/TROPOMI", pattern = "*.csv", full.names = TRUE)
precip_csv       <- read.csv("G:/Africa/csv/precip/TropicalAfricaMonthlyPrecipPerEcoregionESAMask04252023.csv", header = TRUE)
era5_dir         <- "G:/ERA5/tropical_africa_ecoregions_stats"
# NOTE: No forest for CNTF or MCBMF when using Africa_merged_2019_2.5km_Buffer.shp
# Ordered to match TROPOMI figure
eco_list         <- c("AECF", "CHF", "CCLF", "CSBCF", "ECSF", "EGF",
                      "NLNDSF", "NECLF", "NWCLF", "WCSF", "WGLF")
month_labs       <- format(seq(as.Date("2019/1/1"), as.Date("2021/12/1"), "month"), "%Y-%m")
year_labs        <- c(2019, 2020, 2021)
cols             <- c("#CC6677", "#117733", "#AA4499")
x                <- 1:36

# Remove Nigerian Lowland and Niger Delta Swamp as I combined them
tropomi_csv_list <- c(tropomi_csv_list[1:6], tropomi_csv_list[8], tropomi_csv_list[10:13])

# Sort Precip df and remove
precip_df <- precip_csv[order(precip_csv$ecoregion),]
precip_df$ecoregion[precip_df$ecoregion == "Niger Delta swamp and Nigerian lowland forests"] <- "Nigerian Lowland and Niger Delta Swamp Forest"
row.names(precip_df) <- 1:nrow(precip_df)

# ERA5 CSVs
era5_csvs <- list.files(era5_dir, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

# PLOT

cairo_pdf("G:/Africa/figs/timeseries_era5_ecoregions_2.5km_mask.pdf", width = 20, height = 15)

par(mfrow = c(4, 3), oma=c(1.0,8,1.25,3.1))

op <- par(mar = c(4.5,1,3,2))

index <- seq(1, 396, by = 36) # For precip

for (i in 1:length(eco_list)) {
  
  # ERA5 Data
  eco_era5 <- read.csv(era5_csvs[i], header = TRUE)
  eco_era5 <- head(eco_era5, 36)

  # SIF data
  tropomi_df     <- read.csv(tropomi_csv_list[i], header = TRUE)
  tropomi_sifd   <- tropomi_df[,1]
  
  # Precip data
  precip <- precip_df$mean[index[i] : (index[i] + 35)]
  
  # Get ecoregion name
  if (i != 7) {
    f_name <- basename(tropomi_csv_list[i])
    tit    <- substr(f_name, 1, nchar(f_name) - 19)
    tit    <- substr(tit, 25, nchar(tit))
    tit    <- gsub("_", " ", tit)
    tit    <- str_to_title(tit)
    tit    <- str_sub(tit, end = -2)
  } else {
    tit    <- "Nigerian Lowland and Niger Delta Forest"
  }
  
  plot(x, tropomi_sifd, type = "l", main = tit, axes = FALSE, xlab = NA, ylab = NA, cex.main = 2.75, ylim = c(0.1, 0.9))
  rect(13, -1, 25, 100, col = rgb(0.85,0.85,0.85), border = NA)
  box()
  
  par(new = TRUE)
  barplot(precip, col = "#648FFF80", axes = FALSE, xlab = "", ylab = "", ylim = c(0, 1100), space = 0, border = NA)

  if (i == 1 || i == 4 || i == 7 || i == 10) {
      axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 2.5, at = seq(0, 600, by = 100),
       col = "#648FFF80",  col.lab = "#648FFF80",  col.axis = "#648FFF80", lwd = 2)
  } else {
      axis(2, labels = FALSE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 2.5, at = seq(0, 600, by = 100),
       col = "#648FFF80",  col.lab = "#648FFF80",  col.axis = "#648FFF80", lwd = 2)
  }

  par(new = TRUE)
  plot(x, tropomi_sifd, type = "l", main = tit, axes = FALSE, xlab = NA, ylab = NA, cex.main = 2.75, ylim = c(0.1, 0.9), lwd = 3)
  if (i == 1 || i == 4 || i == 7 || i == 10) {
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 2.5, at = seq(0.2, 0.6, by = 0.2), line = 5, lwd = 2)
  } else {
    axis(2, labels = FALSE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 2.5, at = seq(0.2, 0.6, by = 0.2), line = 1, lwd = 2)
  }
  
  par(new = TRUE)
  plot(x, eco_era5$t2m_mean, type = "l", main = NA, axes = FALSE, xlab = NA, ylab = NA, cex.main = 2.75, ylim = c(0, 30), col = cols[2], lwd = 3)

  if (i == 1 || i == 4 || i == 7 || i == 10) {
    axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 2.5, at = seq(22, 30, by = 2),
         col = cols[2], col.lab = cols[2], col.axis = cols[2], lwd = 2)
  } else {
      axis(2, labels = FALSE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 2.5, at = seq(22, 30, by = 2),
       col = cols[2], col.lab = cols[2], col.axis = cols[2], lwd = 2)
  }
  
  par(new = TRUE)
  plot(x, eco_era5$vpd_mean, type = "l", main = NA, axes = FALSE, xlab = NA, ylab = NA, cex.main = 2.75, ylim = c(0.2, 3.5), col = cols[3], lwd = 3)

  if (i == 3 || i == 6 || i == 9 || i ==11) {
  axis(4, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 2.5, at = seq(0.2, 1.4, by = 0.4),
       col = cols[3], col.lab = cols[3], col.axis = cols[3], lwd = 2)
  } else {
      axis(4, labels = FALSE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 2.5, at = seq(0.2, 1.4, by = 0.4),
       col = cols[3], col.lab = cols[3], col.axis = cols[3], lwd = 2)
  }
  
  par(new = TRUE)
  plot(x, eco_era5$par_mean, type = "l", main = NA, axes = FALSE, xlab = NA, ylab = NA, cex.main = 2.75, ylim = c(-20, 130), col = cols[1], lwd = 3, lty = 3)
  if (i == 3 || i == 6 || i == 9 || i ==11) {
    axis(4, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 2.5, at = seq(70, 130, by = 30),
         col = cols[1], col.lab = cols[1], col.axis = cols[1], lwd = 2)
  } else {
    axis(4, labels = FALSE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 2.5, at = seq(70, 130, by = 30),
         col = cols[1], col.lab = cols[1], col.axis = cols[1], lwd = 2)
  }
  
  axis(1, labels = year_labs, at =  c(1, 13, 25), tck = 0.03, mgp=c(3, 1.5, 0), las = 1, cex.axis = 2.5)
  
}

plot.new()
lab_sif     <- bquote("TROPOMI SIFdaily 740 nm (mW/m"^"2"*"/sr/nm)")
lab_precip  <- bquote("Mean Total Monthly Precipiptation (mm)")
lab_par     <- bquote("Photosynthetically Active Radiation (W/m"^"2"*")")
legend("topright", legend = c("Temperature (°C)", lab_par,
                              lab_sif, "Vapor Pressure Deficit (kPa)", lab_precip),
       col = c("#117733", "#CC6677", "black", "#AA4499", "#648FFF80"), lty = c(1, 3, 1, 1, NA), 
       pch = c(NA, NA, NA, NA, 15), lwd = 3, cex = 2.15, y.intersp = 1.5)

dev.off()
