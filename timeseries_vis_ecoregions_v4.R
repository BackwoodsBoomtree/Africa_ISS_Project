
library(stringr)

in_dir           <- "G:/Africa/csv/ecoregions/mask_Dans"
tropomi_csv_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/TROPOMI", pattern = "*.csv", full.names = TRUE)
precip_csv       <- read.csv("G:/Africa/csv/precip/TropicalAfricaMonthlyPrecipPerEcoregionESAMask04252023.csv", header = TRUE)
vi_list          <- c("EVI", "NDVI") # Alphabetical
# NOTE: No forest for CNTF or MCBMF when using Africa_merged_2019_2.5km_Buffer.shp
# Ordered to match TROPOMI figure
eco_list         <- c("AECF", "CHF", "CCLF", "CSBCF", "ECSF", "EGF",
                      "NLNDSF", "NECLF", "NWCLF", "WCSF", "WGLF")
month_labs       <- format(seq(as.Date("2019/1/1"), as.Date("2021/12/1"), "month"), "%Y-%m")
year_labs        <- c(2019, 2020, 2021)
cols             <- c("#85B997", "#117733", "#9CB7FF")
x                <- 1:36

# Remove Nigerian Lowland and Niger Delta Swamp as I combined them
tropomi_csv_list <- c(tropomi_csv_list[1:6], tropomi_csv_list[8], tropomi_csv_list[10:13])

# Sort Precip df and remove
precip_df <- precip_csv[order(precip_csv$ecoregion),]
precip_df$ecoregion[precip_df$ecoregion == "Niger Delta swamp and Nigerian lowland forests"] <- "Nigerian Lowland and Niger Delta Swamp Forest"
row.names(precip_df) <- 1:nrow(precip_df)
  
# Get only the directories for VIs
mcd_dir_list <- list.dirs(in_dir, full.names = TRUE, recursive = FALSE)
mcd_dir_list <- grep("*MCD43A4", mcd_dir_list, value = TRUE)
mcd_dir_list <- grep(paste(vi_list, collapse = "|"), mcd_dir_list, value = TRUE)

# All CSVs
csv_list   <- list.files(mcd_dir_list, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

# PLOT

cairo_pdf("G:/Africa/figs/timeseries_vis_ecoregions_2.5km_mask_v4.pdf", width = 20, height = 15)

par(mfrow = c(4, 3), oma=c(1.0,0.1,1.25,0.1))

op <- par(mar = c(4.5,4,3,5))

index <- seq(1, 396, by = 36) # For precip

for (i in 1:length(eco_list)) {
  
  eco_csv_list <- grep(paste0("*", eco_list[i]), csv_list, value = TRUE)
  
  # VI data
  df <- read.csv(eco_csv_list[1], header = TRUE)
  vi <- df[,2] / 10000
  vi <- c(vi[13:48]) # Get 2019-2021
  
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
  
  plot(x, vi, type = "l", main = tit, axes = FALSE, xlab = NA, ylab = NA, cex.main = 3, ylim = c(0, 1))
  rect(13, -1, 25, 100, col = rgb(0.85,0.85,0.85), border = NA)
  
  par(new = TRUE)
  barplot(precip, lwd = 3, col = cols[3], axes = FALSE, xlab = "", ylab = "", ylim = c(0, 1100), space = 0, border = NA)
  axis(4, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 3, at = seq(0, 600, by = 100),
       col = cols[3],  col.lab = cols[3],  col.axis = cols[3])
  
  if (length(eco_csv_list) > 1) {
    for (e in 1:length(eco_csv_list)) {
      df <- read.csv(eco_csv_list[e], header = TRUE)
      vi <- df[,2] / 10000
      vi <- c(vi[13:48]) # Get 2019-2021
      par(new = TRUE)
      
      if (e == 1) {
        plot(x, vi, type = "l", main = NA, axes = FALSE, xlab = NA, ylab = NA, cex.main = 3, ylim = c(0, 1), col = cols[e], lwd = 3)
      } else {
        plot(x, vi, type = "l", main = NA, axes = FALSE, xlab = NA, ylab = NA, cex.main = 3, ylim = c(0, 1), col = cols[e], lwd = 3, lty = 2)
      }
      
    }
  }
  
  lines(x, tropomi_sifd, lwd = 3)

  # axis(1, labels = month_labs, at =  c(1:36), tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
  # mtext(1, text = "Month", line = 3.75)
  # mtext(2, text = "SIFdaily", line = 3.75)
  axis(1, labels = year_labs, at =  c(1, 13, 25), mgp=c(3, 1.5, 0), tck = 0.03, las = 1, cex.axis = 3)
  axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 3)
  
  box()
  
}

plot.new()
lab_sif     <- bquote("TROPOMI SIFdaily 740 nm (mW/m"^"2"*"/sr/nm)")
lab_precip  <- bquote("Mean total monthly precipiptation (mm)")
legend("topright", legend = c("NDVI", "EVI", lab_sif, lab_precip),
       col = c(cols[2], cols[1], "black", cols[3]), lty = c(2, 1, 1, NA), 
       pch = c(NA, NA, NA, 15), lwd = 3, cex = 2)

dev.off()
