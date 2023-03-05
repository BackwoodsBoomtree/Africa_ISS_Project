
library(stringr)

in_dir     <- "G:/Africa/csv/ecoregions/mask_Dans"
vi_list    <- c("EVI", "LSWI", "NDVI", "NIRv") # Alphabetical
# NOTE: No forest for CNTF or MCBMF when using Africa_merged_2019_2.5km_Buffer.shp
# Ordered to match TROPOMI figure
eco_list   <- c("AECF", "CHF", "CCLF", "CSBCF", "ECSF", "EGF", "NDSF",
                "NLF", "NECLF", "NWCLF", "WCSF", "WGLF")
month_labs <- format(seq(as.Date("2018/1/1"), as.Date("2022/12/1"), "month"), "%Y-%m")
year_labs  <- c(2018, 2019, 2020, 2021,2022)
cols       <- c("#FE6100", "#648FFF", "#785EF0", "#DC267F")
x          <- 1:60
  
# Get only the directories for VIs
mcd_dir_list <- list.dirs(in_dir, full.names = TRUE, recursive = FALSE)
mcd_dir_list <- grep("*MCD43A4", mcd_dir_list, value = TRUE)
mcd_dir_list <- grep(paste(vi_list, collapse = "|"), mcd_dir_list, value = TRUE)

# All CSVs
csv_list   <- list.files(mcd_dir_list, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

# PLOTT

cairo_pdf("G:/Africa/figs/timeseries_vis_ecoregions_2.5km_mask.pdf", width = 20, height = 15)

par(mfrow = c(4, 3), oma=c(1.0,0.1,1.25,0.1))

for (i in 1:length(eco_list)) {
  
  eco_csv_list <- grep(paste0("*", eco_list[i]), csv_list, value = TRUE)
  
  df <- read.csv(eco_csv_list[1], header = TRUE)
  vi <- df[,2] / 10000
  
  plot(x, vi, type = "l", main = eco_list[i], axes = FALSE, xlab = NA, ylab = NA, cex.main = 3, ylim = c(0, 1))
  rect(13, -1, 24, 100, col = rgb(0.85,0.85,0.85), border = NA)
  rect(37, -1, 48, 100, col = rgb(0.85,0.85,0.85), border = NA)
  
  if (length(eco_csv_list) > 1) {
    for (e in 1:length(eco_csv_list)) {
      df <- read.csv(eco_csv_list[e], header = TRUE)
      vi <- df[,2] / 10000
      lines(x, vi, lwd = 2, col = cols[e])
    }
  }

  # axis(1, labels = month_labs, at =  c(1:36), tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
  # mtext(1, text = "Month", line = 3.75)
  # mtext(2, text = "SIFdaily", line = 3.75)
  axis(1, labels = year_labs, at =  c(6.5, 18.5, 30.5, 42.5, 54.5), mgp=c(3, 1.5, 0), las = 1, cex.axis = 3)
  axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 3)
  
  if (i == 1) {
    legend("topleft", legend = vi_list, col = cols, horiz = TRUE, lty = 1, cex = 1.5, lwd = 2)
  }
  box()
}

dev.off()
