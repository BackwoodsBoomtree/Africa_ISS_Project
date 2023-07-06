library(terra)

evi_dirs  <- list.dirs("G:/MCD43A4/vis/africa/monthly", full.names = TRUE, recursive = FALSE)

# Remove unused ecoregion
evi_dirs  <- c(evi_dirs[1:6], evi_dirs[8:12])
evi_files <- list.files(evi_dirs, pattern = "*EVI", full.names = TRUE, recursive = TRUE)

evi_files <- as.data.frame(t(cbind.data.frame(split(evi_files, rep(1:60, times = length(evi_files)/60)), stringsAsFactors = F)))

# Remove 2018 and 2022
# evi_files <- evi_files[13:48,]

# Use 2020
evi_files <- evi_files[25:36,]

for (i in 1:ncol(evi_files)) {
  for (j in 1:12) {
    y1 <- rast(evi_files[j, i])
    # y2 <- rast(evi_files[i + 12, 1])
    # y3 <- rast(evi_files[i + 24, 1])
    
    # m_month  <- mean(y1, y2, y3, na.rm = TRUE)
    
    if (j == 1) {
      m_month <- y1
    } else {
      m_month <- c(m_month, y1)
    }
  }

  max_month <- which.max(m_month)
  min_month <- which.min(m_month)
  
  plot(max_month, main = i, col = grDevices::topo.colors(50))
  
  if (i == 1) {
    max_month_merge <- max_month
    min_month_merge <- min_month
  } else {
    max_month_merge <- mosaic(max_month_merge, max_month, fun = "max")
    min_month_merge <- mosaic(min_month_merge, min_month, fun = "max")
  }
}

plot(max_month_merge)

writeCDF(max_month_merge, "G:/MCD43A4/vis/africa/min_max_monthly_2020/max_monthly_2020_EVI.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(min_month_merge, "G:/MCD43A4/vis/africa/min_max_monthly_2020/min_monthly_2020_EVI.nc",
         missval = -9999, compression = 4, overwrite = TRUE)
