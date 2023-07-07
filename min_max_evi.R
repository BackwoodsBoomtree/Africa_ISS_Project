library(terra)

evi_dirs  <- list.dirs("G:/MCD43A4/vis/africa/monthly", full.names = TRUE, recursive = FALSE)

# Remove unused ecoregion
evi_dirs  <- c(evi_dirs[1:6], evi_dirs[8:12])
evi_files <- list.files(evi_dirs, pattern = "*EVI", full.names = TRUE, recursive = TRUE)

evi_files <- as.data.frame(t(cbind.data.frame(split(evi_files, rep(1:60, times = length(evi_files)/60)), stringsAsFactors = F)))

# Remove 2018 and 2022
evi_files <- evi_files[13:48,]

# Use 2020
# evi_files <- evi_files[25:36,]

for (i in 1:ncol(evi_files)) {
  for (j in 1:12) {
    y1_month <- rast(evi_files[j, i])
    y2_month <- rast(evi_files[j + 12, i])
    y3_month <- rast(evi_files[j + 24, i])
    
    # Get mean across all three years
    m  <- mean(y1_month, y2_month, y3_month, na.rm = TRUE)
    
    if (j == 1) {
      m_month <- m
      y1      <- y1_month
      y2      <- y2_month
      y3      <- y3_month
    } else {
      m_month <- c(m_month, m)
      y1      <- c(y1, y1_month)
      y2      <- c(y2, y2_month)
      y3      <- c(y3, y3_month)
    }
  }

  max_month <- which.max(m_month)
  min_month <- which.min(m_month)
  
  max_y1 <- which.max(y1)
  min_y1 <- which.min(y1)
  
  max_y2 <- which.max(y2)
  min_y2 <- which.min(y2)
  
  max_y3 <- which.max(y3)
  min_y3 <- which.min(y3)
  
  # We do not want to compute min/max for gridcells that do not have values for all 12 months
  # create an NA mask
  na_mask          <- app(m_month, fun = sum, na.rm = FALSE)
  max_month_masked <- mask(max_month, na_mask)
  min_month_masked <- mask(min_month, na_mask)
  m_month_masked   <- mask(m_month, na_mask)
  
  # Do the same for each year
  na_mask       <- app(y1, fun = sum, na.rm = FALSE)
  max_y1_masked <- mask(max_y1, na_mask)
  min_y1_masked <- mask(min_y1, na_mask)
  
  na_mask       <- app(y2, fun = sum, na.rm = FALSE)
  max_y2_masked <- mask(max_y2, na_mask)
  min_y2_masked <- mask(min_y2, na_mask)
  
  na_mask       <- app(y3, fun = sum, na.rm = FALSE)
  max_y3_masked <- mask(max_y3, na_mask)
  min_y3_masked <- mask(min_y3, na_mask)
  
  if (i == 1) {
    max_month_merge <- max_month_masked
    min_month_merge <- min_month_masked
    max_y1_merge <- max_y1_masked
    min_y1_merge <- min_y1_masked
    max_y2_merge <- max_y2_masked
    min_y2_merge <- min_y2_masked
    max_y3_merge <- max_y3_masked
    min_y3_merge <- min_y3_masked
    all_merge    <- m_month_masked
  } else {
    # We are using mosaic instead of merge because merge keeps NA values,
    # and we can get around this using mosaic and max argument
    # because the data does not overlap
    max_month_merge <- mosaic(max_month_merge, max_month_masked, fun = "max")
    min_month_merge <- mosaic(min_month_merge, min_month_masked, fun = "max")
    max_y1_merge <- mosaic(max_y1_merge, max_y1_masked, fun = "max")
    min_y1_merge <- mosaic(min_y1_merge, min_y1_masked, fun = "max")
    max_y2_merge <- mosaic(max_y2_merge, max_y2_masked, fun = "max")
    min_y2_merge <- mosaic(min_y2_merge, min_y2_masked, fun = "max")
    max_y3_merge <- mosaic(max_y3_merge, max_y3_masked, fun = "max")
    min_y3_merge <- mosaic(min_y3_merge, min_y3_masked, fun = "max")
    all_merge    <- mosaic(all_merge, m_month_masked, fun = "max")
  }
}

# Min-Max normalize the series
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

all_merge_norm <- app(all_merge, fun = min_max_norm)

# plot(max_month_merge)

writeCDF(max_month_merge, "G:/MCD43A4/vis/africa/min_max_monthly/max_monthly_2019-2021_EVI.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(min_month_merge, "G:/MCD43A4/vis/africa/min_max_monthly/min_monthly_2019-2021_EVI.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(max_y1_merge, "G:/MCD43A4/vis/africa/min_max_monthly/max_monthly_2019_EVI.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(min_y1_merge, "G:/MCD43A4/vis/africa/min_max_monthly/min_monthly_2019_EVI.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(max_y2_merge, "G:/MCD43A4/vis/africa/min_max_monthly/max_monthly_2020_EVI.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(min_y2_merge, "G:/MCD43A4/vis/africa/min_max_monthly/min_monthly_2020_EVI.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(max_y3_merge, "G:/MCD43A4/vis/africa/min_max_monthly/max_monthly_2021_EVI.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(min_y3_merge, "G:/MCD43A4/vis/africa/min_max_monthly/min_monthly_2021_EVI.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(all_merge, "G:/MCD43A4/vis/africa/min_max_monthly/monthly_2019-2021_EVI.nc",
         missval = -9999, compression = 4, overwrite = TRUE)

writeCDF(all_merge_norm, "G:/MCD43A4/vis/africa/min_max_monthly/monthly_2019-2021_EVI_norm.nc",
         missval = -9999, compression = 4, overwrite = TRUE)
