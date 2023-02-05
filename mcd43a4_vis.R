
library(terra)

in_dir    <- "G:/MCD43A4/reflectance/africa/EGF"
out_dir   <- "G:/MCD43A4/vis/africa/EGF"
out_name  <- "MCD43A4.061_"
lc_mask   <- vect("G:/Africa/Forest_Masks/dissolved/Africa_merged_2019_2.5km_Buffer.shp")

file_list <- list.files(in_dir, pattern = "*.nc", full.names = TRUE)

# Create output dirs
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
  print(paste0("Created ", out_dir))
}

save_vis  <- function(f, vi, in_dir, out_dir, land_mask) {
  # FUNCTIONS ##
  calc_evi     <- function(b1, b2, b3) {
    index.start.time <- Sys.time()
    index            <- 2.5 * (b2 - b1) / (b2 + 6 * b1 - 7.5 * b3 + 1)
    index[index > 1] <- NA
    index[index < 0] <- NA
    index            <- round(index, digits = 4) * 10000
    gc()
    
    base_name  <- basename(file_list[1])
    base_name  <- gsub(pattern = "\\.nc$", "",base_name)
    save_name  <- paste0(out_dir, "/", out_name, base_name, "_EVI", ".nc")
    
    writeCDF(index, filename = save_name, varname = "EVI", unit = "", compression = 4, missval = -9999, overwrite = TRUE)
    
    index.end.time <- Sys.time()
    index.time.taken <- index.end.time - index.start.time
    print(paste0("EVI finished. Took ", index.time.taken))
  }
  calc_ndvi    <- function(b1, b2) {
    index.start.time <- Sys.time()
    index            <- (b2 - b1) / (b2 + b1)
    index[index > 1] <- NA
    index[index < 0] <- NA
    index            <- round(index, digits = 4) * 10000
    gc()
    
    base_name  <- basename(file_list[1])
    base_name  <- gsub(pattern = "\\.nc$", "",base_name)
    save_name  <- paste0(out_dir, "/", out_name, base_name, "_NDVI", ".nc")
    
    writeCDF(index, filename = save_name, varname = "NDVI", unit = "", compression = 4, missval = -9999, overwrite = TRUE)
    
    index.end.time <- Sys.time()
    index.time.taken <- index.end.time - index.start.time
    print(paste0("NDVI finished. Took ", index.time.taken))
  }
  calc_nirv    <- function(b1, b2) {
    index.start.time <- Sys.time()
    index            <- (b2 - b1) / (b2 + b1) * b2
    index[index > 1] <- NA
    index[index < 0] <- NA
    index            <- round(index, digits = 4) * 10000
    gc()
    
    base_name  <- basename(file_list[1])
    base_name  <- gsub(pattern = "\\.nc$", "",base_name)
    save_name  <- paste0(out_dir, "/", out_name, base_name, "_NIRv", ".nc")
    
    writeCDF(index, filename = save_name, varname = "NIRv", unit = "", compression = 4, missval = -9999, overwrite = TRUE)
    
    index.end.time <- Sys.time()
    index.time.taken <- index.end.time - index.start.time
    print(paste0("NIRv finished. Took ", index.time.taken))
  }
  calc_lswi    <- function(b2, b6) {
    index.start.time <- Sys.time()
    index             <- (b2 - b6) / (b2 + b6)
    index[index > 1]  <- NA
    index[index < -1] <- NA
    index             <- round(index, digits = 4) * 10000
    gc()
    
    base_name  <- basename(file_list[1])
    base_name  <- gsub(pattern = "\\.nc$", "",base_name)
    save_name  <- paste0(out_dir, "/", out_name, base_name, "_LSWI", ".nc")
    
    writeCDF(index, filename = save_name, varname = "LSWI", unit = "", compression = 4, missval = -9999, overwrite = TRUE)
    
    index.end.time <- Sys.time()
    index.time.taken <- index.end.time - index.start.time
    print(paste0("LSWI finished. Took ", index.time.taken))
  }
  calc_red     <- function(b1) {
    index             <- b1
    index             <- round(index, digits = 4) * 10000
    gc()
    
    base_name  <- basename(file_list[1])
    base_name  <- gsub(pattern = "\\.nc$", "",base_name)
    save_name  <- paste0(out_dir, "/", out_name, base_name, "_RED", ".nc")
    
    writeCDF(index, filename = save_name, varname = "RED", unit = "", compression = 4, missval = -9999, overwrite = TRUE)
  }
  calc_nir     <- function(b2) {
    index             <- b2
    index             <- round(index, digits = 4) * 10000
    gc()
    
    base_name  <- basename(file_list[1])
    base_name  <- gsub(pattern = "\\.nc$", "",base_name)
    save_name  <- paste0(out_dir, "/", out_name, base_name, "_NIR", ".nc")
    
    writeCDF(index, filename = save_name, varname = "NIR", unit = "", compression = 4, missval = -9999, overwrite = TRUE)
  }
  
  band1 <- rast(f, subds = 1)
  band2 <- rast(f, subds = 2)
  band3 <- rast(f, subds = 3)
  band6 <- rast(f, subds = 6)
  
  # ~ 3 times faster to crop lcmask first
  lc_maskc <- crop(lc_mask, band1)
  
  # Mask
  for (i in 1:nlyr(band1)) {
    start.time <- Sys.time()
    clipb1 <- mask(band1[[i]], lc_maskc)
    clipb2 <- mask(band2[[i]], lc_maskc)
    clipb3 <- mask(band3[[i]], lc_maskc)
    clipb6 <- mask(band6[[i]], lc_maskc)
    
    if (i == 1) {
      b1 <- clipb1
      b2 <- clipb2
      b3 <- clipb3
      b6 <- clipb6
    } else {
      b1 <- c(b1, clipb1)
      b2 <- c(b2, clipb2)
      b3 <- c(b3, clipb3)
      b6 <- c(b6, clipb6)
    }
    end.time <- Sys.time()
    print(paste0("Clipped ", time(b1[[i]])))
    time.taken <- end.time - start.time
    print(time.taken)
  }
  calc_evi(b1, b2, b3)
  calc_ndvi(b1, b2)
  calc_nirv(b1, b2)
  calc_lswi(b2, b6)
  calc_red(b1)
  calc_nir(b2)
}