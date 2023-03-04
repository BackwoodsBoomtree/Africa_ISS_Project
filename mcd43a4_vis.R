
library(terra)

# NOTE: No forest for CNTF or MCBMF when using Africa_merged_2019_2.5km_Buffer.shp
in_dir    <- "G:/MCD43A4/reflectance/africa/NECLF"
out_dir   <- "G:/MCD43A4/vis/africa/NECLF"
# vi_list   <- c("EVI", "NDVI", "NIRv", "LSWI", "RED", "NIR")
vi_list   <- c("LSWI")
out_name  <- "MCD43A4.061_NECLF"
lc_mask   <- vect("G:/Africa/Forest_Masks/dissolved/Africa_merged_2019_2.5km_Buffer.shp")

file_list <- list.files(in_dir, pattern = "*.nc", full.names = TRUE)

# Create output dirs
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
  print(paste0("Created ", out_dir))
}

save_vis  <- function(f, in_dir, out_dir, out_name, land_mask, vi_list) {
  # FUNCTIONS ##
  calc_evi     <- function(b1, b2, b3) {
    index            <- 2.5 * (b2 - b1) / (b2 + 6 * b1 - 7.5 * b3 + 1)
    index[index > 1] <- NA
    index[index < 0] <- NA
    index            <- round(index, digits = 4) * 10000
    
    date       <- time(b1)
    save_name  <- paste0(out_dir, "/", out_name, "_", date, "_EVI", ".nc")
    
    writeCDF(index, filename = save_name, varname = "EVI", unit = "", compression = 4, missval = -9999, overwrite = TRUE)
  }
  calc_ndvi    <- function(b1, b2) {
    index            <- (b2 - b1) / (b2 + b1)
    index[index > 1] <- NA
    index[index < 0] <- NA
    index            <- round(index, digits = 4) * 10000
    
    date       <- time(b1)
    save_name  <- paste0(out_dir, "/", out_name, "_", date, "_NDVI", ".nc")
    
    writeCDF(index, filename = save_name, varname = "NDVI", unit = "", compression = 4, missval = -9999, overwrite = TRUE)
  }
  calc_nirv    <- function(b1, b2) {

    index            <- (b2 - b1) / (b2 + b1) * b2
    index[index > 1] <- NA
    index[index < 0] <- NA
    index            <- round(index, digits = 4) * 10000
    
    date       <- time(b1)
    save_name  <- paste0(out_dir, "/", out_name, "_", date, "_NIRv", ".nc")
    
    writeCDF(index, filename = save_name, varname = "NIRv", unit = "", compression = 4, missval = -9999, overwrite = TRUE)
  }
  calc_lswi    <- function(b2, b6) {

    index             <- (b2 - b6) / (b2 + b6)
    index[index > 1]  <- NA
    index[index < -1] <- NA
    index             <- round(index, digits = 4) * 10000
    
    date       <- time(b2)
    save_name  <- paste0(out_dir, "/", out_name, "_", date, "_LSWI", ".nc")
    
    writeCDF(index, filename = save_name, varname = "LSWI", unit = "", compression = 4, missval = -9999, overwrite = TRUE)
  }
  calc_red     <- function(b1) {
    index             <- b1
    index             <- round(index, digits = 4) * 10000
    
    date       <- time(b1)
    save_name  <- paste0(out_dir, "/", out_name, "_", date, "_RED", ".nc")
    
    writeCDF(index, filename = save_name, varname = "RED", unit = "", compression = 4, missval = -9999, overwrite = TRUE)
  }
  calc_nir     <- function(b2) {
    index             <- b2
    index             <- round(index, digits = 4) * 10000
    
    date       <- time(b2)
    save_name  <- paste0(out_dir, "/", out_name, "_", date, "_NIR", ".nc")
    
    writeCDF(index, filename = save_name, varname = "NIR", unit = "", compression = 4, missval = -9999, overwrite = TRUE)
  }
  
  band1 <- rast(f, subds = 1)
  band2 <- rast(f, subds = 2)
  band3 <- rast(f, subds = 3)
  band6 <- rast(f, subds = 4)
  
  # ~ 3 times faster to crop lcmask first
  lc_maskc <- crop(lc_mask, band1)
  
  # Run for each time step
  for (i in 1:nlyr(band1)) {
    start.time <- Sys.time()
    
    # Load only the bands needed
    if ("EVI" %in% vi_list) {
      b3 <- mask(band3[[i]], lc_maskc)
    }
    
    if ("LSWI" %in% vi_list) {
      b6 <- mask(band6[[i]], lc_maskc)
    }
    
    if ("EVI" %in% vi_list || "NDVI" %in% vi_list || "NIRv" %in% vi_list || "LSWI" %in% vi_list || "NIR" %in% vi_list) {
      b2 <- mask(band2[[i]], lc_maskc)
    }
    
    if ("EVI" %in% vi_list || "NDVI" %in% vi_list || "NIRv" %in% vi_list || "RED" %in% vi_list) {
      b1 <- mask(band1[[i]], lc_maskc)
    }
    
    # Run for only VIs in list
    if ("EVI" %in% vi_list) {
      calc_evi(b1, b2, b3)
    } else if ("NDVI" %in% vi_list) {
      calc_ndvi(b1, b2)
    } else if ("LSWI" %in% vi_list) {
      calc_lswi(b2, b6)
    } else if ("NIRv" %in% vi_list) {
      calc_nirv(b1, b2)
    } else if ("RED" %in% vi_list) {
      calc_red(b1)
    } else if ("NIR" %in% vi_list) {
      calc_nir(b2)
    }
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste0("Finished ", vi_list, " for ", time(b2), " in ", time.taken))
    gc()
  }

}

for (i in 1:length(file_list)) {
  save_vis(file_list[i], in_dir, out_dir, out_name, lc_mask, vi_list)
}