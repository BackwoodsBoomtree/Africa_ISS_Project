library(terra)

## Note: for some reason, VI calculation fails for 2022-12-13 and 2019-04-02
## for NECLF. Also NLF in 2019-07. Doing it here manually

out_dir   <- "G:/MCD43A4/vis/africa/test/test/daily/NECLF"
out_name  <- "MCD43A4.061_NECLF"

# Create output dirs
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
  print(paste0("Created ", out_dir))
}


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

dec_red <- rast("G:/MCD43A4/reflectance/africa/NECLF/NECLF_2021-2022.nc", subds = 1)[[712]]
dec_nir <- rast("G:/MCD43A4/reflectance/africa/NECLF/NECLF_2021-2022.nc", subds = 2)[[712]]
dec_blu <- rast("G:/MCD43A4/reflectance/africa/NECLF/NECLF_2021-2022.nc", subds = 3)[[712]]
dec_sw  <- rast("G:/MCD43A4/reflectance/africa/NECLF/NECLF_2021-2022.nc", subds = 4)[[712]]

apr_red <- rast("G:/MCD43A4/reflectance/africa/NECLF/NECLF_2018-2020.nc", subds = 1)[[457]]
apr_nir <- rast("G:/MCD43A4/reflectance/africa/NECLF/NECLF_2018-2020.nc", subds = 2)[[457]]
apr_blu <- rast("G:/MCD43A4/reflectance/africa/NECLF/NECLF_2018-2020.nc", subds = 3)[[457]]
apr_sw  <- rast("G:/MCD43A4/reflectance/africa/NECLF/NECLF_2018-2020.nc", subds = 4)[[457]]

calc_evi(dec_red, dec_nir, dec_blu)
calc_ndvi(dec_red, dec_nir)
calc_nirv(dec_red, dec_nir)
calc_lswi(dec_nir, dec_sw)
calc_red(dec_red)
calc_nir(dec_nir)

calc_evi(apr_red, apr_nir, apr_blu)
calc_ndvi(apr_red, apr_nir)
calc_nirv(apr_red, apr_nir)
calc_lswi(apr_nir, apr_sw)
calc_red(apr_red)
calc_nir(apr_nir)
