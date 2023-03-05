
library(terra)
library(lubridate)
library(stringr)

in_dir    <- "G:/MCD43A4/vis/africa/daily"
out_dir   <- "G:/MCD43A4/vis/africa/monthly"
# vi_list   <- c("EVI", "NDVI", "NIRv", "LSWI", "RED", "NIR")
vi_list   <- c("EVI", "NDVI", "NIRv", "LSWI")

to_month <- function(in_dir, out_dir, vi_list) {
  
  dir_list <- list.dirs(in_dir, full.names = TRUE, recursive = FALSE)
  
  for (d in 1:length(dir_list)) {
    
    for (v in 1:length(vi_list)) {
      
      vi_dir <- paste0(out_dir, "/", basename(dir_list[d]))
      
      # Create output dirs
      if (!dir.exists(vi_dir)) {
        dir.create(vi_dir, recursive = TRUE)
        print(paste0("Created ", vi_dir))
      }
      
      vi_file_list  <- list.files(dir_list[d], pattern = paste0("*", vi_list[v], ".nc"), full.names = TRUE)
      
      # Get list of dates (ymd does not like underscores or other numbers)
      date_list <- gsub("_", ".", substring(basename(vi_file_list), 13))
      date_list <- unique(ymd(date_list))
      
      # List of years
      year_list <- unique(year(date_list))
      
      for (y in 1:length(year_list)) {
        
        # List of files for the year
        vi_file_list_year <- vi_file_list[grepl(year_list[y], vi_file_list)]
        
        # List of year-month for the year
        ym_list <- date_list[grepl(year_list[y], date_list)]
        ym_list <- unique(format(ym_list, "%Y-%m"))
        
        for (m in 1:length(ym_list)) {
          
          print(paste0("Working on ", ym_list[m], " for ", vi_list[v], " in ", basename(dir_list[d])))
          
          vi_file_list_month <- vi_file_list_year[grepl(ym_list[m], vi_file_list_year)]
          
          out_raster <- rast(vi_file_list_month)
          
          out_raster <- app(out_raster, mean, na.rm = TRUE)
          
          out_name   <- paste0(vi_dir, "/MCD43A4.061_", basename(dir_list[d]), "_", ym_list[m], "_", vi_list[v], ".nc")
          
          writeCDF(out_raster, filename = out_name, varname = vi_list[v], unit = "", compression = 4, missval = -9999, overwrite = TRUE)
          
          print(paste0("Saved ", out_name))
        }
      }
    }
  }
}

to_month(in_dir, out_dir, vi_list)