
library(terra)
library(lubridate)

in_dir    <- "G:/MCD43A4/vis/africa/monthly"
out_dir   <- "G:/Africa/csv/ecoregions/mask_Dans"
vi_list   <- c("EVI", "NDVI", "NIRv", "LSWI", "RED", "NIR")
y_range   <- "2018-2022"

ts_csv <- function(in_dir, out_dir, vi_list, y_range) {
  
  dir_list <- list.dirs(in_dir, full.names = TRUE, recursive = FALSE)
  
  for (d in 1:length(dir_list)) {
    
    print(paste0("Working on ", basename(dir_list[d])))
    
    for (v in 1:length(vi_list)) {
      
      save_dir <- paste0(out_dir, "/MCD43A4_", vi_list[v], "/monthly/", basename(dir_list[d]))
      
      # Create output dirs
      if (!dir.exists(save_dir)) {
        dir.create(save_dir, recursive = TRUE)
      }
      
      print(paste0("Working on ", vi_list[v], " for ", basename(dir_list[d])))
      
      vi_file_list <- list.files(dir_list[d], pattern = paste0("\\_", vi_list[v], "\\."), full.names = TRUE)
      
      # Get list of dates (ymd does not like underscores or other numbers)
      date_list <- gsub("_", ".", substring(basename(vi_file_list), 13))
      date_list <- format(unique(ym(date_list)), "%Y-%m")
      
      ts <- global(rast(vi_file_list), "mean", na.rm = TRUE)
      
      df <- data.frame(date_list, ts[,1])
      colnames(df) <- c("Date", vi_list[v])
      
      out_name <- paste0(save_dir, "/", "MCD43A4.061_", basename(dir_list[d]), "_", y_range, "_monthly_", vi_list[v], ".csv")
      
      write.csv(df, out_name, row.names = FALSE, col.names = TRUE)
      
      print(paste0("Saved ", basename(out_name), ". Number of rows:  ", nrow(df)))
    }
  }
}

ts_csv(in_dir, out_dir, vi_list, y_range)