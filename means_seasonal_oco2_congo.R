library(terra)
library(stringr)
library(gsubfn)

# Here we look at all equatorial forests
dir_list    <- list.dirs("G:/OCO2/B10/extracted/africa", recursive = FALSE)
dir_list    <- str_subset(dir_list, pattern = "Congo", negate = FALSE)
file_list   <- list.files(dir_list, full.names = TRUE, recursive = TRUE)

# Create dfs for all obs and clearsky
dates   <- seq(as.Date("2015/1/1"), as.Date("2021/12/1"), "months")
dates   <- format(dates, format = "%Y-%m")
seasons <- c(dates[1], dates[seq(3, (length(dates) - 1), 3)])
df_all     <- data.frame(matrix(NA, nrow = length(seasons), ncol = 5))
df_all[,1] <- seasons
colnames(df_all) <- c("Dates", "SIF_740d", "n", "sd", "sem")
df_cs      <- df_all

index <- seq(0, 83, by = 3)

for (i in 1:28) {
  
  if (i == 1) {
    months    <- c(toString(dates[12]), toString(dates[1]), toString(dates[2]))
    day_files <- str_subset(file_list, paste0(months, collapse = '|'))
    
  } else {
    months    <- c(toString(dates[index[i]]), toString(dates[index[i] + 1]), toString(dates[index[i] + 2]))
    day_files <- str_subset(file_list, paste0(months, collapse = '|'))
  }
  
  message(paste0("Working on months ", paste0(months, collapse = " "), ". Length of file list is: ", length(day_files)))
  
  if (length(day_files) > 0) {
    
    for (j in 1:length(day_files)) {
      sif_file   <- vect(day_files[j])$Daily_SIF_740nm
      cloud_file <- vect(day_files[j])$cloud_flag_abp
      
      if (j == 1) {
        sif_day   <- sif_file
        cloud_day <- cloud_file
      } else {
        sif_day   <- c(sif_day, sif_file)
        cloud_day <- c(cloud_day, cloud_file)
      }
    }
    
    # Filter for only clear sky
    sif_day_cs <- subset(sif_day, cloud_day %in% 0)
    
    # Stats for the day
    sif_day_mean    <- mean(sif_day, na.rm = TRUE)
    sif_day_cs_mean <- mean(sif_day_cs, na.rm = TRUE)
    
    sif_day_n       <- length(sif_day)
    sif_day_cs_n    <- length(sif_day_cs)
    
    sif_day_sd      <- sd(sif_day, na.rm = TRUE)
    sif_day_cs_sd   <- sd(sif_day_cs, na.rm = TRUE)
    
    sif_day_sem     <- sif_day_sd    / sqrt(sif_day_n)
    sif_day_cs_sem  <- sif_day_cs_sd / sqrt(sif_day_cs_n)
    
    df_all[i,2:5] <- c(sif_day_mean, sif_day_n, sif_day_sd, sif_day_sem)
    df_cs[i,2:5]  <- c(sif_day_cs_mean, sif_day_cs_n, sif_day_cs_sd, sif_day_cs_sem)
    
  } else {
    
    df_all[i,2:5] <- c(NA, NA, NA, NA)
    df_cs[i,2:5]  <- c(NA, NA, NA, NA)
  }
}

write.csv(df_all, "G:/Africa/csv/ecoregions/mask_Dans/OCO2_Congo_Seasonal_Mean/OCO2_Congo_Seasonal_Means_2015-2021.csv", row.names = FALSE)
write.csv(df_cs, "G:/Africa/csv/ecoregions/mask_Dans/OCO2_Congo_Seasonal_Mean/OCO2_Congo_Seasonal_Means_2015-2021_cs.csv", row.names = FALSE)
