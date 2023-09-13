library(terra)
library(stringr)
library(gsubfn)

options(scipen = 999)

# Here we look at all equatorial forests
dir_list    <- list.dirs("G:/GOSAT_SIF/extracted/africa", recursive = FALSE)
dir_list    <- str_subset(dir_list, pattern = "Congo", negate = FALSE)
file_list   <- list.files(dir_list, full.names = TRUE, recursive = TRUE)

# Create dfs for all obs and clearsky
dates  <- seq(as.Date("2009/7/1"), as.Date("2020/6/1"), "months")
dates  <- format(dates, format = "%Y-%m")
df_all     <- data.frame(matrix(NA, nrow = length(dates), ncol = 5))
df_all[,1] <- dates
colnames(df_all) <- c("Dates", "SIF_740d", "n", "sd", "sem")
df_cs      <- df_all

for (i in 1:length(dates)) {
  message("Working on ", dates[i])
  
  day_files <- str_subset(file_list, pattern = toString(dates[i]), negate = FALSE)
  
  if (length(day_files) > 0) {
    
    for (j in 1:length(day_files)) {
      sif_file   <- vect(day_files[j])$Daily_SIF_740nm
      cloud_file <- vect(day_files[j])$cloud_flag_abp
      qc_file    <- vect(day_files[j])$qc
      mode_file  <- vect(day_files[j])$Mode
      
      if (j == 1) {
        sif_day   <- sif_file
        cloud_day <- cloud_file
        qc_day    <- qc_file
        mode_day  <- mode_file
      } else {
        sif_day   <- c(sif_day, sif_file)
        cloud_day <- c(cloud_day, cloud_file)
        qc_day    <- c(qc_day, qc_file)
        mode_day  <- c(mode_day, mode_file)
      }
    }
    
    # Filter for QC < 2 and observation mode 0 (nominal)
    sif_day_qc <- subset(sif_day, qc_day < 2 & mode_day == 0)
    
    # Filter for only clear sky
    sif_day_cs <- subset(sif_day, qc_day < 2 & mode_day == 0 & cloud_day %in% 0)
    
    # Stats for the day
    sif_day_mean    <- mean(sif_day_qc, na.rm = TRUE)
    sif_day_cs_mean <- mean(sif_day_cs, na.rm = TRUE)
    
    sif_day_n       <- length(sif_day_qc)
    sif_day_cs_n    <- length(sif_day_cs)
    
    sif_day_sd      <- sd(sif_day_qc, na.rm = TRUE)
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

write.csv(df_all, "G:/Africa/csv/ecoregions/mask_Dans/GOSAT_Congo_Monthly_Mean/GOSAT_Congo_Monthly_Means_2009-2020.csv", row.names = FALSE)
write.csv(df_cs, "G:/Africa/csv/ecoregions/mask_Dans/GOSAT_Congo_Monthly_Mean/GOSAT_Congo_Monthly_Means_2009-2020_cs.csv", row.names = FALSE)
