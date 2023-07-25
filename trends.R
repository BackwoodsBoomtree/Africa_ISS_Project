library(terra)
library(Kendall)
library(stringr)
library(gsubfn)

dir_list    <- list.dirs("G:/TROPOMI/esa/extracted/Africa/ecoregions/masked", recursive = FALSE)
dir_list    <- c(dir_list[1:3], dir_list[6:8], dir_list[10], dir_list[12:15])

df    <- data.frame(matrix(NA, nrow = 1096, ncol = 12))
dates <- seq(as.Date("2019/1/1"), as.Date("2021/12/31"), "days")
df[,1] <- dates
colnames(df)[1] <- "Dates"

for (i in 1:length(dir_list)) {
  message("Working on directory ", i, "\n")
  file_list <- list.files(dir_list[i], pattern = "*.nc", full.names = TRUE)
  file_list <- str_subset(file_list, pattern = "2018", negate = TRUE)

  for (j in 1:length(file_list)) {
    # Get SIF
    sif_day <- vect(file_list[j])$SIF_Corr_743
    sif_day <- mean(sif_day, na.rm = TRUE)
    
    # Get file date
    file_date <- strapplyc(file_list[j], "[0-9-]{8,}", simplify = TRUE)
    file_date <- as.Date(file_date)
    
    # Index the date and insert to that row number
    date_index <- match(file_date, dates)
    
    df[date_index, (i + 1)] <- sif_day
  
  title <- gsub("_", " ", basename(dir_list[i]))
  title <- str_to_title(title)
  colnames(df)[(i + 1)] <- title
  }
}




plot(sif_series_ts, type = "l")

sif_series_ts <- ts(sif_series, start = c(2019,1), end = c(2021, 365), frequency = 365)

MannKendall(sif_series)
SeasonalMannKendall(sif_series_ts)
