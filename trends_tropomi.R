library(terra)
library(Kendall)
library(stringr)
library(gsubfn)
library(trend)
library(forecast)

tropomi_csv_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/TROPOMI", pattern = "*.csv", full.names = TRUE)
# Remove Nigerian Lowland and Niger Delta Swamp as I combined them
tropomi_csv_list <- c(tropomi_csv_list[1:6], tropomi_csv_list[8], tropomi_csv_list[10:13])

dir_list    <- list.dirs("G:/TROPOMI/esa/extracted/Africa/ecoregions/masked", recursive = FALSE)
dir_list    <- c(dir_list[1:3], dir_list[5:7], dir_list[10], dir_list[12:15])

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

write.csv(df, "G:/Africa/csv/ecoregions/mask_Dans/TROPOMI_Daily_Mean/TROPOMI_Daily_Means_2019-2021.csv", row.names = FALSE)

## Daily PLOTS
x <- 1:length(dates)
year_labs <- c(2019, 2020, 2021)

cairo_pdf("G:/Africa/figs/trends_tropomi.pdf", width = 20, height = 15)

par(mfrow = c(4, 3), oma=c(1.0,0.1,1.25,0.1))

for (i in 1:1) {
  
  # Get ecoregion name
  title <- colnames(df)[(i + 1)]
  data  <- df[, (i + 1)]
  reg   <- lm(data ~ x)
  
  plot(x, data, type = "l", main = title, axes = FALSE, xlab = NA, ylab = NA, cex.main = 3, ylim = c(0.20, 1.0))
  
  rect(366, 0, 731, 100, col = rgb(0.85,0.85,0.85), border = NA)
  lines(x, data, lwd = 3)
  abline(reg, lwd = 3,col = "red")
  
  data_ts <- ts(data, start = c(2019, 1), end = c(2021, 365), 
                frequency = 365 + 1 * (!2019%%400 || ((2019%%100)&&!2019%%4)))

MannKendall(data_ts)
SeasonalMannKendall(data_ts)
  
  MannKendall(sif_series)
  SeasonalMannKendall(sif_series_ts)

  axis(1, labels = year_labs, at =  c(1, 366, 731), tck = 0.03, mgp=c(3, 1.5, 0), las = 1, cex.axis = 3)
  axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 3)
  box()
  
}

plot(x, df[, (1 + 1)])
reg <- lm(df[, (1 + 1)] ~ x)
abline(reg)

plot.new()
lab_sif     <- bquote("Values are SIFdaily 740nm (mW/m"^"2"*"/sr/nm)")
legend("topright", legend = c("TROPOMI SIF", "OCO-2 SIF", "OCO-3 SIF", lab_sif),
       col = c("black", oco2_col, oco3_col), lty = c(1, NA, NA, NA), pch = c(NA, 16, 16, NA), lwd = 3, cex = 2)

dev.off()

## Montly PLOTS
x <- 1:36
year_labs <- c(2019, 2020, 2021)

cairo_pdf("G:/Africa/figs/trends_tropomi_monthly.pdf", width = 20, height = 15)

par(mfrow = c(4, 3), oma=c(1.0,0.1,1.25,0.1))

for (i in 1:length(tropomi_csv_list)) {
  
  # Get ecoregion name
  if (i != 7) {
    f_name <- basename(tropomi_csv_list[i])
    tit    <- substr(f_name, 1, nchar(f_name) - 19)
    tit    <- substr(tit, 25, nchar(tit))
    tit    <- gsub("_", " ", tit)
    tit    <- str_to_title(tit)
    tit    <- str_sub(tit, end = -2)
  } else {
    tit    <- "Nigerian Lowland and Niger Delta Forest"
  }
  
  tropomi_df     <- read.csv(tropomi_csv_list[i], header = TRUE)
  tropomi_sifd   <- tropomi_df[,1]
  
  reg <- lm(tropomi_sifd ~ x)
  
  plot(x, tropomi_sifd, type = "l", main = tit, axes = FALSE, xlab = NA, ylab = NA, cex.main = 3, ylim = c(0.20, 0.8))
  
  rect(13, 0, 25, 100, col = rgb(0.85,0.85,0.85), border = NA)
  lines(x, tropomi_sifd, lwd = 3)
  abline(reg, lwd = 3,col = "red")
  
  data_ts <- ts(tropomi_sifd, start = c(2019, 1), frequency = 12)
  
  data_ts_desea <- seasadj(decompose(data_ts))
  
  MannKendall(data_ts)
  SeasonalMannKendall(data_ts)
  
  mk.test(data_ts)
  smk.test(data_ts)
  
  axis(1, labels = year_labs, at =  c(1, 366, 731), tck = 0.03, mgp=c(3, 1.5, 0), las = 1, cex.axis = 3)
  axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 3)
  box()
  
}

plot(x, df[, (1 + 1)])
reg <- lm(df[, (1 + 1)] ~ x)
abline(reg)

plot.new()
lab_sif     <- bquote("Values are SIFdaily 740nm (mW/m"^"2"*"/sr/nm)")
legend("topright", legend = c("TROPOMI SIF", "OCO-2 SIF", "OCO-3 SIF", lab_sif),
       col = c("black", oco2_col, oco3_col), lty = c(1, NA, NA, NA), pch = c(NA, 16, 16, NA), lwd = 3, cex = 2)

dev.off()



plot(sif_series_ts, type = "l")

sif_series_ts <- ts(sif_series, start = c(2019,1), end = c(2021, 365), frequency = 365)

MannKendall(sif_series)
SeasonalMannKendall(sif_series_ts)
