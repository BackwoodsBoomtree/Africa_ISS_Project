
library(stringr)

tropomi_csv_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/TROPOMI", pattern = "*.csv", full.names = TRUE)
oco2_csv_list    <- list.files("G:/Africa/csv/ecoregions/mask_Dans/OCO2", pattern = "*.csv", full.names = TRUE)
oco3_csv_list    <- list.files("G:/Africa/csv/ecoregions/mask_Dans/OCO3", pattern = "*.csv", full.names = TRUE)
month_labs <- format(seq(as.Date("2019/1/1"), as.Date("2021/12/1"), "month"), "%Y-%m")
year_labs  <- c(2019, 2020, 2021)
x          <- 1:36
err_col    <- "#8ab8e6"

# Remove Nigerian Lowland and Niger Delta Swamp as I combined them
tropomi_csv_list <- c(tropomi_csv_list[1:6], tropomi_csv_list[8], tropomi_csv_list[10:13])
oco2_csv_list    <- c(oco2_csv_list[1:6], oco2_csv_list[8:12])
oco3_csv_list    <- c(oco3_csv_list[1:6], oco3_csv_list[8], oco3_csv_list[10:13])


cairo_pdf("G:/Africa/figs/timeseries_sifd_ecoregions_all_2.5km_mask.pdf", width = 20, height = 15)

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

  
  # Do TROPOMI First
  tropomi_df     <- read.csv(tropomi_csv_list[i], header = TRUE)
  tropomi_sifd   <- tropomi_df[,1]
  tropomi_sem    <- tropomi_df[,3]
  
  plot(x, tropomi_sifd, type = "l", main = tit, axes = FALSE, xlab = NA, ylab = NA, cex.main = 3, ylim = c(0.20, 1.0))
  
  rect(13, 0, 25, 100, col = rgb(0.85,0.85,0.85), border = NA)
  
  # This code was specific to data using MCD12 90% filter where there were months with no data
  # if(i != 8) {
  #   polygon(c(x, rev(x)), c(sifd+sem, rev(sifd-sem)), col = err_col, border = NA)
  #   lines(x, sifd)
  # }
  
  polygon(c(x, rev(x)), c(tropomi_sifd + tropomi_sem, rev(tropomi_sifd - tropomi_sem)), col = err_col, border = NA)
  lines(x, tropomi_sifd, lwd = 2)
  
  # Add OCO2 and 3 (No data for Nigerian lowland forests)

  oco2_df     <- read.csv(oco2_csv_list[i], header = TRUE)
  oco3_df     <- read.csv(oco3_csv_list[i], header = TRUE)
  
  # Toss out points where n < 3
  for (j in 1:nrow(oco2_df)) {
    if (!is.na(oco2_df$n[j])) {
      if (oco2_df$n[j] < 3) {
        oco2_df[j,] <- NA
      }
    }
  }
  
  for (j in 1:nrow(oco3_df)) {
    if (!is.na(oco3_df$n[j])) {
      if (oco3_df$n[j] < 3) {
        oco3_df[j,] <- NA
      }
    }

    
    oco2_sifd   <- oco2_df[,1]
    oco2_sem    <- oco2_df[,3]
    
    oco3_sifd   <- oco3_df[,1]
    oco3_sem    <- oco3_df[,3]

    points(x, oco2_sifd, col = "red", pch = 16)
    arrows(x0 = x, y0 = oco2_sifd - oco2_sem, x1 = x, y1 = oco2_sifd + oco2_sem, code = 3, angle = 90, length = 0.05, col = "red", lwd = 2)
    
    points(x, oco3_sifd, col = "blue", pch = 16)
    arrows(x0 = x, y0 = oco3_sifd - oco3_sem, x1 = x, y1 = oco3_sifd + oco3_sem, code = 3, angle = 90, length = 0.05, col = "blue", lwd = 2)
    
  }



  # axis(1, labels = month_labs, at =  c(1:36), tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
  # mtext(1, text = "Month", line = 3.75)
  # mtext(2, text = "SIFdaily", line = 3.75)
  axis(1, labels = year_labs, at =  c(6.5, 18.5, 30.5), tck = 0.03, mgp=c(3, 1.5, 0), las = 1, cex.axis = 3)
  axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 3)
  box()
  
}

dev.off()
