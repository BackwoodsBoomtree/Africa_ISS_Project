
library(stringr)

trop_list   <- list.files("G:/Africa/csv/ecoregions/mask_Dans/TROPOMI", pattern = "*.csv", full.names = TRUE)
trop_list   <- c(trop_list[3], trop_list[5], trop_list[9], trop_list[10], trop_list[11], trop_list[12])
oco2_list   <- list.files("G:/Africa/csv/ecoregions/mask_Dans/OCO2", pattern = "*.csv", full.names = TRUE)
oco2_list   <- c(oco2_list[3], oco2_list[5], oco2_list[8], oco2_list[9], oco2_list[10], oco2_list[11])
month_labs <- format(seq(as.Date("2019/1/1"), as.Date("2021/12/1"), "month"), "%Y-%m")
year_labs  <- c(2019, 2020, 2021)
x          <- 1:36
err_col    <- "#8ab8e6"

cairo_pdf("G:/Africa/figs/timeseries_sifd_ecoregions_tropomi_oco2_2.5km_mask.pdf", width = 20, height = 7.5)

par(mfrow = c(2, 3), oma=c(1.0,0.1,1.25,0.1))

for (i in 1:length(trop_list)) {
  
  f_name <- basename(trop_list[i])
  tit    <- substr(f_name, 1, nchar(f_name) - 19)
  tit    <- substr(tit, 25, nchar(tit))
  tit    <- gsub("_", " ", tit)
  tit    <- str_to_title(tit)
  tit    <- str_sub(tit, end = -2)
  
  trop_df     <- read.csv(trop_list[i], header = TRUE)
  trop_sifd   <- trop_df[,1]
  trop_sem    <- trop_df[,3]
  
  oco2_df     <- read.csv(oco2_list[i], header = TRUE)
  oco2_sifd   <- oco2_df[,1]
  oco2_sem    <- oco2_df[,3]
  
  plot(x, trop_sifd, type = "l", main = tit, axes = FALSE, xlab = NA, ylab = NA, cex.main = 3, ylim = c(0.20, 0.70))
  
  rect(13, 0, 25, 100, col = rgb(0.85,0.85,0.85), border = NA)
  
  # This code was specific to data using MCD12 90% filter where there were months with no data
  # if(i != 8) {
  #   polygon(c(x, rev(x)), c(sifd+sem, rev(sifd-sem)), col = err_col, border = NA)
  #   lines(x, sifd)
  # }
  
  polygon(c(x, rev(x)), c(trop_sifd+trop_sem, rev(trop_sifd-trop_sem)), col = err_col, border = NA)
  polygon(c(x, rev(x)), c(oco2_sifd+oco2_sem, rev(oco2_sifd-oco2_sem)), col = err_col, border = NA)
  lines(x, trop_sifd, lwd = 2)
  lines(x, oco2_sifd, lwd = 2, col = "red")

  # axis(1, labels = month_labs, at =  c(1:36), tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
  # mtext(1, text = "Month", line = 3.75)
  # mtext(2, text = "SIFdaily", line = 3.75)
  axis(1, labels = year_labs, at =  c(6.5, 18.5, 30.5), tck = 0.03, mgp=c(3, 1.5, 0), las = 1, cex.axis = 3)
  axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 3)
  box()
  
}

dev.off()
