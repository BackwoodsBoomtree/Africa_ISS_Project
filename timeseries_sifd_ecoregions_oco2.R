
csv_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/OCO2", pattern = "*.csv", full.names = TRUE)
x_labs   <- format(seq(as.Date("2019/1/1"), as.Date("2021/12/1"), "month"), "%Y-%m")
x        <- 1:36
err_col  <- "#8ab8e6"

cairo_pdf("G:/Africa/figs/timeseries_sifd_ecoregions_OCO2_2.5km_mask.pdf", width = 20, height = 15)

par(mfrow = c(4, 3), oma=c(2.0,0.1,1.25,0.1))

for (i in 1:length(csv_list)) {
  
  f_name <- basename(csv_list[i])
  tit    <- substr(f_name, 1, nchar(f_name) - 19)
  tit    <- substr(tit, 19, nchar(tit))
  df     <- read.csv(csv_list[i], header = TRUE)
  sifd     <- df[,1]
  sem      <- df[,3]
  
  plot(x, sifd, type = "l", main = tit, ylab = "SIFdaily", axes = FALSE, xlab = NA)
  
  # This code was specific to data using MCD12 90% filter where there were months with no data
  # if(i != 8) {
  #   polygon(c(x, rev(x)), c(sifd+sem, rev(sifd-sem)), col = err_col, border = NA)
  #   lines(x, sifd)
  # }
  
  # polygon(c(x, rev(x)), c(sifd+sem, rev(sifd-sem)), col = err_col, border = NA)
  lines(x, sifd)

  axis(1, labels = x_labs, at =  c(1:36), tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
  axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
  mtext(1, text = "Month", line = 3.75)
  box()
  
}


dev.off()
