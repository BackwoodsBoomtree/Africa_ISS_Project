
library(stringr)

#### Output PDF name ####
out_name  <- "G:/Africa/figs/nirv_vs_precip.pdf"
nirv_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/MCD43A4_NIRv/monthly", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
df_precip <- read.csv("G:/Africa/csv/precip/TropicalAfricaMonthlyPrecipPerEcoregion.csv")

# Remove some
nirv_list <- c(nirv_list[1], nirv_list[3], nirv_list[2], nirv_list[5], nirv_list[6:7],
              nirv_list[10], nirv_list[12:14])
df_precip <- df_precip[df_precip$ecoregion != "Nigerian lowland forests",]
df_precip <- df_precip[df_precip$ecoregion != "Niger Delta swamp forests",]
df_precip <- df_precip[df_precip$ecoregion != "Cross-Niger transition forests",]
df_precip <- df_precip[df_precip$ecoregion != "Mount Cameroon and Bioko montane forests",]

# Alphabetize the precip data to match TROPOMI
df_precip <- df_precip[order(df_precip$ecoregion),]

### Can be used for pvalues
round2 = function(x, n, p) {
  posneg = sign(x)
  z <- abs(x)*10^n
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10^n
  z <- z*posneg
  
  if (p == TRUE) {
    if (z < 0.05 && z >= 0.01) {
      z <- "p < 0.05"
    } else if (z < 0.01) {
      z <- "p < 0.01"
    } else {
      z <- paste0("p = ", z)
    }
  }
  return(z)
}

# Labels
lab_nirv   <- bquote("NIRv")
lab_precip <- bquote("Total Precipitation (mm)")

#### Plot ####
cairo_pdf(out_name, width = 7.5, height = 8.5)

par(mfrow = c(4, 3), oma=c(3.5,3,0,1.5))

# Index for precip data
p_start <- seq(1, 360, 36)
p_end   <- seq(36, 360, 36)

for (i in 1:length(nirv_list)) {
  
  # Read Data
  nirv   <- read.csv(nirv_list[i])$NIRv / 10000
  nirv   <- nirv[13:48]
  precip <- df_precip$mean[p_start[i]:p_end[i]]
  df     <- data.frame(precip, nirv)
  colnames(df) <- c("x", "y")
  
  # Run regression
  spear <- cor.test(precip, nirv, method = "spearman")
  reg   <- lm(y ~ x, data = df)
  reg_r <- bquote(r[s]~" = "~.(round2(spear$estimate, 2, FALSE)))
  reg_p <- round2(spear$p.value, 2, TRUE)
  
  # predicts + interval
  newx  <- seq(min(df$x), max(df$x), length.out = 100)
  preds <- predict(reg, newdata = data.frame(x = newx), interval = 'confidence')
  
  # Get title
  tit <- unique(df_precip$ecoregion)
  tit <- str_to_title(tit)[i]
  tit <- str_sub(tit, end = -8)
  
  # Plot
  op <- par(mar = c(0,2,4,0.5))
  
  xlim = c(0, max(precip))
  
  plot(precip, nirv, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, xlim = c(0, max(precip)), ylim = c(0.1, 0.4), pch = NA)
  polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)
  par(new=TRUE)
  plot(precip, nirv, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, xlim = c(0, max(precip)), ylim = c(0.1, 0.4))
  abline(reg)
  axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
  axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
  
  mtext(3, text = tit, line = 0.5, cex = 0.85)
  legend("topleft", bty = "n", legend = c(as.expression(reg_r), as.expression(reg_p)))

  box()
}

mtext(lab_precip, side = 1, outer = TRUE, line = 2.5)
mtext(lab_nirv, side = 2, outer = TRUE, line = 1)

dev.off()
