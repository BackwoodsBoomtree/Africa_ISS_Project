
library(stringr)

#### Output PDF name ####
out_name  <- "G:/Africa/figs/tropomi_vs_precip.pdf"
trop_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/TROPOMI", pattern = "*.csv", full.names = TRUE)
df_precip <- read.csv("G:/Africa/csv/precip/TropicalAfricaMonthlyPrecipPerEcoregion.csv")

# Remove Niger forest
trop_list <- c(trop_list[1:6], trop_list[9:12])
df_precip <- df_precip[df_precip$ecoregion != "Nigerian lowland forests",]
df_precip <- df_precip[df_precip$ecoregion != "Niger Delta swamp forests",]
df_precip <- df_precip[df_precip$ecoregion != "Cross-Niger transition forests",]
df_precip <- df_precip[df_precip$ecoregion != "Mount Cameroon and Bioko montane forests",]

# Aphabetize the precip data to match TROPOMI
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
lab_tsif   <- bquote("TROPOMI SIFdaily 740nm (mW/m"^"2"*"/sr/nm)")
lab_precip <- bquote("Total Precipitation (mm)")

#### Plot ####
cairo_pdf(out_name, width = 7.5, height = 8.5)

par(mfrow = c(4, 3), oma=c(3.5,3,0,1.5))

# Index for precip data
p_start <- seq(1, 360, 36)
p_end   <- seq(36, 360, 36)

for (i in 1:length(trop_list)) {
  
  # Read Data
  t_sif  <- read.csv(trop_list[i])$Mean
  precip <- df_precip$mean[p_start[i]:p_end[i]]
  df     <- data.frame(precip, t_sif)
  colnames(df) <- c("x", "y")
  
  # Run regression
  spear <- cor.test(precip, t_sif, method = "spearman")
  reg   <- lm(y ~ x, data = df)
  reg_r <- bquote(r[s]~" = "~.(round2(spear$estimate, 2, FALSE)))
  reg_p <- round2(spear$p.value, 2, TRUE)
  
  # predicts + interval
  newx  <- seq(min(df$x), max(df$x), length.out = 100)
  preds <- predict(reg, newdata = data.frame(x = newx), interval = 'confidence')
  
  # Get title
  f_name <- basename(trop_list[i])
  tit    <- substr(f_name, 1, nchar(f_name) - 19)
  tit    <- substr(tit, 25, nchar(tit))
  tit    <- gsub("_", " ", tit)
  tit    <- str_to_title(tit)
  tit    <- str_sub(tit, end = -8)
  
  # Plot
  op <- par(mar = c(0,2,4,0.5))
  
  xlim = c(0, max(precip))
  
  plot(precip, t_sif, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, xlim = c(0, max(precip)), ylim = c(0.2, 0.7), pch = NA)
  polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)
  par(new=TRUE)
  plot(precip, t_sif, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, xlim = c(0, max(precip)), ylim = c(0.2, 0.7))
  abline(reg)
  axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
  axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
  
  mtext(3, text = tit, line = 0.5, cex = 0.85)
  legend("topleft", bty = "n", legend = c(as.expression(reg_r), as.expression(reg_p)))

  box()
}

mtext(lab_precip, side = 1, outer = TRUE, line = 2.5)
mtext(lab_tsif, side = 2, outer = TRUE, line = 1)

dev.off()