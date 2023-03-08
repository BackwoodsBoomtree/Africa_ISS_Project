
library(stringr)

#### Output PDF name ####
out_name  <- "G:/Africa/figs/precip_ann_vs_precip_sd.pdf"
df_precip <- read.csv("G:/Africa/csv/precip/TropicalAfricaMonthlyPrecipPerEcoregion.csv")

# Remove Niger forest
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

# Index for precip data
p_start <- seq(1, 360, 36)
p_end   <- seq(36, 360, 36)

# Get annual total means
for (i in 1:length(unique(df_precip$ecoregion))) {
  precip_eco <- sum(df_precip$mean[(p_start[i]):(p_end[i])]) / 3
  
  if (i == 1) {
    precip_ann <- precip_eco
  } else {
    precip_ann <- c(precip_ann, precip_eco)
  }
}

# Get sd
for (i in 1:length(unique(df_precip$ecoregion))) {
  precip_eco <- sd(df_precip$mean[(p_start[i]):(p_end[i])])
  
  if (i == 1) {
    precip_sd <- precip_eco
  } else {
    precip_sd <- c(precip_sd, precip_eco)
  }
}

# Labels
lab_sd  <- bquote("Standard Deviation of Monthly Precipitation (mm)")
lab_ann <- bquote("Mean Annual Total Precipitation (mm)")

#### Plot ####
cairo_pdf(out_name, width = 2.5, height = 2.5)

par(oma=c(3.5,3,0,1.5))

# Read Data
df     <- data.frame(precip_ann, precip_sd)
colnames(df) <- c("x", "y")

# Run regression
spear <- cor.test(precip_ann, precip_sd, method = "spearman")
reg   <- lm(y ~ x, data = df)
reg_r <- bquote(r[s]~" = "~.(round2(spear$estimate, 2, FALSE)))
reg_p <- round2(spear$p.value, 2, TRUE)

# predicts + interval
newx  <- seq(min(df$x), max(df$x), length.out = 100)
preds <- predict(reg, newdata = data.frame(x = newx), interval = 'confidence')

# Plot
op <- par(mar = c(0,2,4,0.5))

plot(precip_ann, precip_sd, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, pch = NA)
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)
par(new=TRUE)
plot(precip_ann, precip_sd, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA)
abline(reg)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)

legend("topleft", bty = "n", legend = c(as.expression(reg_r), as.expression(reg_p)))

box()

mtext(lab_ann, side = 1, outer = TRUE, line = 2.5)
mtext(lab_sd, side = 2, outer = TRUE, line = 1)

dev.off()
