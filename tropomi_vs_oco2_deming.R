
library(stringr)
library(mcr)

#### Output PDF name ####
out_name   <- "G:/Africa/figs/tropomi_vs_oco2_deming.pdf"
trop_list   <- list.files("G:/Africa/csv/ecoregions/mask_Dans/TROPOMI", pattern = "*.csv", full.names = TRUE)
trop_list   <- c(trop_list[3], trop_list[5], trop_list[9], trop_list[10], trop_list[11], trop_list[12])
oco2_list   <- list.files("G:/Africa/csv/ecoregions/mask_Dans/OCO2", pattern = "*.csv", full.names = TRUE)
oco2_list   <- c(oco2_list[3], oco2_list[5], oco2_list[8], oco2_list[9], oco2_list[10], oco2_list[11])

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
lab_tsif     <- bquote("Tropomi SIFdaily 740nm (mW/m"^"2"*"/sr/nm)")
lab_osif     <- bquote("OCO-2 SIFdaily 740nm (mW/m"^"2"*"/sr/nm)")

#### Plot ####
cairo_pdf(out_name, width = 7.5, height = 5.5)

par(mfrow = c(2, 3), oma=c(3.5,3,0,1.5))

for (i in 1:length(trop_list)) {
  
  # Read Data
  t_sif  <- read.csv(trop_list[i])$Mean
  o_sif  <- read.csv(oco2_list[i])$Mean
  t_var  <- read.csv(trop_list[i])$SD ^ 2
  o_var  <- read.csv(oco2_list[i])$SD ^ 2
  
  # Compute error ratio for deming
  var_ratio <- o_var / t_var
  
  # Run Deming regression
  dreg <- mcreg(t_sif, o_sif, method.reg = "Deming", error.ratio = var_ratio, 
               mref.name = "TROPOMI", mtest.name = "OCO-2")
  reg_i   <- round2(MCResult.getCoefficients(dreg)[1,1], 2, FALSE)
  reg_s   <- round2(MCResult.getCoefficients(dreg)[2,1], 2, FALSE)
  eq      <- bquote(y~"="~.(reg_s)~x~"+"~.(reg_i))
  
  # Get title
  f_name <- basename(trop_list[i])
  tit    <- substr(f_name, 1, nchar(f_name) - 19)
  tit    <- substr(tit, 25, nchar(tit))
  tit    <- gsub("_", " ", tit)
  tit    <- str_to_title(tit)
  tit    <- str_sub(tit, end = -8)
  
  # Plot
  op <- par(mar = c(0,2,4,0.5))
  
  MCResult.plot(dreg, x.lab = "", y.lab = "", main = NA, xlim = c(0, 0.7), ylim = c(0, 0.7), digits = list(cor = 2),
                sub = "", tck = 0.03, mgp = c(1.5, 0.3, 0), identity.lwd = 2, add.legend = FALSE, add.cor = TRUE,
                add.grid = FALSE)
  
  legend('topleft', inset = c(0.01, 0.01), legend = c(eq), bty = 'o', bg = "transparent", box.col = "transparent")
  mtext(3, text = tit, line = 0.5)
  
  # plot(t_sif, o_sif, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA,
  #      xlim = c(0, 0.7), ylim = c(0, 0.7))
  # abline(reg)
  # axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
  # axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
  # 
  # mtext(3, text = tit, line = 0.5)
  # legend("topleft", bty = "n", legend = c(as.expression(eq), as.expression(reg_r), as.expression(reg_p)))

  box()
}

mtext(lab_tsif, side = 1, outer = TRUE, line = 2.5)
mtext(lab_osif, side = 2, outer = TRUE, line = 1)

dev.off()
