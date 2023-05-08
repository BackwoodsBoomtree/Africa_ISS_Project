
library(stringr)
library(mcr)

#### Output PDF name ####
out_name   <- "G:/Africa/figs/sensors_vs_sensors_basin_deming.pdf"
tropomi_csv_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/TROPOMI", pattern = "*.csv", full.names = TRUE)
oco2_csv_list    <- list.files("G:/Africa/csv/ecoregions/mask_Dans/OCO2", pattern = "*.csv", full.names = TRUE)
oco3_csv_list    <- list.files("G:/Africa/csv/ecoregions/mask_Dans/OCO3", pattern = "*.csv", full.names = TRUE)

# Remove Nigerian Lowland and Niger Delta Swamp as I combined them
tropomi_csv_list <- c(tropomi_csv_list[1:6], tropomi_csv_list[8], tropomi_csv_list[10:13])
oco2_csv_list    <- c(oco2_csv_list[1:6], oco2_csv_list[8:12])
oco3_csv_list    <- c(oco3_csv_list[1:6], oco3_csv_list[8], oco3_csv_list[10:13])

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

# Read Data
for (i in 1:length(tropomi_csv_list)) {
  
  t_sif  <- read.csv(tropomi_csv_list[i])$Mean
  o2_sif <- read.csv(oco2_csv_list[i])$Mean
  o3_sif <- read.csv(oco3_csv_list[i])$Mean
  
  t_var  <- read.csv(tropomi_csv_list[i])$SD ^ 2
  o2_var <- read.csv(oco2_csv_list[i])$SD ^ 2
  o3_var <- read.csv(oco3_csv_list[i])$SD ^ 2
  
  if (i == 1) {
    t_sif_all  <- t_sif
    o2_sif_all <- o2_sif
    o3_sif_all <- o3_sif
    t_var_all  <- t_var
    o2_var_all <- o2_var
    o3_var_all <- o3_var
  } else {
    t_sif_all  <- c(t_sif_all, t_sif)
    o2_sif_all <- c(o2_sif_all, o2_sif)
    o3_sif_all <- c(o3_sif_all, o3_sif)
    t_var_all  <- c(t_var_all, t_var)
    o2_var_all <- c(o2_var_all, o2_var)
    o3_var_all <- c(o3_var_all, o3_var)
  }
}

df_sif           <- cbind.data.frame(t_sif_all, o2_sif_all, o3_sif_all)
colnames(df_sif) <- c("t_sif", "o2_sif", "o3_sif")
df_var           <- cbind.data.frame(t_var_all, o2_var_all, o3_var_all)
colnames(df_var) <- c("t_var", "o2_var", "o3_var")

t_o2_var <- df_var$o2_var / df_var$t_var
t_o3_var <- df_var$o3_var / df_var$t_var
o2_o3_var <- df_var$o3_var / df_var$o2_var

df_ratio <- cbind.data.frame(t_o2_var, t_o3_var, o2_o3_var)
colnames(df_ratio) <- c("t_o2_var", "t_o3_var", "o2_o3_var")

# Change Nans to NA
df_sif[is.na(df_sif)] <- NA
df_var[is.na(df_var)] <- NA
df_ratio[is.na(df_ratio)] <- NA

# Remove outliar
df_sif[df_sif < 0.1] <- NA

# Run Deming regression
df_t_o2 <- cbind.data.frame(df_sif$t_sif, df_sif$o2_sif, df_ratio$t_o2_var)
df_t_o2 <- na.omit(df_t_o2)
colnames(df_t_o2) <- c("t_sif", "o2_sif", "t_o2_var")
t_o2_dreg <- mcreg(df_t_o2$t_sif, df_t_o2$o2_sif, method.reg = "Deming", error.ratio = df_t_o2$t_o2_var, 
                   mref.name = "TROPOMI", mtest.name = "OCO-2", na.rm = TRUE)
reg_i   <- round2(MCResult.getCoefficients(t_o2_dreg)[1,1], 2, FALSE)
reg_s   <- round2(MCResult.getCoefficients(t_o2_dreg)[2,1], 2, FALSE)
eq_t_o2 <- bquote(y~"="~.(reg_s)~x~"+"~.(reg_i))

# Run Deming regression
df_t_o3 <- cbind.data.frame(df_sif$t_sif, df_sif$o3_sif, df_ratio$t_o3_var)
df_t_o3 <- na.omit(df_t_o3)
colnames(df_t_o3) <- c("t_sif", "o3_sif", "t_o3_var")
t_o3_dreg <- mcreg(df_t_o3$t_sif, df_t_o3$o3_sif, method.reg = "Deming", error.ratio = df_t_o3$t_o3_var, 
                   mref.name = "TROPOMI", mtest.name = "OCO-3", na.rm = TRUE)
reg_i   <- round2(MCResult.getCoefficients(t_o3_dreg)[1,1], 2, FALSE)
reg_s   <- round2(MCResult.getCoefficients(t_o3_dreg)[2,1], 2, FALSE)
eq_t_o3 <- bquote(y~"="~.(reg_s)~x~"+"~.(reg_i))

# Run Deming regression
df_o2_o3 <- cbind.data.frame(df_sif$o2_sif, df_sif$o3_sif, df_ratio$o2_o3_var)
df_o2_o3 <- na.omit(df_o2_o3)
colnames(df_o2_o3) <- c("o2_sif", "o3_sif", "o2_o3_var")
o2_o3_dreg <- mcreg(df_o2_o3$o2_sif, df_o2_o3$o3_sif, method.reg = "Deming", error.ratio = df_o2_o3$o2_o3_var, 
                   mref.name = "TROPOMI", mtest.name = "OCO-3", na.rm = TRUE)
reg_i    <- round2(MCResult.getCoefficients(o2_o3_dreg)[1,1], 2, FALSE)
reg_s    <- round2(MCResult.getCoefficients(o2_o3_dreg)[2,1], 2, FALSE)
eq_o2_o3 <- bquote(y~"="~.(reg_s)~x~"+"~.(reg_i))

# Labels
lab_tsif  <- bquote("Tropomi SIFdaily 740nm (mW/m"^"2"*"/sr/nm)")
lab_o2sif <- bquote("OCO-2 SIFdaily 740nm (mW/m"^"2"*"/sr/nm)")
lab_o3sif <- bquote("OCO-3 SIFdaily 740nm (mW/m"^"2"*"/sr/nm)")

#### Plot ####
cairo_pdf(out_name, width = 12.5, height = 4)

par(mfrow = c(1, 3), oma=c(3.5,0,0.5,0))

op <- par(mar = c(0,4,0,0.5))

# Plot
MCResult.plot(t_o2_dreg, x.lab = "", y.lab = "", main = NA, xlim = c(0, 0.7), ylim = c(0, 0.7), digits = list(cor = 2),
              sub = "", tck = 0.03, mgp = c(1.5, 0.3, 0), identity.lwd = 2, add.legend = FALSE, add.cor = TRUE,
              add.grid = FALSE)

legend('topleft', inset = c(0.01, 0.01), legend = c(eq_t_o2), bty = 'o', bg = "transparent", box.col = "transparent")
mtext(lab_tsif, side = 1, line = 2.5)
mtext(lab_o2sif, side = 2, line = 1.5)

# Plot
MCResult.plot(t_o3_dreg, x.lab = "", y.lab = "", main = NA, xlim = c(0, 0.7), ylim = c(0, 0.7), digits = list(cor = 2),
              sub = "", tck = 0.03, mgp = c(1.5, 0.3, 0), identity.lwd = 2, add.legend = FALSE, add.cor = TRUE,
              add.grid = FALSE)

legend('topleft', inset = c(0.01, 0.01), legend = c(eq_t_o3), bty = 'o', bg = "transparent", box.col = "transparent")
mtext(lab_tsif, side = 1, line = 2.5)
mtext(lab_o3sif, side = 2, line = 1.5)

# Plot
MCResult.plot(o2_o3_dreg, x.lab = "", y.lab = "", main = NA, xlim = c(0, 0.7), ylim = c(0, 0.7), digits = list(cor = 2),
              sub = "", tck = 0.03, mgp = c(1.5, 0.3, 0), identity.lwd = 2, add.legend = FALSE, add.cor = TRUE,
              add.grid = FALSE)

legend('topleft', inset = c(0.01, 0.01), legend = c(eq_o2_o3), bty = 'o', bg = "transparent", box.col = "transparent")
mtext(lab_o2sif, side = 1, line = 2.5)
mtext(lab_o3sif, side = 2, line = 1.5)

dev.off()
