library(corrplot)
library(stringr)

out_name         <- "G:/Africa/figs/correlation_matrix_spearmans.pdf"
era5_csv_list    <- list.files("G:/ERA5/tropical_africa_ecoregions_stats", pattern = "*.csv", full.names = TRUE)
tropomi_csv_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/TROPOMI", pattern = "*.csv", full.names = TRUE)
oco2_csv_list    <- list.files("G:/Africa/csv/ecoregions/mask_Dans/OCO2", pattern = "*.csv", full.names = TRUE)
oco3_csv_list    <- list.files("G:/Africa/csv/ecoregions/mask_Dans/OCO3", pattern = "*.csv", full.names = TRUE)
precip_csv       <- read.csv("G:/Africa/csv/precip/TropicalAfricaMonthlyPrecipPerEcoregionESAMask04252023.csv", header = TRUE)
mcd_csv_dir      <- "G:/Africa/csv/ecoregions/mask_Dans"
vi_list          <- c("EVI", "LSWI", "NDVI", "NIRv") # Alphabetical
eco_list         <- c("AECF", "CHF", "CCLF", "CSBCF", "ECSF", "EGF",
                      "NLNDSF", "NECLF", "NWCLF", "WCSF", "WGLF") # This order matches the ecoregion names spelled out

# Remove Nigerian Lowland and Niger Delta Swamp as I combined them
tropomi_csv_list <- c(tropomi_csv_list[1:6], tropomi_csv_list[8], tropomi_csv_list[10:13])

# Sort Precip df and remove
precip_df <- precip_csv[order(precip_csv$ecoregion),]
precip_df$ecoregion[precip_df$ecoregion == "Niger Delta swamp and Nigerian lowland forests"] <- "Nigerian Lowland and Niger Delta Swamp Forest"
row.names(precip_df) <- 1:nrow(precip_df)

# Get only the directories for VIs
mcd_dir_list <- list.dirs(mcd_csv_dir, full.names = TRUE, recursive = FALSE)
mcd_dir_list <- grep("*MCD43A4", mcd_dir_list, value = TRUE)
mcd_dir_list <- grep(paste(vi_list, collapse = "|"), mcd_dir_list, value = TRUE)

# All VI CSVs
vi_csv_list   <- list.files(mcd_dir_list, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

# P function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], method = "spearman", ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

#### Matrix plot
cairo_pdf(out_name, width = 11, height = 12.5)

par(mfrow = c(4, 3), oma=c(0, 0, 0.25, 0))

index <- seq(1, 396, by = 36) # For precip

for (i in 1:length(eco_list)) {
  ## BUILD DF for regressions
  era5 <- read.csv(era5_csv_list[i], header = TRUE)
  era5 <- head(era5, 36)
  
  # Get VI csvs for the correct ecoregion
  eco_vi_csv_list <- grep(paste0("*", eco_list[i]), vi_csv_list, value = TRUE)
  
  # date   <- era5$Date
  temp   <- era5$t2m_mean
  vpd    <- era5$vpd_mean
  par    <- era5$par_mean
  sif    <- read.csv(tropomi_csv_list[i], header = TRUE)$Mean
  evi    <- read.csv(eco_vi_csv_list[1], header = TRUE)$EVI[13:48]
  lswi   <- read.csv(eco_vi_csv_list[2], header = TRUE)$LSWI[13:48]
  ndvi   <- read.csv(eco_vi_csv_list[3], header = TRUE)$NDVI[13:48]
  nirv   <- read.csv(eco_vi_csv_list[4], header = TRUE)$NIRv[13:48]
  precip <- precip_df$mean[index[i] : (index[i] + 35)]
  
  df           <- data.frame(cbind(sif, vpd, precip, temp, par, evi, lswi, ndvi, nirv))
  colnames(df) <- c("SIF", "VPD", "Precip", "Temp", "PAR", "EVI", "LSWI", "NDVI", "NIRv")
  
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
  
  # Run correlations
  df_cor <- round(cor(df, method = "spearman"),  2)
  p.mat  <- cor.mtest(df)
  
  # Plot
  corrplot(df_cor, method="color", col=col(10),  
           order="original",
           addCoef.col = "black", # Add coefficient of correlation
           tl.col="black", tl.srt=45, #Text label color and rotation
           # Combine with significance
           p.mat = p.mat, sig.level = 0.05, insig = "pch",
           pch.cex = 5, pch.col = "#00000050",
           diag=TRUE, type="upper", title = tit,
           mar = c(0,0,0.75,0), addCoefasPercent = TRUE,
           tl.cex = 1.25, cl.cex = 1.25, number.cex = 1.25, cl.ratio = 0.3)
}

dev.off()
