library(tidyverse)
library(cowplot)

# Read in what we need to plot
evi_list     <- list.files("G:/Africa/csv/ecoregions/mask_Dans/MCD43A4_EVI/monthly", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
lswi_list    <- list.files("G:/Africa/csv/ecoregions/mask_Dans/MCD43A4_LSWI/monthly", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
ndvi_list    <- list.files("G:/Africa/csv/ecoregions/mask_Dans/MCD43A4_NDVI/monthly", pattern = "*.csv", full.names = TRUE, recursive = TRUE)
tropomi_list <- list.files("G:/Africa/csv/ecoregions/mask_Dans/TROPOMI", pattern = "*.csv", full.names = TRUE)
precip_csv       <- "G:/Africa/csv/precip/TropicalAfricaMonthlyPrecipPerEcoregionESAMask04252023.csv"
out_start        <- "G:/Africa/figs/lags/"

# Remove Nigerian Lowland and Niger Delta Swamp as I combined them
tropomi_list <- c(tropomi_list[1:6], tropomi_list[8], tropomi_list[10:13])

#Function to compute cross-correlations between CHIRPS and each veg
vegFun <- function(index, vi_list, precip_csv){
  ###
  evi_list  <- vi_list
  df_precip <- read.csv(precip_csv, header = TRUE)
  
  if (index != "SIF") {
    # Remove Mount Cameroon and Bioko montane forests and rearrange to match
    evi_list <- c(evi_list[1], evi_list[3], evi_list[2], evi_list[4:6],
                  evi_list[9], evi_list[8], evi_list[10:12])
  }

  # Alphabetize the precip data to match
  df_precip <- df_precip[order(df_precip$ecoregion),]
  df_precip$ecoregion[df_precip$ecoregion == "Niger Delta swamp and Nigerian lowland forests"] <- "Nigerian Lowland and Niger Delta Swamp Forest"
  row.names(df_precip) <- 1:nrow(df_precip)
  
  # Index for precip data
  p_start <- seq(1, 396, 36)
  p_end   <- seq(36, 396, 36)
  
  #Dataframe for storing means
  eviAll <- data.frame(matrix(ncol = 1, nrow = 0))
  precipAll <- data.frame(matrix(ncol = 1, nrow = 0))
  
  colnames(precipAll) <- 'precipMean'
  colnames(eviAll) <- 'eviMean'
  # Run for each site
  for (i in 1:length(evi_list)) {
    
    # Read Data
    evi  <- read.csv(evi_list[i])
    if (index != "SIF") {
      evi1 <- evi[[index]]/10000
      evi  <- as.data.frame(evi1[13:48])
    } else {
      evi1 <- evi$Mean
      evi  <- as.data.frame(evi1)
    }
  
    
    colnames(evi)    <- 'eviMean'
    eviAll           <- rbind(eviAll, evi)
    precip           <- as.data.frame(df_precip$mean[p_start[i]:p_end[i]])
    colnames(precip) <- 'precipMean'
    precipAll        <- rbind(precipAll, precip)
  }
  
  #Lagged correlation function
  
  laggedCorPlot <- function(df1, region11, color1, xlab1 = "", ylab1 = ""){
    df2 <- df1 %>% 
      filter(region1 == region11)
    ccfPlot <- ggplot(df2, aes(x=lag, y=acf_vals)) +
      geom_bar(stat="identity", width=.025, fill = color1) +
      geom_point(stat="identity", size=3, fill = color1, shape=21, color=color1) +
      geom_hline(yintercept = 0) +
      geom_hline(data = df_ci, aes(yintercept = -ci), color="blue", linetype="dotted") +
      geom_hline(data = df_ci, aes(yintercept = ci), color="blue", linetype="dotted") +
      labs(x=xlab1, y=ylab1) +
      #facet_wrap(~region1, nrow = 2) +
      ggtitle(region11) +
      scale_y_continuous(limits = c(-0.9, 0.9), breaks = seq(-0.9, 0.9, 0.2)) +
      scale_x_continuous(limits = c(-6.1, 6.1), breaks = seq(-6, 6, 1)) +
      theme_bw() +
      theme(panel.grid.major = element_line(color = 'grey60', size = 0.1), 
            panel.grid.minor = element_line(color = 'grey80', size = 0.05),
            strip.text = element_text(size = 12, color = 'black', face = "bold", vjust = 0))
    ccfPlot
  }
  
  combined_df <- df_precip %>% 
    mutate(precipMean = precipAll,
           eviMean = eviAll,
           region1 = ecoregion)
  #Create lagged correlations
  df_acf <- combined_df %>% 
    group_by(region1) %>% 
    summarise(list_acf=list(ccf(precipMean, eviMean, plot=FALSE, lag.max=6))) %>%
    mutate(acf_vals=purrr::map(list_acf, ~as.numeric(.x$acf))) %>% 
    select(-list_acf) %>% 
    unnest(cols = c(acf_vals)) %>% 
    group_by(region1) %>% 
    mutate(lag= c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6))
  
  df_ci <- combined_df %>% 
    group_by(region1) %>% 
    summarise(ci = qnorm((1 + 0.95)/2)/sqrt(n()))
  
  #Plot lagged correlations
  
  return(df_acf)
}

#EVI
evi_lag  <- vegFun("EVI", evi_list, precip_csv)
lswi_lag <-  vegFun("LSWI", lswi_list, precip_csv)
ndvi_lag <-  vegFun("NDVI", ndvi_list, precip_csv)
sif_lag  <-  vegFun("SIF", tropomi_list, precip_csv)

# Get annual mean total precip
precip_df <- read.csv(precip_csv, header = TRUE)
precip_df <- precip_df[order(precip_df$ecoregion),]
precip_df$ecoregion[precip_df$ecoregion == "Niger Delta swamp and Nigerian lowland forests"] <- "Nigerian Lowland and Niger Delta Swamp Forest"
row.names(precip_df) <- 1:nrow(precip_df)
p_start <- seq(1, 396, 36)
p_end   <- seq(36, 396, 36)

for (i in 1:length(unique(precip_df$ecoregion))) {
  precip_eco <- sum(precip_df$mean[(p_start[i]):(p_end[i])]) / 3
  sd_eco     <- sd(precip_df$mean[(p_start[i]):(p_end[i])])
  
  if (i == 1) {
    precip_ann <- precip_eco
    sd_ann     <- sd_eco
  } else {
    precip_ann <- c(precip_ann, precip_eco)
    sd_ann     <- c(sd_ann, sd_eco)
  }
}
precip_ann <- round(precip_ann)

cairo_pdf("G:/Africa/figs/lag_correlations.pdf", width = 20, height = 15)

x <- -6:6
y <- c(-1, 1)

eco_index <- seq(1, 143, by = 13)

par(mfrow = c(4, 3), oma=c(1.25,4.25,1.25,0.1))

for (i in 1:11) {
  
  # Get ecoregion name
  if (i != 7) {
    f_name <- basename(tropomi_list[i])
    tit    <- substr(f_name, 1, nchar(f_name) - 19)
    tit    <- substr(tit, 25, nchar(tit))
    tit    <- gsub("_", " ", tit)
    tit    <- str_to_title(tit)
    tit    <- str_sub(tit, end = -2)
  } else {
    tit    <- "Nigerian Lowland and Niger Delta Forest"
  }
  
  map <- paste0("MAP: ", round(precip_ann[i], 0), " mm")
  sd  <- paste0("SD:   ", round(sd_ann[i], 0), " mm")
  
  
  # Do TROPOMI First
  plot(x, rep(0, 13), type = "l", main = tit, 
       axes = FALSE, xlab = NA, ylab = NA, cex.main = 2.8, ylim = y, lwd = 3, col = "gray")
  lines(x, sif_lag$acf_vals[eco_index[i] : (eco_index[i] + 12)], lwd = 3)
  lines(x, ndvi_lag$acf_vals[eco_index[i] : (eco_index[i] + 12)], lwd = 3, col = "#117733", lty = 2)
  lines(x, evi_lag$acf_vals[eco_index[i] : (eco_index[i] + 12)], lwd = 3, col = "#85B997")
  # lines(x, lswi_lag$acf_vals[eco_index[i] : (eco_index[i] + 12)], lwd = 3, col = "#ffb000")

  axis(1, labels = TRUE, tck = 0.03, mgp=c(3, 1.5, 0), las = 1, cex.axis = 3)
  axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 3)
  legend("topleft", legend = c(map, sd), cex = 2, bty = "n")
  box()
  
}

mtext("Lag", side = 1, outer = TRUE, line = -0.75, cex = 3)
mtext("Coefficient", side = 2, outer = TRUE, line = 0.75, cex = 3)

plot.new()
legend("top", legend = c("TROPOMI SIF", "EVI", "NDVI", "MAP: Mean Annual Precipitation",
                         "SD: Standard Deviation of Monthly Precipitation",
                         "x-axis: Lag in Months", "y-axis: Correlation Coefficient"),
       col = c("black", "#85B997", "#117733"),
       lty = c(1, 1, 2), lwd = c(3, 3, 3, NA, NA, NA, NA), cex = 2)

dev.off()


#Notes
'
*Lag 0*

ABCDEFGHIJKLMNOPQRSTUVWXYZ

abcdefghijklmnopqrstuvwxyz

*Lag 1*

 ABCDEFGHIJKLMNOPQRSTUVWXYZ
 
abcdefghijklmnopqrstuvwxyz

*Lag -1*

ABCDEFGHIJKLMNOPQRSTUVWXYZ

 abcdefghijklmnopqrstuvwxyz
 

* A lag of +1 means that we are comparing precipitation one month after SIF. Thus, precipitation of June 2020 was compared to SIF of July 2020.

* A lag of -1 means that we are comparing precipitation one month before SIF. Thus, precipitation of June 2020 was compared to SIF of May 2020. 

* While the x variable (precipitation in our case) always leads the y (SIF here) in time, positive lags mean that the x variable is lagging the y variable.
'

  