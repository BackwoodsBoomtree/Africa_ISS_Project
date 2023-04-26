rm(list=ls())
mypaths = .libPaths()
mypaths = c("C:/RLibraries", mypaths[1])
.libPaths(mypaths)

# load libraries
library(raster)
library(rgdal)
library(ggplot2) 
library(reshape2)
library(data.table)
# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)
library(sf)
library(extrafont)
require('Hmisc')
library(spatialEco)
library(DT)
library(reactable)
library(reactablefmtr)
library(cowplot)
library(leaflet)
library(tmap)
library(EnvStats)
library(stringi)
library(terra)
library(scales)
library(RColorBrewer)
sf::sf_use_s2(FALSE) #This is important, if not run, st_intersects returns 0 features


# Read in what we need to plot
rootPath <- 'F:/Dropbox (Univ. of Oklahoma)/work/Wanyama/Projects/NASA_Carbon_Project/'
outData <- paste0(rootPath, 'Africa/Africa_Precipitation/Maps_and_Figures/')
ecoregions <- st_read(paste0(rootPath, 'Africa/Shapefiles/Combined_Ecoregions.shp')) %>% 
  st_transform(crs='epsg:4326')

#Function to compute crosscorrelations between CHIRPS and each veg index, and plotting the CCF graphs
vegFun <- function(index1){
  ###
  evi_list <- list.files(paste0(rootPath, "Africa/Africa_Precipitation/Russ_Veg_Indices/MCD43A4_",index1,"/monthly"), pattern = "*.csv", full.names = TRUE, recursive = TRUE)
  df_precip <- read.csv(paste0(rootPath, "Africa/Africa_Precipitation/TropicalAfricaMonthlyPrecipPerEcoregion_03152023.csv"))
  
  # Remove some
  evi_list <- c(evi_list[1], evi_list[3], evi_list[2], evi_list[5], evi_list[6:7],
                evi_list[10], evi_list[12:14])
  df_precip <- df_precip[df_precip$ecoregion != "Nigerian lowland forests",]
  df_precip <- df_precip[df_precip$ecoregion != "Niger Delta swamp forests",]
  df_precip <- df_precip[df_precip$ecoregion != "Cross-Niger transition forests",]
  df_precip <- df_precip[df_precip$ecoregion != "Mount Cameroon and Bioko montane forests",]
  
  # Alphabetize the precip data to match
  df_precip <- df_precip[order(df_precip$ecoregion),]
  
  # Index for precip data
  p_start <- seq(1, 360, 36)
  p_end   <- seq(36, 360, 36)
  
  #Dataframe for storing means
  eviAll <- data.frame(matrix(ncol = 1, nrow = 0))
  precipAll <- data.frame(matrix(ncol = 1, nrow = 0))
  
  colnames(precipAll) <- 'precipMean'
  colnames(eviAll) <- 'eviMean'
  # Run for each site
  for (i in 1:length(evi_list)) {
    
    # Read Data
    evi  <- read.csv(evi_list[i])
    evi1 <- evi[[index1]]/10000
    
    evi  <- as.data.frame(evi1[13:48])
    colnames(evi) <- 'eviMean'
    eviAll <- rbind(eviAll, evi)
    precip <- as.data.frame(df_precip$mean[p_start[i]:p_end[i]])
    colnames(precip) <- 'precipMean'
    precipAll <- rbind(precipAll, precip)
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
  # Western Guinean lowland forests
  wglf <- laggedCorPlot(df_acf, 'Western Guinean lowland forests', 'cyan', "", "CCF")
  #Eastern Guinean Forests
  egf <- laggedCorPlot(df_acf, 'Eastern Guinean forests', 'cyan')
  #Nigerian Lowland Forests
  #nlf <- laggedCorPlot(df_acf, 'Nigerian lowland forests', 'darkorchid')
  
  #Niger Delta Swamp Forests
  #ndsf <- laggedCorPlot(df_acf, 'Niger Delta swamp forests', 'darkorchid', "", "CCF")
  
  #Cross-Sanaga-Bioko Coastal Forests
  csbf <- laggedCorPlot(df_acf, 'Cross-Sanaga-Bioko coastal forests', 'darkorchid')
  
  #Cross-Niger Transition Forests - wont work, no forest mask available. 
  # cntf <- precip_fun(monthly, 'Cross-Niger transition forests')
  # name2 = paste0(outfolder, "CHIRPS_SIF_Cross-Niger_Transition_Forests_09202022.tiff")
  # ggsave(name2, units="cm", width=30, height=15, dpi=600, compression = 'lzw')
  
  #Cameroonian Highlands Forests
  chf <- laggedCorPlot(df_acf, 'Cameroonian Highlands forests', 'darkorchid', "", "CCF")
  
  #Northeastern Congolian Lowland Forests
  neclf <- laggedCorPlot(df_acf, 'Northeastern Congolian lowland forests', 'darkgreen', "")
  
  #Central Congolian Lowland Forests
  cclf <- laggedCorPlot(df_acf, 'Central Congolian lowland forests', 'darkgreen')
  
  #Central Congolian Lowland Forests
  nwclf <- laggedCorPlot(df_acf, 'Northwestern Congolian lowland forests', 'darkgreen', "", "CCF")
  
  #Atlantic Equatorial Coastal Forests
  aecf <- laggedCorPlot(df_acf, 'Atlantic Equatorial coastal forests', 'darkgreen', "Lag (months)")
  
  #Eastern Congolian Swamp Forests
  ecsf <- laggedCorPlot(df_acf, 'Eastern Congolian swamp forests', 'darkgreen', "Lag (months)", "")
  
  #Western Congolian swamp Forests
  wcsf <- laggedCorPlot(df_acf, 'Western Congolian swamp forests', 'darkgreen', "Lag (months)", "CCF")
  
  all_figs <- plot_grid(wglf, egf, csbf, chf, neclf, cclf, nwclf, aecf, ecsf, wcsf, align = "v", labels = NULL, scale = c(1, 1), ncol = 3)
  all_figs
  name2 = paste0(outData, "CHIRPS_", index1, "_Lagged_Correlations_03152023.tiff")
  ggsave(name2, units="cm", width=45, height=22, dpi=600, compression = 'lzw')
}

#EVI
eviFigs <- vegFun(index1 = 'EVI')
#LSWI
lswiFigs <- vegFun(index1 = 'LSWI')
#NDVI
ndviFigs <- vegFun(index1 = 'NDVI')
#NIRv
nirvFigs <- vegFun(index1 = 'NIRv')

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

  