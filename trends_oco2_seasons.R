library(Kendall)

options(scipen = 999)

df_all <- read.csv("G:/Africa/csv/ecoregions/mask_Dans/OCO2_Congo_Monthly_Mean/OCO2_Congo_Monthly_Means_2015-2021.csv", header = TRUE)
df_cs  <- read.csv("G:/Africa/csv/ecoregions/mask_Dans/OCO2_Congo_Monthly_Mean/OCO2_Congo_Monthly_Means_2015-2021_cs.csv", header = TRUE)

monthly_all_ts <- ts(df_all$SIF_740d, start = c(2015, 1), frequency = 12)
monthly_cs_ts  <- ts(df_cs$SIF_740d, start = c(2015, 1), frequency = 12)

# Fill the missing values using a seasonal Kalman filter (needed for using in trend package)
monthly_all_ts <- na.StructTS(monthly_all_ts)
monthly_cs_ts  <- na.StructTS(monthly_cs_ts)

monthly_all_ts_desea <- seasadj(decompose(monthly_all_ts))
monthly_cs_ts_desea  <- seasadj(decompose(monthly_cs_ts))


# PLOTS
dates  <- seq(as.Date("2015/1/1"), as.Date("2021/12/1"), "months")
dates  <- format(dates, format = "%Y-%m")
x         <- 1:length(dates)
year_labs <- seq(2015, 2021)

cairo_pdf("G:/Africa/figs/trends_oco2_all.pdf", width = 7.5, height = 4)

par(mfrow = c(2, 2), oma=c(1.0,3.25,0,0.1))

op <- par(mar = c(1,0,1.25,0.25))

# Monthly all obs 
reg       <- lm(monthly_all_ts ~ x)

plot(x, monthly_all_ts, type = "l", main = "All observations", axes = FALSE, xlab = NA, ylab = NA, cex.main = 1, ylim = c(0.20, 0.8))

lines(x, monthly_all_ts, lwd = 3)
abline(reg, lwd = 3,col = "red")

axis(1, labels = NA, at =  seq(1, 73, by = 12), tck = 0.03, mgp=c(3, 1.5, 0), las = 1, cex.axis = 1)
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 1)

mk    <- round(MannKendall(monthly_all_ts)[[2]][1], 2)
slope <- round(coef(reg)[[2]], 4)

legend("topright", border = NA, legend = c(paste0("MK p-value = ", mk), paste0("slope = ", slope)))

box()

# Monthly all obs deseasonalized
reg       <- lm(monthly_all_ts_desea ~ x)

plot(x, monthly_all_ts_desea, type = "l", main = "All observations deseasonalized", axes = FALSE, xlab = NA, ylab = NA, cex.main = 1, ylim = c(0.20, 0.8))

lines(x, monthly_all_ts_desea, lwd = 3)
abline(reg, lwd = 3,col = "red")

axis(1, labels = NA, at =  seq(1, 73, by = 12), tck = 0.03, mgp=c(3, 1.5, 0), las = 1, cex.axis = 1)
axis(2, labels = NA, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 1)

smk   <- round(SeasonalMannKendall(monthly_all_ts)[[2]][1], 2)
slope <- round(coef(reg)[[2]], 4)

legend("topright", border = NA, legend = c(paste0("SMK p-value = < 0.05"), paste0("slope = ", slope)))

box()

# Monthly cs obs 
reg       <- lm(monthly_cs_ts ~ x)

plot(x, monthly_cs_ts, type = "l", main = "Clearsky observations", axes = FALSE, xlab = NA, ylab = NA, cex.main = 1, ylim = c(0.20, 0.8))

lines(x, monthly_cs_ts, lwd = 3)
abline(reg, lwd = 3,col = "red")

axis(1, labels = year_labs, at =  seq(1, 73, by = 12), tck = 0.03, mgp=c(3, 0.2, 0), las = 1, cex.axis = 1)
axis(2, labels = TRUE, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 1)

mk    <- round(MannKendall(monthly_cs_ts)[[2]][1], 2)
slope <- round(coef(reg)[[2]], 4)

legend("topright", border = NA, legend = c(paste0("MK p-value = ", mk), paste0("slope = ", slope)))

box()

# Monthly cs obs deseasonalized
reg       <- lm(monthly_cs_ts_desea ~ x)

plot(x, monthly_cs_ts_desea, type = "l", main = "Clearsky observations deseasonalized", axes = FALSE, xlab = NA, ylab = NA, cex.main = 1, ylim = c(0.20, 0.8))

lines(x, monthly_cs_ts_desea, lwd = 3)
abline(reg, lwd = 3,col = "red")

axis(1, labels = year_labs, at =  seq(1, 73, by = 12), tck = 0.03, mgp=c(3, 0.2, 0), las = 1, cex.axis = 1)
axis(2, labels = NA, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, cex.axis = 1)

smk   <- round(SeasonalMannKendall(monthly_cs_ts)[[2]][1], 2)
slope <- round(coef(reg)[[2]], 4)

legend("topright", border = NA, legend = c(paste0("SMK p-value = < 0.01"), paste0("slope = ", slope)))

box()

mtext(text = bquote("Monthly Mean SIFdaily 740nm (mW/m"^"2"*"/sr/nm)"), side = 2, outer = TRUE, line = 1.5)

dev.off()
