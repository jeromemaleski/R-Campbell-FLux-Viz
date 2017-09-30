
###########################################################
# Program to read and visualize flux (30-min) data from EC
# EJS week of June 15 2015
# V3 edits start 4/29/2016 to:
#          clean up NA strings
#          eliminate explicit column number references - go to name
#          use mutate/transform instead of explicit dataframe notation
###########################################################
#
#adapted to Tifton EC towers 
#ogletree 9877
#ponder 9690
#last edit 10/02/2017
###################################

rm(list = ls())
# Set up R context
library(plyr)
library(utils)

#setwd("C:/Users/jmaleski/Documents/R/Tift Viz")
#setwd("C:/Users/jmaleski/Documents/R/Sadler")
# setwd("W:\\Sadler Lab\\EC_ASP\\CSV data")
#setwd("T:/LTAR_CMRB/Eddy_Flux/FLUX")
#setwd("C:\\Users\\John\\Documents\\EJS Work")

station<-select.list(c("Ogletree 9877", "Ponder 9690"), preselect = NULL, multiple = FALSE,
            title = "Select Station", graphics = TRUE)


#1 is ogletree 9877
#2 is Ponder 9690

#ogletree rainfal needs to be multiplied by 0.1

if (station =="Ogletree 9877"){

  rainMulti <-0.1
  
} else if (station =="Ponder 9690"){
  rainMulti<-1
} else
  rainMulti<-1
  
###########################################################
#### Plot all columns -----
# function to plot columns in a DF, for period of record in df
# legend at pos with names. 
# pos= one of "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center".
# Col 1 must be Posixct.
# label goes above plot.
# names of df will be legend ids.
# yaxislab should be generic with units.
# scale allows text to stay proportional. 
# scale of 1 is good for interactive, 2 for pdf.
plotdf.all <- function(df, pos, label, yaxislab, scale){                   
  colors <- c("black", "blue", "red", "green", "orange", "cyan",
              "magenta")
  ncol <- length(df[1, ])
  maxy <- max(df[, 2:ncol], na.rm = TRUE)
  miny <- min(df[, 2:ncol], na.rm = TRUE)
  ylim <- range(pretty(c(maxy, miny)))
  leglab <- names(df)[2:ncol]
  dt <- df[, 1]
  plot(dt, df[,2], type="l", col=colors[1], main=label, ylab=yaxislab,
       xlab="", ylim=ylim, cex=scale)
  if (ncol > 2) {
    for (i in 3:ncol) {
      lines(dt, df[, i], type="l", col=colors[i-1])
    }
  }
  abline(h=0, lty=2, col="gray")
  legend(x=pos, col=colors[1:(ncol-1)], cex=scale/2, lty=1, 
         legend=leglab)
}
###########################################################
#### Plot by day -----
# function to plot columns in a DF, by date - one graph per day
# legend at pos with names. 
# pos= one of "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center".
# Col 1 must be Posixct.
# label goes above plot.
# names of df will be legend ids.
# yaxislab should be generic with units.
# scale allows text to stay proportional. 
# scale of 1 is good for interactive, 2 for pdf.
plotdf.byday <- function(df, pos, label, yaxislab, scale){
  colors <- c("black", "blue", "red", "green", "orange", "cyan",
              "magenta")
  ncol <- length(df[1, ])
  maxy <- max(df[, 2:ncol], na.rm = TRUE)
  miny <- min(df[, 2:ncol], na.rm = TRUE)
  ylim <- range(pretty(c(maxy, miny)))
  leglab <- names(df)[2:ncol]
  dt <- df[, 1]
  Day <- strftime(dt, "%Y-%m-%d")
  Days <- levels(as.factor(Day))
  ndays <- length(Days)
  for (i in 1:ndays) {
    These <- which(Day == Days[i])
    ddf <- df[These,]
    plotdf.all(ddf, pos, label, yaxislab, scale)
    mtext(Days[i], 1, 3)
  }
}


##### look for flux tables in TOA5 format ------

# fnames <- list.files(pattern="flux")  # check this for pattern on RSC versions.
# fnames <- "Fx_flux_2016-04_27538.dat"
fnames <- choose.files()
for (infile in fnames) {
  meta.data1 <- read.csv(infile, as.is=TRUE, nrows=1, header=FALSE)
  fields.data1 <- read.csv(infile, as.is=TRUE, skip=1, nrows=1, 
                           header=FALSE)
  units.data1 <- read.csv(infile, as.is=TRUE, skip=2, nrows=1, 
                          header=FALSE)
  type.data1 <- read.csv(infile, as.is=TRUE, skip=3, nrows=1, 
                         header=FALSE)
  Flux.data1 <- read.csv(infile, as.is=TRUE, skip=4, header=FALSE, na.strings="NAN")
  names(Flux.data1) <- fields.data1 # Names can have _ and ()
  
  if (infile == fnames[1]) {
    meta.data <- meta.data1
    fields.data <- fields.data1
    units.data <- units.data1
    type.data <- type.data1
    Flux.data <- Flux.data1
  }
  else 
  {
    fields <- setequal(fields.data, fields.data1)
    units <- setequal(units.data, units.data1)
    type <- setequal(type.data, type.data1)
    match <- fields & units & type
    if (match)
    { 
      meta.data <- rbind(meta.data, meta.data1)
      Flux.data <- rbind(Flux.data, Flux.data1)
    }
    else
      if (!match)
      {
        stop("Flux tables are different, cannot merge")
      }
  }  
}
# NAN now handled in read.csv - should not need this
# coercenum <- c(3, 36:52, 55:62, 89:90, 112:113)
# for (i in 1:length(coercenum)) {
#   Flux.data[, coercenum[i]] <- as.numeric(Flux.data[, coercenum[i]])
# }

dt <- as.POSIXct(Flux.data[, "TIMESTAMP"])
ncol <- length(Flux.data[1, ])
# sort because file order isn't always right
sorted <- order(dt)
dt <- dt[sorted]
Flux.data <- Flux.data[sorted, ]
# Done with concatenating files 

# trim first to start where you want
start <- as.POSIXct("2016-04-01 00:00")
lookat <- which(dt > start)
dt <- dt[lookat]
Flux.data <- Flux.data[lookat, ]
dtdf <- as.data.frame(dt)

###########################################################
# Check for NAN and missing, ranges, etc -----
nNaN  <- rep(0, ncol)
nmiss <- rep(0, ncol)
ninf  <- rep(0,ncol)
nzero <- rep(0, ncol)
maxes <- rep(-999, ncol)
mins  <- rep(9999, ncol)
for (i in 1:ncol){
  nNaN[i]  <- length(which(is.nan(Flux.data[, i])))
  nmiss[i] <- length(which(is.na(Flux.data[, i])))
  ninf[i]  <- length(which(is.infinite(Flux.data[, i])))
  nzero[i] <- length(which(Flux.data[, i] == 0))
  maxes[i] <- max(Flux.data[, i], na.rm=TRUE)
  mins[i]  <- min(Flux.data[, i], na.rm=TRUE)
}
df.diag <- as.data.frame(t(rbind(fields.data, nNaN, nmiss, ninf, nzero, mins, maxes)), row.names=NULL)
names(df.diag) <- c("Field", "nNaN", "nmiss", "ninf", "nzero", "Min", "Max")
row.names(df.diag) <- NULL

# Done with writing diagnostic stats
lastrec <- df.diag[1, "Max"]
enddate <- as.POSIXct(lastrec)
dateonly <- strftime(enddate, "%Y-%m-%d")

write.csv(df.diag, paste0("Fluxdiag",dateonly,".csv") , row.names=FALSE)

# Set up for plots
# pscale=1 good for interactive
# pscale=2 better for printed
pscale <- 1
pscale <- 2
if (pscale > 1){
  outplots <- paste("Fluxplots", dateonly, ".pdf", sep="")
  pdf(file=outplots, title=outplots)
}

###########################################################

###########################################################

# Battery and panel temp -----
BV <- Flux.data[, "batt_volt_Avg"]
Rsolscal <- Flux.data[, "Rs_incoming_Avg"]/1000 + 10
df <- cbind(dtdf, BV, Rsolscal)
names(df) <- c("Datetime", "Battery, V", "Rs, scaled")
plotdf.all(df, "topleft", "Battery", "Varies", pscale)

PanT <- Flux.data[, "panel_tmpr_Avg"]
IRTbody <- Flux.data[, "si_body_tmpr_Avg"] - 273.16
Ts <- Flux.data[, "Ts_Avg"] #sonic we think
TCNR4 <- Flux.data[,"T_nr_Avg"] - 273.16
df <- cbind(dtdf, PanT, IRTbody, Ts, TCNR4)
names(df) <- c("Datetime", "Panel T", "IRT body T", "Sonic T", "CNR4 T")
plotdf.all(df, "topleft", "Diagnostic Temps", "C", pscale)

#rainfall

precip <- Flux.data[, "precip_Tot"]*rainMulti

#calc cum
cprecip <- cumsum(precip)

df <- cbind(dtdf, precip, cprecip)
names(df) <- c("Datetime", "precip", "cprecip")
plotdf.all(df, "topleft", "Precipitation", "inches", pscale)


#soil water
VWC1 <- Flux.data[, "cs65x_wcr_Avg(1)"] # 2.5 cm, halfway to SHF
VWC2 <- Flux.data[, "cs65x_wcr_Avg(2)"] #2.5 cm
df <- cbind(dtdf, VWC1, VWC2)
names(df) <- c("Datetime", "VWC1", "VWC2")
plotdf.all(df, "topleft", "Soil Water", "ratio v/v", pscale)


#relative humidity

RH <- Flux.data[, "RH_tmpr_rh_mean"] 
df <- cbind(dtdf, RH)
names(df) <- c("Datetime", "RH")
plotdf.all(df, "topleft", "Relative Humidity", "%", pscale)






# Calculate soil heat flux and storage correction  -----
# Get soil moisture first 
# If others exist, need to plot too. ----------------------------------
#VWC1 <- Flux.data[, "cs65x_wcr_Avg(1)"] # 2.5 cm, halfway to SHF
#VWC2 <- Flux.data[, "cs65x_wcr_Avg(2)"] #12.5 cm
T6551 <- Flux.data[, "cs65x_tmpr_Avg(1)"] # compare with TCAV's
T6552 <- Flux.data[, "cs65x_tmpr_Avg(2)"] 
DelTs1 <- Flux.data[, "del_Tsoil(1)"] # goes with SHF 1-2
DelTs2 <- Flux.data[, "del_Tsoil(2)"] # goes with SHF 3-4
SHF12 <- rowMeans(Flux.data[, c("shf_Avg(1)", "shf_Avg(2)")], 
              na.rm=TRUE)
SHF34 <- rowMeans(Flux.data[, c("shf_Avg(3)", "shf_Avg(4)")], 
              na.rm=TRUE)
# # assuming dry BD=1.35 g/cm^3 or Mg/m^3, so 1350 kg/m^3
SolidF <- 1.35 / 2.65
Porosity <- 1 - SolidF
Orgmat <- 0.017 * SolidF # Assumes clay fraction is of only solids
Clay <- 0.16 * SolidF
Quartz <- (1 - Orgmat - Clay) * SolidF
# Numbers from GS Campbell 1985. Soil Physics with BASIC
# Quartz is 2.13 MJ/m3K, Clay=2.39, OM=2.50, Water=4.182
Drysoil <- Orgmat * 2.5 + Clay * 2.39 + Quartz * 2.13
# # This comes to 1.20371 MJ/m3K (converted below to J/m3K)
# # assuming the upper VWC, which is VWC1, applies to both sets.
# NAN fixed in read, should not need next 2
# VWC1 <- as.numeric(VWC1)
# VWC1 <- ifelse(is.nan(VWC1), 0, VWC1)
HCvolwet <- 1203710 + VWC1 * 4182000 # still per m^3
# To put actual # for 0.05m depth and 30-min interval
Stor12 <- DelTs1 * HCvolwet * 0.05 / 1800
Stor34 <- DelTs2 * HCvolwet * 0.05 / 1800
df <- cbind(dtdf, SHF12, SHF34, Stor12, Stor34)
names(df) <- c("Datetime", "SHF12", "SHF34", "Stor12", "Stor34")
plotdf.all(df, "topleft", "Soil heat", "W/m^2", pscale)
# Calculate averages for energy balance us
SHF <- (SHF12 + SHF34) / 2
Stor <- (Stor12 + Stor34) / 2

# Plot energy balance -----
LE <- as.numeric(Flux.data[, "LE_wpl"]) 
#LE <- ifelse(is.nan(LE), 0, LE)
RN <- Flux.data[, "Rn_Avg"] 
#Hs <- Flux.data[, 3] # Using Hs
Hc <- as.numeric(Flux.data[, "Hc"]) # Using Hs
EB <- RN - LE - Hc - SHF - Stor
#build df for plot function
df <- cbind(dtdf, LE, RN, SHF, Hc, EB, Stor)
names(df) <- c("Datetime", "Latent", "Net radiant", "Soil heat",
               "Sensible heat", "Energy balance", "Storage")
plotdf.all(df, "topleft", "Energy Balance", "W/m^2", pscale)
plotdf.byday(df, "topleft", "Energy Balance", "W/m^2", pscale)

#look at all SHF components and avg ------

#df <- cbind(dtdf, SHF, Flux.data[, 118:121]) 
df <- cbind(dtdf, SHF, Flux.data[, c("shf_Avg(1)", "shf_Avg(2)", "shf_Avg(3)", "shf_Avg(4)")])
names(df) <- c("Datetime", "SHF avg", "SHF-1", "SHF-2", "SHF-3", 
               "SHF-4")
plotdf.all(df, "topleft", "Soil Heat Flux - uncorr", "W/m^2", pscale)


# Look at radiation components and net -------
# RN is #97, but already done
###### need to fix col nums to names ##############-----------------------
colnames <- c("Rs_incoming_Avg", "Rs_outgoing_Avg", "Rl_incoming_Avg", "Rl_outgoing_Avg", "Rl_incoming_meas_Avg", "Rl_outgoing_meas_Avg")
df <- cbind(dtdf, RN, Flux.data[, colnames])
names(df) <- c("Datetime", "Rnet", "Rsi", "Rso", "RLi", "RLo", 
               "RLi-mea", "RLo-mea")
plotdf.all(df, "topleft", "Net Radiation", "W/m^2", pscale)

#Now just look at LW parts 
# RL in=101, RLo=102 T_NR=103, RLi-correction=104, RLo-correction=106
###### need to fix col nums to names ##############-----------------------
T_nr <- Flux.data[, "T_nr_Avg"]
Rbody <- 5.67e-8*T_nr ^ 4
RLi <- Flux.data[, "Rl_incoming_Avg"]
RLo <- Flux.data[, "Rl_outgoing_Avg"]
RLim <- Flux.data[, "Rl_incoming_meas_Avg"]
RLom <- Flux.data[, "Rl_outgoing_meas_Avg"]
RLierr <- RLi - RLim - Rbody
RLoerr <- RLo - RLom - Rbody
df <- cbind(dtdf, RLi, RLim, Rbody, RLierr)
names(df) <- c("Datetime", "RLi", "RLim", "Rbody", "RLierr")
plotdf.all(df, "left", "LWi Radiation", "W/m^2", pscale)

#RL outgoing
df <- cbind(dtdf, RLo, RLom, Rbody, RLoerr)
names(df) <- c("Datetime", "RLo", "RLom", "Rbody", "RLoerr")
plotdf.all(df, "left", "LWo Radiation", "W/m^2", pscale)

# Temperatures of air, crop, soil ----------
Tc <- Flux.data[, "ir_tmpr_Avg"]
Ta <- Flux.data[, "T_tmpr_rh_mean"]
Ea <- Flux.data[, "e_tmpr_rh_mean"]
Es <- Flux.data[, "e_sat_tmpr_rh_mean"]
TcTa <- Tc - Ta
VPD <- Es - Ea
logterm <- log(Ea / 0.61078)
Tdew <- 237.3 * logterm / (17.269 - logterm)
Ts <- Flux.data[, "Ts_Avg"] #sonic we think
df <- cbind(dtdf, Ts, Tdew, Ta, Tc, Flux.data[, c("Tsoil_mean(1)", "Tsoil_mean(2)")])
names(df) <- c("Datetime", "Tsonic", "Tdew", "Tair", "Tcrop", "Tsoil1", "Tsoil2")
plotdf.all(df, "topleft", "Temperatures", "Degrees C", pscale)

# Delta T and VPD -----
TcTa <- Tc - Ta
VPDTa <- Es - Ea
VPDTc  <- .61078 * exp(17.269 * Tc / (237.3 + Tc)) - Ea
df <- cbind(dtdf, TcTa, VPDTa, VPDTc)
names(df) <- c("Datetime", "Tc-Ta, C", "EsTa-Ea, kPa", "EsTc-Ea, kPa")
plotdf.all(df, "topleft", "Differences", "Degrees C or kPa", pscale)

# Concentrations and densities ----------
CO2 <- Flux.data[, "CO2_mean"] #mg/m^3, and in 400-700 range
H2O <- Flux.data[, "H2O_mean"] # g/m^3, and in 10-20 range
den3D <- Flux.data[, "rho_a_mean"] # kg/m^3 and in 1.1-1.2 range
denTRH <- Flux.data[, "rho_a_tmpr_rh_mean"] # same
CO2m <- CO2 / den3D # mg/kg, or ppm by mass
CO2v <- CO2m / 44 * 28 # now by moles, about same as volume
df <- cbind(dtdf, CO2, CO2m, CO2v)
names(df) <- c("Datetime", "Co2, mg/m^3", "CO2, mg/kg", "Co2, ppmv")
plotdf.all(df, "bottomleft", "CO2 Concentration", "vary", pscale)

df <- cbind(dtdf, CO2v / 10, H2O)
names(df) <- c("Datetime", "CO2, ppmv/10", "H2O, g/m^3")
plotdf.all(df, "left", "Concentrations", "vary", pscale)


df <- cbind(dtdf, den3D, denTRH)
names(df) <- c("Datetime", "Rho 3D, kg/m^3", "Rho TRH, kg/m^3")
plotdf.all(df, "bottomleft", "Air Density", "kg/m^3", pscale)

# Wind -----------
Ux <- Flux.data[, "Ux_Avg"]
Uy <- Flux.data[, "Uy_Avg"]
Uz <- Flux.data[, "Uz_Avg"]
Wind <- Flux.data[, "wnd_spd"]
df <- cbind(dtdf, Wind, Ux, Uy, Uz)
names(df) <- c("Datetime", "Wind (scalar)", "Ux", "Uy", "Uz")
plotdf.all(df, "bottomleft", "Wind and Components", "m/s", pscale)

# Now for Carbon Flux ----------
#all have units of mass flux and are CO2 related
###### need to fix col nums to names ##############-----------------------
df <- cbind(dtdf, Flux.data[, c("Fc_wpl", "CO2_wpl_LE", "CO2_wpl_H", "Fc_irga")]) 
names(df)[1] <- "Datetime"
# ???
plotdf.all(df[ , ], "bottomleft", "Carbon flux terms", "mg/(m^2s)", pscale)
mtext("Fc_wpl = Fc_irga+CO2_wpl_LE+CO2_wpl_H", 1, 3)
lines(df[ , 1:2], col="black")
# ???
# abline(v=as.POSIXct("2015-06-10 00:00"), col="gray")
# text(x=as.POSIXct("2015-06-10 00:00"), y=0.9, col="black", 
#      labels="2015-06-10 00:00")

# Now for LE terms
#        LE_wpl = LE_irga+H2O_wpl_LE+H2O_wpl_H
df <- cbind(dtdf, Flux.data[, c("LE_wpl", "LE_irga", "H2O_wpl_LE", "H2O_wpl_H")]) 
names(df)[1] <- "Datetime"
plotdf.all(df, "bottomleft", "Latent flux terms", "W/m^2", pscale)
mtext("LE_wpl = LE_irga+H2O_wpl_LE+H2O_wpl_H", 1, 3)
lines(df[, 1:2], col="black")


# Now calculate daily values of energy balance terms -----

SHFc <- SHF + Stor
df <- cbind(dtdf, LE, RN, SHFc, Hc, EB)
df[, "Date"] <- as.character(strftime(dt, "%Y-%m-%d"))
names(df) <- c("Datetime", "LE", "RN", "SHFc", "Hc", "EB", "Date")
ddf <- ddply(df, .(Date), summarize,
             LEd=sum(LE*0.0018, na.rm=TRUE), 
             RNd=sum(RN*0.0018, na.rm=TRUE), 
             SHFcd=sum(SHFc*0.0018, na.rm=TRUE),
             Hcd=sum(Hc*0.0018, na.rm=TRUE), 
             EBd=sum(EB*0.0018, na.rm=TRUE))
# plot daily values
ddf[, "Date"] <- as.POSIXct(ddf[, "Date"])
plotdf.all(ddf, "topleft", "Daily Energy Terms", "MJ/m2d", pscale)

## Now daily values of carbon flux -----------
Par_net <- Flux.data[, "Rs_incoming_Avg"]
df <- cbind(dtdf, Flux.data[, "Fc_wpl"], Par_net)
#df <- cbind(dtdf, Flux.data[, "Fc_wpl"],Flux.data[,"par_Avg"])
df[, "Date"] <- as.character(strftime(dt, "%Y-%m-%d"))
names(df)[2] <- "Fc_wpl"
ddf <- ddply(df, .(Date), summarize,
             Fcd=sum(Fc_wpl*1.8, na.rm=TRUE),
             Pard=sum(Par_net*0.0018, na.rm=TRUE))
# plot daily values
names(ddf) <- c("Date", "Fc_wpl (g/m2d)", "Rs_in (mol/m2d)")
ddf[, "Date"] <- as.POSIXct(ddf[, "Date"])
#names(ddf) <- c("Date", "Fc_wpl (g/m2d)")
plotdf.all(ddf, "topleft", "Daily Carbon Flux, Rs in", "vary", pscale)


if (pscale > 1) dev.off()


