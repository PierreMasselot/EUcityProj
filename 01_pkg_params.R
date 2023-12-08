################################################################################
# NEW CODE FOR PROJECTIONS
################################################################################

#------------------------
# LOAD THE PACKAGES
#------------------------

#----- Data management
library(dplyr) # Data.frame management
library(tidyr) # For reshaping functions
library(data.table) # For very large databases
library(dtplyr) # To use dplyr verbs on data.tables
library(arrow) # To deal with datasets that cannot be loaded all at once
library(stringr) # Deal with strings
library(sf) # For mapping
library(openxlsx) # To export data in excel

#----- Statistical analysis
library(dlnm) # Create bases for RR computation
library(doParallel) # Parallelize computation
library(doSNOW) # To ensure parallelisation works fine
library(PHEindicatormethods) # Includes European Standard Population (2013)
library(mixmeta) # For the function xpndMat
library(collapse) # For fquantile, fast quantile computation

#----- Plotting
library(ggplot2) # For some plots 
library(patchwork) # Put ggplots together
library(viridis) # Best palettes
library(ggnewscale) # Color scale management
library(scico) # For accessible color palettes
library(giscoR) # For Europe map
library(scales) # For color scale fine tuning
library(flextable) # To create tables
library(ggdist) # To visualise distributions

#----- Custom functions
source("bc_isimip3.R") # ISIMIP3 bias-correction method
source("impact.R") # Functions to compute and combine impact summaries

#------------------------
# PARAMETERS
#------------------------

#----- ANALYSIS

# AGE GROUPS (first and last define boundaries to exclude too young or too old)
agebreaks <- c(20, 45, 65, 75, 85, Inf)
agelabs <- gsub("-Inf", "+", 
  paste(agebreaks[-length(agebreaks)], agebreaks[-1] - 1, sep = "-"))

# SPECIFICATION OF THE EXPOSURE FUNCTION
varfun <- "bs"
vardegree <- 2
varper <- c(10, 75, 90)

# TEMPERATURE PERCENTILES
predper <- c(seq(0, 1, 0.1), 2:98, seq(99, 100, 0.1))

# NUMBER OF ITERATIONS IN THE MONTE-CARLO SIMULATIONS AND PARALLELIZATION CORES
nsim <- 100
ncores <- pmax(detectCores() - 2, 1)

# DENOMINATOR FOR RATES
byrate <- 10^5

# Length of period of reporting (in years)
perlen <- 5

# WEIGHTS EUROPEAN STANDARD POPULATION 2013 (ONLY 20+)
# NB: SEE V:\VolumeQ\AGteam\ONS\standardization
espbreaks <- (seq_along(esp2013) - 1) * 5
espgrps <- cut(espbreaks, agebreaks, right = F, labels = agelabs)
stdweight<- tapply(esp2013, espgrps, sum)

#----- SERIES AND LABELS

# DEFINE RANGE PERIODS 
# NB: DEMOGRAPHIC PROJECTIONS START IN 2020
obsrange <- c(1990, 2019)
# projrange <- c(2015, 2099)
projrange <- seq(2010, 2100, by = 10)
histrange <- c(2010, 2014)

# DAY SEQUENCES
# NB: REMOVE LEAP DAYS FROM PROJECTION PERIOD
obsday <- seq(as.Date(paste(obsrange[1], 01, 01, sep = "-")),
  as.Date(paste(obsrange[2], 12, 31, sep = "-")), by = 1)
obsday <- obsday[month(obsday) != 2 | mday(obsday) != 29]
projday <- seq(as.Date(paste(projrange[1], 01, 01, sep = "-")),
  as.Date(paste(tail(projrange, 1) - 1, 12, 31, sep = "-")), by = 1)
projday <- projday[month(projday) != 2 | mday(projday) != 29]

# Regions
ordreg <- c("Northern", "Western", "Eastern", "Southern")

#----- CLIMATE MODELS AND SCENARIOS

# SSP Scenarios
ssplist <- 1:3

# Considered warming levels and window length (on each side)
targets <- c(1.5, 2, 3, 4)
win_len <- 10

# Combination of the two
ssp_targets <- c(outer(targets, ssplist, paste, sep = "_"))

# GCMs to exclude
gcmexcl <- c("CMCC_CM2_SR5", "TaiESM1")
gcmavail <- open_dataset("data/tmeanproj.gz.parquet")$schema$names |> 
  grep(pattern = "tas_", value = T) |> gsub(pattern = "tas_", repl = "")
gcmlist <- setdiff(gcmavail, gcmexcl)

#----- Aesthetic parameters

# SSP
ssplabs <- c("1" = "SSP1-RCP2.6", "2" = "SSP2-RCP4.5", "3" = "SSP3-RCP7.0")

# Warming levels
levellabs <- sprintf("%s C", targets) 
levelcol <- scico(length(targets) + 1, palette = "grayC", direction = -1)[-1]
names(levellabs) <- names(levelcol) <- targets

# Temperature range
rnglabs <- c("tot" = "Total", "cold" = "Cold", "heat" = "Heat")
# rngpal <- c("tot" = 1, "cold" = 4, "heat" = 2)
rngpal <- scico(n = 5, palette = "berlin")[c(1, 3, 4)] |> 
  "names<-"(c("cold", "tot", "heat"))
# rngalpha <- c("tot" = 1, "cold" = .4, "heat" = .4)

# Geography
cntrpal <- viridis(30)
regpal <- viridis(length(ordreg))
# regpal <- scico(length(unique(cities$region)), palette = "batlow")
names(regpal) <- ordreg

# Age
agepal <- scico(length(agelabs), palette = "batlow")
names(agepal) <- agelabs

# GCM
gcmpal <- scico(length(gcmavail), palette = "batlow")
gcmlntp <- rep_len(c("solid", "dashed", "dotted", "dotdash"), length(gcmavail))
names(gcmpal) <- names(gcmlntp) <- gcmavail
