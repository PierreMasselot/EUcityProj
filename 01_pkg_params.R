################################################################################
# 
# Contrasting future heat and cold-related mortality under climate change, 
# demographic and adaptation scenarios in 854 European cities
#
# R Code Part 1: Packages and analysis parameters
#
# Pierre Masselot & Antonio Gasparrini
#
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
library(zen4R) # To download input data

#----- Statistical analysis
library(dlnm); library(splines) # Create bases for RR computation
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
library(ggtext) # To display degree symbols
library(lemon) # For the pointpath geom

#----- Custom functions
source("functions/isimip3.R") # ISIMIP3 bias-correction method
source("functions/impact.R") # Functions to compute and combine impact summaries

#------------------------
# Download data from Zenodo
#------------------------

# Download if nonexistent
if (!dir.exists("data")){
  dir.create("data")
  
  # Download the data from the repo. timeout has been increased for large files
  download_zenodo("10.5281/zenodo.14004321", path = "data", timeout = 10000,
    files = "data.zip")
  
  # Unzip data and delete zipfile
  unzip("data/data.zip")
  unlink("data/data.zip")
}

#------------------------
# PARAMETERS
#------------------------

#----- Computation

# Number of simulations for the Monte-Carlo eCI and number of cores
# Too many cores can result in insufficient memory allocated to each
nsim <- 500
ncores <- pmax(detectCores() - 1, 1) |> pmin(15)

# Size of groups of cities to parallelise
grpsize <- 10

# Directory for temporary saving (keep memory clear)
# tdir <- tempdir() # WARNING: files can be deleted automatically after some time
tdir <- "temp_results"

#----- Analysis

# Age groups (first and last define boundaries to exclude too young or too old)
agebreaks <- c(20, 45, 65, 75, 85, Inf)
agelabs <- gsub("-Inf", "+", 
  paste(agebreaks[-length(agebreaks)], agebreaks[-1] - 1, sep = "-"))

# Specification of the exposure-response function (follows Masselot et al 2023 Lancet Plan. Health)
varfun <- "bs"
vardegree <- 2
varper <- c(10, 75, 90)
vardf <- vardegree + length(varper)

# Temperature percentiles
predper <- c(seq(0, 1, 0.1), 2:98, seq(99, 100, 0.1))

# Denominator for excess death rates
byrate <- 10^5

# Length of period of reporting (in years)
perlen <- 5

#----- Series and labels

# Define historical, projections and calibration periods
histrange <- c(2000, 2014)
projrange <- c(2015, seq(2030, 2100, by = 10))

# Day sequences (NB Leap Days removed)
totrange <- range(c(histrange, projrange))
dayvec <- seq(as.Date(paste(totrange[1], 01, 01, sep = "-")),
  as.Date(paste(totrange[2] - 1, 12, 31, sep = "-")), by = 1)
dayvec <- dayvec[month(dayvec) != 2 | mday(dayvec) != 29]

# Region ordering
ordreg <- c("Northern", "Western", "Eastern", "Southern")

#----- Climate models and scenarios

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

# Prepare the adaptation scenarios:
# No adaptation for SSP 1 and 2, adaptation of 5 and 10% for SSP3
adaptdf <- data.frame(
  adapt = rep(c("0%", "10%", "50%", "90%"), 3),
  ssp = rep(1:3, each = 4),
  heat = rep(c(0, 10, 50, 90), 3),
  cold = 0)

#----- Aesthetic parameters

# SSP
ssplabs <- c("1" = "SSP1-2.6", "2" = "SSP2-4.5", "3" = "SSP3-7.0")
ssppal <- scico(length(ssplist), palette = "glasgow", direction = -1)

# Warming levels
levellabs <- sprintf("%s\u00B0C", targets) 
levelcol <- scico(length(targets) + 1, palette = "grayC", direction = -1)[-1]
names(levellabs) <- names(levelcol) <- targets

# Temperature range
rnglabs <- c("cold" = "Cold", "heat" = "Heat", "tot" = "Net effect")
rngpal <- scico(n = 5, palette = "berlin")[c(1, 3, 4)] |> 
  "names<-"(c("cold", "tot", "heat"))

# Geography
cntrpal <- viridis(30)
regpal <- viridis(length(ordreg))
names(regpal) <- ordreg

# Age
agepal <- viridis(length(agelabs), direction = -1)
names(agepal) <- agelabs

# GCM
gcmpal <- scico(length(gcmavail), palette = "batlow")
gcmlntp <- rep_len(c("solid", "dashed", "dotted", "dotdash"), length(gcmavail))
names(gcmpal) <- names(gcmlntp) <- gcmavail

# Adaptation factors
adapal <- scico(length(unique(adaptdf$adapt)), palette = "tokyo", end = .8)
names(adapal) <- unique(adaptdf$adapt) |> sort()
