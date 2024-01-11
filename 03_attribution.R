################################################################################
# NEW CODE FOR PROJECTIONS
################################################################################

# cities <- cities[sample(1:854, 20),]
# nsim <- 10
# gcmexcl <- c("BCC_CSM2_MR", "CMCC_ESM2", "CanESM5", "EC_Earth3", "EC_Earth3_Veg_LR",
#   "GFDL_ESM4", "INM_CM4_8", "INM_CM5_0", "IPSL_CM6A_LR", "KACE_1_0_G",
#   "MIROC6", "MPI_ESM1_2_HR", "MPI_ESM1_2_LR", "MRI_ESM2_0", "NorESM2_LM",
#   "CMCC_CM2_SR5", "TaiESM1")
# gcmlist <- setdiff(gcmavail, gcmexcl)

#--------------------------------
# Loop on cities and scenarios
#--------------------------------

#----- Initialise iterations

# Initialise data.frame to merge series consistently
init_df <- data.table(date = unique(c(obsday, projday)))[,":="(
  doy = yday(date), day = mday(date), month = month(date), year = year(date)
)]
init_df[,  ":="(year5 = floor(year / 5) * 5)]
setkey(init_df, date)

# List of necessary packages
packs <- c("dlnm", "dplyr", "data.table", "doSNOW")
libs <- .libPaths()

# Initialize trace
writeLines(c(""), "temp/attr.txt")
cat(as.character(as.POSIXct(start <- Sys.time())),
  file="temp/attr.txt", append=T)

st <- Sys.time()

#----- Iterate on SSPs and cities within countries within regions
# Summary of impacts are computed iteratively and binded together
# Full results are summed iteratively to avoid keeping everything on memory
# Aggregation at each level is performed in the `combres` and `finalcomb` fct
# (see `impact.R`)

# Iterate on SSPs, and bind all geographical levels within
finalres <- foreach(issp = ssplist, 
    .combine = function(x1, x2) Map(rbind, x1, x2)) %:% 
  # Iterate on regions, sum everything and add info about the current SSP
  foreach(reg = unique(cities$region), .combine = combres, 
    .final = function(x) finalcomb(x, geolev = "eu", ensonly = F,
      write = sprintf("temp/res_eu_%s", reg, issp),
      perlen = perlen, warming_win = warming_win[ssp == issp,]) |> 
      lapply(mutate, ssp = issp) |> _[-1]) %:% 
  # Iterate on countries within the region and sum everything 
  foreach(country = unique(subset(cities, region == reg, CNTR_CODE, drop = T)),
    .combine = combres, 
    .final = function(x) finalcomb(x, geolev = "region", geoval = reg,
      write = sprintf("temp/res_region_%s_%s", reg, issp),
      perlen = perlen, warming_win = warming_win[ssp == issp,])) %:%
  # Iterate on cities within the country and sum everything
  foreach(city = subset(cities, CNTR_CODE == country, URAU_CODE, drop = T),
    .combine = combres, 
    .final = function(x) finalcomb(x, geolev = "country", geoval = country, 
      perlen = perlen, warming_win = warming_win[ssp == issp,])) %do% 
{
  
  # Trace iterators
  cat(as.character(Sys.time()), as.character(city), as.character(issp), 
    "\n", file = "temp/attr.txt", append = T)
  
  #----- Load climate data

  # Load observed temperature
  tmeanobs <- open_dataset("data/era5series.gz.parquet") |>
    filter(URAU_CODE == city) |> collect() |>
    rename(tmeanobs = era5landtmean) |>
    dplyr::select(!URAU_CODE)

  # Load projections
  tmeanproj <- open_dataset("data/tmeanproj.gz.parquet") |>
    filter(URAU_CODE == city & ssp %in% c(issp, "hist")) |> collect() |>
    dplyr::select(!URAU_CODE)

  # Add them to the series data.frame (removes leap days and select periods)
  tmeandf <- init_df |>
    merge(tmeanobs, all.x = T) |>
    merge(tmeanproj, all.x = T) |>
    setkey(date)

  # Exclude some GCMs and other useless variables
  tmeandf[, c(sprintf("tas_%s", gcmexcl), "ssp") := NULL]

  # Fill NAs for IITM_ESM in 2099 and SSP3
  fillvals <- tmeandf[year == 2098, tas_IITM_ESM, drop = T]
  tmeandf[year == 2099, tas_IITM_ESM := fillvals]
  
  #----- Calibration of projections
  
  # Save the historical period
  histdf <- tmeandf[year %between% histrange,]
  
  # Main calibration
  # tmeandf[, sprintf("cal2_%s", gcmlist) := bc_isimip3(tmeanobs,
  #     .SD[year %between% obsrange], .SD, year,
  #     year[year %between% obsrange], year),
  #   .SDcols = sprintf("tas_%s", gcmlist), by = month]
  tmeandf[, calperiod := cut(year, projrange, right = F)]
  tmeandf[, sprintf("cal_%s", gcmlist) := 
      lapply(gcmlist, function(gcm) bc_isimip3(
        histdf[["tmeanobs"]][histdf$month == month],
        histdf[[sprintf("tas_%s", gcm)]][histdf$month == month], 
        .SD[[sprintf("tas_%s", gcm)]],
        histdf$year[histdf$month == month], 
        histdf$year[histdf$month == month],
        year) |> unlist()), 
    by = .(month, calperiod)]
  
  
  # Calibrate all periods on the calibrated during historical period
  tmeandf[, sprintf("cons_%s", gcmlist) := 
    lapply(gcmlist, function(gcm) bc_isimip3(
      tmeandf[[sprintf("cal_%s", gcm)]][tmeandf$month == month & 
          tmeandf$year %between% histrange],
      .SD[[sprintf("cal_%s", gcm)]], .SD[[sprintf("cal_%s", gcm)]],
      tmeandf$year[tmeandf$month == month & tmeandf$year %between% histrange], 
      year, year) |> unlist()), 
    by = .(month, year5)]

  # Select years for tmean
  tmeandf <- tmeandf[year %between% range(projrange),]
  
  #----- Prepare ERFs 
  
  # Prepare basis parameters (common to all age groups)
  tper <- quantile(tmeanobs$tmeanobs, predper / 100)
  varknots <- tper[paste0(varper, ".0%")]
  varbound <- range(tper)
  argvar <- list(fun = varfun, degree = vardegree, knots = varknots,
    Bound = varbound)
  
  # Extract city-specific coefficients and order by age
  citycoef <- subset(coefs, URAU_CODE == city)
  
  # Extract simulated coefficients
  citycoefsim <- open_dataset("data/coef_simu.gz.parquet") |>
    filter(URAU_CODE == city, sim <= nsim) |>
    collect() |>
    mutate(agegroup = factor(agegroup, levels = agelabs))
  
  # Extract demographic projections
  cityproj <- projdata[URAU_CODE == city & ssp == issp, 
      .(agegroup, year5, pop, death)] |> 
    merge(tmeandf[, .(year, year5)], allow.cartesian = T)
  
  # Prepare parallel
  cl <- makeCluster(ncores)
  registerDoParallel(cl)

  #----- Loop on age groups, GCMs and scenarios
  citysspres <- foreach(a = agelabs, 
      acoef = iter(citycoef, by = "row"), 
      asim = isplit(citycoefsim, citycoefsim$agegroup),
      aproj = isplit(cityproj, cityproj$agegroup),
      .combine = rbind, .packages = packs) %:% 
    foreach(sc = c("demo", "full"), gcmprefix = c("cons", "cal"),
      .combine = rbind, .packages = packs) %:%
    foreach(gcm = gcmlist, tseries = iter(tmeandf[, .SD, 
      .SDcols = sprintf("%s_%s", gcmprefix, gcmlist)], by = "col"),
      .combine = rbind, .packages = packs) %dopar% 
  {
    
    .libPaths(libs)
    
    # Bind estimated coefficient and simulated ones
    allcoefs <- rbind(select(acoef, matches("b[[:digit:]]")), 
        select(asim$value, matches("b[[:digit:]]"))) |>
      as.matrix()

    # Evaluate MMT
    bper <- suppressWarnings(do.call(onebasis, c(list(x = tper), argvar)))
    ind <- tper %between% tper[c("25.0%", "99.0%")]
    mmt <- tper[ind][which.min(drop(bper[ind,] %*% allcoefs[1,]))]

    #----- Compute daily AN 

    # Create centred basis
    bvar <- suppressWarnings(do.call(onebasis,
      c(list(x = tseries), argvar)))
    cenvec <- do.call(onebasis, c(list(x = mmt), argvar))
    bcen <- scale(bvar, center = cenvec, scale = F)

    # INDICATOR FOR COLD/HEAT DAYS
    indheat <- tseries > mmt

    # COMPUTE THE DAILY CONTRIBUTIONS OF ATTRIBUTABLE DEATHS
    # NB: REMOVE RR<1 AS THIS CAN LEAD TO NEGATIVE DEATHS
    rr <- pmax(exp(drop(bcen %*% t(allcoefs))), 1)
    an <- drop((1 - 1 / rr) * aproj$value$death / 365)

    #----- Aggregate by year

    # Prepare data to aggregate
    # This includes deaths, pop and tmean that are kept to compute impacts
    colnames(an) <- c("est", sprintf("sim%i", 1:nsim))
    an <- cbind(aproj$value, heat = indheat, an)

    # Aggregate by year and temperature range
    an <- an[, c(list(range = c("tot", "heat", "cold"), 
        pop = pop[1], death = sum(death)), 
        lapply(.SD, function(x) c(sum(x), sum(x[heat]), sum(x[!heat])))), 
      by = year, .SDcols = patterns("est|sim")]
    
    # Free some memory and return
    an[, ":="(sc = sc, agegroup = a, gcm = gcm)]
  }
  
  stopCluster(cl)

  #----- Compute summaries
  
  # Rename
  setnames(citysspres, c("est", sprintf("sim%i", 1:nsim)), 
    paste0("an_", c("est", sprintf("sim%i", 1:nsim))))
  
  # Compute impacts
  cityimpacts <- impact(citysspres, perlen = perlen, ensonly = T, allage = T,
    diffsc = T, warming_win = warming_win[ssp == issp,])
  cityimpacts <- lapply(cityimpacts, mutate, city = city)
  names(cityimpacts) <- sprintf("city_%s", names(cityimpacts))
  
  # Summary of temperature
  tdf_long <- melt(tmeandf, id.vars = c("date", "year", "tmeanobs"), 
    measure.vars = list(tas = sprintf("tas_%s", gcmlist),
      cal = sprintf("cal_%s", gcmlist), 
      cons = sprintf("cons_%s", gcmlist)), 
    variable.name = "gcm")
  tdf_long[, gcm := gcmlist[gcm]]
  tsum <- tdf_long[, c(list(summary = c("mean", "cold", "heat")), 
      lapply(.SD, function(x) c(mean = mean(x, na.rm = T), 
        quantile(x, c(.01, .99), na.rm = T)))),
    by = .(year, gcm), .SDcols = c("tmeanobs", "tas", "cal", "cons")]
  
  # Quality of calibration
  calscore <- tsum[year %between% histrange, 
    lapply(.SD, function(x) sqrt(mean((tmeanobs - x)^2))), 
    .SDcols = c("tas", "cal"), by = .(gcm, summary)]
  
  #----- Return
  
  # Free up memory
  rm(tmeandf, tmeanobs, tmeanproj, tdf_long); gc()
  
  # Return results 
  c(list(res = citysspres, tsum = tsum[, city := city], 
    calibration = calscore[, city := city]), cityimpacts)
}

#----- European level results

# Track execution
Sys.time() - st

# Save
saveRDS(finalres, file = sprintf("temp/fullresults_%s.RDS", Sys.Date()))
