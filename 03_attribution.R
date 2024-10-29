################################################################################
# 
# Contrasting future heat and cold-related mortality under climate change, 
# demographic and adaptation scenarios in 854 European cities
#
# R Code Part 3: Perform health impact projections across cities
#
# Pierre Masselot & Antonio Gasparrini
#
################################################################################

#----- Prepare data

# Initialise dates for temperature
init_df <- data.table(date = dayvec)[,":="(
  day = mday(date), month = month(date), year = year(date)
)]
init_df[,  ":="(year5 = floor(year / 5) * 5)]
setkey(init_df, date)

# Create groups to iterate though cities
ncities <- nrow(cities)
cities$grp <- rep(1:ceiling(ncities / grpsize), each = grpsize)[1:ncities]

# Determine last city of each country
countries <- summarise(cities, lastcity = last(URAU_CODE), .by = CNTR_CODE) |>
  right_join(countries)

# Determine regions
regions <- summarise(cities, chg = max(grp), .by = region)

#----- Prepare the loop

# List of necessary packages
packs <- c("dlnm", "dplyr", "data.table", "doSNOW", "arrow", "collapse")
libs <- .libPaths()

# Initialize trace
dir.create(tdir, recursive = T)
writeLines(c(""), sprintf("%s/trace.txt", tdir))
cat(as.character(as.POSIXct(start <- Sys.time())), 
  file = sprintf("%s/trace.txt", tdir), append = T)

#----- Iterate on cities by chunks

# Chunks to iterate on
chunklist <- seq_len(max(cities$grp))

# Loop across SSPs and chunks
for (issp in ssplist) {
for (igrp in chunklist) {
  
  cat(as.character(Sys.time()), "Start looping through chunk", igrp, "\n", 
    file = sprintf("%s/trace.txt", tdir), append = T)
  
  # Extract cities
  grpcities <- subset(cities, grp == igrp, URAU_CODE, drop = T)
  
  # Prepare parallel
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  # Start looping through cities and SSPs
  dummy <- foreach(city = grpcities, .packages = packs) %dopar% 
  {
    
    # For when the libraries are stored in a non-standard location
    .libPaths(libs)
    
    # Trace
    cat(as.character(Sys.time()), as.character(city), as.character(issp), "\n", 
      file = sprintf("%s/trace.txt", tdir), append = T) |> try()
    
    #----- Load projection data
    
    # Load projections
    tmeanproj <- open_dataset("data/tmeanproj.gz.parquet") |>
      filter(URAU_CODE == city & ssp %in% c("hist", issp)) |> 
      select(!c(ssp, URAU_CODE)) |>
      collect()
    
    # Create the projection data.table
    tmeandf <- merge(init_df, tmeanproj, all.x = T) |>
      setkey(year, month, day)
    
    # Fill NAs for IITM_ESM in 2099 and SSP3
    if (issp == 3){
      fillvals <- tmeandf[year == 2098, tas_IITM_ESM, drop = T]
      tmeandf[year == 2099, tas_IITM_ESM := fillvals]
    }
    
    # Select GCMs
    tmeandf[, sprintf("tas_%s", gcmexcl) := NULL]
    
    #----- Get the historical data
    
    # Load observed temperature
    tmeanhist <- open_dataset("data/era5series.gz.parquet") |>
      filter(URAU_CODE == city) |> collect() |>
      rename(tmeanobs = era5landtmean) |>
      dplyr::select(!URAU_CODE) |> as.data.table()
    
    # Extract quantiles before doing any any selection
    tper <- quantile(tmeanhist$tmeanobs, predper / 100)
    
    # Add GCMs for historical period
    tmeanhist <- merge(tmeanhist, tmeandf[year %between% histrange, ], 
      by = "date", all.y = T)
    
    #----- Calibration of projections
    
    # Melt data.table
    tmeandf <- melt(tmeandf, 
      id.vars = c("date", "year5", "year", "month", "day"), 
      measure.vars = patterns("tas_"), variable.name = "gcm", 
      value.name = "tas")
    tmeandf[, gcm := gsub("tas_", "", gcm, fixed = T)]
    
    # Create periods of calibration
    tmeandf[, calperiod := cut(year, c(histrange[1], projrange), right = F,
      labels = c("hist", 
        paste(projrange[-length(projrange)], projrange[-1] - 1, sep = "-")))]
    setkey(tmeandf, calperiod, date, gcm)
    
    # Index of historical period
    hind <- tmeandf$calperiod == "hist"
    
    # Calibrate projections: for full climate change
    tmeandf[, full := isimip3(
      obshist = tmeanhist[month == .BY$month, tmeanobs],
      simhist = tmeanhist[month == .BY$month, .SD, 
        .SDcols = sprintf("tas_%s", .BY$gcm)][[1]],
      simfut = tas, 
      yearobshist = tmeanhist[month == .BY$month, year], 
      yearsimhist = tmeanhist[month == .BY$month, year], 
      yearsimfut = year), 
      by = .(month, calperiod, gcm)]
      
    # Create the no climate change series 
    # Recalibrate each 5 y period on last 5y of historical period
    tmeandf[, demo := isimip3(
      obshist = tmeandf[month == .BY$month & gcm == .BY$gcm & 
        year5 == (min(projrange) - perlen), full],
      simhist = full, simfut = full, 
      yearobshist = tmeandf[month == .BY$month & gcm == .BY$gcm & 
        year5 == (min(projrange) - perlen), year], 
      yearsimhist = year, yearsimfut = year), 
      by = .(month, year5, gcm)]
    
    #----- Export summary of temperature
    
    # Full distribution for historical period (to assess calibration)
    tsumhist <- tmeandf[calperiod == "hist", c(list(perc = predper), 
        lapply(.SD, function(x) fquantile(x, predper / 100))),
      .SDcols = c("tas", "full", "demo"),
      by = gcm]
    
    # Reduced summary for other periods
    redper <- c(0, 1, 25, 50, 75, 99, 100)
    tsumproj <- tmeandf[calperiod != "hist", c(list(perc = c(redper, "mean")), 
      lapply(.SD, function(x) 
        c(fquantile(x, redper / 100), mean(x)))),
      by = .(calperiod, gcm), 
      .SDcols = c("tas", "full", "demo")]
    
    #----- Prepare ERFs 
    
    # Remove part of the historical period for which we don't want ANs
    tmeandf <- tmeandf[year >= (min(projrange) - perlen),]
    
    # Prepare basis parameters (common to all age groups)
    # tper extracted above (after reading tmeanhist)
    varknots <- tper[paste0(varper, ".0%")]
    varbound <- range(tper)
    argvar <- list(fun = varfun, degree = vardegree, knots = varknots,
      Bound = varbound)
    
    # Loop on age groups
    for (a in agelabs){
      
      #----- Load coefficients
      
      # Point estimate
      ptcoef <- subset(coefs, URAU_CODE == city & agegroup == a) |> 
        select(matches("b[[:digit:]]"))
      
      # Simulations (stored on disk)
      simcoef <- open_dataset("data/coef_simu.gz.parquet") |>
        filter(URAU_CODE == city & agegroup == a, sim <= nsim) |>
        select(matches("b[[:digit:]]")) |>
        collect()
      
      # Put together
      allcoefs <- rbind(ptcoef, simcoef) |> t()
      colnames(allcoefs) <- c("est", sprintf("sim%i", 1:nsim))
      
      #----- Prepare the basis
      
      # Evaluate MMT
      bper <- suppressWarnings(do.call(onebasis, c(list(x = tper), argvar)))
      ind <- tper %between% tper[c("25.0%", "99.0%")]
      mmt <- tper[ind][which.min(drop(bper[ind,] %*% allcoefs[,1]))]
      
      # Create centred basis
      cenvec <- do.call(onebasis, c(list(x = mmt), argvar))
      
      # Extract death rate projections
      agedf <- merge(
        tmeandf[, .(year, year5, gcm, full, demo)], 
        projdata[URAU_CODE == city & agegroup == a & ssp %in% c("hist", issp), 
          .(year5, death, pop)], 
        by = c("year5"), all.x = T)
      
      # Melt for more efficient computation
      agedf <- melt(agedf, measure.vars = c("full", "demo"), 
        variable.name = "sc", value.name = "tas")
      agedf[, range := factor(tas > mmt, lab = c("cold", "heat"))]
      
      # Extract adaptation scenarios and expand data
      iadapt <- subset(adaptdf, ssp == issp, -ssp)
      agedf <- apply(iadapt, 1, function(ada) 
        cbind(agedf, adapt = ada["adapt"], 
          adapt_fac = ada[as.character(agedf$range)])) |>
        rbindlist()
      agedf[, adapt_fac := as.numeric(adapt_fac)]
      
      #----- Compute annual AN
      
      # Compute total AN for each year, temp range and adaptation scenario
      setkey(agedf, sc, range, year, adapt, gcm)
      resy <- agedf[, .(pop = pop[1], death = death[1], 
        res = c("est", sprintf("sim%i", 1:nsim)), an = {
          
          # Compute RR
          bcen <- do.call(onebasis, c(list(x = tas), argvar)) |>
            scale(center = cenvec, scale = F) |> 
            suppressWarnings()  
          rr <- pmax(exp(bcen %*% allcoefs), 1)
          
          # Adapt RR
          rr <- 1 + (rr - 1) * (1 - adapt_fac / 100)
          
          # Compute AN
          an <- (1 - 1 / rr) * death / 365
          
          # Aggregate
          colSums(an)
          
        }), by = c("sc", "range", "year", "adapt", "gcm")]
      
      # Fill missing years with zero
      resy <- expand.grid(
        c(lapply(agedf[, c("sc", "range", "year", "adapt", "gcm")], unique),
          list(res = c("est", sprintf("sim%i", 1:nsim))))) |>
        join(resy, how = "left") |>
        as.data.table()
      resy[, ":="(death = unique(na.omit(death)), 
        pop = unique(na.omit(pop))), 
        by = c("year")]
      setnafill(resy, fill = 0, cols = "an")
      
      # Save in temporary directory
      dir.create(sprintf("%s/proj_loop/%s/%s", tdir, city, a), 
        recursive = T)
      write_dataset(resy, sprintf("%s/proj_loop/%s/%s", tdir, 
        city, a), partitioning = c("adapt"), hive_style = F)
      
      #----- Compute impacts
      
      # Total tmean range
      resy <- impact_aggregate(resy, agg = c(range = "tot"), vars = "an",
        by = c("sc", "year", "gcm", "range", "res", "adapt", "pop", "death"))
      
      # Compute part due to climate change
      resy <- dcast(resy, range + year + adapt + res + gcm + death + pop ~ sc,
        value.var = "an")
      resy[, clim := full - demo]
      
      # Compute impact measures and rename
      resy <- impact_measures(resy, vars = c("full", "demo", "clim"),
        by = c("year", "adapt", "gcm", "range", "res"))
      setnames(resy, c("full", "demo", "clim"),
        sprintf("an_%s", c("full", "demo", "clim")))
      allvars <- lapply(c("full", "demo", "clim"), grep, names(resy),
        value = T) |> unlist()
      
      # Aggregate by period
      resy[, period := floor(year / perlen) * perlen]
      periodres <- impact_summarise(resy, vars = allvars,
        by = c("period", "adapt", "range"))
      
      # Aggregate by warming level
      resy <- merge(resy, subset(warming_win, ssp == issp, -ssp),
        by = c("gcm", "year"), allow.cartesian = T)
      levelres <- impact_summarise(resy, vars = allvars,
        by = c("level", "adapt", "range"))
      
      # Save impacts
      idir <- sprintf("%s/proj_city_period/%s/%s/%s", 
        tdir, city, issp, a)
      dir.create(idir, recursive = T)
      write_parquet(periodres, sprintf("%s/res.parquet", idir))
      idir <- sprintf("%s/proj_city_level/%s/%s/%s", 
        tdir, city, issp, a)
      dir.create(idir, recursive = T)
      write_parquet(levelres, sprintf("%s/res.parquet", idir))
      rm(resy); gc()
    }
    
    # Export temp summary
    dir.create(sprintf("%s/proj_tsum/%s/%s", tdir, city, issp), 
      recursive = T)
    rbind(tsumhist[, calperiod := "hist"], tsumproj) |>
      write_parquet(sprintf("%s/proj_tsum/%s/%s/tsum.parquet", 
        tdir, city, issp))
    
    #----- Impacts for all ages
    
    # Sum results for all ages
    resa <- open_dataset(sprintf("%s/proj_loop/%s", tdir, city), 
      partitioning = c("agegroup", "adapt")) |>
      group_by(year, gcm, range, res, adapt, sc) |>
      summarise(across(all_of(c("an", "pop", "death")), 
        sum)) |>
      ungroup() |>
      collect() |> as.data.table()
    
    # Total tmean range
    resa <- impact_aggregate(resa, agg = c(range = "tot"), vars = "an",
      by = c("sc", "year", "gcm", "range", "res", "adapt", "pop", "death"))
    
    # Compute part due to climate change
    resa <- dcast(resa, range + year + adapt + res + gcm + death + pop ~ sc,
      value.var = "an")
    resa[, clim := full - demo]
    
    # Compute impact measures and rename
    resa <- impact_measures(resa, vars = c("full", "demo", "clim"),
      by = c("year", "adapt", "gcm", "range", "res"))
    setnames(resa, c("full", "demo", "clim"),
      sprintf("an_%s", c("full", "demo", "clim")))
    allvars <- lapply(c("full", "demo", "clim"), grep, names(resa),
      value = T) |> unlist()
    
    # Aggregate by period
    resa[, period := floor(year / perlen) * perlen]
    periodres <- impact_summarise(resa, vars = allvars,
      by = c("period", "adapt", "range"))
    
    # Aggregate by warming level
    resa <- merge(resa, subset(warming_win, ssp == issp, -ssp),
      by = c("gcm", "year"), allow.cartesian = T)
    levelres <- impact_summarise(resa, vars = allvars,
      by = c("level", "adapt", "range"))
    
    # Save impacts
    idir <- sprintf("%s/proj_city_period/%s/%s/all", 
      tdir, city, issp)
    dir.create(idir, recursive = T)
    write_parquet(periodres, sprintf("%s/res.parquet", idir))
    idir <- sprintf("%s/proj_city_level/%s/%s/all", 
      tdir, city, issp)
    dir.create(idir, recursive = T)
    write_parquet(levelres, sprintf("%s/res.parquet", idir))
    rm(resa)
      
      
  } 
  
  stopCluster(cl)

  
  #----------------------
  # Aggregate impacts
  #----------------------
  
  #----- Country level
  
  cat(as.character(Sys.time()), "Aggregate country results for chunk", igrp, "\n", 
    file = sprintf("%s/trace.txt", tdir), append = T)
  
  # Extract represented countries
  grpcntr <- subset(cities, grp == igrp) |> 
    summarise(lastgrp = last(URAU_CODE), .by = CNTR_CODE) |>
    left_join(countries, by = "CNTR_CODE") |>
    mutate(complete = lastgrp == lastcity)
  
  # Prepare parallel (replace with dopar below)
  # cl <- makeCluster(ncores)
  # registerDoParallel(cl)  
  
  # Loop over countries
  dummy <- foreach(cntr = iter(grpcntr, by = "row"), .packages = packs) %:%
    foreach(ada = subset(adaptdf, ssp == issp, adapt, drop = T), 
      .packages = packs) %do% 
  {
  
    # Extract country and adapt results and sum
    cntrres <- open_dataset(sprintf("%s/proj_loop", tdir), 
        partitioning = c("city", "agegroup", "adapt")) |>
      filter(substr(city, 1, 2) == cntr$CNTR_CODE, adapt == ada) |>
      group_by(year, gcm, range, agegroup, res, adapt, sc) |>
      summarise(across(all_of(c("an", "pop", "death")), sum)) |>
      ungroup() |>
      collect() |> as.data.table()
    
    # Export the aggregated country in temp directory
    dir.create(sprintf("%s/proj_loop/%s", tdir, cntr$CNTR_CODE), 
      recursive = T)
    write_dataset(cntrres, sprintf("%s/proj_loop/%s", tdir, cntr$CNTR_CODE),
      partitioning = c("agegroup", "adapt"),
      hive_style = F)
    
    # If the country is completed compute country level impacts
    if (cntr$comp) {
      
      if (ada == subset(adaptdf, ssp == issp[1], adapt, drop = T)[1]) cat(
          as.character(Sys.time()), "Aggregate country", cntr$CNTR_CODE, "\n", 
          file = sprintf("%s/trace.txt", tdir), append = T)
      
      cntrres[, ":="(adapt = NULL)]
      
      # Total tmean range
      cntrres <- impact_aggregate(cntrres, agg = c(agegroup = "all"), 
        vars = c("an", "pop", "death"),
        by = c("sc", "year", "gcm", "range", "res", "agegroup"))
      cntrres <- impact_aggregate(cntrres, agg = c(range = "tot"), vars = "an",
        by = c("sc", "year", "gcm", "range", "res", "agegroup", "pop", "death"))
      
      # Compute part due to climate change
      cntrres <- dcast(cntrres, 
        range + year + agegroup + res + gcm + death + pop ~ sc,
        value.var = "an")
      cntrres[, clim := full - demo]
      
      # Compute impact measures and rename
      cntrres <- impact_measures(cntrres, vars = c("full", "demo", "clim"),
        by = c("year", "agegroup", "gcm", "range", "res"))
      setnames(cntrres, c("full", "demo", "clim"),
        sprintf("an_%s", c("full", "demo", "clim")))
      allvars <- lapply(c("full", "demo", "clim"), grep, names(cntrres),
        value = T) |> unlist()
      
      # Aggregate by period
      cntrres[, period := floor(year / perlen) * perlen]
      periodres <- impact_summarise(cntrres, vars = allvars,
        by = c("period", "agegroup", "range"))
      
      # Aggregate by warming level
      cntrres <- merge(cntrres, subset(warming_win, ssp == issp, -ssp),
        by = c("gcm", "year"), allow.cartesian = T)
      levelres <- impact_summarise(cntrres, vars = allvars,
        by = c("level", "agegroup", "range"))
      
      # Save impact
      idir <- sprintf("%s/proj_country_period/%s/%s/%s", 
        tdir, cntr$CNTR_CODE, issp, ada)
      dir.create(idir, recursive = T)
      write_parquet(periodres, sprintf("%s/res.parquet", idir))
      idir <- sprintf("%s/proj_country_level/%s/%s/%s", 
        tdir, cntr$CNTR_CODE, issp, ada)
      dir.create(idir, recursive = T)
      write_parquet(levelres, sprintf("%s/res.parquet", idir))
      
      rm(cntrres); gc()
      NULL
    } 
  }
  
  # stopCluster(cl)
  
  #----- Aggregate region if relevant
  
  # Open dataset
  grpds <- open_dataset(sprintf("%s/proj_loop", tdir), 
    partitioning = c("city", "agegroup", "adapt"))
  
  # Regions that are finalised in the current chunk
  grpregs <- subset(regions, chg == igrp, region, drop = T)
  
  # Prepare parallel (replace with dopar below)
  # cl <- makeCluster(ncores)
  # registerDoParallel(cl)  
  
  # Loop across regions to aggregate
  dummy <- foreach(reg = grpregs, .packages = packs) %:%
    foreach(ada = subset(adaptdf, ssp == issp, adapt, drop = T), 
      .packages = packs) %do% 
  {
  
    if (ada == subset(adaptdf, ssp == issp[1], adapt, drop = T)[1]) cat(
          as.character(Sys.time()), "Aggregate region", reg, "\n", 
          file = sprintf("%s/trace.txt", tdir), append = T)
    
    # Extract relevant countries
    regcntr <- subset(countries, region %in% reg, CNTR_CODE, drop = T)
      
    # Extract country and age group results and sum
    regres <- grpds |>
      filter(city %in% regcntr, adapt == ada) |>
      group_by(year, gcm, range, agegroup, res, sc) |>
      summarise(across(all_of(c("an", "pop", "death")), sum)) |>
      ungroup() |>
      collect() |> as.data.table()
    
    # Export the aggregated country in temp directory
    dir.create(sprintf("%s/proj_reg/%s/%s/%s", tdir, reg, issp, ada), 
      recursive = T)
    write_parquet(regres, sprintf("%s/proj_reg/%s/%s/%s/an.parquet", 
      tdir, reg, issp, ada))
    
    # Total tmean range
    regres <- impact_aggregate(regres, agg = c(agegroup = "all"), 
      vars = c("an", "pop", "death"),
      by = c("sc", "year", "gcm", "range", "res", "agegroup"))
    regres <- impact_aggregate(regres, agg = c(range = "tot"), vars = "an",
      by = c("sc", "year", "gcm", "range", "res", "agegroup", "pop", "death"))
    
    # Compute part due to climate change
    regres <- dcast(regres, 
      range + year + agegroup + res + gcm + death + pop ~ sc,
      value.var = "an")
    regres[, clim := full - demo]
    
    # Compute impact measures and rename
    regres <- impact_measures(regres, vars = c("full", "demo", "clim"),
      by = c("year", "agegroup", "gcm", "range", "res"))
    setnames(regres, c("full", "demo", "clim"),
      sprintf("an_%s", c("full", "demo", "clim")))
    allvars <- lapply(c("full", "demo", "clim"), grep, names(regres),
      value = T) |> unlist()
    
    # Aggregate by period
    regres[, period := floor(year / perlen) * perlen]
    periodres <- impact_summarise(regres, vars = allvars,
      by = c("period", "agegroup", "range"))
    
    # Aggregate by warming level
    regres <- merge(regres, subset(warming_win, ssp == issp, -ssp),
      by = c("gcm", "year"), allow.cartesian = T)
    levelres <- impact_summarise(regres, vars = allvars,
      by = c("level", "agegroup", "range"))
      
    # Save impact
    idir <- sprintf("%s/proj_region_period/%s/%s/%s", 
      tdir, reg, issp, ada)
    dir.create(idir, recursive = T)
    write_parquet(periodres, sprintf("%s/res.parquet", idir))
    idir <- sprintf("%s/proj_region_level/%s/%s/%s", 
      tdir, reg, issp, ada)
    dir.create(idir, recursive = T)
    write_parquet(levelres, sprintf("%s/res.parquet", idir))
    
    rm(regres); gc()
    NULL
  }
  
  # stopCluster(cl)
  
  #----- Erase all temporary city/country files
  
  # Files to erase
  allf <- list.files(sprintf("%s/proj_loop", tdir))
  delf <- allf %in% c(as.character(grpcities), 
    subset(countries, region %in% grpregs, CNTR_CODE, drop = T))
  
  # Erase
  unlink(sprintf("%s/proj_loop/%s", tdir, allf[delf]), recursive = T)
}
}


#-----------------------
# Tidy up everything
#-----------------------

#----- Compute EU level results

cat(as.character(Sys.time()), "Aggregate EU\n", 
  file = sprintf("%s/trace.txt", tdir), append = T)

# Open dataset
euds <- open_dataset(sprintf("%s/proj_reg", tdir), 
  partitioning = c("region", "ssp", "adapt"))

# Loop across SSP
euimpres <- foreach(issp = ssplist, 
    .combine = function(x, y) Map(rbind, x, y)) %:%
  foreach(ada = subset(adaptdf, ssp == issp, adapt, drop = T), 
    .combine = function(x, y) Map(rbind, x, y)) %do% 
{
  # Get regional results and sum
  eures <- euds |>
    filter(ssp == issp, adapt == ada) |>
    group_by(year, gcm, range, agegroup, res, sc) |>
    summarise(across(all_of(c("an", "pop", "death")), sum)) |>
    ungroup() |>
    collect() |> as.data.table()
  
  # Total tmean range
  eures <- impact_aggregate(eures, agg = c(agegroup = "all"), 
    vars = c("an", "pop", "death"),
    by = c("sc", "year", "gcm", "range", "res", "agegroup"))
  eures <- impact_aggregate(eures, agg = c(range = "tot"), vars = "an",
    by = c("sc", "year", "gcm", "range", "res", "agegroup", "pop", "death"))
  
  # Compute part due to climate change
  eures <- dcast(eures, 
    range + year + agegroup + res + gcm + death + pop ~ sc,
    value.var = "an")
  eures[, clim := full - demo]
  
  # Compute impact measures and rename
  eures <- impact_measures(eures, vars = c("full", "demo", "clim"),
    by = c("year", "agegroup", "gcm", "range", "res"))
  setnames(eures, c("full", "demo", "clim"),
    sprintf("an_%s", c("full", "demo", "clim")))
  allvars <- lapply(c("full", "demo", "clim"), grep, names(eures),
    value = T) |> unlist()
  
  # Aggregate by period
  eures[, period := floor(year / perlen) * perlen]
  periodres <- impact_summarise(eures, vars = allvars,
    by = c("period", "agegroup", "range"))
  gcmres <- impact_summarise(eures, vars = allvars,
    by = c("period", "agegroup", "range", "gcm"))
  periodres <- rbind(periodres[, gcm := "ens"], gcmres)
  
  # Aggregate by warming level
  eures <- merge(eures, subset(warming_win, ssp == issp, -ssp),
    by = c("gcm", "year"), allow.cartesian = T)
  levelres <- impact_summarise(eures, vars = allvars,
    by = c("level", "agegroup", "range"))
  gcmres <- impact_summarise(eures, vars = allvars,
    by = c("level", "agegroup", "range", "gcm"))
  levelres <- rbind(levelres[, gcm := "ens"], gcmres)
  
  # Melt and return
  lapply(list(period = periodres, level = levelres), 
    function(x) cbind(x, ssp = issp, adapt = ada))
}
names(euimpres) <- sprintf("eu_%s", names(euimpres))

#----- Put together results from lower levels

# Load all results
finalres <- list(
  city_period = open_dataset(sprintf("%s/proj_city_period", tdir), 
    partitioning = c("city", "ssp", "agegroup")) |> collect(),
  city_level = open_dataset(sprintf("%s/proj_city_level", tdir), 
    partitioning = c("city", "ssp", "agegroup")) |> collect(),
  country_period = open_dataset(sprintf("%s/proj_country_period", tdir), 
    partitioning = c("country", "ssp", "adapt")) |> collect(),
  country_level = open_dataset(sprintf("%s/proj_country_level", tdir), 
    partitioning = c("country", "ssp", "adapt")) |> collect(),
  region_period = open_dataset(sprintf("%s/proj_region_period", tdir), 
    partitioning = c("region", "ssp", "adapt")) |> collect(),
  region_level = open_dataset(sprintf("%s/proj_region_level", tdir), 
    partitioning = c("region", "ssp", "adapt")) |> collect()
)

# Add the EU level
finalres <- c(finalres, euimpres)

#----- Pivot the sub-scenario

# Variables to melt
varlist <- expand.grid(c("an", "af", "rate", "cuman"), 
  c("est", "high", "low")) |> as.matrix()
measvar <- apply(varlist, 1, function(x) grep(
  sprintf("^%s_.*_%s", as.character(x[1]), as.character(x[2])), 
  colnames(finalres[[1]]), value = T),
  simplify = F)
names(measvar) <- apply(varlist, 1, paste, collapse = "_")

# Pivot all results
finalres <- lapply(finalres, melt, measure.vars = measvar, variable.name = "sc")

# Rename sc
sclist <- str_split_i(measvar[[1]], "_", 2)
finalres <- lapply(finalres, function(x) x[, sc := sclist[sc]])

#----- Save results

# Export everything in parquet (more efficient)
dir.create("results_parquet")
lapply(names(finalres), function(nm) {
  write_parquet(finalres[[nm]], sprintf("results_parquet/%s.parquet", nm))
}) |> invisible()

# Export everything in csv (more readable)
dir.create("results_csv")
lapply(names(finalres), function(nm) {
  write_csv_arrow(finalres[[nm]], sprintf("results_csv/%s.csv", nm))
}) |> invisible()

#----- Put together temperature summaries and copy to result

# Read temperature summaries and write
open_dataset(sprintf("%s/proj_tsum", tdir), partitioning = c("city", "ssp")) |>
  write_dataset("results_parquet", basename_template = "tsum{i}.parquet")
open_dataset(sprintf("%s/proj_tsum", tdir), partitioning = c("city", "ssp")) |>
  write_csv_dataset("results_csv", basename_template = "tsum{i}.csv",
    quote = "none")

# Erase temporary files
list.files(sprintf("%s/proj_tsum", tdir), include.dirs = T, full.names = T) |>
  unlink(recursive = T)

#----- The end

# Delete all created temporary files
unlink(sprintf("%s/%s", tdir, c("proj_loop", "proj_reg")), recursive = T)
unlink(list.files(tdir, "proj_", include.dirs = T, full.names = T), 
  recursive = T)


# How much time?
Sys.time() - start
