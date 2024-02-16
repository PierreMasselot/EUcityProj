################################################################################
# Attribution script
################################################################################

#----- Prepare the loop

# Directory for temporary saving (keep memory clear)
tdir <- tempdir()
# tdir <- "D:/Pierre/EUcityProj"

# Initialise data
init_df <- data.table(date = dayvec)[,":="(
  day = mday(date), month = month(date), year = year(date)
)]
init_df[,  ":="(year5 = floor(year / 5) * 5)]
setkey(init_df, date)

# List of necessary packages
packs <- c("dlnm", "dplyr", "data.table", "doSNOW", "arrow", "collapse")
libs <- .libPaths()

# Initialize trace
writeLines(c(""), "temp/attr.txt")
cat(as.character(as.POSIXct(start <- Sys.time())),
  file="temp/attr.txt", append=T)

# Create groups to iterate though cities
ncities <- nrow(cities)
cities$grp <- rep(1:ceiling(ncities / grpsize), each = grpsize)[1:ncities]

# Determine regions
regions <- summarise(cities, chg = max(grp), .by = region)

st <- Sys.time()

#----- Iterate on cities by chunks

# Loop across chunks
for (igrp in seq_len(max(cities$grp))) {
  
  cat(as.character(Sys.time()), "Start looping through chunk", igrp, "\n", 
    file = "temp/attr.txt", append = T)
  
  # Extract cities
  grpcities <- subset(cities, grp == igrp, URAU_CODE, drop = T)
  
  # Prepare parallel
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  # Start looping through cities
  res <- foreach(city = grpcities, .packages = packs) %dopar% {
    
    # For when the libraries are stored in a non-standard location
    .libPaths(libs)
    
    # Trace
    cat(as.character(Sys.time()), as.character(city), "\n", 
      file = "temp/attr.txt", append = T)
    
    #----- Load projection data
    
    # Load projections
    tmeanproj <- open_dataset("data/tmeanproj.gz.parquet") |>
      filter(URAU_CODE == city) |> collect() |>
      dplyr::select(!URAU_CODE)
    
    # Create the projection data.table
    tmeandf <- merge(init_df, tmeanproj, all.x = T) |>
      melt(id.vars = c("day", "month", "year", "year5", "ssp"), 
        measure.vars = patterns("tas_"), variable.name = "gcm", 
        value.name = "tas") |> 
      setkey(ssp, gcm, year, month, day)
    tmeandf[, gcm := gsub("tas_", "", gcm, fixed = T)]
    
    # Exclude some GCMs
    tmeandf <- tmeandf[gcm %in% gcmlist,]
    
    # Fill NAs for IITM_ESM in 2099 and SSP3
    fillvals <- tmeandf[year == 2098 & gcm == "IITM_ESM" & ssp == 3, 
      tas, drop = T]
    tmeandf[year == 2099 & gcm == "IITM_ESM" & ssp == 3, tas := fillvals]
    
    # Reorder
    setkey(tmeandf, year, month, day, ssp, gcm)
    
    #----- Get the historical data
    
    # Load observed temperature
    tmeanhist <- open_dataset("data/era5series.gz.parquet") |>
      filter(URAU_CODE == city) |> collect() |>
      rename(tmeanobs = era5landtmean) |>
      dplyr::select(!URAU_CODE) |> as.data.table()
    
    # Extract quantiles before doing any any selection
    tper <- quantile(tmeanhist$tmeanobs, predper / 100)
    
    # Keep only the defined historical period
    tmeanhist <- merge(init_df[year %between% histrange,], tmeanhist)
    
    # Add GCMs for historical period
    tmeanhist <- merge(tmeanhist, tmeanproj, by = "date")
    
    # Extract month and year
    setnames(tmeanhist, old = c("month", "year"), new = c("m", "y"))
    
    #----- Calibration of projections
    
    # Create periods of calibration
    tmeandf[, calperiod := cut(year, c(histrange[1], projrange), right = F)]
    
    # Calibrate projections: for full climate change
    tmeandf[, full := isimip3(obshist = tmeanhist[m == month, tmeanobs],
      simhist = tmeanhist[m == month, .SD[[1]], 
        .SDcols = sprintf("tas_%s", gcm)],
      simfut = tas, 
      yearobshist = tmeanhist[m == month, y], 
      yearsimhist = tmeanhist[m == month, y], 
      yearsimfut = year), 
      by = .(month, calperiod, ssp, gcm)]
    
    # Create the no climate change series 
    # Recalibrate each 5 y period on last 5y of historical period
    tmeandf[, demo := isimip3(obshist = tmeandf[month == .BY$month & 
        year5 == (min(projrange) - perlen) & gcm == .BY$gcm, full],
      simhist = full, simfut = full, 
      yearobshist = tmeandf[month == .BY$month & 
          year5 == (min(projrange) - perlen) & gcm == .BY$gcm, year], 
      yearsimhist = year, yearsimfut = year), 
      by = .(month, year5, ssp, gcm)]
    
    # Remove part of the historical period for which we don't want ANs
    tmeandf <- tmeandf[year5 >= (min(projrange) - perlen),]
    
    #----- Export summary of temperature
    
    # Summary of temperatures
    tsum <- tmeandf[, c(list(summary = c("mean", "cold", "heat")), 
      lapply(.SD, function(x) c(mean = mean(x, na.rm = T), 
        quantile(x, c(.01, .99), na.rm = T)))),
      by = .(year, gcm, ssp), .SDcols = c("tas", "full", "demo")]
    
    # Quality of calibration
    tsumhist <- tmeanhist[, c(list(summary = c("mean", "cold", "heat")), 
      lapply(.SD, function(x) c(mean = mean(x, na.rm = T), 
        quantile(x, c(.01, .99), na.rm = T)))),
      by = y, .SDcols = "tmeanobs"]
    calscore <- tsum[tsumhist, on = c(year = "y", summary = "summary")][,
      lapply(.SD, function(x) sqrt(mean((tmeanobs - x)^2))), 
      .SDcols = c("tas", "full"), by = .(gcm, summary),
    ]
    
    # Export
    dir.create(sprintf("%s/tsum/%s", tdir, city), recursive = T)
    write_parquet(tsum, sprintf("%s/tsum/%s/tsum.parquet", tdir, city))
    write_parquet(tsum, sprintf("%s/tsum/%s/cal.parquet", tdir, city))
    
    #----- Prepare ERFs 
    
    # Prepare basis parameters (common to all age groups)
    # tper extracted above (after reading tmeanhist)
    varknots <- tper[paste0(varper, ".0%")]
    varbound <- range(tper)
    argvar <- list(fun = varfun, degree = vardegree, knots = varknots,
      Bound = varbound)
    
    # Initialise the sum of age groups
    ageagg <- NULL
    
    # Loop on age groups
    for (a in agelabs){
      
      # Extract coefficients
      allcoefs <- rbind(
        # Point estimate
        subset(coefs, URAU_CODE == city & agegroup == a) |> 
          select(matches("b[[:digit:]]")),
        # Simulations (stored on disk)
        open_dataset("data/coef_simu.gz.parquet") |>
          filter(URAU_CODE == city & agegroup == a, sim <= nsim) |>
          select(matches("b[[:digit:]]")) |>
          collect()
      )
      allcoefs <- as.matrix(allcoefs) |> t()
      
      # Evaluate MMT
      bper <- suppressWarnings(do.call(onebasis, c(list(x = tper), argvar)))
      ind <- tper %between% tper[c("25.0%", "99.0%")]
      mmt <- tper[ind][which.min(drop(bper[ind,] %*% allcoefs[,1]))]
      
      # Extract death rate projections
      agedf <- merge(tmeandf[, .(year, year5, ssp, gcm, full, demo)], 
        projdata[URAU_CODE == city & agegroup == a, 
          .(year5, ssp, death, pop)], by = c("year5", "ssp"), all.x = T)
      
      # Create centred basis
      cenvec <- do.call(onebasis, c(list(x = mmt), argvar))
      
      #----- Compute daily AN 
      # Loop through GCMs and years with data.table
      res <- agedf[, c(list(range = rep(c("heat", "cold"), each = nsim + 1), 
        res = rep(c("est", sprintf("sim%i", 1:nsim)), 2), 
        death = death[1], pop = pop[1]), 
        lapply(.SD, function(x){
          # Center basis
          bcen <- do.call(onebasis, c(list(x = x), argvar)) |>
            scale(center = cenvec, scale = F) |> 
            suppressWarnings()
          
          # Daily AN contribution
          rr <- pmax(exp(bcen %*% allcoefs), 1)
          an <- (1 - 1 / rr) * death / 365
          
          # Sum all for the year
          c(colSums(an[x > mmt,, drop = F]), colSums(an[x <= mmt,, drop = F]))
        })), 
        by = .(ssp, gcm, year), .SDcols = c("full", "demo")]
      
      # Save in temporary directory
      dir.create(sprintf("%s/loop/%s/%s", tdir, substr(city, 1, 2), a), 
        recursive = T)
      write_parquet(res, sprintf("%s/loop/%s/%s/%s.parquet", tdir, 
        substr(city, 1, 2), a, city))
      
      # Compute impact
      impres <- impact(res, perlen = perlen, warming_win = warming_win)
      
      # Save impact
      lapply(names(impres), function(nm){
        dir.create(sprintf("%s/city_%s/%s/%s", tdir, nm, city, a), 
          recursive = T)
        write_parquet(impres[[nm]], 
          sprintf("%s/city_%s/%s/%s/impact.parquet", tdir, nm, city, a))
      })
      
      # Aggregate age groups
      ageagg <- rbind(ageagg, res)[, lapply(.SD, sum), 
        by = .(ssp, gcm, year, range, res), 
        .SDcols = c("death", "pop", "full", "demo")]
      
      rm(res)
    }
    
    # Compute impacts and return
    allageres <- impact(ageagg, perlen = perlen, warming_win = warming_win)
    
    # Save impact
    lapply(names(allageres), function(nm){
      dir.create(sprintf("%s/city_%s/%s/all", tdir, nm, city), 
        recursive = T)
      write_parquet(allageres[[nm]], 
        sprintf("%s/city_%s/%s/all/impact.parquet", tdir, nm, city))
    })
  } 
  
  stopCluster(cl)
  
  #----------------------
  # Aggregate impacts
  #----------------------
  
  cat(as.character(Sys.time()), "Aggregate results for chunk", igrp, "\n", 
    file = "temp/attr.txt", append = T)
  
  #----- Represented countries
  
  # Extract represented countries
  grpcntr <- subset(cities, grp == igrp) |> 
    # Determine which city was the last in the chunk
    summarise(lastgrp = last(URAU_CODE), .by = CNTR_CODE) |>
    # Check if the country is completed or not 
    left_join(countries) |>
    mutate(complete = lastgrp == lastcity)
  
  # Open dataset
  grpds <- open_dataset(sprintf("%s/loop", tdir), 
    partitioning = c("CNTR_CODE", "agegroup"))
  
  
  #----- Aggregate by country
  
  # Loop over age group within countries, cumulate age groups
  cntrres <- foreach(cntr = grpcntr$CNTR_CODE, comp = grpcntr$complete) %:%
    foreach(a = agelabs, .combine = combres, 
      .final = function(x) if (comp) allages(x,
        lev = "country", lab = cntr,  perlen = perlen, 
        warming_win = warming_win)) %do% 
  {
    
    # Extract country and age group results and sum
    cntrageres <- grpds |>
      filter(CNTR_CODE == cntr & agegroup == a) |>
      group_by(ssp, year, gcm, range, res) |>
      summarise(across(all_of(c("death", "pop", "full", "demo")), sum)) |>
      ungroup() |>
      collect() |> as.data.table()
    
    # If the country is completed compute country level impacts
    if (comp) {
      
      # Compute measures of impact by period and level
      impres <- impact(cntrageres, perlen = perlen, warming_win = warming_win)
      
      # Output impact
      lapply(names(impres), function(nm){
        dir.create(sprintf("%s/country_%s/%s/%s", tdir, nm, cntr, a), 
          recursive = T)
        write_parquet(impres[[nm]], 
          sprintf("%s/country_%s/%s/%s/impact.parquet", tdir, nm, cntr, a))
      })
    } else {
      # if not completed, don't bother compute
      impres <- NULL
    }
    
    # Export the aggregated cities in temp directory
    write_parquet(cntrageres, 
      sprintf("%s/loop/%s/%s/agg.parquet", tdir, cntr, a))
    
    # Return to then aggregate age groups
    cntrageres 
  }
  
  #----- Erase all temporary city files
  
  # Files to erase
  allf <- list.files(sprintf("%s/loop", tdir), recursive = T, full.names = T)
  keepf <- grep("agg\\.parquet", allf, value = T)
  delf <- setdiff(allf, keepf)

  # Erase
  unlink(delf, recursive = T)
  
  #----- Aggregate region if relevant
  
  if (igrp %in% regions$chg){
    
    # Countries
    reg <- subset(regions, chg == igrp, region, drop = T)
    regcntr <- subset(countries, region == reg, CNTR_CODE, drop = T)
    
    # Reload dataset
    regds <- open_dataset(sprintf("%s/loop", tdir), 
      partitioning = c("CNTR_CODE", "agegroup"))
    
    # Loop through ages
    foreach(a = agelabs, .combine = combres, .final = function(x) allages(x,
      lev = "region", lab = reg,  perlen = perlen, 
      warming_win = warming_win)) %do% 
    {
      
      # Extract country and age group results and sum
      regageres <- regds |>
        filter(CNTR_CODE %in% regcntr & agegroup == a) |>
        group_by(ssp, year, gcm, range, res) |>
        summarise(across(all_of(c("death", "pop", "full", "demo")), sum)) |>
        ungroup() |>
        collect() |> as.data.table()
      
      # Compute measures of impact by period and level
      impres <- impact(regageres, perlen = perlen, warming_win = warming_win)
        
      # Output impact
      lapply(names(impres), function(nm){
        dir.create(sprintf("%s/region_%s/%s/%s", tdir, nm, reg, a), 
          recursive = T)
        write_parquet(impres[[nm]], 
          sprintf("%s/region_%s/%s/%s/impact.parquet", tdir, nm, reg, a))
      })
      
      # Export the aggregated cities in temp directory
      dir.create(sprintf("%s/loopreg/%s/%s", tdir, reg, a), recursive = T)
      write_parquet(regageres, 
        sprintf("%s/loopreg/%s/%s/res.parquet", tdir, reg, a))
      
      # Return to then aggregate age groups
      regageres 
    }
    
    # Erase results for countries of this region
    deld <- sprintf("%s/loop/%s", tdir, regcntr)
    unlink(deld, recursive = T)
  }
}

#-----------------------
# Aggregate EU level
#-----------------------

# Open dataset
euds <- open_dataset(sprintf("%s/loopreg", tdir), 
  partitioning = c("region", "agegroup"))

# Loop through age groups
euageres <- foreach(a = agelabs, 
  .combine = function(x1, x2) Map(rbind, x1, x2)) %do% 
{
  
  # Extract country and age group results and sum
  ageres <- euds |>
    filter(agegroup == a) |>
    group_by(ssp, year, gcm, range, res) |>
    summarise(across(all_of(c("death", "pop", "full", "demo")), sum)) |>
    ungroup() |>
    collect() |> as.data.table()
  
  # Compute measures of impact by period and level
  impres <- impact(ageres, perlen = perlen, warming_win = warming_win)
  lapply(impres, function(x) x[,":="(agegroup = a, gcm = "ens")])
}

# Extract all ages for EU
eures <- euds |>
  group_by(ssp, year, gcm, range, res) |>
  summarise(across(all_of(c("death", "pop", "full", "demo")), sum)) |>
  ungroup() |>
  collect() |> as.data.table()

# Compute impacts for all ages (including all GCMs)
alleu <- impact(eures, perlen = perlen, warming_win = warming_win, ensonly = F)
alleu <- lapply(alleu, function(x) x[,":="(agegroup = "all")])

# Put together
alleu <- Map(rbind, euageres, alleu)
names(alleu) <- sprintf("eu_%s", names(alleu))

#--------------------------
# Reorganise results
#--------------------------

#----- Read city, country and region results

# List of folders
reslist <- expand.grid(c("city", "country", "region"), c("period", "level")) 
nmlist <- apply(reslist, 1, paste, collapse = "_")
dirlist <- sprintf("%s/%s", tdir, nmlist)

# Read all results
finalres <- Map(function(d, lev) open_dataset(d, 
  partitioning = c(lev, "agegroup")) |> collect(),
  dirlist, as.character(reslist[[1]]))
names(finalres) <- nmlist

# Add the EU level
finalres <- c(finalres, alleu)

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

#----- Expand historical period for easier plotting later

# Period elements
perres <- grep("period", names(finalres))

# Extract historical period and expand
histpers <- lapply(finalres[perres], function(x){
  his <- x[ssp == "hist",]
  hisexp <- lapply(ssplist, function(issp) copy(his)[, ssp := issp])
  rbindlist(hisexp)
})

# Remove historical period and add the expanded one
finalres[perres] <- lapply(finalres[perres], function(x) x[ssp != "hist",])
finalres[perres] <- Map(rbind, histpers, finalres[perres])

#----- Save results

# Results directory
resdir <- sprintf("results/%s_%s", Sys.Date(), nsim)
dir.create(resdir)

# Export everything
lapply(names(finalres), function(nm) {
  write_parquet(finalres[[nm]], sprintf("%s/%s.parquet", resdir, nm))
})

# Delete all created temporary files
unlink(sprintf("%s/%s", tdir, c("loop", "loopreg")), recursive = T)
unlink(dirlist, recursive = T)

#----- Put together temperature summaries and move

# Read summaries
tsums <- list(
  tsum = open_dataset(sprintf("%s/tsum", tdir), partitioning = c("city"),
      factory_options = list(selector_ignore_prefixes = "cal")) |>
    collect(),
  cal = open_dataset(sprintf("%s/tsum", tdir), partitioning = c("city"),
      factory_options = list(selector_ignore_prefixes = "tsum")) |>
    collect()
)

# Expand historical period
tsumhist <- lapply(tsums, function(x){
  his <- x[ssp == "hist",]
  hisexp <- lapply(ssplist, function(issp) copy(his)[, ssp := issp])
  rbindlist(hisexp)
})

# Remove historical period and add the expanded one
tsums <- lapply(tsums, function(x) x[ssp != "hist",])
tsums <- Map(rbind, tsumhist, tsums)

# Export
lapply(names(tsums), 
  function(nm) write_parquet(tsums[[nm]], sprintf("%s/%s.parquet", resdir, nm)))

# Erase temporary files
list.files(sprintf("%s/tsum", tdir), include.dirs = T, full.names = T) |>
  unlink(recursive = T)

#----- The end

Sys.time() - st