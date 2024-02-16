################################################################################
#
# Functions to compute and combine results
#
################################################################################

#--------------------------------
# Functions combining results between iterations
#--------------------------------

#----- Combining two successive ages within a city
combres <- function(x1, x2){
  
  # Sum full results for geographical aggregation
  fullres <- rbind(x1, x2)[, lapply(.SD, sum), 
    by = .(year, range, gcm, ssp, res), 
    .SDcols = c("pop", "death", "full", "demo")]
  
  # Return everything
  fullres
}

#----- Computing impacts for a specific level
finalcomb <- function(x, lev = NULL, lab = NULL, write = NULL, 
  format = "parquet", ...)
{
  
  # Compute impacts for this level
  aggres <- impact(x$res, ...)

  # Add label
  if (!is.null(lab)){
    aggres <- lapply(aggres, function(y) y[, (lev) := lab])
  }

  # Reorganise and return
  names(aggres) <- sprintf("%s_%s", lev, names(aggres))
  aggres <- c(x$aggres, aggres)
  res <- list(res = x$res, aggres = aggres)
  if (!is.null(write)) write_dataset(res$res, path = write, format = format)
  res
}

#----- Aggregate all ages
allages <- function(x, lev, lab, ...){
  
  # Compute impacts for this level
  aggres <- impact(x, ...)
  
  # Output impact
  lapply(names(aggres), function(nm){
    dir.create(sprintf("%s/%s_%s/%s/all", tdir, lev, nm, lab), 
      recursive = T)
    write_parquet(aggres[[nm]], 
      sprintf("%s/%s_%s/%s/all/impact.parquet", tdir, lev, nm, lab))
  })
}


#--------------------------------
# Compute impact measures and aggregate results
#--------------------------------

#' Compute various measures and aggregate simulations
#' 
#' @param res data.table containing all results from a geographical level, 
#' including simulations.
#' @param measures List of impact measures to compute. A subset of 
#' `c("an", "af", "rate", "cuman")`.
#' @param perlen The length of a period for aggregation, in years.
#' @param ensonly If FALSE, GCM-specific results are also computed.
#' @param warming_win data.table containing years included in the warming level 
#' windows.

##### Function computing various impact measure from estimated ANs
impact <- function(res, measures = c("an", "af", "rate", "cuman"), 
  perlen = 1, ensonly = T, warming_win = NULL)
{
  
  #----- Aggregate
  
  # Compute heat + cold
  totres <- res[, 
    .(death = death[1], pop = pop[1], full = sum(full), demo = sum(demo)), 
    by = c("ssp", "gcm", "year", "res")]
  res <- rbind(res, totres[, range := "tot"])
  rm(totres)

  # Compute the part due to climate change
  res[, clim := full - demo]
  
  # Columns containing ANs
  setnames(res, c("full", "demo", "clim"), 
    sprintf("an_%s", c("full", "demo", "clim")))
  ancols <- sprintf("an_%s", c("full", "demo", "clim"))
  
  #----- Compute impact measures
  
  # Compute attributable fraction
  if ("af" %in% measures){
    res[, gsub("an", "af", ancols) := lapply(.SD, "/", death), 
      .SDcols = ancols]
  }
  
  # Compute deaths rates
  if ("rate" %in% measures){
    res[, gsub("an", "rate", ancols) := lapply(.SD, "/", pop), 
      .SDcols = ancols]
  }
  
  # Compute cumulative AN
  if ("cuman" %in% measures){
    res[order(year), gsub("an", "cuman", ancols) := lapply(.SD, cumsum), 
      by = c("gcm", "range", "ssp", "res"), .SDcols = ancols]
  }
  
  # Store measure columns
  meascols <- c(outer(measures, c("full", "demo", "clim"), paste, sep = "_"))
  
  # Fill ANs inheriting from null death rates
  setnafill(res, fill = 0, cols = meascols)
  
  #----- Results by period
  
  # Compute period
  res[, period := floor(year / perlen) * perlen]
  
  # Compute average between GCMs
  estres <- res[res == "est", lapply(.SD, mean, na.rm = T), 
    by = c("period", "ssp", "range"), .SDcols = meascols]
  
  # Compute confidence intervals
  cires <- res[res != "est", 
    as.list(unlist(lapply(.SD, fquantile, c(.025, .975), na.rm = T))),
    by = c("period", "ssp", "range"), .SDcols = meascols]
  
  # GCM specific aggregation
  if (!ensonly){
    estresgcm <- res[res == "est", lapply(.SD, mean, na.rm = T), 
      by = c("period", "ssp", "range", "gcm"), .SDcols = meascols]
    estres <- rbind(estres[, gcm := "ens"], estresgcm)
    ciresgcm <- res[res != "est", 
      as.list(unlist(lapply(.SD, fquantile, c(.025, .975), na.rm = T))),
      by = c("period", "ssp", "range", "gcm"), .SDcols = meascols]
    cires <- rbind(cires[, gcm := "ens"], ciresgcm)
    rm(estresgcm, ciresgcm)
  }
  
  # Rename
  setnames(estres, meascols, sprintf("%s_est", meascols))
  names(cires) <- gsub("\\.97.*\\%", "_high", names(cires))
  names(cires) <- gsub("\\.2.*\\%", "_low", names(cires))

  # Merge estimates and CIs
  periodres <- merge(estres, cires)
  rm(estres, cires); gc()
  
  # Prepare output
  out <- list(period = periodres)
  
  #----- Results by warming level
  
  if (!is.null(warming_win)){

    # Merge to warming level windows
    res <- merge(res, warming_win, 
      by = c("gcm", "year", "ssp"), allow.cartesian = T)
    
    # Ensemble averages and confidence intervals by period
    estres <- res[res == "est", lapply(.SD, mean, na.rm = T), 
      by = c("level", "ssp", "range"), .SDcols = meascols]
    cires <- res[res != "est", 
      as.list(unlist(lapply(.SD, fquantile, c(.025, .975), na.rm = T))),
      by = c("level", "ssp", "range"), .SDcols = meascols]
    
    # GCM specific aggregation
    if (!ensonly){
      estresgcm <- res[res == "est", lapply(.SD, mean, na.rm = T), 
        by = c("level", "ssp", "range", "gcm"), .SDcols = meascols]
      estres <- rbind(estres[, gcm := "ens"], estresgcm)
      ciresgcm <- res[res != "est", 
        as.list(unlist(lapply(.SD, fquantile, c(.025, .975), na.rm = T))),
        by = c("level", "ssp", "range", "gcm"), .SDcols = meascols]
      cires <- rbind(cires[, gcm := "ens"], ciresgcm)
      rm(estresgcm, ciresgcm)
    }
    
    # Rename
    setnames(estres, meascols, sprintf("%s_est", meascols))
    names(cires) <- gsub("\\.97.*\\%", "_high", names(cires))
    names(cires) <- gsub("\\.2.*\\%", "_low", names(cires))
    
    # Merge estimates and CIs
    levelres <- merge(estres, cires)
    rm(estres, cires)
    
    # return
    out$level <- levelres
  }
  
  #----- Return
  rm(res)
  out
}


