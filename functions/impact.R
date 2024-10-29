################################################################################
#
# Functions to compute and combine results
#
################################################################################

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
  
  #----- Compute impact measures
  
  # Compute attributable fraction
  if ("af" %in% measures){
    res[, af := an / death]
  }
  
  # Compute deaths rates
  if ("rate" %in% measures){
    res[, rate := an / pop]
  }
  
  # Compute cumulative AN
  if ("cuman" %in% measures){
    res[order(year), cuman := cumsum(an), 
      by = c("gcm", "range", "sc", "agegroup", "res")]
  }
  
  # Fill ANs inheriting from null death rates
  setnafill(res, fill = 0, cols = measures)
  
  #----- Results by period
  
  # Compute period
  res[, period := floor(year / perlen) * perlen]
  
  # Compute average between GCMs
  estres <- res[res == "est", lapply(.SD, mean, na.rm = T), 
    by = c("period", "sc", "range", "agegroup"), 
    .SDcols = measures]
  
  # Compute confidence intervals
  cires <- res[res != "est", 
    as.list(unlist(lapply(.SD, fquantile, c(.025, .975), na.rm = T))),
    by = c("period", "sc", "range", "agegroup"), .SDcols = measures]
  
  # GCM specific aggregation
  if (!ensonly){
    estresgcm <- res[res == "est", lapply(.SD, mean, na.rm = T), 
      by = c("period", "sc", "range", "agegroup", "gcm"), 
      .SDcols = measures]
    estres <- rbind(estres[, gcm := "ens"], estresgcm)
    ciresgcm <- res[res != "est", 
      as.list(unlist(lapply(.SD, fquantile, c(.025, .975), na.rm = T))),
      by = c("period", "sc", "range", "agegroup", "gcm"), .SDcols = measures]
    cires <- rbind(cires[, gcm := "ens"], ciresgcm)
    rm(estresgcm, ciresgcm)
  }
  
  # Rename
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
      by = c("gcm", "year"), allow.cartesian = T)
    
    # Compute average between GCMs
    estres <- res[res == "est", lapply(.SD, mean, na.rm = T), 
      by = c("level", "sc", "range", "agegroup"), 
      .SDcols = measures]
    
    # Compute confidence intervals
    cires <- res[res != "est", 
      as.list(unlist(lapply(.SD, fquantile, c(.025, .975), na.rm = T))),
      by = c("level", "sc", "range", "agegroup"), .SDcols = measures]
    
    # GCM specific aggregation
    if (!ensonly){
      estresgcm <- res[res == "est", lapply(.SD, mean, na.rm = T), 
        by = c("level", "sc", "range", "agegroup", "gcm"), 
        .SDcols = measures]
      estres <- rbind(estres[, gcm := "ens"], estresgcm)
      ciresgcm <- res[res != "est", 
        as.list(unlist(lapply(.SD, fquantile, c(.025, .975), na.rm = T))),
        by = c("level", "sc", "range", "agegroup", "gcm"), .SDcols = measures]
      cires <- rbind(cires[, gcm := "ens"], ciresgcm)
      rm(estresgcm, ciresgcm)
    }
    
    # Rename
    names(cires) <- gsub("\\.97.*\\%", "_high", names(cires))
    names(cires) <- gsub("\\.2.*\\%", "_low", names(cires))
    
    # Merge estimates and CIs
    levelres <- merge(estres, cires)
    rm(estres, cires); gc()
    
    # Prepare output
    out$level <- levelres
  }
  
  #----- Return
  rm(res)
  out
}


# Aggregate
impact_aggregate <- function(res, agg, vars = "an", by = key(res)){
  
  # Loop on variables to aggregate
  for (vi in names(agg)){
    restot <- res[, lapply(.SD, sum), by = setdiff(by, vi), .SDcols = vars]
    res <- rbind(res, restot[, (vi) := agg[vi]])
  }
  
  # Export
  res
}


# Assumes that the data.table has appropriate key set
impact_measures <- function(res, vars = "an", 
  measures = c("af", "rate", "cuman"), by = key(res))
{

  # Compute attributable fraction
  if ("af" %in% measures){
    res[, sprintf("af_%s", vars) := lapply(.SD, "/", death),
      .SDcols = vars]
  }
  
  # Compute deaths rates
  if ("rate" %in% measures){
    res[, sprintf("rate_%s", vars) := lapply(.SD, "/", pop),
      .SDcols = vars]
  }
  
  # Compute cumulative AN
  if ("cuman" %in% measures){
    res[, sprintf("cuman_%s", vars) := lapply(.SD, cumsum),
      .SDcols = vars, by = setdiff(by, "year")]
  }
  
  # Fill ANs inheriting from null death rates
  newvars <- outer(measures, vars, paste, sep = "_") |> c()
  setnafill(res, fill = 0, cols = newvars)
  
  # Return
  res
}


impact_summarise <- function(res, vars = "an", by = key(res), prob = .95){
  
  # Compute point estimate
  estres <- res[res == "est", lapply(.SD, mean, na.rm = T), 
    by = by, .SDcols = vars]
  
  # Compute confidence intervals
  alpha <- 1 - prob
  lims <- c(alpha / 2, 1 - (alpha / 2))
  cires <- res[res != "est", 
    as.list(unlist(lapply(.SD, fquantile, lims, na.rm = T))),
    by = by, .SDcols = vars]
  
  # Rename
  setnames(estres, vars, sprintf("%s_est", vars))
  names(cires) <- gsub(sprintf("\\.%s\\%%", lims[1] * 100), "_low", 
    names(cires))
  names(cires) <- gsub(sprintf("\\.%s\\%%", lims[2] * 100), "_high", 
    names(cires))
  
  # Merge and return
  merge(estres, cires)
}
