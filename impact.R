################################################################################
#
# Functions to compute and combine results
#
################################################################################

#--------------------------------
# Functions combining results between iterations
#--------------------------------

#----- Combining two successive iterations within the same level
combres <- function(x1, x2){
  
  # Sum full results for geographical aggregation
  fullres <- rbind(x1$res, x2$res)[, lapply(.SD, sum), 
    by = .(agegroup, year, range, gcm, sc), 
    .SDcols = c("pop", "death", "an_est", sprintf("an_sim%i", 1:nsim))]
  
  # Bind results of impact measures
  impactres <- Map(rbind, x1[names(x1) != "res"], x2[names(x2) != "res"])
  
  # Return everything
  c(list(res = fullres), impactres)
}

#----- Computing impacts for a specific level
finalcomb <- function(x, geolev = NULL, geoval = NULL, ...){
  
  # Compute impacts for this geographical level
  aggres <- impact(x$res, ...)

  # Add label
  if (!is.null(geoval)){
    aggres <- lapply(aggres, function(y) y[, (geolev) := geoval])
  }

  # Reorganise and return
  names(aggres) <- sprintf("%s_%s", geolev, names(aggres))
  c(x, aggres)
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
#' If `stdweight` is not null, standardised rates 
#' are also computed with provided weights.
#' @param perlen The length of a period for aggregation, in years.
#' @param ensonly If FALSE, GCM-specific results are also computed.
#' @param allage If TRUE, also compute results for aggregated age groups.
#' @param diffsc If TRUE, compute differences of impact between sub-scenarios.
#' @param stdweights Vectors of weights used to compute standardised weights. 
#' Names must correspond to age-groups foudn in `res`.
#' @param warming_win data.table containing years included in the warming level 
#' windows.

##### Function computing various impact measure from estimated ANs
impact <- function(res, measures = c("an", "af", "rate", "cuman"), 
  perlen = 1, ensonly = T, allage = T, diffsc = T, stdweight = NULL, 
  warming_win = NULL)
{
  
  # Detect AN columns
  ancols <- grep("an_", colnames(res), value = T)
  
  #----- Aggregate all ages
  
  if (allage){
    # Compute sum of all ages
    aggages <- res[, lapply(.SD, sum), 
      by = .(year, range, gcm, sc),
      .SDcols = c(ancols, "death", "pop")]
    
    # Add to the results and clean up
    res <- rbind(res, aggages[, agegroup := "all"])
    rm(aggages)
  }
  
  #----- Differences between scenarios
  
  if (diffsc){
    
    # Pivot to wide for more efficient computation
    reswide <- dcast(res, agegroup + year + range + gcm + death + pop ~ sc, 
      value.var = ancols)
    
    # Compute differences
    reswide <- reswide[, sprintf("%s_full-demo", ancols) := 
        lapply(ancols, function(nm){
          .SD[[sprintf("%s_full", nm)]] - .SD[[sprintf("%s_demo", nm)]]})]
    
    # Pivot back to long
    scvar <- c("demo", "full", "full-demo")
    res <- melt(reswide, 
      id.vars = c("agegroup", "year", "range", "gcm", "death", "pop"),
      measure.vars = lapply(ancols, sprintf, fmt = "%s_%s", scvar) |> 
        setNames(ancols), 
      variable.name = "sc")
    res[, sc := scvar[sc]]
    rm(reswide)
  }
  
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
    
    # Compute standardised rates
    if (!is.null(stdweight)){
      res <- merge(res, data.frame(agegroup = names(stdweight), w = stdweight),
        all.x = T)
      res[, gsub("an", "stdrate", ancols) := 
          lapply(.SD, function(x) sum(x * w, na.rm = T) / sum(w, na.rm = T)), 
        by = c("year", "gcm", "sc", "range"), 
        .SDcols = gsub("an", "rate", ancols)]
      res[, ":="(w = NULL)]
      measures <- c(measures, "stdrate")
    }
  }
  
  # Compute cumulative AN
  if ("cuman" %in% measures){
    res[order(year), gsub("an", "cuman", ancols) := lapply(.SD, cumsum), 
      by = c("gcm", "sc", "agegroup", "range"), .SDcols = ancols]
  }
  
  #----- Results by period
  
  # Compute period
  res[, period := floor(year / perlen) * perlen]
  
  # pivot to long for more efficient computation
  reslong <- melt(res, 
    id.vars = c("agegroup", "year", "range", "gcm", "sc", "period"),
    measure.vars = lapply(measures, function(x) grep(sprintf("^%s_", x), 
      colnames(res), value = T)) |> setNames(measures), 
    variable.name = "result")
  reslong[, result := gsub("an_", "", ancols)[result]]
  
  # Compute average between GCMs
  estres <- reslong[result == "est", lapply(.SD, mean, na.rm = T), 
    by = c("period", "agegroup", "sc", "range"), 
    .SDcols = measures]
  
  # Compute confidence intervals
  cires <- reslong[result != "est", 
    as.list(unlist(lapply(.SD, fquantile, c(.025, .975), na.rm = T))),
    by = c("period", "agegroup", "sc", "range"), 
    .SDcols = measures]
  
  # GCM specific aggregation
  if (!ensonly){
    estresgcm <- reslong[result == "est", lapply(.SD, mean, na.rm = T), 
      by = c("period", "agegroup", "sc", "range", "gcm"), 
      .SDcols = measures]
    estres <- rbind(estres[, gcm := "ens"], estresgcm)
    ciresgcm <- reslong[result != "est", 
      as.list(unlist(lapply(.SD, fquantile, c(.025, .975), na.rm = T))),
      by = c("period", "agegroup", "sc", "range", "gcm"), 
      .SDcols = measures]
    cires <- rbind(cires[, gcm := "ens"], ciresgcm)
    rm(estresgcm, ciresgcm)
  }
  
  # Rename
  setnames(estres, measures, sprintf("%s_est", measures))
  names(cires) <- gsub("\\.97.*\\%", "_high", names(cires))
  names(cires) <- gsub("\\.2.*\\%", "_low", names(cires))

  # Merge estimates and CIs
  periodres <- merge(estres, cires)
  rm(estres, cires)
  
  # Prepare output
  out <- list(period = periodres)
  
  #----- Results by warming level
  
  if (!is.null(warming_win)){

    # Merge to warming level windows
    reslong <- merge(reslong, warming_win, 
      by = c("gcm", "year"), allow.cartesian = T)
    
    # Ensemble averages and confidence intervals by period
    estres <- reslong[result == "est", lapply(.SD, mean, na.rm = T), 
      by = c("level", "agegroup", "sc", "range"), 
      .SDcols = measures]
    cires <- reslong[result != "est", 
      as.list(unlist(lapply(.SD, fquantile, c(.025, .975), na.rm = T))),
      by = c("level", "agegroup", "sc", "range"), 
      .SDcols = measures]
    
    # GCM specific aggregation
    if (!ensonly){
      estresgcm <- reslong[result == "est", lapply(.SD, mean, na.rm = T), 
        by = c("level", "agegroup", "sc", "range", "gcm"), 
        .SDcols = measures]
      estres <- rbind(estres[, gcm := "ens"], estresgcm)
      ciresgcm <- reslong[result != "est", 
        as.list(unlist(lapply(.SD, fquantile, c(.025, .975), na.rm = T))),
        by = c("level", "agegroup", "sc", "range", "gcm"), 
        .SDcols = measures]
      cires <- rbind(cires[, gcm := "ens"], ciresgcm)
      rm(estresgcm, ciresgcm)
    }
    
    # Rename
    setnames(estres, measures, sprintf("%s_est", measures))
    names(cires) <- gsub("\\.97.*\\%", "_high", names(cires))
    names(cires) <- gsub("\\.2.*\\%", "_low", names(cires))
    
    # Merge estimates and CIs
    levelres <- merge(estres, cires)
    rm(estres, cires)
    
    # return
    out$level <- levelres
  }
  
  #----- Return
  rm(reslong)
  out
}


