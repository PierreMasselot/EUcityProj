# simhist and simfut can be data.frame with the same number of columns
# We assume yearsimhist and yearsimfut are common to all simhist and simfut

bc_isimip3 <- function(obshist, simhist, simfut, 
  yearobshist, yearsimhist, yearsimfut, detrend = T)
{

  #----- Some preprocessing
  if (length(dim(simhist)) < 2) simhist <- as.data.frame(simhist)
  if (length(dim(simfut)) < 2) simfut <- as.data.frame(simfut)
  
  #----- Step 3: detrend series
  if (detrend){
    
    # Estimate trends
    obstrend <- lm(obshist ~ yearobshist, na.action = na.exclude) |> 
      predict() |> scale(scale = F)
    simhisttrends <- apply(simhist, 2, function(x){
      lm(x ~ yearsimhist, na.action = na.exclude) |> 
        predict() |> scale(scale = F)
    })
    simfuttrends <- apply(simfut, 2, function(x){
      lm(x ~ yearsimfut, na.action = na.exclude) |> predict() |> scale(scale = F)
    })
    
    # Detrend
    obshist <- obshist - obstrend
    simhist <- simhist - simhisttrends
    simfut <- simfut - simfuttrends
  }
  
  #----- Step 5: Map the climate change signal of sim to obs
  
  # Compute empirical distribution function of observed series
  ecdfobs <- ecdf(obshist)(obshist)
  
  # Compute transfer function
  deltaadd <- apply(simfut, 2, quantile, ecdfobs, na.rm = T) - 
    apply(simhist, 2, quantile, ecdfobs, na.rm = T)

  # Mapped future observed values
  obsfut <- apply(deltaadd, 2, "+", obshist)
  
  #----- Step 6: Quantile mapping
  
  # Fit Gaussian distributions to future series
  simfutcdf <- Map(pnorm, simfut, colMeans(simfut, na.rm = T), 
    apply(simfut, 2, sd, na.rm = T))

  # Map using "future observed"
  calsimfut <- mapply(qnorm, p = simfutcdf, mean = colMeans(obsfut, na.rm = T),
    sd = apply(obsfut, 2, sd, na.rm = T))

  #----- Step 7: add back trend
  if (detrend){
    calsimfut <- calsimfut + simfuttrends
  }
  
  # Return
  as.data.frame(calsimfut)
}
