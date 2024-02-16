isimip3 <- function(obshist, simhist, simfut, 
  yearobshist, yearsimhist, yearsimfut, detrend = T)
{
  
  #----- Step 3: detrend series
  if (detrend){
    
    # Estimate trends
    obstrend <- lm(obshist ~ yearobshist, na.action = na.exclude) |> 
      predict() |> scale(scale = F)
    simhisttrend <- lm(simhist ~ yearsimhist, na.action = na.exclude) |> 
        predict() |> scale(scale = F)
    simfuttrend <- lm(simfut ~ yearsimfut, na.action = na.exclude) |> 
      predict() |> scale(scale = F)
    
    # Detrend
    obshist <- obshist - obstrend
    simhist <- simhist - simhisttrend
    simfut <- simfut - simfuttrend
  }
  
  #----- Step 5: Map the climate change signal of sim to obs
  
  # Compute empirical distribution function of observed series
  ecdfobs <- ecdf(obshist)(obshist)
  
  # Compute transfer function
  deltaadd <- quantile(simfut, ecdfobs) - quantile(simhist, ecdfobs)
  
  # Mapped future observed values
  obsfut <- deltaadd + obshist
  
  #----- Step 6: Quantile mapping
  
  # Fit Gaussian distributions to future series
  simfutcdf <- pnorm(simfut, mean(simfut, na.rm = T), sd(simfut, na.rm = T))
  
  # Map using "future observed"
  calsimfut <- qnorm(p = simfutcdf, mean = mean(obsfut, na.rm = T),
    sd = sd(obsfut, na.rm = T))
  
  #----- Step 7: add back trend
  if (detrend){
    calsimfut <- calsimfut + simfuttrend
  }
  
  # Return
  calsimfut
}
