################################################################################
# RESULT EXPORT
################################################################################

#--------------------------
# In-text numbers
#--------------------------

#----- Patterns of excess deaths

# Death rates by period
lapply(1:3, function(i){ finalres$eu_period[agegroup == "all" & 
    sc == "clim" & gcm == "ens" & range == "tot" & ssp == i, 
  .(period, rate = sprintf("%2.1f (95%%eCI: %2.1f to %2.1f)", rate_est * byrate, 
    rate_low * byrate, rate_high * byrate))]
})

# Total numbers at the end of century
finalres$eu_period[agegroup == "all" & sc == "clim" & 
    gcm == "ens" & range == "tot" & ssp %in% ssplist & period == max(period),
  .(ssp, an = sprintf("%.0f (95%%eCI: %.0f to %.0f)", an_est, an_low, an_high))]

# Cumulative numbers
finalres$eu_period[agegroup == "all" & sc == "clim" & 
    gcm == "ens" & range == "tot" & ssp %in% ssplist & period == max(period),
  .(ssp, an = sprintf("%.0f (95%%eCI: %.0f to %.0f)", cuman_est, cuman_low, 
    cuman_high))]

# Death rates by warming level
finalres$eu_level[agegroup == "all" & sc == "clim" & gcm == "ens" & 
    range == "tot" & ssp == 3, 
  .(level, rate = sprintf("%2.1f (95%%eCI: %2.1f to %2.1f)", rate_est * byrate, 
    rate_low * byrate, rate_high * byrate),
    an = sprintf("%.0f (95%%eCI: %.0f to %.0f)", an_est, an_low, an_high))]

#----- Regional level

# Rate results
finalres$region_level[agegroup == "all" & sc == "clim" & 
    range == "tot" & ssp == 3, 
  .(region, level, 
    rate = sprintf("%2.1f (95%%eCI: %2.1f to %2.1f)", rate_est * byrate, 
      rate_low * byrate, rate_high * byrate))]

#----- Country level

countryres <- subset(finalres$country_level, 
  agegroup == "all" & sc == "clim" & range == "tot" & ssp == 3)

# Countries with increase decrease
countryres[, .N, by = .(level, sign(rate_est))]

# Most and least impacted country
countryres |> 
  filter(rate_est == max(rate_est), .by = level) |>
  mutate(rate = sprintf("%2.1f (95%%eCI: %2.1f to %2.1f)", 
    rate_est * byrate, rate_low * byrate, rate_high * byrate))
countryres |> 
  filter(rate_est == min(rate_est), .by = level) |>
  mutate(rate = sprintf("%2.1f (95%%eCI: %2.1f to %2.1f)", 
    rate_est * byrate, rate_low * byrate, rate_high * byrate))

# Relative increases
subset(finalres$country_period, 
  agegroup == "all" & range == "tot" & ssp == 3) |>
  summarise((rate_est[sc == "clim" & period == max(period)] - 
      rate_est[sc == "full" & period == min(period)]) / 
      rate_est[sc == "full" & period == min(period)],
    .by = country)

#----- Attributable fractions

# Extract total
finalres$eu_period[agegroup == "all" & sc == "full" & gcm == "ens" &
    range == "tot" & ssp %in% ssplist & period == max(period),
  .(ssp, af_est, af_low, af_high)]

# By country
cntraf <- finalres$country_period[agegroup == "all" & sc == "full" &
    range == "tot" & ssp %in% ssplist & period == max(period),
  .(country, ssp, af_est, af_low, af_high)]

#--------------------------
# Export results for Visualization
#--------------------------

#------ Prepare data

# Select data to export
expdata <- finalres$eu_period[ssp %in% ssplist & 
    !grepl("clim", sc) & gcm == "ens" & agegroup == "all",
  .(period, sc, range, ssp, an_est, an_low, an_high, 
    rate_est, rate_low, rate_high)]

# Rename columns
setnames(expdata, c("period", "sc", "range", "ssp", "an_est", "an_low", 
    "an_high", "rate_est", "rate_low", "rate_high"), 
  c("Period", "Sub-scenario", "Temperature Range", "SSP", "Excess", 
    "Excess lower", "Excess higher", "Excess rate", "Excess rate lower",
    "Excess rate higher"))

# Descriptions
desctab <- data.frame(column = c("Period", "Sub-scenario", "Temperature Range", 
    "SSP", "Excess", "Excess lower", "Excess higher", "Excess rate", 
    "Excess rate lower", "Excess rate higher"), 
  Description = c("Result period", "demo: Demographic change only (Temp distribution does not change); full: Both demography and temperature change; full - demo: difference btween the two, provides comparison for the same underlying population", 
    "Temperature range, compared to the Minimum Mortality Temperature. tot corresponds to net effect, i.e. the excess death expected when accounting for both heat and cold", 
    "SSP scenario", 
    "Total (Panel 1) and average annual (Panels 2 and 3) number of excess death attributed to temperature", 
    "Lower bound of uncertainty around Excess", 
    "Higher bound of uncertainty around Excess", 
    "Estimated annual excess death rates attrinuted to temperature, i.e. Excess / Population", 
    "Lower bound of uncertianty around Excess rate", 
    "higher bound of uncertainty around Excess rate"))

# Panel 1 data
panel1data <- subset(expdata, 
    Period > 2015 & SSP %in% c(1, 3) & `Sub-scenario` == "full-demo",
    c("Period", "Temperature Range", "SSP", "Excess")) |>
  mutate(Excess = perlen * Excess)

# Panel 2 data
panel2data <- subset(expdata, Period > 2015 & SSP %in% c(1, 3), 
    -c(`Excess rate`, `Excess rate lower`, `Excess rate higher`)) |>
  pivot_wider(names_from = "Sub-scenario", 
    values_from = c("Excess", "Excess lower", "Excess higher")) |>
  subset(select = -c(`Excess lower_demo`, `Excess lower_full`, 
    `Excess higher_demo`, `Excess higher_full`))

# Panel 3 data
panel3data <- subset(expdata, 
    Period > 2015 & SSP %in% c(1, 3) & `Sub-scenario` == "full-demo", 
    -c(`Sub-scenario`, `Excess rate`, `Excess rate lower`, 
      `Excess rate higher`)) |>
  pivot_wider(names_from = "Temperature Range", 
    values_from = c("Excess", "Excess lower", "Excess higher"))

# Range of years to reach warming levels
levelranges <- subset(warming_years, ssp %in% c(1, 3) & gcm %in% gcmlist) |> 
  group_by(level, ssp) |>
  reframe(range = paste(range(year, na.rm = T), collapse = "-"))

#----- Export

# Create excel workbook and sheets
wb <- createWorkbook()
addWorksheet(wb, "Data")
addWorksheet(wb, "ssp3 Panel 1 (dots)")
addWorksheet(wb, "ssp3 Panel 2 (Full,Dem)")
addWorksheet(wb, "ssp3 Panel 3 (heat vs cold)")
addWorksheet(wb, "ssp1 Panel 1 (dots)")
addWorksheet(wb, "ssp1 Panel 2 (Full,Dem)")
addWorksheet(wb, "ssp1 Panel 3 (heat vs cold)")
addWorksheet(wb, "Description")
addWorksheet(wb, "Warming levels")

# Write data
writeDataTable(wb, "Data", expdata)
writeDataTable(wb, "ssp3 Panel 1 (dots)", subset(panel1data, SSP == 3, -SSP))
writeDataTable(wb, "ssp3 Panel 2 (Full,Dem)", 
  subset(panel2data, SSP == 3, -SSP))
writeDataTable(wb, "ssp3 Panel 3 (heat vs cold)", 
  subset(panel3data, SSP == 3, -SSP))
writeDataTable(wb, "ssp1 Panel 1 (dots)", subset(panel1data, SSP == 1, -SSP))
writeDataTable(wb, "ssp1 Panel 2 (Full,Dem)", 
  subset(panel2data, SSP == 1, -SSP))
writeDataTable(wb, "ssp1 Panel 3 (heat vs cold)", 
  subset(panel3data, SSP == 1, -SSP))
writeDataTable(wb, "Description", desctab)
writeDataTable(wb, "Warming levels", levelranges)

# Write on disk
saveWorkbook(wb, sprintf("results/Visualisation_data_%s.xlsx", Sys.Date()), 
  overwrite = T)


