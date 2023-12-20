################################################################################
#
# Proj-Adapt
# Table(s)
#
################################################################################

if(length(ls()) == 0){
  source("01_pkg_params.R")
  source("02_prep_data.R")
  finalres <- readRDS("temp/fullresults_2023-12-08.RDS")
  finalres <- lapply(finalres, rename_with, ~ gsub(".2.5%", "_low", .x, fixed = T))
  finalres <- lapply(finalres, rename_with, ~ gsub(".97.5%", "_high", .x, fixed = T))
}

#--------------------------
# Some results
#--------------------------

#----- Patterns of excess deaths

# Death rates by period
lapply(1:3, function(i){ finalres$eu_period[agegroup == "all" & 
    sc == "full-demo" & gcm == "ens" & range == "tot" & ssp == i, 
  .(period, rate = sprintf("%2.1f (95%%eCI: %2.1f to %2.1f)", rate_est * byrate, 
    rate_low * byrate, rate_high * byrate))]
})
  
# Total numbers at the end of century
finalres$eu_period[agegroup == "all" & sc == "full-demo" & 
    gcm == "ens" & range == "tot" & ssp %in% ssplist & period == max(period),
  .(ssp, an = sprintf("%.0f (95%%eCI: %.0f to %.0f)", an_est, an_low, an_high))]

# Cumulative numbers
finalres$eu_period[agegroup == "all" & sc == "full-demo" & 
    gcm == "ens" & range == "tot" & ssp %in% ssplist & period == max(period),
  .(ssp, an = sprintf("%.0f (95%%eCI: %.0f to %.0f)", cuman_est, cuman_low, 
    cuman_high))]

# Death rates by warming level
finalres$eu_level[agegroup == "all" & sc == "full-demo" & gcm == "ens" & 
    range == "tot" & ssp == 3, 
  .(level, rate = sprintf("%2.1f (95%%eCI: %2.1f to %2.1f)", rate_est * byrate, 
      rate_low * byrate, rate_high * byrate),
    an = sprintf("%.0f (95%%eCI: %.0f to %.0f)", an_est, an_low, an_high))]

#----- Regional level

# Rate results
finalres$region_level[agegroup == "all" & sc == "full-demo" & 
    range == "tot" & ssp == 3, 
  .(region, level, 
    rate = sprintf("%2.1f (95%%eCI: %2.1f to %2.1f)", rate_est * byrate, 
    rate_low * byrate, rate_high * byrate))]


#----- Country level

countryres <- subset(finalres$country_level, 
  agegroup == "all" & sc == "full-demo" & range == "tot" & ssp == 3)

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
  summarise((rate_est[sc == "full-demo" & period == max(period)] - 
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
# Big table of rates
#--------------------------

#----- Parameters

# Selected period
pertab <- c(2050, 2095)

# Selected SSP for warming levels
ssptab <- 3

#----- Select data

# Data label
tablab <- "country_rates"

# Get country-level data by period
bigtabdata <- finalres$country_period |> 
  subset(agegroup == "all" & sc == "full-demo" & ssp %in% ssptab & 
      period %in% pertab)

# Add info about countries
cntr_info <- group_by(cities, CNTR_CODE) |> 
  summarise(cntr_name = cntr_name[1], region = region[1], lat = mean(lat))
bigtabdata <- merge(bigtabdata, cntr_info, by.x = "country", by.y = "CNTR_CODE")

#----- Add totals

# Select regional totals
regper <- finalres$region_period |> 
  subset(agegroup == "all" & sc == "full-demo" & ssp %in% ssptab & 
      period %in% pertab) |> 
  mutate(lat = -Inf, country = region, cntr_name = as.character(region))

# Select European totals
euper <- finalres$eu_period |> 
  subset(agegroup == "all" & sc == "full-demo" & ssp %in% ssptab & 
      period %in% pertab & gcm == "ens") |>
  mutate(country = "Total", lat = -Inf, gcm = NULL, region = "", 
    cntr_name = "Total")

# Put everything together
bigtabdata <- rbind(bigtabdata, regper, euper)

#----- Prepare table

# Tidy cell values
bigtabdata <- mutate(bigtabdata, 
  measure = sprintf("%.1f\n(%.1f to %.1f)", 
    rate_est * byrate, rate_low * byrate, rate_high * byrate),
  range = rnglabs[range])

# Pivot period to wide
bigtab <- pivot_wider(bigtabdata, 
  id_cols = c("country", "region", "lat", "cntr_name"), 
  names_from =  c("range", "period", "ssp"), values_from = "measure")

# Order rows and columns
bigtab <- mutate(bigtab, region = factor(region, ordreg)) |>
  arrange(region, desc(lat)) |>
  select(cntr_name, contains(as.character(pertab)))

#----- Format table for word

# Create table with flextable
outtab <- flextable(bigtab)

# Highlight totals
outtab <- bold(outtab, ~ cntr_name %in% c("Total", ordreg))

# Relabel header
outtab <- set_header_labels(outtab, values = as.list(c(
  cntr_name = "", 
  "names<-"(str_split_i(names(bigtab)[-1], "_", 1), names(bigtab)[-1])
)))

# Add header row
if(length(pertab) > 1) outtab <- add_header_row(outtab, 
  colwidth = c(1, rep(3, length(pertab) * length(ssptab))),
  values = c("", pertab))
if(length(ssptab) > 1) outtab <- add_header_row(outtab, 
  colwidth = c(1, rep(3, length(pertab) * length(ssptab))),
  values = c("", ssptab))

# Column alignment
outtab <- align(outtab, j = seq_len(ncol(bigtab))[-1], align = "right") |>
  align(i = seq_len(1 + (length(pertab) > 1) + (length(ssptab) > 1)), 
    align = "center", part = "header")

# Page fitting
outtab <- autofit(outtab) |> fit_to_width(7)

#----- Export

save_as_docx(outtab, path = sprintf("figures/Tab1_%s.docx", tablab))


# #--------------------------
# # Big AN table
# #--------------------------
# 
# #----- Parameters
# 
# # Selected period
# pertab <- 2050
# 
# # Selected SSP for warming levels
# ssptab <- 3
# 
# #----- Select data (country level)
# 
# # Data label
# tablab <- "country"
# 
# # Get country-level data by period
# cntrper <- finalres$country$period |> 
#   subset(agegroup == "all" & sc == "full-demo" & ssp %in% ssplist & 
#       period == pertab)
# 
# # Get country-level data by warming level
# cntrlev <- finalres$country$level |> 
#   subset(agegroup == "all" & sc == "full-demo" & ssp == ssptab) |>
#   rename(period = "level")
# 
# # Put together
# bigtabdata <- rbind(cntrper, cntrlev)
# 
# # Add info about countries
# cntr_info <- group_by(cities, CNTR_CODE) |> 
#   summarise(cntr_name = cntr_name[1], region = region[1], lat = mean(lat))
# bigtabdata <- merge(bigtabdata, cntr_info, by = "CNTR_CODE")
# 
# # Rename
# bigtabdata <- rename(bigtabdata, geo = "CNTR_CODE", label = "cntr_name")
# 
# #----- Select data (main cities)
# 
# # Data label
# tablab <- "cities"
# 
# # Select cities
# citysel <- subset(cities, substr(URAU_CODE, 3, 5) == "001" | pop > 1e6, 
#   "URAU_CODE", drop = T)
# 
# # Get country-level data by period
# cityper <- finalres$city$period |> 
#   subset(agegroup == "all" & sc == "full-demo" & ssp %in% ssplist & 
#       period == pertab & URAU_CODE %in% citysel)
# 
# # Get country-level data by warming level
# citylev <- finalres$city$level |> 
#   subset(agegroup == "all" & sc == "full-demo" & ssp == ssptab & 
#       URAU_CODE %in% citysel) |>
#   rename(period = "level")
# 
# # Put together
# bigtabdata <- rbind(cityper, citylev)
# 
# # Add info about cities
# bigtabdata <- merge(bigtabdata, 
#   cities[, c("URAU_CODE", "LABEL", "region", "lat")], by = "URAU_CODE")
# 
# # Rename
# bigtabdata <- rename(bigtabdata, geo = "URAU_CODE", label = "LABEL")
# 
# #----- Add totals
# 
# # Select regional totals
# regper <- finalres$region$period |> 
#   subset(agegroup == "all" & sc == "full-demo" & ssp %in% ssplist & 
#       period == pertab) |> 
#   mutate(lat = -Inf, geo = region, label = as.character(region))
# reglev <- finalres$region$level |> 
#   subset(agegroup == "all" & sc == "full-demo" & ssp == ssptab) |>
#   rename(period = "level") |>
#   mutate(lat = -Inf, geo = region, label = as.character(region))
# 
# # Select European totals
# euper <- finalres$total$period |> 
#   subset(agegroup == "all" & sc == "full-demo" & ssp %in% ssplist & 
#       period == pertab & gcm == "ens") |>
#   mutate(geo = "Total", lat = -Inf, gcm = NULL, region = "", label = "Total")
# eulev <- finalres$total$level |> 
#   subset(agegroup == "all" & sc == "full-demo" & ssp == ssptab & 
#       gcm == "ens") |>
#   mutate(geo = "Total", lat = -Inf, gcm = NULL, region = "", label = "Total") |>
#   rename(period = "level")
# 
# # Put everything together
# bigtabdata <- rbind(bigtabdata, regper, reglev, euper, eulev)
# 
# #----- Prepare table
# 
# # Tidy cell values
# bigtabdata <- mutate(bigtabdata, 
#   measure = sprintf("%.0f\n(%.0f \u2013 %.0f)", an_est, an_low, an_high),
#   range = rnglabs[range])
# 
# # Pivot period to wide
# bigtab <- pivot_wider(bigtabdata, 
#   id_cols = c("geo", "range", "region", "geo", "lat", "label"), 
#   names_from =  c("period", "ssp"), values_from = "measure")
# 
# # Order rows and columns
# bigtab <- mutate(bigtab, region = factor(region, ordreg)) |>
#   arrange(region, desc(lat), range) |>
#   select(label, range, starts_with(sprintf("%s_", targets)), 
#     starts_with(as.character(pertab)))
# 
# #----- Format table for word
# 
# # Create table with flextable
# outtab <- flextable(bigtab)
# 
# # Merge Country labels
# outtab <- merge_v(outtab, 1)
# 
# # Separate countries and highlight totals
# outtab <- hline(outtab, i = seq(3, nrow_part(outtab), by = 3)) |>
#   bold(~ label %in% c("Total", ordreg))
# 
# # Relabel header
# outtab <- set_header_labels(outtab, values = as.list(c(
#   label = "", range = "",
#   "names<-"(levellabs, paste(targets, ssptab, sep = "_")),
#   "names<-"(ssplabs, paste(pertab, ssplist, sep = "_"))
# )))
# 
# # Add header row
# outtab <- add_header_row(outtab, 
#   colwidth = c(2, length(targets), length(ssplabs)),
#   values = c("", sprintf("Warming level (%s)", ssplabs[ssptab]), 
#     sprintf("Year %i by SSP", pertab)))
# 
# # Column alignment
# outtab <- align(outtab, j = 3:8, align = "right") |>
#   align(i = 1, align = "center", part = "header") |>
#   align(i = 2, align = "right", part = "header")
# 
# # Page fitting
# outtab <- autofit(outtab) |> fit_to_width(7)
# 
# #----- Export
# 
# save_as_docx(outtab, path = sprintf("figures/Tab1_%s.docx", tablab))
# 
# #--------------------------
# # Big table of cumulative deaths
# #--------------------------
# 
# #----- Select data (country level)
# 
# # Data label
# tablab <- "country_cumu_excess"
# 
# # Get country-level data by period
# cntrdat <- finalres$country$period |> 
#   subset(agegroup == "all" & sc == "full-demo" & ssp %in% ssplist & 
#       range == "tot")
# 
# # Aggregate to get the total
# cumudata <- cntrdat[, .(an_est = sum(an_est) * perlen), by = .(CNTR_CODE, ssp)]
# 
# # Add info about countries
# cntr_info <- group_by(cities, CNTR_CODE) |> 
#   summarise(cntr_name = cntr_name[1], region = region[1], lat = mean(lat))
# cumudata <- merge(cumudata, cntr_info, by = "CNTR_CODE")
# 
# # Rename
# cumudata <- rename(cumudata, geo = "CNTR_CODE", label = "cntr_name")
# 
# #----- Add cumulative totals
# 
# # Select regional totals
# regdat <- finalres$region$period |> 
#   subset(agegroup == "all" & sc == "full-demo" & ssp %in% ssplist & 
#       range == "tot")
# regcumu <- regdat[, .(an_est = sum(an_est) * perlen, lat = -Inf, geo = region, 
#   label = as.character(region)), by = .(region, ssp)]
# 
# # Select European totals
# eudat <- finalres$total$period |> 
#   subset(agegroup == "all" & sc == "full-demo" & ssp %in% ssplist & 
#       range == "tot" & gcm == "ens")
# eucumu <- eudat[, .(an_est = sum(an_est) * perlen, lat = -Inf, geo = "Total", 
#   label = "Total", region = ""), by = .(ssp)]
# 
# # Put everything together
# cumudata <- rbind(cumudata, regcumu, eucumu)
# 
# #----- Prepare table
# 
# # Tidy cell values
# cumudata <- mutate(cumudata, measure = formatC(an_est, format = "f", digits = 0,
#   big.mark = ","))
# 
# # Pivot period to wide
# cumudata <- pivot_wider(cumudata, 
#   id_cols = c("geo", "region", "lat", "label"), 
#   names_from = c("ssp"), values_from = "measure")
# 
# # Order rows and columns
# cumudata <- mutate(cumudata, region = factor(region, ordreg)) |>
#   arrange(region, desc(lat)) |>
#   select(!c(geo, region, lat))
# 
# #----- Format table for word
# 
# # Create table with flextable
# outtab <- flextable(cumudata)
# 
# # Highlight totals
# outtab <- hline(outtab, i = ~ label %in% c("Total", ordreg)) |>
#   bold(~ label %in% c("Total", ordreg))
# 
# # Relabel header
# outtab <- set_header_labels(outtab, values = as.list(c(
#   label = "", ssplabs)))
# 
# # Column alignment
# outtab <- align(outtab, j = -1, align = "right") |>
#   align(i = 1, align = "center", part = "header")
# 
# # Page fitting
# outtab <- autofit(outtab) |> fit_to_width(7)
# 
# #----- Export
# 
# save_as_docx(outtab, path = sprintf("figures/Tab1_%s.docx", tablab))

# #--------------------------
# # Big table of cumulative rates
# #--------------------------
# 
# #----- Select data (country level)
# 
# # Data label
# tablab <- "country_cumu_rate"
# 
# # Get country-level data by period
# cntrdat <- finalres$country$period |> 
#   subset(agegroup == "all" & sc == "full-demo" & ssp %in% ssplist & 
#       range == "tot", select = -c(pop, death)) |> 
#   merge(subset(finalres$country$period, 
#       agegroup == "all" & sc == "full" & ssp %in% ssplist & range == "tot", 
#       select = c(period, ssp, CNTR_CODE, death, pop)), 
#     by = c("period", "ssp", "CNTR_CODE"))
# 
# # Aggregate to get the total
# cumudata <- cntrdat[, .(rate_est = perlen * sum(an_est) / sum(pop)), 
#   by = .(CNTR_CODE, ssp)]
# 
# # Add info about countries
# cntr_info <- group_by(cities, CNTR_CODE) |> 
#   summarise(cntr_name = cntr_name[1], region = region[1], lat = mean(lat))
# cumudata <- merge(cumudata, cntr_info, by = "CNTR_CODE")
# 
# # Rename
# cumudata <- rename(cumudata, geo = "CNTR_CODE", label = "cntr_name")
# 
# #----- Add cumulative totals
# 
# # Select regional totals
# regdat <- finalres$region$period |> 
#   subset(agegroup == "all" & sc == "full-demo" & ssp %in% ssplist & 
#       range == "tot", select = -c(pop, death)) |> 
#   merge(subset(finalres$region$period, 
#       agegroup == "all" & sc == "full" & ssp %in% ssplist & range == "tot", 
#       select = c(period, ssp, region, death, pop)), 
#     by = c("period", "ssp", "region"))
# regcumu <- regdat[, .(rate_est = sum(an_est) / sum(pop) * perlen, 
#   lat = -Inf, geo = region, label = as.character(region)), by = .(region, ssp)]
# 
# # Select European totals
# eudat <- finalres$total$period |> 
#   subset(agegroup == "all" & sc == "full-demo" & ssp %in% ssplist & 
#       range == "tot" & gcm == "ens", select = -c(pop, death)) |> 
#   merge(subset(finalres$total$period, 
#     agegroup == "all" & sc == "full" & ssp %in% ssplist & range == "tot" & gcm == "ens", 
#     select = c(period, ssp, death, pop)), 
#     by = c("period", "ssp"))
# eucumu <- eudat[, .(rate_est = sum(an_est) * perlen / sum(pop), 
#   lat = -Inf, geo = "Total", label = "Total", region = ""), by = .(ssp)]
# 
# # Put everything together
# cumudata <- rbind(cumudata, regcumu, eucumu)
# 
# #----- Prepare table
# 
# # Tidy cell values
# cumudata <- mutate(cumudata, measure = formatC(rate_est * byrate, format = "f", digits = 0,
#   big.mark = ","))
# 
# # Pivot period to wide
# cumudata <- pivot_wider(cumudata, 
#   id_cols = c("geo", "region", "lat", "label"), 
#   names_from = c("ssp"), values_from = "measure")
# 
# # Order rows and columns
# cumudata <- mutate(cumudata, region = factor(region, ordreg)) |>
#   arrange(region, desc(lat)) |>
#   select(!c(geo, region, lat))
# 
# #----- Format table for word
# 
# # Create table with flextable
# outtab <- flextable(cumudata)
# 
# # Highlight totals
# outtab <- hline(outtab, i = ~ label %in% c("Total", ordreg)) |>
#   bold(~ label %in% c("Total", ordreg))
# 
# # Relabel header
# outtab <- set_header_labels(outtab, values = as.list(c(
#   label = "", ssplabs)))
# 
# # Column alignment
# outtab <- align(outtab, j = -1, align = "right") |>
#   align(i = 1, align = "center", part = "header")
# 
# # Page fitting
# outtab <- autofit(outtab) |> fit_to_width(7)
# 
# #----- Export
# 
# save_as_docx(outtab, path = sprintf("figures/Tab1_%s.docx", tablab))
# 


