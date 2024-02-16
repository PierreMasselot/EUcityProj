################################################################################
#
# Proj-Adapt
# Table(s)
#
################################################################################

if(length(ls()) == 0){
  source("01_pkg_params.R")
  source("02_prep_data.R")
  
  # Read
  nmlist <- expand.grid(c("city", "country", "region", "eu"), 
      c("period", "level")) |>
    apply(1, paste, collapse = "_")
  resdir <- "results/2024-02-14_1000"
  flist <- sprintf("%s/%s.parquet", resdir, nmlist)
  
  # Read all results
  finalres <- lapply(flist, read_parquet)
  names(finalres) <- nmlist
  
  # Read summaries of temperature
  tf <- c("tsum", "cal")
  tflist <- sprintf("%s/%s.parquet", resdir, tf)
  tsum <- lapply(tflist, read_parquet)
  names(tsum) <- tf
}


#--------------------------
# Some results
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
  subset(agegroup == "all" & sc == "clim" & ssp %in% ssptab & 
      period %in% pertab)

# Add info about countries
cntr_info <- group_by(cities, CNTR_CODE) |> 
  summarise(cntr_name = cntr_name[1], region = region[1], lat = mean(lat))
bigtabdata <- merge(bigtabdata, cntr_info, by.x = "country", by.y = "CNTR_CODE")

#----- Add totals

# Select regional totals
regper <- finalres$region_period |> 
  subset(agegroup == "all" & sc == "clim" & ssp %in% ssptab & 
      period %in% pertab) |> 
  mutate(lat = -Inf, country = region, cntr_name = as.character(region))

# Select European totals
euper <- finalres$eu_period |> 
  subset(agegroup == "all" & sc == "clim" & ssp %in% ssptab & 
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

