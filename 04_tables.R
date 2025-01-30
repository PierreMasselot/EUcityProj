################################################################################
# 
# Contrasting future heat and cold-related mortality under climate change, 
# demographic and adaptation scenarios in 854 European cities
#
# R Code Part 4: Main Table of results
#
# Pierre Masselot & Antonio Gasparrini
#
################################################################################

# If the session has been ended before
if (length(ls()) == 0){
  source("01_pkg_params.R")
  source("02_prep_data.R")
  
  # Download if nonexistent
  if (!dir.exists("results_parquet")){
    dir.create("results_parquet")
    
    # Download the data from the repo. timeout has been increased for large files
    download_zenodo("10.5281/zenodo.14004321", path = "results_parquet",
      files = "results_parquet.zip", timeout = 10000)
    
    # Unzip data and delete zipfile
    unzip("results_parquet/results_parquet.zip")
    unlink("results_parquet/results_parquet.zip")
  }
  
  # Read
  nmlist <- expand.grid(c("city", "country", "region", "eu"), 
      c("period", "level")) |>
    apply(1, paste, collapse = "_")
  resdir <- "results_parquet"
  flist <- sprintf("%s/%s.parquet", resdir, nmlist)
  
  # Read all results
  finalres <- lapply(flist, read_parquet)
  names(finalres) <- nmlist
}

if (!dir.exists("figures")) dir.create("figures")

#--------------------------
# Table 1: Big table of rates
#--------------------------

#----- Parameters

# Selected period
pertab <- c(2050, 2095)

# Selected scenario for warming levels
ssptab <- 3
adatab <- "0%"

#----- Select data

# Data label
tablab <- "country_rates"

# Get country-level data by period
bigtabdata <- finalres$country_period |> 
  subset(agegroup == "all" & sc == "clim" & ssp %in% ssptab & 
      adapt %in% adatab & period %in% pertab)

# Add info about countries
cntr_info <- group_by(cities, CNTR_CODE) |> 
  summarise(cntr_name = cntr_name[1], region = region[1], lat = mean(lat))
bigtabdata <- merge(bigtabdata, cntr_info, by.x = "country", by.y = "CNTR_CODE")

#----- Add totals

# Select regional totals
regper <- finalres$region_period |> 
  subset(agegroup == "all" & sc == "clim" & ssp %in% ssptab & 
      adapt %in% adatab & period %in% pertab) |> 
  mutate(lat = -Inf, country = region, cntr_name = as.character(region))

# Select European totals
euper <- finalres$eu_period |> 
  subset(agegroup == "all" & sc == "clim" & ssp %in% ssptab & 
      adapt %in% adatab & period %in% pertab & gcm == "ens") |>
  mutate(country = "Total", lat = -Inf, gcm = NULL, region = "", 
    cntr_name = "Total")

# Put everything together
bigtabdata <- rbind(bigtabdata, regper, euper)

#----- Prepare table

# Tidy cell values
bigtabdata <- mutate(bigtabdata, 
  measure = sprintf("%.1f\n(%.1f to %.1f)", 
    rate_est * byrate, rate_low * byrate, rate_high * byrate),
  range = factor(range, levels = names(rnglabs), labels = rnglabs))

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
  values = c("", sprintf("%i-%s", pertab, substr(pertab + perlen - 1, 3, 4))))
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

save_as_docx(outtab, path = "figures/Tab1.docx")

