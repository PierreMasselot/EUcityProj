################################################################################
# NEW CODE FOR PROJECTIONS
################################################################################

################################################################################
# LOAD EUcityTRM info
################################################################################

# read coefficients 
coefs <- read.csv("data/coefs.csv") |>
  mutate(agegroup = factor(agegroup, levels = agelabs))

# Read city and age values
cityage <- read.csv("data/city_results.csv")

# Reduce to cities only
cities <- unique(subset(cityage, select = c("URAU_CODE", "LABEL", "CNTR_CODE", 
  "cntr_name", "region", "lon", "lat", "pop")))

# Countries
countries <- summarise(cities, 
    ncities = length(URAU_CODE), lat = mean(lat), region = unique(region), 
    .by = CNTR_CODE) |>
  mutate(region = factor(region, levels = ordreg)) |>
  arrange(region, desc(lat))

# Reorder cities
cities <- mutate(cities, region = factor(region, levels = ordreg),
    CNTR_CODE = factor(CNTR_CODE, levels = countries$CNTR_CODE)) |>
  arrange(region, CNTR_CODE, URAU_CODE)

################################################################################
# LOAD DEMOGRAPHIC DATA
################################################################################

#----- Load Wittgenstein projections

# Read Age-specific survival ratios
projdeath <- fread("data/wittgenstein_assr.csv")

# Attach the first year to each period to later merge with population
projdeath[, year5 := as.numeric(substr(period, 1, 4))]

# Read Population
projpop <- fread("data/wittgenstein_pop.csv") |> rename(year5 = "year")

# Select sex and age
projdeath <- projdeath[age != "Newborn"]
projpop <- projpop[age != "All" & sex != "Both",]

# Merge population and deaths
proj <- merge(projpop, projdeath)

# Select only relevant years
proj <- proj[year5 %between% histrange | year5 %between% range(projrange),]

# Rescale population and compute deaths
proj[, ":="(pop = 1000 * pop, death = 1000 * pop * (1 - assr))]

# Aggregate sex
proj <- proj[, .(pop = sum(pop), death = sum(death)), 
  by = .(CNTR_CODE, age, ssp, year5)]

#----- Aggregate by age group

# Create age groups
proj[, agegroup := cut(as.numeric(sapply(strsplit(age, "[-+]"), "[", 1)), 
  agebreaks, include.lowest = T, right = F, labels = agelabs)]

# Sum population and death by age group
proj <- proj[!is.na(agegroup), .(wittpop = sum(pop), wittdeath = sum(death)), 
  by = .(CNTR_CODE, agegroup, ssp, year5)]

# Rescale number of deaths as annual average
proj[, ":="(wittdeath = wittdeath / 5)]

#----- Calibrate using Urban Audit

# Average pop and deaths of Wittgenstein over historical period
histo_witt <- proj[year5 %between% histrange, 
  .(histowpop = mean(wittpop), histowdeaths = mean(wittdeath)), 
  by = .(CNTR_CODE, agegroup, ssp)]

# Extract pop and deaths from EUcityTRM
citycal <- rename(cityage, uraupop = agepop, uraudeath = death) |>
  subset(select = c(URAU_CODE, CNTR_CODE, agegroup, uraupop, uraudeath))

# Merge Eurostat and projection data
citycal <- merge(histo_witt, citycal, all = T, allow.cartesian = T)

# Multiplicative factor for each city from national
citycal[, ":="(popfac = uraupop / histowpop, dfac = uraudeath / histowdeaths)]

# Add correction to population projection
projdata <- merge(proj, citycal, allow.cartesian = T, all = T)
projdata[, ":="(pop = wittpop * popfac, death = wittdeath * dfac)]

#----- Some further data cleaning

# SELECT SSP. Discard the duplicates of historical period
projdata <- projdata[ssp %in% ssplist,]
projdata[, ssp := as.character(ssp)]
projdata[year5 %between% histrange, ssp := "hist"]
projdata <- unique(projdata)

# SELECT CITIES WITH FULL DATA 
cities <- subset(cities, URAU_CODE %in% projdata$URAU_CODE)
projdata <- subset(projdata, URAU_CODE %in% cities$URAU_CODE)

# DEFINE FACTORS WITH PROPER ORDER, ROW NAMES
cities <- mutate(cities, 
  across(URAU_CODE:cntr_name, ~ factor(.x, levels = unique(.x))),
  region = factor(region, levels = ordreg))

# ORDER DEMOGRAPHIC DATA
projdata[, ":="(URAU_CODE = factor(URAU_CODE, levels = levels(cities$URAU_CODE)),
  agegroup = factor(agegroup, levels = agelabs))]
setkey(projdata, ssp, URAU_CODE, agegroup)


################################################################################
# Load warming target data

# Read data
warming_years <- read.csv("data/warming_years.csv", check.names = F)

# Reshape into long data.frame
warming_years <- pivot_longer(warming_years, !model_run, names_to = "level", 
  values_to = "year")

# Rename and select targets
warming_years <- mutate(warming_years, 
    model_run = str_split_i(model_run, "_", 1),
    gcm = gsub("-", "_", model_run),
    model_run = NULL,
    ssp = str_split_i(level, "_", 2),
    ssp = substr(str_extract(ssp, ".*ssp[[:digit:]]"), 4, 4),
    level = str_split_i(level, "_", 1)) |>
  subset(level %in% targets & ssp %in% ssplist)

# Expand for all years within the window and for all cities/ssp/agegroup
warming_win <- group_by(warming_years, gcm, level, ssp) |>
  reframe(year = if (is.na(year)) NA else (year - win_len):(year + win_len)) |>
  ungroup() |>
  mutate(year5 = floor(year / 5) * 5) |> 
  data.table()

################################################################################
# Final tidying

# Load European map data
euromap <- gisco_get_countries(year = "2020")

