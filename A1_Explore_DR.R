################################################################################
#
#  Compare death rates between Eurostat and Wittgenstein
#
################################################################################

library(eurostat)

#--------------------
# Load from Eurostat
#--------------------

# Download number of deaths
eurodeath <- get_eurostat("demo_r_magec3", time_format = "num") |> 
  rename(death = values)
eurodeathtot <- subset(eurodeath, sex == "T" & age == "TOTAL")

# Download population
europop <- get_eurostat("demo_r_pjangrp3", time_format = "num") |> 
  rename(pop = values)
europoptot <- subset(europop, sex == "T" & age == "TOTAL")

# Merge
eurodr <- merge(eurodeathtot, europoptot) |> 
  subset(select = -c(sex, unit, age)) |>
  as.data.table()

# Merge for the 2015-2019 period
eurodr5 <- eurodr[time %in% 2015:2019, .(death5 = sum(death), pop5 = first(pop), 
  death1 = mean(death), pop1 = mean(pop)), by = c("geo")]

# Compute death rate and extract country
eurodr5[, ":="(dr1 = death1 / pop1, dr5 = death5 / pop5, 
  cntr = substr(geo, 1, 2))]

#--------------------
# Load Wittgenstein
#--------------------

# load crude death rates
pathdemo <- "V:/VolumeQ/AGteam/Eurostat/data/MNM/RDS/Wittgenstein"
wittdr <- readRDS(paste0(pathdemo, "/wittgenstein_cdr.RDS")) |> data.table()

# Transform
wittdr <- wittdr[,":="(cdr = cdr / 1000, cntr = substr(URAU_CODE, 1, 2))]

# Select historical period and unique by country
wittdrper <- wittdr[period == "2015-2020", .(cdr, cntr, ssp)] |> unique()


#-------------------
# Compare
#-------------------

ggplot(eurodr5) + theme_bw() + 
  geom_point(aes(x = cntr, y = dr5, size = 6 - nchar(geo)), col = "grey") + 
  scale_size_continuous(limits = c(1, 4), range = c(1, 4), 
    labels = sprintf("NUTS%i", 3:0), name = "Eurostat") + 
  geom_point(aes(x = cntr, y = cdr, col = ssp), data = wittdrper, size = 4, 
    alpha = .5) + 
  labs(col = "Wittgenstein")

ggplot(eurodr5) + theme_bw() + 
  geom_point(aes(x = cntr, y = dr1, size = 6 - nchar(geo)), col = "grey") + 
  scale_size_continuous(limits = c(1, 4), range = c(1, 4), 
    labels = sprintf("NUTS%i", 0:3), name = "Eurostat") + 
  geom_point(aes(x = cntr, y = cdr, col = ssp), data = wittdrper, size = 4, 
    alpha = .5) + 
  labs(col = "Wittgenstein")



#####################################
# Compare age specific assr and Eurostat
#####################################

#----------------------
# Compute age dr by assr
#----------------------

# load assr
pathdemo <- "V:/VolumeQ/AGteam/Eurostat/data/MNM/RDS/Wittgenstein"
wittassr <- readRDS(paste0(pathdemo, "/wittgenstein_assr.RDS")) |> data.table()

# Select period and unique
wittassr <- wittassr[period == "2015-2020", .(assr, CNTR_CODE, ssp, sex, age)] |>
  unique()

# Compute dr by age group
wittassr[, ":="(dr = 1 - assr, cntr = CNTR_CODE,
  lowage = as.numeric(sapply(strsplit(age, "[-+]"), "[", 1)))]


#----------------------
# get it from Eurostat
#----------------------

# Merge pop and death
eurodrage <- merge(eurodeath, europop) |> 
  subset(select = -unit) |>
  as.data.table()

# Merge for the 2015-2019 period
eurodrage <- eurodrage[time %in% 2015:2019, .(death5 = sum(death), pop5 = first(pop), 
  death1 = mean(death), pop1 = mean(pop)), by = c("geo", "age", "sex")]

# Compute death rate and extract country and lower age
eurodrage[, ":="(dr1 = death1 / pop1, dr5 = death5 / pop5, 
  cntr = substr(geo, 1, 2), 
  lowage = as.numeric(sapply(strsplit(age, "[-Y]"), "[", 2)))]


#----------------------
# Plot
#----------------------

ggplot(eurodrage) + theme_bw() + 
  geom_point(aes(x = lowage, y = dr5, size = 6 - nchar(geo)), col = "grey") + 
  scale_size_continuous(limits = c(1, 4), range = c(1, 4), 
    labels = sprintf("NUTS%i", 3:0), name = "Eurostat") +
  geom_point(aes(x = lowage, y = dr, col = ssp, shape = sex), data = wittassr, 
    alpha = .5) + 
  scale_y_continuous(limits = c(0, 1), name = "Death rate") + 
  facet_wrap(~ cntr)

ggplot(eurodrage) + theme_bw() + 
  geom_point(aes(x = lowage, y = dr1, size = 6 - nchar(geo)), col = "grey") + 
  scale_size_continuous(limits = c(1, 4), range = c(1, 4), 
    labels = sprintf("NUTS%i", 3:0), name = "Eurostat") +
  geom_point(aes(x = lowage, y = dr, col = ssp, shape = sex), data = wittassr, 
    alpha = .5) + 
  scale_y_continuous(limits = c(0, 1), name = "Death rate") + 
  facet_wrap(~ cntr)




#####################################
# Comparisons death rates with metadata
#####################################

# load assr
pathdemo <- "V:/VolumeQ/AGteam/Eurostat/data/MNM/RDS/Wittgenstein"
wittassr <- readRDS(paste0(pathdemo, "/wittgenstein_assr.RDS")) |> data.table()
wittassr[, ":="(dr = 1 - assr, cntr = CNTR_CODE,
  lowage = as.numeric(sapply(strsplit(age, "[-+]"), "[", 1)))]
wittassr <- wittassr[period == "2015-2020",]
wittassr <- unique(wittassr[,":="(URAU_CODE = NULL, LABEL = NULL)])

# Get metadata
eucitypath <- "../../EUcityTRM/MCC-EUcityTRM/data"
metadata <- read.csv(paste0(eucitypath, "/metadata.csv"))
longdr <- melt(metadata, id = c("URAU_CODE", "CNTR_CODE"), 
    measure = grep("deathrate", names(metadata), value = T), 
    variable.name = "age", value.name = "dr") |>
  subset(age != "deathrate_tot") |>
  mutate(age = gsub("deathrate_", "", age), 
    lowage = as.numeric(substr(age, 1, 2)), cntr = CNTR_CODE)

# Compare
ggplot() + theme_bw() + 
  geom_point(aes(x = lowage, y = dr), col = "grey", size = 3,
    data = longdr) + 
  geom_point(aes(x = lowage, y = dr, col = ssp, shape = sex), 
    data = wittassr, alpha = .5, size = 2) + 
  scale_y_continuous(limits = c(0, 1), name = "Death rate") + 
  facet_wrap(~ cntr)

ggplot() + theme_bw() + 
  geom_point(aes(x = lowage, y = 1 - (1 - dr)^5), col = "grey", size = 3,
    data = longdr) + 
  geom_point(aes(x = lowage, y = dr, col = ssp, shape = sex), 
    data = wittassr, alpha = .5, size = 2) + 
  scale_y_continuous(limits = c(0, 1), name = "Death rate") + 
  facet_wrap(~ cntr)
