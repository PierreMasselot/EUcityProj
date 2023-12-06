source("00.pkg_params.R")
load("temp/cleandata.RData")

#----- Parameters

issp <- "ssp3"
city <- subset(cities, URAU_CODE == "UK001C")
a <- agelabs[1]
gcm <- "cal_wp3"

#----- Load climate data

# Initialise data.frame to merge series consistently
init_df <- rbind(
  data.frame(date = obsday, period = "Hist"),
  data.frame(date = projday, period = projperday)) |>
  mutate(doy = format(date, '%j'), year = as.numeric(format(date, "%Y")), 
    year5 = floor(year / 5) * 5, 
    period = factor(period, levels = c("Hist", projperlab)))

# LOAD THE OBSERVED TEMPERATURE
tmeanobs <- readRDS(paste0(pathobs, "/", city$URAU_CODE, ".RDS")) |>
  rename(tmeanobs = tas_era5_land)

# Load WP3 climate model
cityfile <- system(sprintf("ls %s/%s*/%s* -f -R", pathproj1, issp,
  city$URAU_CODE), intern = T) # Way faster than list.files
wp3read <- readRDS(cityfile) |> rename(tas_wp3 = rcm_1)

# Load NASA-NEX models
nasafile <- system(sprintf("ls %s/%s*/%s* -f -R", pathproj2, issp,
  city$URAU_CODE), intern = T)
nasaread <- readRDS(nasafile) |> dplyr::select(!city)

# Add them to the series data.frame (removes leap days)
tmeandf <- init_df |> 
  merge(tmeanobs, all.x = T) |>
  merge(wp3read, all.x = T) |>
  merge(nasaread, all.x = T)

# Fill NAs for IITM_ESM in 2099 and SSP3
if (issp == "ssp3"){
  indna <- which(tmeandf$year == 2099)
  indprev <- which(tmeandf$year == 2098)
  # Make sure the right DOY are matched
  indmatch <- indprev[match(tmeandf[indna, "doy"], tmeandf[indprev, "doy"])]
  tmeandf[indna, "tas_IITM_ESM"] <- tmeandf[indmatch, "tas_IITM_ESM"]
} 

# CALIBRATE (LEAP DAYS ALREADY EXCLUDED)
tmeandf <- fhempel(na.omit(tmeandf[,c("date", "tmeanobs")]), 
  dplyr::select(tmeandf, date, starts_with("tas"))) |>
  rename_with(gsub, pattern = "tas", replacement = "cal") |> 
  merge(tmeandf, all.y = T)

#----- EXTRACT PARAMETERS

# Prepare basis parameters
tper <- quantile(tmeanobs$tmeanobs, predper/100)
varknots <- tper[paste0(varper, ".0%")]
varbound <- range(tper)
argvar <- list(fun=varfun, degree=vardegree, knots=varknots,
  Bound=varbound)

# Load coefficients
par <- readRDS(paste0(pathpar, "/", city$URAU_CODE, ".RDS"))
coef <- par[[a]]$coef
vcov <- par[[a]]$vcov
coefsim <- par[[a]]$coefsim[seq(min(nsim, nrow(par[[a]]$coefsim))),]

# MORTALITY SERIES (CONSTANT FOR 5 YEARS)
agedf <- subset(projdata, URAU_CODE == city$URAU_CODE & agegroup == a & 
    ssp == issp, c(year, deaths, uraudeath)) |> 
  mutate(deaths = deaths / (365 * 5), uraudeath = uraudeath / (365 * 5)) |>
  rename(year5 = year, deathhist = uraudeath) |>
  right_join(tmeandf, multiple = "all") |>
  arrange(date)

death <- agedf$deathhist

#----- Create historical temp series

# Create temperature series replicating average
tmean_ave <- subset(tmeandf, year %between% obsrange) |>
  aggregate(x = as.formula(sprintf("%s ~ doy", gcm)), FUN = mean) |>
  right_join(agedf[,c("date", "doy")], multiple = "all") |>
  arrange(date) |>
  subset(select = gcm, drop = T)

# Create temperature series sampling
yearsamp <- na.omit(subset(tmeandf, year %between% obsrange, 
    select = c("year", gcm))) |>
  subset(select = year, drop = T) |>
  unique() |>
  sample(nrow(tmeandf), replace = T)
tmean_samp <- data.frame(tmeandf[,c("date", "doy")], year = yearsamp) |>
  left_join(subset(tmeandf, select = c("year", "doy", gcm))) |>
  arrange(date) |>
  subset(select = gcm, drop = T)

#----- Compare series graphically

# Compare series
matplot(tmeandf$date[tmeandf$year%between% c(2015, 2019)], 
  cbind(tmeandf[,gcm], tmean_samp, tmean_ave)[
    tmeandf$year%between% c(2015, 2019),], 
  pch = c(1, 20, 20), xlab = "Date", ylab = "Tmean")
legend("topleft", legend = c("GCM", "DOY average", "Sampling"), 
  pch = c(1, 20, 20), col = 1:3, horiz = T)
dev.print(pdf, "plots/A3/Historical_series.pdf")

# Distribution
df <- data.frame(date = rep(tmeandf$date, 3), 
    tmean = c(tmeandf[,gcm], tmean_samp, tmean_ave),
    type = rep(c("GCM", "DOY average", "Sampling"), each = nrow(tmeandf))) |>
  subset(year(date) %in% 2015:2019)

ggplot(df) + theme_bw() + 
  geom_violin(aes(x = type, y = tmean, col = type)) + 
  labs(x = "Series", y = "Tmean")
ggsave("plots/A3/distributions.pdf")

#----- Compute AN

anlist <- foreach(x = list(tmeandf[,gcm], tmean_samp, tmean_ave),
  lab = c("GCM", "Sampling", "DOY average"), .combine = rbind) %do% 
{
  bvar <- suppressWarnings(do.call(onebasis, c(list(x = x), argvar)))
  
  # MMT
  ind <- x > tper["25.0%"] & x < tper["99.0%"]
  mmt <- x[ind][which.min(drop(bvar[ind,] %*% coef))]  
  
  # CENTRE THE BASIS
  cenvec <- do.call(onebasis, c(list(x = mmt), argvar))
  bcen <- scale(bvar, center = cenvec, scale=F)
  
  # INDICATOR FOR COLD/HEAT DAYS
  indheat <- x > mmt
  
  # COMPUTE THE DAILY CONTRIBUTIONS OF ATTRIBUTABLE DEATHS
  # NB: REMOVE RR<1 AS THIS CAN LEAD TO NEGATIVE DEATHS HIGHER THAN TOTAL
  rr <- pmax(exp(drop(bcen %*% coef)), 1)
  an <- drop((1 - 1 / rr) * death)
  anagg <- tapply(na.omit(an), agedf$period[!is.na(an)], mean) * 365
  data.frame(period = factor(rownames(anagg), levels = rownames(anagg)), 
    an = anagg, lab = lab)
}

ggplot(anlist) + theme_bw() + 
  geom_line(aes(x = period, y = an, group = lab, col = lab), size = 2)
ggsave("plots/A3/an.pdf")
