################################################################################
#
#  Test ERF extrapolation
#
################################################################################

source("00.pkg_params.R")
load("temp/cleandata.RData")

#----- Select parameters

city <- "UK001C"
cityname <- subset(cities, URAU_CODE == city, LABEL, drop = T)
issp <- ssp <- "ssp3"
agegr <- "65-75"
includepop <- F

#----- Load data

# Initialise data.frame to merge series consistently
init_df <- rbind(
  data.frame(date = obsday, period = "Hist"),
  data.frame(date = projday, period = projperday)) |>
  mutate(doy = format(date, '%j'), year = as.numeric(format(date, "%Y")), 
    year5 = floor(year / 5) * 5, 
    period = factor(period, levels = c("Hist", projperlab)))

# LOAD THE OBSERVED TEMPERATURE
  tmeanobs <- readRDS(paste0(pathobs, "/", city, ".RDS")) |>
    rename(tmeanobs = tas_era5_land)

# load
nasafile <- system(sprintf("ls %s/%s*/%s* -f -R", pathproj2, ssp,
  city), intern = T)
nasaread <- readRDS(nasafile) |> dplyr::select(!city)

# Add them to the series data.frame (removes leap days)
tmeandf <- init_df |> 
  merge(tmeanobs, all.x = T) |>
  merge(nasaread, all.x = T)

# CALIBRATE (LEAP DAYS ALREADY EXCLUDED)
tmeandf <- fhempel(na.omit(tmeandf[,c("date", "tmeanobs")]), 
  dplyr::select(tmeandf, date, starts_with("tas"))) |>
  rename_with(gsub, pattern = "tas", replacement = "cal") |> 
  merge(tmeandf, all.y = T)

# Select the model with highets range to test
tmeansel <- names(which.max(dplyr::select(tmeandf, starts_with("cal")) |>
  apply(2, function(x) diff(range(x, na.rm = T)))))
tmeandf <- tmeandf[,c("date", "period", "year", "year5", "tmeanobs", 
 tmeansel)]

#----- Load info for ERF

path <- paste0("C:/Users/lshpm4.ADS/OneDrive - London School of Hygiene and ", 
  "Tropical Medicine/Research/Projects/EXHAUSTION/EUcityTRM/MCC-EUcityTRM/data")

# Load second-stage model
load(paste0(path, "/meta-model.RData"))

# Load metadata
metadata <- read.csv(paste0(path, "/metadata.csv"))
vars <- c("pop", "prop_65p", "popdens", "lifexp_00", "isol",
    "gdp", "unempl", "educ", "depriv", "bedrates",
    "imperv", "tree", "grass", "water", "woody",
    "elevation", "coast_dist", "ndvi", "pm25", "no2",
    "tmean", "trange")
metapreds <- subset(metadata, URAU_CODE == city, 
  select = vars)

# Compute ave T and range for each decade
tperiod <- group_by(tmeandf, period) |> 
  summarise(tmean = mean(.data[[tmeansel]]), 
    trange = diff(range(.data[[tmeansel]]))) |>
  subset(period != "Hist")

# Prepare data
metapreds <- cbind(subset(metapreds, select = -(tmean:trange)),
  tperiod[,-1])

# Include pop
if (includepop){
  popproj <- subset(decprojdata, 
    URAU_CODE == city & agegroup == agegr & ssp == issp)
  metapreds$pop <- popproj$popyear[match(tperiod$period, popproj$period)]
}
#----- Extrapolate coefficients

# Extract cnetering and scaling factors
inds <- rownames(plsres$model$metapls)
facts <- attributes(scale(metadata[,vars]))

# Center and scale
metasc <- mapply(scale, metapreds, center = facts$`scaled:center`,
  scale = facts$`scaled:scale`)

# Compute PLS scores
pcextra <- predict(plsres, newdata = metasc, ncomp = 1:4, type = "scores")
colnames(pcextra) <- sprintf("pls%i", seq_len(4))

# Create second-stage df
extradf <- cbind(subset(cityage, URAU_CODE == city & agegroup == agegr,
    select = c("URAU_CODE", "region", "agegroup", "age", "lon", "lat")),
  pcextra)

# Predict coefs
extrapred <- predict(stage2res, extradf)

# Predict BLUP residual at location
extrageo <- extradf
coordinates(extrageo) <- ~ lon + lat
proj4string(extrageo) <- CRS(sprintf("EPSG:%s", 4326))
extrakrig <- predict(vgfit, extrageo)

# Add to prediction
extracoefs <- extrapred + data.matrix(extrakrig@data)[,1:5 * 2 - 1]
rownames(extracoefs) <- tperiod$period

#----- Compute historical ERFs

# Parametrisation
tper <- quantile(tmeandf$tmeanobs, predper/100, na.rm = T)
varknots <- tper[paste0(varper, ".0%")]
varbound <- range(tper)
argvar <- list(fun=varfun, degree=vardegree, knots=varknots,
  Bound=varbound)

# Basis
bvar <- suppressWarnings(do.call(onebasis, c(list(x=tper), argvar)))
ind <- tper > tper["25.0%"] & tper < tper["99.0%"]

# Load historical coefficients
par <- readRDS(paste0(pathpar, "/", city, ".RDS"))

# Historical ERF
firsterf <- bvar %*% par[[agegr]]$coef
mmt <- tper[ind][which.min(firsterf[ind,])]
cenvec <- do.call(onebasis, c(list(x = mmt), argvar))
bcen <- scale(bvar, center = cenvec, scale=F)
histoerf <- data.frame(x = tper, rr = exp(bcen %*% par[[agegr]]$coef))

#----- Historical ERF with proj temp
tperproj <- quantile(tmeandf[[tmeansel]], predper/100, na.rm = T)

# Basis
bvarproj <- suppressWarnings(do.call(onebasis, c(list(x=tperproj), argvar)))

# Historical ERF
firsterf <- bvarproj %*% par[[agegr]]$coef
bcen <- scale(bvarproj, center = cenvec, scale=F)
projerf <- data.frame(x = tperproj, rr = exp(bcen %*% par[[agegr]]$coef))

#----- Proj ERF
adaerf <- foreach(lab = projperlab, .combine = rbind) %do% {
  tper <- quantile(subset(tmeandf, period == lab, tmeansel),
    predper/100, na.rm = T)
  varknots <- tper[paste0(varper, ".0%")]
  varbound <- range(tper)
  argvar <- list(fun=varfun, degree=vardegree, knots=varknots,
    Bound=varbound)

  # Basis
  bvar <- suppressWarnings(do.call(onebasis, c(list(x=tper), argvar)))
  ind <- tper > tper["25.0%"] & tper < tper["99.0%"]
  
  # Historical ERF
  firsterf <- bvar %*% extracoefs[lab,]
  mmt <- tper[ind][which.min(firsterf[ind,])]
  cenvec <- do.call(onebasis, c(list(x = mmt), argvar))
  bcen <- scale(bvar, center = cenvec, scale=F)
  data.frame(period = lab, x = tper, rr = exp(bcen %*% extracoefs[lab,]))
}

#----- Prepare histogram data

# breaks
ran <- range(tmeandf[[tmeansel]], na.rm = T)
tgrid <- seq(ran[1], ran[2], length.out = 100)

# Compute density for each period
histtemp <- tapply(tmeandf[[tmeansel]], tmeandf$period, hist,
    breaks = tgrid, plot = F)

# Neat df
histo_df <- foreach(i = seq_along(histtemp), .combine = rbind) %do% {
  data.frame(period = names(histtemp)[i], histtemp[[i]][c("mids", "density")])
}
histo_df$period <- factor(histo_df$period, levels = c("Hist", projperlab))
pal <- c("black", viridis(length(projperlab)))
names(pal) <- levels(histo_df$period)

#----- Plot
ggplot(adaerf) + theme_bw() + 
  geom_line(aes(x = x, y = rr, col = period)) + 
  scale_color_viridis(discrete = T, guide = "none") + 
  geom_line(aes(x = x, y = rr), data = histoerf, col = 1) + 
  geom_line(aes(x = x, y = rr), data = projerf, col = 1, linetype = 2) + 
  geom_hline(yintercept = 1) + 
  geom_tile(aes(x = mids, 
    y = as.numeric(period) / 10 * .8, 
    alpha = density, fill = period), data = histo_df) + 
  scale_fill_manual(values = pal, name = "Period") +
  scale_alpha(range = c(0, 1), name = "Density", trans = "sqrt", guide = "none") + 
  ggtitle(sprintf("%s: %s, Age group: %s, GCM: %s", city, cityname, agegr, tmeansel)) +
  coord_cartesian(ylim = c(0, 3))

ggsave(sprintf("plots/test_erf/%s_%s.pdf", city, agegr))
