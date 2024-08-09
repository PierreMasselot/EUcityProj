################################################################################
# Supplementary plots
################################################################################

# Initialise count
figcount <- 0

# Name of these figures
fignm <- "SupFig"

#-------------------------------
# Section B: Description of projections
#-------------------------------

#----- Calibration: RMSE between quantiles

# Load only historical temperature summaries
tsumhist <- open_dataset(sprintf("%s/tsum0.parquet", resdir)) |>
  filter(calperiod == "hist") |>
  select(!c(calperiod, ssp)) |>
  collect()
tsumhist$perc <- as.numeric(tsumhist$perc)

# Load observed and compute quantiles
tsumobs <- read_parquet("data/era5series.gz.parquet") |>
  subset(year(date) %between% histrange) |>
  reframe(perc = predper, obs = quantile(era5landtmean, predper / 100), 
    .by = URAU_CODE)

# Merge together
tsumhist <- merge(tsumhist, tsumobs, by.x = c("city", "perc"), 
  by.y = c("URAU_CODE", "perc"))

# Compute RMSEs and pivot
tcalib <- tsumhist[,
  .(Original = sqrt(mean((obs - tas)^2)), 
    Calibrated = sqrt(mean((obs - full)^2))),
  by = .(city, gcm)]
tcalib <- melt(tcalib, id.vars = c("city", "gcm"), 
  measure.vars = c("Original", "Calibrated"), 
  variable.name = "type", value.name = "rmse")

# Plot boxplots of calibration
ggplot(tcalib) + 
  theme_classic() +
  theme(panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0)) +
  geom_boxplot(aes(fill = type, y = rmse, x = gcm, 
    group = interaction(gcm, type)), outlier.size = .1,
    size = .1) + 
  geom_hline(yintercept = 0) + 
  labs(x = "", y = "RMSE", fill = "")

# Save
ggsave(sprintf("figures/%s%i_calibration.png", fignm, figcount <- figcount + 1))

#----- GCM projections

# Compute city and period average temperature
tavecity <- open_dataset(sprintf("%s/tsum0.parquet", resdir)) |> 
  filter(perc == "mean") |> 
  select(calperiod, gcm, ssp, city, tmean = full) |>
  collect()

# Average for EU
taveeu <- tavecity[, .(tmean = mean(tmean)), by = c("calperiod", "ssp", "gcm")]

# Plot layout and theme
ggplot(taveeu) +
  theme_classic() + 
  theme(plot.margin = unit(c(1, 7, 1, 1), "line"), 
    panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x.bottom = element_text(angle = -45, vjust = 1, hjust = 0),
    strip.background = element_rect(colour = NA, fill = NA)) + 
  facet_wrap(~ ssp, labeller = labeller(ssp = ssplabs)) + 
  labs(x = "Year", y = "Annual mean temperature") + 
  geom_line(aes(x = calperiod, y = tmean, col = gcm, 
    linetype = gcm, group = gcm)) + 
  scale_color_manual(values = gcmpal, name = "GCM") + 
  scale_linetype_manual(values = gcmlntp, name = "GCM")

# Save plot
ggsave(sprintf("figures/%s%i_GCMtmean.png", fignm, figcount <- figcount + 1), 
  width = 10)


#----- Average warming by city

# Extract calibrated series (last 5y period)
citytsum <- open_dataset(sprintf("%s/tsum0.parquet", resdir)) |>
  filter(perc == "50") |>
  collect() |>
  filter(calperiod %in% c("hist", tail(levels(calperiod), 1)))

# Compute difference of period median for each city and average across GCM
citytsum <- citytsum[, .(ssp = ssplist, 
    tdiff = .SD[calperiod != "hist", full] - .SD[calperiod == "hist", full]), 
  by = .(city, gcm)]
citytsum <- citytsum[, .(tdiff = mean(tdiff)), by = .(city, ssp)]

# Add city info for mapping
citytsum <- merge(citytsum, cities[,c("URAU_CODE", "lon", "lat", "pop")],
  by.x = "city", by.y = "URAU_CODE")

# Plot
citywarmfig <- ggplot(citytsum) + 
  theme_void() +
  theme(legend.position = "bottom", legend.box = "vertical",
    strip.text.x = element_text(hjust = 0.5, face = "bold", size = 14), 
    title = element_text(hjust = 0, face = "bold", size = 12),
    panel.border = element_rect(colour = 1, fill = NA)) + 
  facet_wrap(. ~ ssp, labeller = labeller(ssp = ssplabs)) + 
  geom_sf(data = euromap, fill = grey(.9), inherit.aes = F) +
  coord_sf(xlim = range(citytsum$lon), ylim = range(citytsum$lat),
    lims_method = "box", crs = st_crs(euromap), default_crs = st_crs(4326)) +
  geom_point(aes(x = lon, y = lat, fill = tdiff, size = pop), 
    shape = 21, stroke = .01) +
  scale_fill_scico(palette = "lipari", direction = -1) +
  scale_size(range = c(1, 10), breaks = c(0.1, 0.5, 3, 7.5) * 10^6,
    labels = ~ number(./10^6)) +
  labs(size = "Population (in millions)",
    fill = "Average warming (C)") +
  guides(size = guide_legend(override.aes = list(col = 1)))

# Save
ggsave(sprintf("figures/%s%i_cityWarm.png", fignm, figcount <- figcount + 1), 
  citywarmfig, width = 10)


#----- GCMs reaching each warming level

# Compute the proportion of GCMs reaching each warming level
propreach <- subset(warming_years, gcm %in% gcmlist) |>
  summarise(reach =sum(!is.na(year)), .by = c("ssp", "level"))

# Plot
ggplot(propreach, aes(x = level, y = reach, group = ssp, col = factor(ssp))) + 
  theme_classic() + 
  theme(panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    axis.title = element_text(face = "bold")) + 
  geom_line(size = 1) + 
  geom_point(size = 4) + 
  scale_color_manual(values = ssppal, name = "", labels = ssplabs) + 
  labs(x = "Warming level", y = "Number of GCMs")

# Save
ggsave(sprintf("figures/%s%i_GCMlevels.png", fignm, figcount <- figcount + 1), 
  width = 10)


#----- GCM distribution at levels and periods

# Compute average tmean for each warming level
tavelevel <- data.frame(year = seq(min(projrange) - perlen, max(projrange))) |>
  mutate(calperiod = cut(year, c(histrange[1], projrange), right = F,
    labels = c("hist", 
      paste(projrange[-length(projrange)], projrange[-1] - 1, sep = "-")))) |> 
  merge(taveeu, allow.cartesian = T) |>
  merge(warming_win, by = c("gcm", "year", "ssp"), all.x = T, 
    allow.cartesian = T)
tavelevel <- summarise(tavelevel, tmean = mean(tmean), .by = c(level, ssp, gcm))
tavelevel <- na.omit(tavelevel)

# Limit of the plot
ylim <- range(c(tavelevel$tmean, taveeu$tmean), na.rm = T)

# Plot distribution for periods
figperiod <- ggplot(taveeu) +
  theme_classic() + 
  theme(plot.margin = unit(c(1, 7, 1, 1), "line"), 
    panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x.bottom = element_text(angle = -45, vjust = 1, hjust = 0),
    strip.background = element_rect(colour = NA, fill = NA)) + 
  facet_wrap(~ ssp, labeller = labeller(ssp = ssplabs), ncol = 1) + 
  labs(y = "Mean temperature") + 
  coord_cartesian(ylim = ylim) +
  stat_interval(aes(x = calperiod, y = tmean), .width = c(.5, .8, .95, 1)) + 
  scale_color_discrete(type = scico(4, palette = "batlow", direction = -1), 
    labels = function(x) paste0(as.numeric(x)*100, "%"),
    name = "") +
  labs(x = "Period")

# Plot distribution for levels
figlevel <- ggplot(tavelevel) +
  theme_classic() + 
  theme(plot.margin = unit(c(1, 7, 1, 1), "line"), 
    panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(colour = NA, fill = NA)) + 
  facet_wrap(~ ssp, labeller = labeller(ssp = ssplabs), ncol = 1) + 
  labs(y = "Mean temperature") + 
  coord_cartesian(ylim = ylim) +
  stat_interval(aes(x = level, y = tmean), .width = c(.5, .8, .95, 1)) + 
  scale_color_discrete(type = scico(4, palette = "batlow", direction = -1), 
    labels = function(x) paste0(as.numeric(x)*100, "%"),
    name = "") +
  labs(x = "Warming level")

# Put together
figall <- figperiod + figlevel + plot_layout(guides = "collect")

# Save
ggsave(sprintf("figures/%s%i_GCMdist.png", fignm, figcount <- figcount + 1), 
  figall, height = 7)



#-------------------------------
# Section C: Demographic projections
#-------------------------------

# Country palette ordered like regions
names(cntrpal) <- summarise(cities, lat = mean(lat), 
    .by = c("CNTR_CODE", "region")) |>
  mutate(region = factor(region, levels = ordreg)) |>
  arrange(region, desc(lat)) |>
  pull(CNTR_CODE)

#----- Population projections

# Compute population change
popsum <- proj[year5 >= min(projrange), .(pop = sum(wittpop)), 
  by = .(CNTR_CODE, ssp, year5)]
popchange <- popsum[, .(year5, pop = 100 * (pop - pop[year5 == min(year5)]) / 
    pop[year5 == min(year5)]), by = .(CNTR_CODE, ssp)]

# Compute the total for EU
popeu <- popsum[, .(pop = sum(pop)), by = .(ssp, year5)][,
  .(year5, pop = 100 * (pop - pop[year5 == min(year5)]) / 
      pop[year5 == min(year5)]), by = ssp]

# Plot layout
plotlayout <- ggplot(popchange) + 
  theme_classic() + 
  theme(plot.margin = unit(c(1, 7, 1, 1), "line"), 
    panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0),
    strip.background = element_rect(colour = NA, fill = NA)) + 
  labs(x = "", y = "Population change (%)") + 
  geom_hline(yintercept = 0) + 
  scale_color_manual(values = ssppal, name = "", labels = ssplabs)

# Plot by country
plotcntr <- plotlayout +
  geom_line(aes(x = year5, y = pop, group = factor(ssp), col = factor(ssp)),
    size = 1) + 
  facet_wrap(~ CNTR_CODE, labeller = labeller(ssp = ssplabs)) 
  
# Plot full Europe
ploteu <- plotlayout + 
  geom_line(aes(x = year5, y = pop, group = factor(ssp), col = factor(ssp)),
    size = 1, data = popeu) +
  labs(title = "European level")

# Put together
design <- "
  111
  222
  222
"
ploteu / plotcntr + plot_layout(guides = "collect", design = design)

# Save
ggsave(sprintf("figures/%s%i_popproj.png", fignm, figcount <- figcount + 1), 
  height = 10)

#----- Population structure

# Compute population structure for EU
eustruct <- proj[year5 >= min(projrange), .(pop = sum(wittpop)), 
  by = .(agegroup, ssp, year5)]
eustruct[, popprop := 100 * pop / sum(pop), by = .(ssp, year5)]

# Plot
ggplot(eustruct) + 
  theme_classic() + 
  theme(plot.margin = unit(c(1, 7, 1, 1), "line"), 
    panel.grid.major = element_line(colour = grey(.5), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(colour = NA, fill = NA),
    panel.ontop = T,
    axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0)) + 
  labs(x = "", y = "Population structure (%)") + 
  facet_wrap(~ ssp, labeller = labeller(ssp = ssplabs))  + 
  geom_area(aes(x = year5, y = popprop, fill = agegroup)) +
  scale_fill_manual(values = agepal, name = "Age group") + 
  coord_cartesian(expand = F)

# Save
ggsave(sprintf("figures/%s%i_structproj.png", fignm, figcount <- figcount + 1),
  width = 10)

#----- Death rate projections

# Compute population change
drsum <- proj[year5 >= min(projrange), 
  .(pop = sum(wittpop), death = sum(wittdeath)), 
  by = .(CNTR_CODE, ssp, year5)]
drsum[, dr := death / pop]
drchange <- drsum[, .(year5, dr = 100 * (dr - dr[year5 == min(year5)]) / 
    dr[year5 == min(year5)]), by = .(CNTR_CODE, ssp)]

# Compute the total for EU
dreu <- drsum[, .(pop = sum(pop), death = sum(death)), by = .(ssp, year5)]
dreu[, dr := death / pop]
dreu <- dreu[,.(year5, dr = 100 * (dr - dr[year5 == min(year5)]) / 
      dr[year5 == min(year5)]), by = ssp]

# Plot layout
plotlayout <- ggplot(drchange) + 
  theme_classic() + 
  theme(plot.margin = unit(c(1, 7, 1, 1), "line"), 
    panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0),
    strip.background = element_rect(colour = NA, fill = NA)) + 
  labs(x = "", y = "Death rate change (%)") + 
  geom_hline(yintercept = 0) + 
  scale_color_manual(values = ssppal, name = "", labels = ssplabs)

# Plot by country
plotcntr <- plotlayout +
  geom_line(aes(x = year5, y = dr, group = factor(ssp), col = factor(ssp)),
    size = 1) + 
  facet_wrap(~ CNTR_CODE, labeller = labeller(ssp = ssplabs)) 

# Plot full Europe
ploteu <- plotlayout + 
  geom_line(aes(x = year5, y = dr, group = factor(ssp), col = factor(ssp)),
    size = 1, data = dreu) +
  labs(title = "European level")

# Put together
design <- "
  111
  222
  222
"
ploteu / plotcntr + plot_layout(guides = "collect", design = design)

# Save
ggsave(sprintf("figures/%s%i_DRproj.png", fignm, figcount <- figcount + 1), 
  height = 10)

#-------------------------------
# Section D: ERF extrapolation
#-------------------------------

#----- Show European level ERFs with extrapolation

# Load data about second-stage meta-regression
load("data/meta-model.RData")

# Get design matrix for age only (because of factor expansion of region)
mixterms <- delete.response(terms(stage2res))
euages <- summarise(cityage, age = mean(age), .by = agegroup)
agemm <- model.matrix(mixterms[grep("age", attr(mixterms, "term.labels"))],
  euages)

# Get meta coefficients and vcov for intercept and age
allmetac <- coef(stage2res)
ageinds <- grep("age|(Intercept)", names(allmetac))
agemetac <- allmetac[ageinds]
agemetav <- vcov(stage2res)[ageinds, ageinds]

# Predict DLNM coefficients and vcov for all ages
agepreds <- apply(agemm, 1, function(x){
  xexp <- t(x) %x% diag(length(agemetac) / 2)
  coefpred <- xexp %*% agemetac
  vcovpred <- xexp %*% agemetav %*% t(xexp)
  list(fit = coefpred, vcov = vcovpred)
})

# Get average European tmean distribution and associated basis
tdist <- read_parquet("data/era5series.gz.parquet") |>
  subset(year(date) %between% histrange) |>
  reframe(perc = predper, obs = quantile(era5landtmean, predper / 100), 
    .by = URAU_CODE) |>
  summarise(tmean = mean(obs), .by = perc)
varknots <- subset(tdist, perc %in% varper, tmean, drop = T)
varbound <- range(tdist$tmean)
argvar <- list(fun = varfun, degree = vardegree, knots = varknots,
  Bound = varbound)
ov_basis <- do.call(onebasis, c(list(x = tdist$tmean), argvar))

# Find MMP
firstpred <- ov_basis %*% sapply(agepreds, "[[", "fit")
inrange <- predper >= 25 & predper <= 99
agemmt <- tdist$tmean[inrange][apply(firstpred[inrange,], 2, which.min)]

# Get list of ERFs on an expanded distribution (for illustration)
agecp <- Map(crosspred, basis = list(ov_basis), 
  coef = lapply(agepreds, "[[", "fit"), vcov = lapply(agepreds, "[[", "vcov"),
  model.link = "log", cen = agemmt, at = 
    list(seq(min(tdist$tmean) - 2, max(tdist$tmean) + 3, length.out = 100))) |>
  suppressWarnings()
names(agecp) <- agelabs
ageerf <- lapply(agecp, 
    function(x) x[c("predvar", "allRRfit", "allRRlow", "allRRhigh")] |>
      as.data.frame()) |> 
  rbindlist(idcol = "agegroup")

# Constrain >1 and projection part
ageerf[, ":="(hist = predvar %between% range(tdist$tmean), 
  allRRfit = pmax(allRRfit, 1), allRRlow = pmax(allRRlow))]

# Plot
ggplot(ageerf) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    axis.title = element_text(face = "bold")) +
  geom_line(aes(x = predvar, y = allRRfit, col = agegroup), 
    linewidth = 1, linetype = 3) +
  geom_line(aes(x = predvar, y = allRRfit, col = agegroup), 
    linewidth = 1, linetype = 1, data = ageerf[hist == TRUE,]) + 
  geom_hline(yintercept = 1) + 
  scale_colour_manual(values = agepal, name = "Age group") + 
  scale_x_continuous(
    breaks = subset(tdist, perc %in% c(1, 25, 50, 75, 99), tmean, drop = T),
    labels = c(1, 25, 50, 75, 99)) +
  labs(y = "RR", x = "Temperature percentile")

# Save
ggsave(sprintf("figures/%s%i_ERFextrapol.png", fignm, figcount <- figcount + 1),
  height = 4, width = 6)


#----- Adaptation

# Select age to display and adaptation scenarios
ageada <- "85+"
adasc <- unique(adaptdf$heat)
adammt <- agemmt[agelabs == ageada]

# Select ERF and apply adaptation
newnames <- sprintf("RR_%s", adasc)
adaerf <- ageerf[agegroup == ageada,] 
adaerf[,  (newnames) := lapply(adasc, function(x) ifelse(predvar > adammt, 
  1 + (allRRfit - 1) * (1 - x / 100), allRRfit))]
adaerf <- melt(adaerf, measure.vars = newnames, id.vars = "predvar", 
  variable.name = "ada", value.name = "RR")

# Plot ERFs
ggplot(adaerf) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    axis.title = element_text(face = "bold")) +
  geom_line(aes(x = predvar, y = RR, col = ada), linewidth = 1) +
  geom_line(aes(x = predvar, y = RR), col = 1, linewidth = 1, 
    data = adaerf[ada == "RR_0"]) +
  geom_hline(yintercept = 1) + 
  scale_colour_manual(values = "names<-"(adapal, sprintf("RR_%s", adasc)), 
    name = "Risk attenuation", labels = unique(adaptdf$adapt)) + 
  scale_x_continuous(
    breaks = subset(tdist, perc %in% c(1, 25, 50, 75, 99), tmean, drop = T),
    labels = c(1, 25, 50, 75, 99)) +
  labs(y = "RR", x = "Temperature")

# Save
ggsave(sprintf("figures/%s%i_ERFadapt.png", fignm, figcount <- figcount + 1),
  height = 4, width = 6)

#-------------------------------
# Section E: Additional results
#-------------------------------

#----- Decomposition of AN

# Extract detail of total deaths
plotan <- finalres$eu_period[agegroup == "all" & range != "tot" &
    sc %in% c("clim", "demo") & ssp %in% ssplist & gcm == "ens",]

# Create interaction for aesthetic
plotan[, group := factor(interaction(range, sc),
  levels = c("heat.clim", "heat.demo", "cold.demo", "cold.clim"),
  labels = c("Climate change - Heat", "Baseline - Heat",
    "Baseline - Cold", "Climate change - Cold"))]

# Create aesthetic
pal <- scico(n = 9, palette = "berlin")[c(1, 3, 6, 8)]
names(pal) <- c("Baseline - Cold", "Climate change - Cold", 
  "Climate change - Heat", "Baseline - Heat")

# Scaling factor for numbers
byan <- 1000

# Plot areas
ggplot(plotan) +
  theme_classic() + 
  theme(plot.margin = unit(c(1, 5, 1, 1), "line"), 
    panel.grid.major = element_line(colour = grey(.5), linewidth = .1),
    panel.border = element_rect(fill = NA), 
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom") + 
  facet_grid(cols = vars(ssp), rows = vars(adapt), 
    labeller = labeller(ssp = ssplabs)) + 
  labs(x = "", y = sprintf("Excess deaths (x%s)", 
    formatC(byan, format = "f", digits = 0, big.mark = ","))) + 
  coord_cartesian(clip = "off") + 
  geom_area(aes(x = period, y = an_est / byan, group = group, fill = group)) +
  scale_fill_manual(values = pal, name = "", 
    breaks = c("Climate change - Cold", "Climate change - Heat", 
      "Baseline - Cold", "Baseline - Heat"),
    guide = guide_legend(ncol = 2, byrow = T)) + 
  geom_hline(yintercept = 0)

# Export
ggsave(sprintf("figures/%s%i_TotalExcess.png", fignm, figcount <- figcount + 1), 
  width = 10, height = 10)


#----- Results by age

# Select age groups and clim only scenario for european wide
plotage <- finalres$eu_period[agegroup != "all" & sc == "clim" &
    ssp %in% ssplist & gcm == "ens" & range == "tot",]

# Plot
ggplot(plotage) +
  theme_classic() + 
  theme(plot.margin = unit(c(1, 5, 1, 1), "line"), 
    panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(colour = NA, fill = NA)) + 
  facet_wrap(~ adapt + agegroup, ncol = length(agelabs), scales = "free_y") + 
  labs(x = "Year", y = sprintf("Excess death rate (x%s)", 
    formatC(byrate, format = "f", digits = 0, big.mark = ","))) +
  geom_line(aes(x = period, y = rate_est * byrate, col = factor(ssp)),
    size = .5) + 
  scale_color_manual(values = ssppal, name = "", labels = ssplabs) +
  geom_hline(yintercept = 0)

# Save
ggsave(sprintf("figures/%s%i_age.png", fignm, figcount <- figcount + 1), 
  width = 10, height = 7)

#----- Result by GCM

# Select age groups and clim only scenario for european wide
plotgcm <- finalres$eu_period[agegroup == "all" & sc == "clim" &
    ssp %in% ssplist & range == "tot",]

# Multiply rates by the denominator
ratevars <- grep("rate", colnames(plotgcm), value = T)
plotgcm[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]

# Build plot
ggplot(plotgcm) +
  theme_classic() + 
  theme(plot.margin = unit(c(1, 7, 1, 1), "line"), 
    panel.grid.major = element_line(colour = grey(.9), linewidth = .2),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(colour = NA, fill = NA)) + 
  geom_hline(yintercept = 0) + 
  facet_grid(cols = vars(ssp), rows = vars(adapt), 
    labeller = labeller(ssp = ssplabs)) + 
  labs(x = "Year", y = sprintf("Excess death rate (x%s)", 
    formatC(byrate, format = "f", digits = 0, big.mark = ","))) + 
  coord_cartesian(xlim = range(plotgcm$period), clip = "off") + 
  geom_line(aes(x = period, y = rate_est, col = gcm, linetype = gcm),
    subset(plotgcm, gcm != "ens"), linewidth = .5) + 
  geom_line(aes(x = period, y = rate_est), col = 1, show.legend = F,
    subset(plotgcm, gcm == "ens"), linewidth = 1) +
  scale_color_manual(values = gcmpal, name = "") + 
  scale_linetype_manual(values = gcmlntp, name = "")

# Save plot
ggsave(sprintf("figures/%s%i_resultGCM.png", fignm, figcount <- figcount + 1), 
  width = 10, height = 8)


#----- Trends for countries and regions

# Select countries, all ages and difference full-demo sub-scenario and net
plotcntr <- finalres$country_period[agegroup == "all" & sc == "clim" &
    range == "tot" & ssp %in% ssplist,]

# Add info about countries
cntr_info <- group_by(cities, CNTR_CODE) |>
  summarise(cntr_name = cntr_name[1], region = region[1],lat = mean(lat))
plotcntr <- merge(plotcntr, cntr_info, by.x = "country", by.y = "CNTR_CODE")

# Select regional data
plotreg <- finalres$region_period[agegroup == "all" & sc == "clim" &
    range == "tot" & ssp %in% ssplist,]

# Multiply rates by the denominator
ratevars <- grep("rate", colnames(plotcntr), value = T)
plotcntr[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]
plotreg[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]

# Data to display country on the right
cntrlabs <- plotcntr[period == max(period) & ssp == 3,]

# Plot layout and theme
figtrend_cntr <- ggplot(plotcntr) +
  theme_classic() +
  theme(plot.margin = unit(c(1, 5, 1, 1), "line"), legend.position = "bottom",
    panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(colour = NA, fill = NA)) +
  geom_hline(yintercept = 0) +
  facet_grid(cols = vars(ssp), rows = vars(adapt), 
    labeller = labeller(ssp = ssplabs), switch = "y") +
  labs(x = "", y = sprintf("Excess death rate (x%s)",
    formatC(byrate, format = "f", digits = 0, big.mark = ",")),
    color = "", fill = "") +
  coord_cartesian(clip = "off", xlim = range(plotcntr$period))

# Add country level curves
figtrend_cntr <- figtrend_cntr +
  geom_line(aes(x = period, y = rate_est, col = region, group = country),
    alpha = .3) +
  geom_text(aes(y = rate_est, label = cntr_name, x = max(period) + 5,
      col = region), alpha = .8, size = 3,
    data = plotcntr[period == max(period),], hjust = -0,
    show.legend = F, check_overlap = T, nudge_x = 1)

# Add region level curves
figtrend_cntr <- figtrend_cntr +
  geom_line(aes(x = period, y = rate_est, col = region, group = region),
    data = plotreg, linewidth = rel(1.5)) +
  scale_color_manual(values = regpal)

# Save
ggsave(sprintf("figures/%s%i_trendCountries.png", fignm, 
  figcount <- figcount + 1), figtrend_cntr, height = 10, width = 10)


#----- City level rates for each adaptation level

# All age group and net effect
adamap <- finalres$city_level[agegroup == "all" & sc == "clim" & 
    ssp == 3 & range == "tot" & adapt != "0%", ]

# Add geographical info
adamap <- merge(adamap, cities[, c("URAU_CODE", "lon", "lat", "pop")],
  by.x = "city", by.y = "URAU_CODE")

# Multiply rates by the denominator
ratevars <- grep("rate", colnames(adamap), value = T)
adamap[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]

# Cut points for palette
cutpts <- unique(sort(c(0, unname(round(
  quantile(adamap$rate_est, seq(0, 1, length.out = 20)) / 5) * 5))))
adamap[, colgrp := cut(rate_est, cutpts)]
signtab <- table(factor(sign(cutpts), c(-1, 0, 1)))

# Palettes (fill and border)
npal <- (max(signtab)) * 2
# pal <- scico(npal, palette = "bam", direction = -1)[
#   (max(signtab) - signtab[1] + 1):(max(signtab) + signtab[3])]
pal <- c(scico(npal, palette = "bam", direction = -1)[
  max(signtab) - signtab[1] + seq_len(signtab[1])],
  scico(tail(signtab, 1), palette = "acton", direction = -1))
bpal <- rep(c("white", "black"), signtab[c("-1", "1")])
names(pal) <- names(pal) <- levels(adamap$colgrp)

# Theme and layout
adamapfig <- ggplot(adamap) + theme_void() +
  theme(legend.position = "bottom", legend.box = "vertical",
    strip.text = element_text(hjust = 0.5, face = "bold", size = 14), 
    title = element_text(hjust = 0, face = "bold", size = 12),
    panel.border = element_rect(colour = 1, fill = NA)) +
  facet_grid(rows = vars(level), cols = vars(adapt), 
    labeller = labeller(level = levellabs), switch = "y")

# European map layout
adamapfig <- adamapfig + 
  geom_sf(data = euromap, fill = grey(.9), col = "white", inherit.aes = F) +
  coord_sf(xlim = range(adamap$lon), ylim = range(adamap$lat),
    lims_method = "box", crs = st_crs(euromap), default_crs = st_crs(4326))

# Add cities
adamapfig <- adamapfig +
  geom_point(aes(x = lon, y = lat, fill = colgrp, size = pop),
    shape = 21, stroke = .5, col = "black", alpha = .8) +
  scale_fill_manual(values = pal) +
  scale_size(range = c(1, 10), breaks = c(0.1, 0.5, 3, 7.5) * 10^6,
    labels = ~ number(./10^6)) +
  labs(size = "Population (in millions)",
    colour = sprintf("Excess death rate (x%s)", 
      formatC(byrate, format = "f", digits = 0, big.mark = ",")), 
    fill = sprintf("Excess death rate (x%s)", 
      formatC(byrate, format = "f", digits = 0, big.mark = ","))) +
  guides(size = guide_legend(override.aes = list(col = 1, fill = "darkgrey",
      stroke = .5)),
    fill = guide_bins(override.aes = list(size = 5), direction = "horizontal"))

# Save plot
ggsave(sprintf("figures/%s%i_mapsAdaptation.png", fignm, 
  figcount <- figcount + 1), adamapfig, height = 20, width = 15)
