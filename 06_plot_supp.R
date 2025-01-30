################################################################################
# 
# Contrasting future heat and cold-related mortality under climate change, 
# demographic and adaptation scenarios in 854 European cities
#
# R Code Part 6: Extended data
#
# Pierre Masselot & Antonio Gasparrini
#
################################################################################

#-------------------------------
# Extended Data Figure 1: Age-specific death rates
#-------------------------------

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
    linewidth = .5) + 
  scale_color_manual(values = ssppal, name = "", labels = ssplabs) +
  geom_hline(yintercept = 0)

# Save
ggsave("figures/ExtendedFig1.eps", width = 11, height = 8)


#-------------------------------
# Extended Data Figure 2: Maps with full results
#-------------------------------

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
ggsave("figures/ExtendedFig2.jpg", adamapfig, height = 20, width = 15, 
  dpi = 600)


#-------------------------------
# Extended Data Figure 3: GCM average temperatures
#-------------------------------

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
    strip.text = element_text(face = "bold", size = rel(1.5)),
    axis.title = element_text(face = "bold", size = rel(1.5)),
    axis.text = element_text(size = rel(1.2)),
    axis.text.x.bottom = element_text(angle = -45, vjust = 1, hjust = 0),
    strip.background = element_rect(colour = NA, fill = NA)) + 
  facet_wrap(~ ssp, labeller = labeller(ssp = ssplabs)) + 
  labs(x = "Year", y = "Annual mean temperature") + 
  geom_line(aes(x = calperiod, y = tmean, col = gcm, 
    linetype = gcm, group = gcm)) + 
  scale_color_manual(values = gcmpal, name = "GCM") + 
  scale_linetype_manual(values = gcmlntp, name = "GCM")

# Save plot
ggsave("figures/ExtendedFig3.eps", height = 7.5, width = 14)

#-------------------------------
# Extended Data Figure 4: RMSE calibration
#-------------------------------

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
ggsave("figures/ExtendedFig4.eps", width = 10, height = 7.5)


#-------------------------------
# Extended Data Figure 5: Wittgenstein projections
#-------------------------------

#----- Population

# Compute the total for EU
popeu <- proj[year5 >= min(projrange), .(pop = sum(wittpop)), 
    by = .(ssp, year5)][,
  .(year5, pop = 100 * (pop - pop[year5 == min(year5)]) / 
      pop[year5 == min(year5)]), by = ssp]

# Plot
plotpop <- ggplot(popeu) + 
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
  scale_color_manual(values = ssppal, name = "", labels = ssplabs) + 
  geom_line(aes(x = year5, y = pop, group = factor(ssp), col = factor(ssp)),
    size = 1, data = popeu) +
  labs(title = "a) Total population")

#----- Death rates

# Compute the total for EU
dreu <- proj[year5 >= min(projrange), 
  .(pop = sum(wittpop), death = sum(wittdeath)), 
  by = .(ssp, year5)]
dreu[, dr := death / pop]
dreu <- dreu[,.(year5, dr = 100 * (dr - dr[year5 == min(year5)]) / 
    dr[year5 == min(year5)]), by = ssp]

# Plot layout
plotdr <- ggplot(dreu) + 
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
  scale_color_manual(values = ssppal, name = "", labels = ssplabs) + 
  geom_line(aes(x = year5, y = dr, group = factor(ssp), col = factor(ssp)),
    size = 1, data = dreu) +
  labs(title = "b) Baseline death rate")

#----- Population structure

# Compute population structure for EU
eustruct <- proj[year5 >= min(projrange), .(pop = sum(wittpop)), 
  by = .(agegroup, ssp, year5)]
eustruct[, popprop := 100 * pop / sum(pop), by = .(ssp, year5)]

# Plot
plotstruct <- ggplot(eustruct) + 
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
  coord_cartesian(expand = F) +
  labs(title = "c) Population structure")

#----- Put together

(plotpop + plotdr) / plotstruct + plot_layout(guides = "collect")
ggsave("figures/ExtendedFig5.eps", width = 12, height = 9)


#-------------------------------
# Extended Data Figure 6: Exposure-response function illustration
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
plotextrapol <- ggplot(ageerf) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    axis.title = element_text(face = "bold", size = rel(1.2)),
    axis.text = element_text(size = rel(1.2)),
    title = element_text(size = rel(1.5)),
    legend.text = element_text(size = rel(1.2))) +
  geom_line(aes(x = predvar, y = allRRfit, col = agegroup), 
    linewidth = 1, linetype = 3) +
  geom_line(aes(x = predvar, y = allRRfit, col = agegroup), 
    linewidth = 1, linetype = 1, data = ageerf[hist == TRUE,]) + 
  geom_hline(yintercept = 1) + 
  scale_colour_manual(values = agepal, name = "Age group") + 
  scale_x_continuous(
    breaks = subset(tdist, perc %in% c(1, 25, 50, 75, 99), tmean, drop = T),
    labels = c(1, 25, 50, 75, 99)) +
  labs(y = "RR", x = "Temperature percentile", 
    title = "a) Extrapolation in future climate")

#----- Illustrate adaptation

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
plotadapt <- ggplot(adaerf) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    axis.title = element_text(face = "bold", size = rel(1.2)),
    axis.text = element_text(size = rel(1.2)),
    title = element_text(size = rel(1.5)),
    legend.text = element_text(size = rel(1.2))) +
  geom_line(aes(x = predvar, y = RR, col = ada), linewidth = 1) +
  geom_line(aes(x = predvar, y = RR), col = 1, linewidth = 1, 
    data = adaerf[ada == "RR_0"]) +
  geom_hline(yintercept = 1) + 
  scale_colour_manual(values = "names<-"(adapal, sprintf("RR_%s", adasc)), 
    name = "Risk attenuation", labels = unique(adaptdf$adapt)) + 
  scale_x_continuous(
    breaks = subset(tdist, perc %in% c(1, 25, 50, 75, 99), tmean, drop = T),
    labels = c(1, 25, 50, 75, 99)) +
  labs(y = "RR", x = "Temperature percentile", title = "b) Risk attenuation")

#----- Put together

# Patch together
plotextrapol + plotadapt

# Save
ggsave("figures/ExtendedFig6.eps", height = 7.5, width = 17)


#-------------------------------
# Extended Data Figure 7: Decomposition of the European burden
#-------------------------------

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
ggsave("figures/ExtendedFig7.eps", width = 10, height = 10)
