################################################################################
# Supplementary plots
################################################################################

#-------------------------------
# Description of projections
#-------------------------------

#----- Calibration

# Compute difference between calibrated and raw
diffdat <- finalres$calibration[, diff := tas - cal]

# Plot by summary and GCM
ggplot(diffdat) + 
  theme_classic() +
  theme(panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0)) +
  geom_boxplot(aes(fill = summary, y = diff, x = gcm, 
    group = interaction(gcm, summary)), outlier.size = .1,
    size = .1) + 
  geom_hline(yintercept = 0) + 
  scale_fill_manual(values = frename(rngpal, tot = mean), 
    name = "Temperature range",
    labels = c(cold = "Cold", heat = "Heat", mean = "Centre")) +
  labs(x = "", y = "RMSE decrease")

# Save
ggsave("figures/calibration.pdf")

#----- GCM projections

# Compute EU mean by 5 years
tsumeu <- finalres$tsum[summary == "mean",] |> 
  mutate(year5 = floor(year / 5) * 5)
tsumeu <- tsumeu[, .(tmean = mean(cal)), by = .(year5, ssp, gcm)]

# Plot layout and theme
figgcm <- ggplot(tsumeu) +
  theme_classic() + 
  theme(plot.margin = unit(c(1, 7, 1, 1), "line"), 
    panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(colour = NA, fill = NA)) + 
  facet_wrap(~ ssp, labeller = labeller(ssp = ssplabs)) + 
  labs(x = "Year", y = "Annual mean temperature")

# Plot GCM projections
figgcm <- figgcm + 
  geom_line(aes(x = year5, y = tmean, col = gcm, linetype = gcm)) + 
  scale_color_manual(values = gcmpal, name = "GCM") + 
  scale_linetype_manual(values = gcmlntp, name = "GCM")

# Save plot
ggsave("figures/Fig_GCMtmean.pdf", figgcm, width = 10)


#----- GCM distribution at levels and periods

# Compute average tmean for each warming level
tsumlevel <- merge(finalres$tsum[summary == "mean",], warming_win, 
  by = c("gcm", "year", "ssp"), all.x = T, allow.cartesian = T)
tsumlevel <- tsumlevel[, .(tmean = mean(cal)), by = .(level, ssp, gcm)]
tsumlevel <- na.omit(tsumlevel)

# Limit of the plot
ylim <- range(c(tsumlevel$tmean, tsumeu$tmean), na.rm = T)

# Plot distribution for periods
figperiod <- ggplot(tsumeu) +
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
  stat_interval(aes(x = year5, y = tmean), .width = c(.5, .8, .95, 1)) + 
  scale_color_discrete(type = scico(4, palette = "batlow", direction = -1), 
    labels = function(x) paste0(as.numeric(x)*100, "%"),
    name = "") +
  labs(x = "Period")
  
# Plot distribution for levels
figlevel <- ggplot(tsumlevel) +
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
ggsave("figures/fig_GCMdist.pdf", figall, height = 7)

#----- Average warming by city

# Extract calibrated series (last 5y period)
citytsum <- mutate(finalres$tsum, year5 = floor(year / 5) * 5) |>
  subset(summary == "mean" & year5 %in% range(year5)) |>
  melt(id.vars = c("year", "year5", "city", "ssp"), 
    measure.vars = patterns("^cal"), 
    variable.name = "gcm", value.name = "tmean") |>
  mutate(gcm = gsub("cal_", "", gcm, fixed = T))

# Compute difference of period averages for each city
citytsum <- citytsum[order(year5), .(tmean = mean(tmean, na.rm = T)), 
  by = .(city, year5, ssp)]
citytsum <- citytsum[, .(tmean = diff(tmean)), by = .(city, ssp)]

# Add city info for mapping
citytsum <- merge(citytsum, cities[,c("URAU_CODE", "lon", "lat", "pop")],
  by.x = "city", by.y = "URAU_CODE")

# Plot
citywarmfig <- ggplot(citytsum) + 
  theme_void() +
  theme(legend.position = "bottom", legend.box = "vertical",
    strip.text.x = element_text(hjust = 0.5, face = "bold", size = 14), 
    title = element_text(hjust = 0, face = "bold", size = 12),
    panel.background = element_rect(fill = "#cae9f5"),
    panel.border = element_rect(colour = 1, fill = NA)) + 
  facet_wrap(. ~ ssp, labeller = labeller(ssp = ssplabs)) + 
  geom_sf(data = euromap, fill = grey(.9), inherit.aes = F) +
  coord_sf(xlim = range(citytsum$lon), ylim = range(citytsum$lat),
    lims_method = "box", crs = st_crs(euromap), default_crs = st_crs(4326)) +
  geom_point(aes(x = lon, y = lat, fill = tmean, size = pop), 
    shape = 21, stroke = .01) +
  scale_fill_scico(palette = "lipari", direction = -1) +
  scale_size(range = c(1, 10), breaks = c(0.1, 0.5, 3, 7.5) * 10^6,
    labels = ~ number(./10^6)) +
  labs(size = "Population (in millions)",
    fill = "Average warming (C)") +
  guides(size = guide_legend(override.aes = list(col = 1)))

# Save
ggsave("figures/fig_cityWarm.pdf", citywarmfig, width = 10)


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
ggsave("figures/fig_GCMlevels.pdf", width = 10)


#-------------------------------
# Demographic projections
#-------------------------------

# Country palette ordred like regions
names(cntrpal) <- summarise(cities, lat = mean(lat), 
    .by = c("CNTR_CODE", "region")) |>
  mutate(region = factor(region, levels = ordreg)) |>
  arrange(region, desc(lat)) |>
  pull(CNTR_CODE)

#----- Population projections

# Compute population change
popsum <- proj[, .(pop = sum(wittpop)), by = .(CNTR_CODE, ssp, year5)]
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
ggsave("figures/Fig_Popproj.pdf", height = 10)

#----- Population structure

# Compute population structure
# popstruct <- proj[, popprop := 100 * wittpop / sum(wittpop), 
#   by = .(CNTR_CODE, ssp, year5)]

# Compute population structure for EU
eustruct <- proj[, .(pop = sum(wittpop)), by = .(agegroup, ssp, year5)]
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
ggsave("figures/Fig_Structproj.pdf")

#----- Death rate projections

# Compute population change
drsum <- proj[, .(pop = sum(wittpop), death = sum(wittdeath)), 
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
ggsave("figures/Fig_DRproj.pdf", height = 10)


#-------------------------------
# Additional results
#-------------------------------

#----- Decomposition of AN

# Extract detail of total deaths
plotan <- finalres$eu_period[agegroup == "all" & range != "tot" &
    sc %in% c("full-demo", "demo") & ssp %in% ssplist & gcm == "ens",]

# Create interaction for aesthetic
plotan[, group := factor(interaction(range, sc),
  levels = c("heat.full-demo", "heat.demo", "cold.demo", "cold.full-demo"),
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
    panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom") + 
  facet_wrap(~ ssp, labeller = labeller(ssp = ssplabs)) + 
  labs(x = "", y = sprintf("Excess deaths (x%s)", 
    formatC(byan, format = "f", digits = 0, big.mark = ","))) + 
  coord_cartesian(clip = "off") + 
  geom_area(aes(x = period, y = an_est / byan, group = group, fill = group)) +
  scale_fill_manual(values = pal, labels = nms, name = "", 
    breaks = c("Climate change - Cold", "Climate change - Heat", 
      "Baseline - Cold", "Baseline - Heat"),
    guide = guide_legend(ncol = 2, byrow = T)) + 
  geom_hline(yintercept = 0)

# Export
ggsave("figures/Fig_TotalExcess.pdf", width = 12, height = 7)


#----- Results by age

# Select age groups and clim only scenario for european wide
plotage <- finalres$eu_period[agegroup != "all" & sc == "full-demo" &
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
  facet_wrap(~ agegroup, scales = "free_y") + 
  labs(x = "Year", y = sprintf("Excess death rate (x%s)", 
    formatC(byrate, format = "f", digits = 0, big.mark = ","))) +
  geom_line(aes(x = period, y = rate_est * byrate, col = factor(ssp)),
    size = 1.5) + 
  scale_color_manual(values = ssppal, name = "", labels = ssplabs) +
  geom_hline(yintercept = 0)

# Save
ggsave("figures/Fig_age.pdf", width = 10, height = 5)

#----- Result by GCM

# Select age groups and clim only scenario for european wide
plotgcm <- finalres$eu_period[agegroup == "all" & sc == "full-demo" &
    ssp %in% ssplist & range == "tot",]

# Multiply rates by the denominator
ratevars <- grep("rate", colnames(plotgcm), value = T)
plotgcm[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]

# Build plot
ggplot(plotgcm) +
  theme_classic() + 
  theme(plot.margin = unit(c(1, 7, 1, 1), "line"), 
    panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(colour = NA, fill = NA)) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ ssp, labeller = labeller(ssp = ssplabs)) + 
  labs(x = "Year", y = sprintf("Excess death rate (x%s)", 
    formatC(byrate, format = "f", digits = 0, big.mark = ","))) + 
  coord_cartesian(xlim = range(plotgcm$period), clip = "off") + 
  geom_line(aes(x = period, y = rate_est, col = gcm, linetype = gcm),
    subset(plotgcm, gcm != "ens"), linewidth = 1) + 
  geom_line(aes(x = period, y = rate_est), col = 1, show.legend = F,
    subset(plotgcm, gcm == "ens"), linewidth = 1.5) +
  scale_color_manual(values = gcmpal, name = "") + 
  scale_linetype_manual(values = gcmlntp, name = "")

# Save plot
ggsave("figures/Fig_GCMresult.pdf", width = 10)







### TRENDS FOR COUNTRIES/REGIONS






#-------------------------------
# Decomposition of uncertainty
#-------------------------------
# 
# #----- Compute two levels of uncertainty
# 
# # Extract only climate part and net effet
# uncert_data <- subset(finalres$total$level, agegroup == "all" & 
#     sc == "full-demo" & ssp %in% ssplist & range == "tot" & ssp == 3,
#   -c(range, sc, agegroup))
# 
# # Compute range due to climate models
# uncert_decomp <- subset(uncert_data, gcm != "ens") |>
#   group_by(level) |>
#   summarise(rate_low_gcm = min(rate_est), rate_high_gcm = max(rate_est)) |>
#   merge(subset(uncert_data, gcm == "ens", c(level, ssp, rate_low, rate_high)))
# 
# #----- Plot
# 
# # Palette
# pal <- scico(2, palette = "bamako")
# 
# # Plot layout and theme
# figlayout <- ggplot(uncert_decomp) +
#   theme_classic() + 
#   theme(plot.margin = unit(c(1, 7, 1, 1), "line"), 
#     panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
#     panel.border = element_rect(fill = NA), 
#     panel.background = element_rect(fill = NA),
#     strip.text = element_text(face = "bold"),
#     axis.title = element_text(face = "bold"),
#     strip.background = element_rect(colour = NA, fill = NA)) + 
#   geom_hline(yintercept = 0) + 
#   labs(x = "Warming level", y = sprintf("Excess death rate (x%s)", 
#     formatC(byrate, format = "f", digits = 0, big.mark = ",")))
# 
# # Add confidence intervals
# figlayout + 
#   geom_segment(aes(x = level, xend = level, y = rate_low, yend = rate_high), 
#     linewidth = 20, col = pal[1]) +
#   geom_segment(aes(x = level, xend = level, y = rate_low_gcm, 
#     yend = rate_high_gcm), linewidth = 20, col = pal[2])




