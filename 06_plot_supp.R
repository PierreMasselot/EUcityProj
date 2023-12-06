################################################################################
# Supplementary plots
################################################################################


#-------------------------------
# Age structure of deaths
#-------------------------------

#----- Select data

# Select age groups and clim only scenario for european wide
plotage <- finalres$eu_period[agegroup != "all" & sc == "full-demo" &
    ssp %in% ssplist & gcm == "ens" & range == "tot",]

# Compute proportion by age
plotage[, death_struct := an_est / sum(an_est), 
  by = .(period, sc, ssp)]

# Compute rate according to tot population
plotage <- merge(plotage[, !"pop"], 
    finalres$eu_period[agegroup != "all" & sc == "full" & ssp %in% ssplist & 
      gcm == "ens" & range == "tot", .(period, agegroup, ssp, pop)],
  by = c("period", "agegroup", "ssp"))
plotage[, rate_totpop := byrate * an_est / sum(pop), by = .(period, sc, ssp)]

#----- Build plot

# Plot layout and theme
figlayout <- ggplot(plotage) +
  theme_classic() + 
  theme(plot.margin = unit(c(1, 5, 1, 1), "line"), 
    panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    panel.ontop = T,
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(colour = NA, fill = NA)) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ ssp, labeller = labeller(ssp = ssplabs)) + 
  labs(x = "Year", y = sprintf("Excess death rate (x%s)", 
    formatC(byrate, format = "f", digits = 0, big.mark = ","))) + 
  coord_cartesian(expand = F, clip = "off", xlim = range(plotage$period)) 

# Add age structure
figage <- figlayout + 
  geom_area(aes(x = period, y = rate_totpop, fill = agegroup)) + 
  scale_fill_manual(values = agepal)

# Save
ggsave("figures/Fig_age.pdf", figage, width = 10)

#-------------------------------
# GCM projections
#-------------------------------

#----- Compute projections mean

# Compute Eu mean
tsumeu <- finalres$tsum[summary == "mean", .(tmean = mean(cal)), 
  by = .(year, ssp, gcm)]

#----- Plot

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
  geom_smooth(aes(x = year, y = tmean, col = gcm), se = F) + 
  scale_color_manual(values = gcmpal, name = "GCM")

# Save plot
ggsave("figures/Fig_GCMproj.pdf", figgcm, width = 10)

#-------------------------------
# GCM distribution at targets and periods
#-------------------------------

#----- prepare data

# Compute 20 years moving average
rolltsum <- tsumeu[, .(tmean = frollmean(tmean, 21, align = "center"), 
  year = year), by = .(ssp, gcm)]

# Add info about warming level years
rolltsum <- merge(rolltsum, warming_years, allow.cartesian = T, all = T) |>
  mutate(level = factor(level, levels = targets), 
    decade = floor(year / 10) * 10)

# Limit
ylim <- range(rolltsum$tmean, na.rm = T)

#----- Plot

# Plot layout
plotlayout <- ggplot(rolltsum) +
  theme_classic() + 
  theme(plot.margin = unit(c(1, 7, 1, 1), "line"), 
    panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    panel.background = element_rect(fill = NA),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(colour = NA, fill = NA)) + 
  facet_wrap(~ ssp, labeller = labeller(ssp = ssplabs)) + 
  labs(x = "Level", y = "Annual mean temperature") + 
  coord_cartesian(ylim = ylim)

# Boxplot for levels
figlev <- plotlayout + 
  geom_boxplot(aes(x = level, y = tmean)) + 
  scale_x_discrete(limits = factor(targets, levels = targets))

# Boxplot for decades
figdec <- plotlayout + 
  geom_boxplot(aes(x = factor(decade), y = tmean), 
    data = na.omit(rolltsum[,-"level"]))

# Save
ggsave("figures/fig_boxplotTmeanLevels.pdf", figlev, width = 10)
ggsave("figures/fig_boxplotTmeanDecades.pdf", figdec, width = 10)

#-------------------------------
# Average warming by city
#-------------------------------

#----- Extract cities warming

# Extract calibrated series (last 5y period)
citytsum <- mutate(finalres$tsum, year5 = floor(year / 5) * 5) |>
  subset(summary == "mean" & year5 %in% range(year5)) |>
  melt(id.vars = c("year", "year5", "URAU_CODE", "ssp"), 
    measure.vars = patterns("^cal"), 
    variable.name = "gcm", value.name = "tmean") |>
  mutate(gcm = gsub("cal_", "", gcm, fixed = T))

# Compute difference of period averages for each city
citytsum <- citytsum[order(year5), .(tmean = mean(tmean, na.rm = T)), 
    by = .(URAU_CODE, year5, ssp)]
citytsum <- citytsum[, .(tmean = diff(tmean)), by = .(URAU_CODE, ssp)]

# Add city info for mapping
citytsum <- merge(citytsum, cities[,c("URAU_CODE", "lon", "lat", "pop")])

#----- Create maps

# Theme and layout
citywarmfig <- ggplot(subset(citytsum, ssp == 3)) + theme_void() +
  theme(legend.position = "bottom", legend.box = "vertical",
    strip.text.x = element_text(hjust = 0.5, face = "bold", size = 14), 
    title = element_text(hjust = 0, face = "bold", size = 12),
    panel.background = element_rect(fill = "#cae9f5"),
    panel.border = element_rect(colour = 1, fill = NA))

# European map layout
citywarmfig <- citywarmfig + 
  geom_sf(data = euromap, fill = grey(.9), inherit.aes = F) +
  coord_sf(xlim = range(citytsum$lon), ylim = range(citytsum$lat),
    lims_method = "box", crs = st_crs(euromap), default_crs = st_crs(4326))

# Add cities
citywarmfig <- citywarmfig +
  geom_point(aes(x = lon, y = lat, fill = tmean, size = pop), 
    shape = 21, stroke = .01) +
  scale_fill_scico(palette = "bilbao", direction = -1) +
  scale_size(range = c(1, 10), breaks = c(0.1, 0.5, 3, 7.5) * 10^6,
    labels = ~ number(./10^6)) +
  labs(size = "Population (in millions)",
    fill = "Average warming (C)") +
  guides(size = guide_legend(override.aes = list(col = 1)))

# Save
ggsave("figures/fig_cityWarm.pdf", citywarmfig, width = 10)

#-------------------------------
# Results by GCM
#-------------------------------

#----- Select data

# Select age groups and clim only scenario for european wide
plotgcm <- finalres$eu_period[agegroup == "all" & sc == "full-demo" &
    ssp %in% ssplist & range == "tot",]

# Multiply rates by the denominator
ratevars <- grep("rate", colnames(plotgcm), value = T)
plotgcm[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]

#----- Build plot

# Plot layout and theme
figlayout <- ggplot(plotgcm) +
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
  coord_cartesian(xlim = range(plotgcm$period), clip = "off") 

# Plot GCM results as lines
figgcmlines <- figlayout + 
  geom_line(aes(x = period, y = rate_est, col = gcm, linetype = gcm),
    subset(plotgcm, gcm != "ens")) + 
  geom_line(aes(x = period, y = rate_est), col = 1, show.legend = F,
    subset(plotgcm, gcm == "ens"), linewidth = 1.5) +
  scale_color_manual(values = gcmpal, name = "GCM") + 
  scale_linetype_manual(values = gcmlntp, name = "GCM")

# # Plot GCM results as points
# figgcmpts <- figlayout + 
#   geom_point(aes(x = period, y = rate_est, col = gcm), alpha = .3, 
#     show.legend = F) + 
#   scale_color_manual(values = gcmpal)  + 
#   geom_text(aes(y = rate_est, label = gcm, x = max(period) + 5, col = gcm), 
#     data = plotgcm[period == max(period) & ssp == max(ssplist),], 
#     alpha = .8, size = 3, hjust = -0, check_overlap = T, show.legend = F, 
#     nudge_x = 5)
# 
# # Plot GCM results as distributions
# figgcmintervals <- figlayout + 
#   stat_interval(aes(x = period, y = rate_est), width = perlen, 
#     .width = c(.5, .8, .9, .95)) + 
#   scale_color_scico_d(palette = "bamako", direction = -1, 
#     name = "Interval length")

# Save plot
ggsave("figures/Fig_GCMline.pdf", figgcmlines, width = 10)
# ggsave("figures/Fig_GCMpts.pdf", figgcmpts, width = 10)
# ggsave("figures/Fig_GCMintvls.pdf", figgcmintervals, width = 10)

#-------------------------------
# Difference of deaths between SSP1 and SSP3
#-------------------------------

#----- Select data

# Select age groups and clim only scenario for european wide
plotsspdiff <- finalres$total$period[agegroup == "all" & sc == "full-demo" &
    ssp == 13 & gcm == "ens",]

# Multiply rates by the denominator
ratevars <- grep("rate", colnames(plotsspdiff), value = T)
plotsspdiff[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]

#----- Build plot

# Plot layout and theme
figlayout <- ggplot(plotsspdiff) +
  theme_classic() + 
  theme(panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold")) + 
  geom_hline(yintercept = 0) + 
  labs(x = "Year", y = sprintf("Excess death rate (x%s)", 
    formatC(byrate, format = "f", digits = 0, big.mark = ","))) + 
  scale_x_continuous(limits = range(plotsspdiff$period)) +
  coord_cartesian(clip = "off")

# Plot lines
trendssp <- figlayout + 
  geom_ribbon(aes(x = period, ymin = rate_low, ymax = rate_high, fill = range,
    group = range), col = NA, alpha = .1) + 
  geom_line(aes(x = period, y = rate_est, group = range, col = range), 
    linewidth = 1) + 
  scale_color_manual(values = rngpal, labels = rnglabs, name = "") + 
  scale_fill_manual(values = rngpal, labels = rnglabs, name = "")

# Save
ggsave("figures/Fig_SSPdiff.pdf", trendssp, width = 10)

#-------------------------------
# Warming level windows
#-------------------------------

#----- Select data

# Select GCMs
plotwarms <- subset(warming_years, ssp %in% ssplist & gcm %in% gcmlist)

# Add all possible GCMs as factor levels
plotwarms <- mutate(plotwarms, gcm = factor(gcm, levels = gcmlist))

#----- Build plot

# Plot layout and theme
figlayout <- ggplot(plotwarms) +
  theme_classic() + 
  theme(panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    strip.text = element_text(face = "bold", size = 6),
    axis.title = element_text(face = "bold")) + 
  facet_wrap(gcm ~ ., ncol = 1, drop = F, strip.position = "top") + 
  labs(x = "Year", y = "") + 
  coord_cartesian(xlim = range(finalres$total$period$period))

# Add windows
figwindows <- figlayout + 
  geom_segment(aes(y = factor(ssp), yend = factor(ssp), 
    x = year - 10, xend = year + 10, col = level), 
    arrow = arrow(len = unit(.1, "in"), ends = "both")) + 
  geom_point(aes(x = year, y = factor(ssp), col = level)) + 
  scale_color_manual(values = levelcol, name = "Level", labels = levellabs)

# Save
ggsave("figures/Fig_WarmingLevels.pdf", figwindows, height = 15)




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


#-------------------------------
# Map of ranks
#-------------------------------

#----- Select data

# All age group and net effect
plotrank <- finalres$city$level[agegroup == "all" & sc == "full-demo" & 
    ssp == 3 & range == "tot", !"pop"]

# Compute ranks
plotrank[, rate_rank := 100 * frank(rate_est) / max(frank(rate_est)), 
  by = level]

# Add geographical info
plotrank <- merge(plotrank, cities[, c("URAU_CODE", "lon", "lat", "pop")])

# Cut points for palette
plotrank[, colgrp := cut(rate_rank, seq(0, 100, by = 10))]

# Palettes (fill and border)
npal <- length(levels(plotrank$colgrp))
pal <- scico(npal, palette = "batlowW", direction = -1)

#----- Build rank plot

# Theme and layout
rkfig <- ggplot(plotrank) + theme_void() +
  theme(legend.position = "bottom", legend.box = "vertical",
    strip.text.x = element_text(hjust = 0.5, face = "bold", size = 14), 
    title = element_text(hjust = 0, face = "bold", size = 12),
    panel.background = element_rect(fill = "#cae9f5"),
    panel.border = element_rect(colour = 1, fill = NA)) +
  facet_grid(. ~ level, labeller = labeller(level = levellabs))

# European map layout
rkfig <- rkfig + 
  geom_sf(data = euromap, fill = grey(.9), inherit.aes = F) +
  coord_sf(xlim = range(plotrank$lon), ylim = range(plotrank$lat),
    lims_method = "box", crs = st_crs(euromap), default_crs = st_crs(4326))

# Add cities
rkfig <- rkfig +
  geom_point(aes(x = lon, y = lat, fill = colgrp, size = pop), col = 1,
    shape = 21, stroke = .01) +
  scale_fill_manual(values = pal) +
  scale_size(range = c(1, 10), breaks = c(0.1, 0.5, 3, 7.5) * 10^6,
    labels = ~ number(./10^6)) +
  labs(size = "Population (in millions)",
    fill = "Excess death rank (%)") +
  guides(size = guide_legend(override.aes = list(col = 1)),
    fill = guide_bins(override.aes = list(size = 5), axis = F, 
      direction = "horizontal", nrow = 1, label.position = "bottom"))

# Save plot
ggsave("figures/Fig_rankMaps.pdf", plot = rkfig, width = 13, height = 10)

#----- Rank progression

# Compute progression
rankprog <- plotrank[, .(prog = rate_rank[level == 3] - rate_rank[level == 1.5],
  lon = lon, lat = lat, pop = pop), by = URAU_CODE]
rankprog[, colgrp := cut(prog, seq(-50, 50, by = 20))]

# Theme and layout
progfig <- ggplot(rankprog) + theme_void() +
  theme(legend.position = "bottom", legend.box = "vertical",
    panel.background = element_rect(fill = "#cae9f5"),
    panel.border = element_rect(colour = 1, fill = NA))

# European map layout
progfig <- progfig + 
  geom_sf(data = euromap, fill = grey(.9), inherit.aes = F) +
  coord_sf(xlim = range(cities$lon), ylim = range(cities$lat),
    lims_method = "box", crs = st_crs(euromap), default_crs = st_crs(4326))

# Add cities
progfig <- progfig +
  geom_point(aes(x = lon, y = lat, fill = colgrp, size = pop, col = colgrp), 
    shape = 21, stroke = .01) +
  scale_color_manual(values = c(rep("white", 2), rep("black", 3)),
    name = "Rank progression") +
  scale_fill_scico_d(palette = "vik", name = "Rank progression") +
  scale_size(range = c(1, 10), breaks = c(0.1, 0.5, 3, 7.5) * 10^6,
    labels = ~ number(./10^6), name = "Population (in millions)") +
  guides(size = guide_legend(override.aes = list(col = 1)),
    colour = guide_bins(override.aes = list(size = 5), axis = F, 
      direction = "horizontal", nrow = 1, label.position = "bottom"),
    fill = guide_bins(override.aes = list(size = 5), axis = F, 
      direction = "horizontal", nrow = 1, label.position = "bottom"))

# Save plot
ggsave("figures/Fig_rankProgMap.pdf", plot = progfig)


#-------------------------------
# Total numbers
#-------------------------------

#----- Extract data

# Extract detail of total deaths
plotan <- finalres$total$period[agegroup == "all" & range != "tot" &
    sc %in% c("full-demo", "demo") & ssp %in% ssplist & gcm == "ens"]

# Create interaction for aesthetic
plotan[, group := factor(interaction(range, sc),
  levels = c("heat.full-demo", "heat.demo", "cold.demo", "cold.full-demo"))]

# Create aesthetic
pal <- scico(n = 9, palette = "berlin")[c(1, 3, 6, 8)]
nms <- c("Baseline - Cold", "Climate change - Cold", "Climate change - Heat", 
  "Baseline - Heat")
names(pal) <- names(nms) <- c("cold.demo", "cold.full-demo", "heat.full-demo", 
  "heat.demo")

# Scaling factor for numbers
byan <- 1000

# Create dataset for uncertainty range (account for stacking)
plotci <- arrange(plotan, desc(group)) |> 
  subset(period == max(period))
plotci[, ":="(an_low = an_low - an_est + cumsum(an_est), 
  an_high = an_high - an_est + cumsum(an_est)), by = .(ssp, sign(an_est))]

#----- Build plot

# Plot layout and theme
figan <- ggplot(plotan) +
  theme_classic() + 
  theme(plot.margin = unit(c(1, 5, 1, 1), "line"), 
    panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(colour = NA, fill = NA),
    legend.position = "bottom") + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ ssp, labeller = labeller(ssp = ssplabs)) + 
  labs(x = "", y = sprintf("Excess deaths (x%s)", 
    formatC(byan, format = "f", digits = 0, big.mark = ","))) + 
  coord_cartesian(clip = "off")

# Draw areas
figan <- figan +
  geom_area(aes(x = period, y = an_est / byan, group = group, fill = group)) +
  scale_fill_manual(values = pal, labels = nms, name = "", 
    breaks = c("cold.full-demo", "heat.full-demo", "cold.demo", "heat.demo"),
    guide = guide_legend(ncol = 2, byrow = T))

# Add uncertainty for last period
figan <- figan + 
  geom_errorbar(aes(x = period + perlen * 2, ymin = an_low / byan, 
    ymax = an_high / byan, group = group, col = group),
    data = plotci, position = position_dodge(10), width = 10, linewidth = .5) + 
  scale_colour_manual(values = pal, labels = nms, name = "", 
    breaks = c("cold.full-demo", "heat.full-demo", "cold.demo", "heat.demo"))

# Export
ggsave("figures/Fig_TotalExcess.pdf", figan, width = 12, height = 7)
