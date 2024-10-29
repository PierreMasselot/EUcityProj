################################################################################
# 
# Contrasting future heat and cold-related mortality under climate change, 
# demographic and adaptation scenarios in 854 European cities
#
# R Code Part 5: Main plots
#
# Pierre Masselot & Antonio Gasparrini
#
################################################################################

#-------------------------
# Figure 1: trends
#-------------------------

#----- Select data

# Select all ages and clim only scenario for european wide
plottrends <- finalres$eu_period[agegroup == "all" & sc == "clim" &
    ssp %in% ssplist & gcm == "ens" & adapt == "0%",]

# Multiply rates by the denominator
ratevars <- grep("rate", colnames(plottrends), value = T)
plottrends[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]

# Compute the warming level windows frequency
winfreq <- warming_win[gcm %in% gcmlist, .(n = length(gcm)), 
  by = .(level, ssp, year)]
winfreq[, ":="(prop = n / length(gcmlist), ssp = as.numeric(ssp))]

#----- Plot parameters

# Size of warming level window lines
winsize <- max(plottrends$rate_high) / 20

# Y-axis breaks
yran <- range(plottrends[,c("rate_high", "rate_low")])
ybr <- pretty(yran, n = 10)
ybr <- ybr[ybr %between% yran]

#----- Build plot

# Plot layout and theme
figlayout <- ggplot(plottrends) +
  theme_classic() + 
  theme(plot.margin = unit(c(1, 5, 1, 1), "line"), 
    panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    strip.text = element_text(face = "bold", size = rel(1.5)),
    axis.title = element_text(face = "bold", size = rel(1.5)),
    axis.text = element_text(size = rel(1.2)),
    strip.background = element_rect(colour = NA, fill = NA)) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ ssp, labeller = labeller(ssp = ssplabs)) + 
  labs(x = "", y = sprintf("Excess death rate (x%s)", 
      formatC(byrate, format = "f", digits = 0, big.mark = ","))) + 
  scale_x_continuous(limits = c(min(plottrends$period), 2100)) +
  coord_cartesian(clip = "off")

# Add warming windows
figlayout <- figlayout + 
  geom_tile(aes(x = year, fill = prop * 100,
      y = max(plottrends$rate_high) + as.numeric(factor(level)) * winsize), 
    data = winfreq, inherit.aes = F, height = winsize / 2) + 
  scale_fill_gradient(low = "white", high = "black", limits = c(0, 100), 
    name = "Warming level window\n(%GCM)", breaks = c(25, 50, 75),
    guide = guide_colourbar(barwidth = rel(.5), 
      barheight = rel(3), title.theme = element_text(size = rel(.9)),
      label.theme = element_text(size = rel(.9)),
      order = 1)) + 
  scale_y_continuous(breaks = ybr, sec.axis = sec_axis(~ ., labels = levellabs,
      breaks = seq_along(levellabs) * winsize + max(plottrends$rate_high))) +
  theme(axis.ticks.length.y.right = unit(0, "mm"), 
    axis.line.y.right = element_blank(), 
    axis.text.y.right = element_text(size = rel(.8)))

# Draw trends as lines with ribbon CIs
trendlines <- figlayout + 
  new_scale("fill") + 
  geom_ribbon(aes(x = period, ymin = rate_low, ymax = rate_high, fill = range,
    group = range), col = NA, alpha = .1) + 
  geom_pointpath(aes(x = period, y = rate_est, group = range, col = range), 
    linesize = .5) +
  scale_color_manual(values = rngpal, labels = rnglabs, name = "") + 
  scale_fill_manual(values = rngpal, labels = rnglabs, name = "")

# Export
ggsave("figures/Fig1.pdf", trendlines, height = 7.5, width = 14)


#-------------------------
# Figure 2 country by target
#-------------------------

#----- Select data

# Select countries, all ages and difference full-demo sub-scenario and net
plotcntr <- finalres$country_level[agegroup == "all" & sc == "clim" &
    ssp == 3 & adapt == "0%",]

# Add info about countries
cntr_info <- group_by(cities, CNTR_CODE) |> 
  summarise(cntr_name = cntr_name[1], region = region[1],lat = mean(lat))
plotcntr <- merge(plotcntr, cntr_info, by.x = "country", by.y = "CNTR_CODE")

# Select regional and european level data
plotreg <- finalres$region_level[agegroup == "all" & sc == "clim" &
    ssp == 3 & adapt == "0%",]
ploteu <- finalres$eu_level[agegroup == "all" & sc == "clim" &
    ssp == 3 & gcm == "ens" & adapt == "0%",]

# Multiply rates by the denominator
ratevars <- grep("rate", colnames(plotcntr), value = T)
plotcntr[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]
plotreg[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]
ploteu[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]

# Relevel region to correct order
plotcntr[, region := factor(region, names(regpal))]
plotreg[, region := factor(region, names(regpal))]
ploteu[, region := factor("")]

# Create position variable for forest plot
setorder(plotcntr, -region, lat)
plotcntr[, id := .GRP - as.numeric(region) * 2, by = .(country)]
plotreg <- plotreg[plotcntr[, .(id = min(id) - 1.5), by = region], 
  on = "region"]
ploteu[, id := min(plotreg$id) - 2]

# Labels for levels
poslab <- rbind(unique(plotcntr[, c("id", "cntr_name")]),
  unique(plotreg[, .(id, cntr_name = "Total")]), 
  unique(ploteu[, .(id, cntr_name = "Europe")]),
  use.names = F)

#----- Build forest plot

# Plot layout and theme
figforest <- ggplot(plotcntr) + 
  theme_classic() + 
  theme(panel.grid.major.x = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    axis.ticks.y = element_blank(),
    strip.text = element_markdown(face = "bold", size = 13),
    strip.background = element_rect(colour = NA, fill = NA),
    axis.text.x.bottom = element_text(angle = -45, hjust = 0.5, vjust = 0.1),
    legend.position = "bottom", legend.direction = "horizontal") + 
  facet_grid(rows = vars(region), cols = vars(level), scales = "free_y", 
    space = "free_y", labeller = labeller(level = levellabs)) +
  geom_hline(aes(yintercept = id + .75), data = plotreg, linewidth = .01) + 
  labs(y = "", x = sprintf("Excess death rate (x%s)", 
    formatC(byrate, format = "f", digits = 0, big.mark = ",")), 
    color = "", fill = "")

# Add Heat and Cold bars
figforest <- figforest + 
  geom_col(aes(y = id, x = rate_est, fill = range), orientation = "y",
    data = ~ subset(.x, range != "tot"), width = .8, alpha = .5) + 
  scale_fill_manual(values = rngpal, labels = rnglabs, 
    name = "Temperature range") + 
  scale_y_continuous(breaks = poslab$id, labels = poslab$cntr_name)

# Add Net points
figforest <- figforest + 
  geom_pointrange(aes(y = id, x = rate_est, xmin = rate_low, xmax = rate_high,
    fill = range), size = .3, data = ~ subset(.x, range == "tot"))

# Add region rates
figforest <- figforest + 
  geom_col(aes(y = id, x = rate_est, fill = range), orientation = "y",
    data = subset(plotreg, range != "tot"), width = .9, alpha = .5) +
  geom_pointrange(aes(y = id, x = rate_est, xmin = rate_low, xmax = rate_high),
    data = subset(plotreg, range == "tot"), shape = 18, size = .9) + 
  geom_vline(xintercept = 0)

# Add European level
figforest <- figforest + 
  geom_col(aes(y = id, x = rate_est, fill = range), orientation = "y",
    data = subset(ploteu, range != "tot"), width = .9, alpha = .5) +
  geom_pointrange(aes(y = id, x = rate_est, xmin = rate_low, xmax = rate_high),
    data = subset(ploteu, range == "tot"), shape = 18, size = .9)

# Save
ggsave("figures/Fig2.pdf", figforest, height = 8, width = 15)

#-------------------------
# Figure 3: City maps
#-------------------------

#----- Select data

# All age group and net effect
plotmap <- finalres$city_level[agegroup == "all" & sc == "clim" & 
    ssp == 3 & range == "tot" & adapt == "0%", ]

# Add geographical info
plotmap <- merge(plotmap, cities[, c("URAU_CODE", "lon", "lat", "pop")],
  by.x = "city", by.y = "URAU_CODE")

# Multiply rates by the denominator
ratevars <- grep("rate", colnames(plotmap), value = T)
plotmap[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]

# Cut points for palette
cutpts <- unique(sort(c(0, unname(round(
  quantile(plotmap$rate_est, seq(0, 1, length.out = 20)) / 5) * 5))))
plotmap[, colgrp := cut(rate_est, cutpts)]
signtab <- table(factor(sign(cutpts), c(-1, 0, 1)))

# Palettes (fill and border)
npal <- (max(signtab)) * 2
pal <- c(scico(npal, palette = "bam", direction = -1)[
    max(signtab) - signtab[1] + seq_len(signtab[1])],
  scico(tail(signtab, 1), palette = "acton", direction = -1))
bpal <- rep(c("white", "black"), signtab[c("-1", "1")])
names(pal) <- names(pal) <- levels(plotmap$colgrp)

#----- Build plot

# Theme and layout
mapfig <- ggplot(plotmap) + theme_void() +
  theme(legend.position = "bottom", legend.box = "vertical",
    strip.text.x = element_text(hjust = 0.5, face = "bold", size = 14), 
    title = element_text(hjust = 0, face = "bold", size = 12),
    panel.border = element_rect(colour = 1, fill = NA)) +
  facet_wrap(. ~ level, labeller = labeller(level = levellabs))

# European map layout
mapfig <- mapfig + 
  geom_sf(data = euromap, fill = grey(.9), col = "white", inherit.aes = F) +
  coord_sf(xlim = range(plotmap$lon), ylim = range(plotmap$lat),
    lims_method = "box", crs = st_crs(euromap), default_crs = st_crs(4326))

# Add cities
mapfig <- mapfig +
  geom_point(aes(x = lon, y = lat, fill = colgrp, size = pop),
    shape = 21, stroke = .01, col = "black", alpha = .8) +
  scale_fill_manual(values = pal) +
  scale_size(range = c(1, 10), breaks = c(0.1, 0.5, 3, 7.5) * 10^6,
    labels = ~ number(./10^6)) +
  labs(size = "Population (in millions)",
    colour = sprintf("Excess death rate (x%s)", 
      formatC(byrate, format = "f", digits = 0, big.mark = ",")), 
    fill = sprintf("Excess death rate (x%s)", 
      formatC(byrate, format = "f", digits = 0, big.mark = ","))) +
  guides(size = guide_legend(override.aes = list(col = 1)),
    fill = guide_bins(override.aes = list(size = 5), direction = "horizontal"))
  
# Save plot
ggsave("figures/Fig3.pdf", plot = mapfig, width = 13, height = 10)

#-------------------------
# Figure 4: Adaptation
#-------------------------

#----- Select data

# Which SSPs have several adaption scenarios
sspada <- subset(adaptdf, heat != 0 | cold != 0, ssp, drop = T) |>
  unique()

# Select only net effect
adadata <- finalres$eu_period[agegroup == "all" & sc == "clim" &
    ssp %in% sspada & gcm == "ens" & range == "tot",]
adadata[, adapt := factor(adapt)]

# Multiply rates by the denominator
ratevars <- grep("rate", colnames(adadata), value = T)
adadata[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]

#----- Prepare plot

# Y-axis breaks
yran <- range(adadata[,c("rate_high", "rate_low")])
ybr <- pretty(yran, n = 10)
ybr <- ybr[ybr %between% yran]

# X-axis limits
xlm <- c(min(adadata$period), 2100)

#----- Plot

# Plot layout and theme
figadapt <- ggplot(adadata) +
  theme_classic() + 
  theme(plot.margin = unit(c(1, 5, 1, 1), "line"), 
    panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    strip.text = element_text(face = "bold", size = rel(1.5)),
    axis.title = element_text(face = "bold", size = rel(1.5)),
    axis.text = element_text(size = rel(1.2)),
    strip.background = element_rect(colour = NA, fill = NA)) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ ssp, labeller = labeller(ssp = ssplabs)) + 
  labs(x = "", y = sprintf("Excess death rate (x%s)", 
    formatC(byrate, format = "f", digits = 0, big.mark = ","))) + 
  scale_x_continuous(limits = xlm) +
  coord_cartesian(clip = "off")

# Draw trends as lines with ribbon CIs
figadapt <- figadapt + 
  geom_ribbon(aes(x = period, ymin = rate_low, ymax = rate_high, fill = adapt,
    group = adapt), col = NA, alpha = .1) + 
  geom_pointpath(aes(x = period, y = rate_est, group = adapt, col = adapt), 
    linesize = .5) +
  scale_color_manual(values = adapal, name = "Adaptation scenario") +
  scale_fill_manual(values = adapal, name = "Adaptation scenario")

# Export
ggsave("figures/Fig4.pdf", figadapt, height = 7.5, width = 14)


