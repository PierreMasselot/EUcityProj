################################################################################
# Plots
################################################################################

if(length(ls()) == 0){
  source("01_pkg_params.R")
  source("02_prep_data.R")
  finalres <- readRDS("temp/fullresults_2023-12-01.RDS")
}
  

#-------------------------
# Figure 1: trends
#-------------------------

#----- Select data

# Select all ages and clim only scenario for european wide
plottrends <- finalres$eu_period[agegroup == "all" & sc == "full-demo" &
    ssp %in% ssplist & gcm == "ens",]

# Multiply rates by the denominator
ratevars <- grep("rate", colnames(plottrends), value = T)
plottrends[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]

# Compute the warming level windows frequency
winfreq <- warming_win[gcm %in% gcmlist, .(n = length(gcm)), 
  by = .(level, ssp, year)]
winfreq[, prop := n / length(gcmlist)]

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
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(colour = NA, fill = NA)) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~ ssp, labeller = labeller(ssp = ssplabs)) + 
  labs(x = "", y = sprintf("Excess death rate (x%s)", 
      formatC(byrate, format = "f", digits = 0, big.mark = ","))) + 
  scale_x_continuous(limits = range(plottrends$period)) +
  coord_cartesian(clip = "off")

# Add warming windows
figlayout <- figlayout + 
  geom_tile(aes(x = year, fill = prop * 100,
      y = max(plottrends$rate_high) + as.numeric(factor(level)) * winsize), 
    data = winfreq, inherit.aes = F, height = winsize / 2) + 
  scale_fill_gradient(low = "white", high = "black", limits = c(0, 100), 
    name = "Warming level window\n(%GCM)", breaks = c(25, 50, 75),
    guide = guide_colourbar(barwidth = rel(.5), 
      barheight = rel(3), title.theme = element_text(size = rel(8)),
      label.theme = element_text(size = rel(8)))) + 
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
  geom_line(aes(x = period, y = rate_est, group = range, col = range), 
    linewidth = 1) + 
  scale_color_manual(values = rngpal, labels = rnglabs, name = "") + 
  scale_fill_manual(values = rngpal, labels = rnglabs, name = "")

# Export
ggsave("figures/Fig1_EUlevel.pdf", trendlines, width = 12)

# #-------------------------
# # Figure 2: trends by country and region
# #-------------------------
# 
# #----- Select data
# 
# # Select countries, all ages and difference full-demo sub-scenario and net
# plotcntr <- finalres$country$period[agegroup == "all" & sc == "full-demo" &
#     range == "tot" & ssp %in% ssplist,]
# 
# # Add info about countries
# cntr_info <- group_by(cities, CNTR_CODE) |> 
#   summarise(cntr_name = cntr_name[1], region = region[1],lat = mean(lat))
# plotcntr <- merge(plotcntr, cntr_info)
# 
# # Select regional data
# plotreg <- finalres$region$period[agegroup == "all" & sc == "full-demo" &
#     range == "tot" & ssp %in% ssplist,]
# 
# # Multiply rates by the denominator
# ratevars <- grep("rate", colnames(plotcntr), value = T)
# plotcntr[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]
# plotreg[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]
# 
# #----- Plot parameters
# 
# # Data to display country on the right
# cntrlabs <- plotcntr[period == max(period) & ssp == 3,]
# 
# #----- Build plot
# 
# # Plot layout and theme
# figlayout <- ggplot(plotcntr) + 
#   theme_classic() + 
#   theme(plot.margin = unit(c(1, 5, 1, 1), "line"), legend.position = "bottom",
#     panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
#     panel.border = element_rect(fill = NA), 
#     strip.text = element_text(face = "bold"),
#     axis.title = element_text(face = "bold"),
#     strip.background = element_rect(colour = NA, fill = NA)) + 
#   geom_hline(yintercept = 0) + 
#   facet_wrap(~ ssp, labeller = labeller(ssp = ssplabs)) +
#   labs(x = "", y = sprintf("Excess death rate (x%s)", 
#     formatC(byrate, format = "f", digits = 0, big.mark = ",")), 
#     color = "", fill = "") +
#   coord_cartesian(clip = "off", xlim = range(plotcntr$period))
# 
# # Add country level curves
# figcountry <- figlayout + 
#   geom_line(aes(x = period, y = rate_est, col = region, group = CNTR_CODE), 
#     alpha = .3) + 
#   geom_text(aes(y = rate_est, label = cntr_name, x = max(period) + 5, 
#       col = region), alpha = .8, size = 3, 
#     data = plotcntr[period == max(period) & ssp == max(ssplist),], hjust = -0, 
#     check_overlap = T, show.legend = F, nudge_x = 5)
#   
# # Add region level curves
# figcountry <- figcountry + 
#   geom_line(aes(x = period, y = rate_est, col = region, group = region), 
#     data = plotreg, linewidth = rel(1.5)) + 
#   scale_color_manual(values = regpal)
# 
# # Save
# ggsave("figures/Fig2a_trendcountries.pdf", figcountry, width = 10)

#-------------------------
# Figure 2 country by target
#-------------------------

#----- Select data

# Select countries, all ages and difference full-demo sub-scenario and net
plotcntr <- finalres$country_level[agegroup == "all" & sc == "full-demo" &
    ssp == 3,]

# Add info about countries
cntr_info <- group_by(cities, CNTR_CODE) |> 
  summarise(cntr_name = cntr_name[1], region = region[1],lat = mean(lat))
plotcntr <- merge(plotcntr, cntr_info, by.x = "country", by.y = "CNTR_CODE")

# Select regional data
plotreg <- finalres$region_level[agegroup == "all" & sc == "full-demo" &
    ssp == 3,]

# Multiply rates by the denominator
ratevars <- grep("rate", colnames(plotcntr), value = T)
plotcntr[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]
plotreg[, (ratevars) := lapply(.SD, "*", byrate), .SDcols = ratevars]

# Relevel region to correct order
plotcntr[, region := factor(region, names(regpal))]
plotreg[, region := factor(region, names(regpal))]

# Create position variable for forest plot
setorder(plotcntr, -region, lat)
plotcntr[, id := .GRP - as.numeric(region) * 2, by = .(country)]
plotreg <- plotreg[plotcntr[, .(id = min(id) - 1.5), by = region], 
  on = "region"]
poslab <- unique(plotcntr[, c("id", "cntr_name")])

#----- Build forest plot

# Plot layout and theme
figforest <- ggplot(plotcntr) + 
  theme_classic() + 
  theme(panel.grid.major.x = element_line(colour = grey(.9), linewidth = .01),
    panel.border = element_rect(fill = NA), 
    axis.ticks.y = element_blank(),
    strip.text = element_text(face = "bold", size = 13),
    strip.background = element_rect(colour = NA, fill = NA)) + 
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
  scale_fill_manual(values = rngpal, labels = rnglabs, name = "") + 
  scale_y_continuous(breaks = poslab$id, labels = poslab$cntr_name)

# Add Net points
figforest <- figforest + 
  geom_pointrange(aes(y = id, x = rate_est, xmin = rate_low, xmax = rate_high),
    size = .3, data = ~ subset(.x, range == "tot"))

# Add region rates
figforest <- figforest + 
  geom_col(aes(y = id, x = rate_est, fill = range), orientation = "y",
    data = subset(plotreg, range != "tot"), width = .9, alpha = .5) +
  geom_pointrange(aes(y = id, x = rate_est, xmin = rate_low, xmax = rate_high),
    data = subset(plotreg, range == "tot"), shape = 18, size = .9) + 
  geom_vline(xintercept = 0)

# Save
ggsave("figures/Fig2_countries.pdf", figforest, height = 8, width = 10)


#----- Build points plot
# 
# # Plot layout and theme
# figlayout <- ggplot(plotcntr) + 
#   theme_classic() + 
#   theme(plot.margin = unit(c(1, 5, 1, 1), "line"), legend.position = "bottom",
#     panel.grid.major = element_line(colour = grey(.9), linewidth = .01),
#     panel.border = element_rect(fill = NA),
#     axis.title = element_text(face = "bold"),
#     axis.text.x.bottom = element_blank(), 
#     axis.ticks.x.bottom = element_blank(),
#     strip.text = element_text(face = "bold"),
#     strip.background = element_rect(colour = NA, fill = NA)) + 
#   geom_hline(yintercept = 0) + 
#   labs(x = "", y = sprintf("Excess death rate (x%s)", 
#     formatC(byrate, format = "f", digits = 0, big.mark = ",")), 
#     color = "", fill = "") +
#   facet_wrap(~ level, labeller = labeller(level = levellabs))
# 
# # Add regions
# figcntrlevel <- figlayout + 
#   geom_point(aes(x = 1, y = rate_est, col = region), alpha = .5,
#     data = plotreg, size = 10) +
#   # geom_hline(aes(yintercept = rate_est, col = region), linetype = 2,
#   #   data = plotreg) + 
#   scale_color_manual(values = regpal, 
#     guide = guide_legend(override.aes = list(alpha = 1, size = 5)))
# 
# # Add countries
# figcntrlevel <- figcntrlevel + 
#   # geom_point(aes(x = 1, y = rate_est, group = 1, fill = region), 
#   #   shape = 21, stroke = .01, col = "white", size = 6, 
#   #   position = position_beeswarm(method = "compactswarm", cex = 3)) + 
#   # geom_text(aes(x = 1, y = rate_est, group = 1, label = CNTR_CODE, col = region),
#   #   size = 4, position = position_beeswarm(method = "compactswarm", cex = 3),
#   #   show.legend = F, fontface = "bold")
#   geom_shadowtext(aes(x = 1, y = rate_est, group = 1, label = CNTR_CODE, colour = region), 
#     size = 4, position = position_beeswarm(method = "compactswarm", cex = 3),
#     show.legend = F, bg.r = 0.1)
# 
# # Save
# ggsave("figures/Fig2b_levelcountries.pdf", figcntrlevel, height = 7)

#-------------------------
# Figure 3: City maps
#-------------------------

#----- Select data

# All age group and net effect
plotmap <- finalres$city_level[agegroup == "all" & sc == "full-demo" & 
    ssp == 3 & range == "tot", ]

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
# pal <- scico(npal, palette = "bam", direction = -1)[
#   (max(signtab) - signtab[1] + 1):(max(signtab) + signtab[3])]
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
    # panel.background = element_rect(fill = "#cae9f5"),
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
  # geom_point(aes(x = lon, y = lat, fill = colgrp, size = pop, col = colgrp), 
  #   shape = 21, stroke = .01) +
  scale_color_manual(values = bpal) +
  scale_fill_manual(values = pal) +
  scale_size(range = c(1, 10), breaks = c(0.1, 0.5, 3, 7.5) * 10^6,
    labels = ~ number(./10^6)) +
  labs(size = "Population (in millions)",
    colour = sprintf("Excess death rate (x%s)", 
      formatC(byrate, format = "f", digits = 0, big.mark = ",")), 
    fill = sprintf("Excess death rate (x%s)", 
      formatC(byrate, format = "f", digits = 0, big.mark = ","))) +
  guides(size = guide_legend(override.aes = list(col = 1)),
    # fill = guide_colorsteps(barwidth = 15, barheight = .5),
    colour = guide_bins(override.aes = list(size = 5), axis = F, 
      direction = "horizontal", nrow = 1, label.position = "bottom"),
    fill = guide_bins(override.aes = list(size = 5), axis = F, 
      direction = "horizontal", nrow = 1, label.position = "bottom"))

# # Legend
# dfleg <- data.frame(x = seq_along(pal), col = names(pal), 
#   b = rep(c(-1, 1), signtab[c("-1", "1")]))
# ggplot(dfleg) + theme_minimal() + 
#   geom_tile(aes(x = x, fill = col, y = 1, col = b)) + 
#   scale_fill_manual(values = pal, guide = "none") + 
#   scale_color_gradient(low = "white", high = "black", guide = "none") + 
#   scale_x_continuous(breaks = seq(0, length(pal)) + .5, labels = cutpts)
  
# Save plot
ggsave("figures/Fig3_cityMaps.pdf", plot = mapfig, width = 13, height = 10)

