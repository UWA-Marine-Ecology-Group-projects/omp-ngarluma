location_plot <- function(plot_limits, study_limits, annotation_labels) {
  # 1. Location overview plot - includes parks zones and an aus inset
  require(tidyverse)
  require(tidyterra)
  require(patchwork)
  require(ggpattern)
  
  p1 <- ggplot() +
    geom_spatraster_contour_filled(data = bathy,
                                   breaks = c(0, -30, -70, -200, - 700, -2000 , -4000, -6000),
                                   colour = NA, show.legend = F) +
    # scale_fill_grey(start = 1, end = 0.5, guide = "none") +
    scale_fill_manual(values = c("#FFFFFF", "#EFEFEF", "#DEDEDE", "#CCCCCC", "#B6B6B6", "#9E9E9E", "#808080")) +
    new_scale_fill() +
    geom_spatraster_contour(data = bathy,
                            breaks = c(-30, -70, -200, - 700, -2000 , -4000, -6000), colour = "white",
                            alpha = 3/5, linewidth = 0.1, show.legend = F) +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    terr_fills +
    new_scale_fill() +
    geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA, alpha = 0.4) +
    scale_fill_manual(name = "State Marine Parks", guide = "legend",
                      values = with(marine_parks_state, setNames(colour, zone))) +
    new_scale_fill() +
    geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
    scale_fill_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
    new_scale_fill() +
    geom_sf(data = kunmunya, aes(fill = zone), colour = NA, alpha = 0.4) +
    scale_fill_manual(name = "Closed Waters", guide = "legend",
                      values = with(kunmunya, setNames(colour, zone))) +
    new_scale_fill() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1, linewidth = 0.4, lineend = "round") +
    geom_sf(data = wrecks, aes(colour = wreck), shape = 9) +
    scale_colour_manual(values = c("#073B4C", "#118AB2"), name = "Shipwreck") +
    new_scale_colour() +
    geom_sf_pattern(data = infrastructure, aes(pattern = Infrastructure, pattern_fill = Infrastructure, colour = Infrastructure), alpha = 0.7,
                    pattern_density = 0.8, pattern_size = 0.2, pattern_spacing = 0.005, pattern_colour = "grey80") +
    scale_colour_manual(values = c("#F35B04", "#D90429")) +
    scale_pattern_fill_manual(values = c("#F35B04", "#D90429")) +
    scale_pattern_manual(values = c("stripe", "crosshatch")) +
    # scale_fill_manual(values = c("Shipping channel" = "",
    #                              "Spoil ground" = )) +
    # new_scale_fill() +
    # new_scale("pattern_fill") +
    # new_scale("pattern") +
    # new_scale_colour() +
    # annotate(geom = "point", x = c(117.042948333243, 117.213089999945), # Glenbank, Dive Wreck
    #          y = c(-20.3212583329681, -20.4297716665203), shape = 9) +
    labs(x = NULL, y = NULL) +
    annotate("rect", xmin = study_limits[1], xmax = study_limits[2], ymin = study_limits[3], ymax = study_limits[4],
             fill = NA, colour = "goldenrod2", linewidth = 0.4) +
    annotate("text", x = annotation_labels$x,
             y = annotation_labels$y,
             label = annotation_labels$label, size = 1.65,
             fontface = "italic") +
    coord_sf(xlim = c(plot_limits[1], plot_limits[2]), ylim = c(plot_limits[3], plot_limits[4]), crs = 4326) +
    theme_minimal()
  
  # inset map
  p1.1 <- ggplot(data = aus) +
    geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
    geom_sf(data = aus_marine_parks, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
    coord_sf(xlim = c(110, 125), ylim = c(-37, -13)) + # This is constant for all plots - its just a map of WA
    annotate("rect", xmin = plot_limits[1], xmax = plot_limits[2], ymin = plot_limits[3], ymax = plot_limits[4],   # Change here
             colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.2) +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_rect(colour = "grey70"))
  
  p1.1 + p1
}