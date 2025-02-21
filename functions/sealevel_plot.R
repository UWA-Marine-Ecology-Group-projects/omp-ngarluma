sealevel_plot <- function(plot_limits, annotation_labels) {
  ggplot() +
    # geom_spatraster(data = clamp(bathy, upper = -50, values = F)) +
    # scale_fill_gradient2(low = "royalblue4", mid = "lightskyblue1", high = "white", name = "Depth (m)",
    #                      na.value = "#f9ddb1") +
    # new_scale_fill() +
    geom_spatraster_contour_filled(data = bathy,
                                   breaks = c(0, -40, -70, -125)) +
    depth_fills +
    new_scale_fill() +
    geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, alpha = 0.8, linewidth = 1) +
    scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                        values = with(marine_parks_amp, setNames(colour, zone))) +
    new_scale_fill() +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey62", size = 0.2) +
    geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA, show.legend = F) +
    terr_fills +
    new_scale_fill() +
    annotate("text", x = annotation_labels$x,
             y = annotation_labels$y,
             label = annotation_labels$label, size = 1.65,
             fontface = "italic") +
    coord_sf(xlim = c(plot_limits[1], plot_limits[2]), ylim = c(plot_limits[3], plot_limits[4]), crs = 4326) +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal()
}