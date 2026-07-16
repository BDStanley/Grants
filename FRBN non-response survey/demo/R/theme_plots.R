theme_plots <- function(base_size = 11, base_family = "Jost") {
  theme_bw(base_size, base_family) +
    theme(
      panel.background = element_rect(fill = "#ffffff", colour = NA),
      title = element_text(size = rel(1), family = "Jost", face = "bold"),
      plot.subtitle = element_text(
        size = rel(0.8),
        family = "Jost",
        face = "plain"
      ),
      plot.caption = element_text(
        margin = margin(t = 10),
        size = rel(0.6),
        family = "Jost",
        face = "plain"
      ),
      panel.border = element_rect(
        color = "grey50",
        fill = NA,
        linewidth = 0.15
      ),
      panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_text(
        size = rel(0.8),
        family = "Jost",
        face = "plain"
      ),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      legend.position = "bottom",
      legend.title = element_text(
        size = rel(0.8),
        vjust = 0.5,
        family = "Jost",
        face = "bold"
      ),
      legend.key.size = unit(0.7, "line"),
      legend.key = element_blank(),
      legend.spacing = unit(0.1, "lines"),
      legend.justification = "left",
      legend.margin = margin(t = -5, b = 0, l = 0, r = 0),
      strip.text = element_text(
        size = rel(0.9),
        hjust = 0,
        family = "Jost",
        face = "plain"
      ),
      strip.background = element_rect(fill = "white", colour = NA),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
}
