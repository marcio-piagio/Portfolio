library(ggplot2)
theme_blue <- function(base_size = 12, base_family = "") {
  require(grid)
  theme(
    # Elements in the first block are not used directly, but are inherited by others
    line = element_line(colour = "steelblue", size = 0.5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "steelblue", size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = "steelblue", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text = element_text(size = rel(0.8), colour = "grey50"),
    strip.text = element_text(size = rel(0.8)),
    
    axis.line = element_blank(),
    axis.text.x = element_text(size = base_size * 0.8, lineheight = 0.9, face = "bold", colour = "steelblue3", vjust = 1),
    axis.text.y = element_text(size = base_size * 0.8, lineheight = 0.9, face = "bold", colour = "steelblue3", hjust = 1),
    axis.ticks = element_line(colour = "steelblue"),
    axis.title.x = element_text(size = base_size, face = "bold", color="steelblue4"),
    axis.title.y = element_text(size = base_size, face = "bold", angle = 90, color="steelblue4"),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.margin = unit(0.10, "cm"),
    legend.position="none",
    # legend.background = element_rect(colour = "white"),
    # legend.margin = unit(0.2, "cm"),
    # legend.key = element_rect(fill = alpha(colour = "azure", alpha = 0.20), colour = "steelblue4"),
    # legend.key.size = unit(1.2, "lines"),
    # legend.key.height = NULL,
    # legend.key.width = NULL,
    # legend.text = element_text(size = base_size * 0.8, face = "bold", color="steelblue4"),
    # legend.text.align = NULL,
    # legend.title = element_text(size = base_size * 0.8, face = "bold", hjust = 0, color="steelblue4"),
    # legend.title.align = NULL,
    # legend.position = "bottom",
    # legend.direction = NULL,
    # legend.justification = "center",
    # legend.box = NULL,
    
    panel.background = element_rect(fill = alpha(colour = "azure", alpha = 0.2), color="steelblue4"),
    panel.border = element_blank(),
    panel.grid.major = element_line(colour = "steelblue", linetype = "dashed", size = 0.25),
    panel.grid.minor = element_line(colour = "steelblue", linetype = "dotted", size = 0.25),
    panel.margin = unit(0.25, "lines"),
    
    strip.background = element_rect(fill = "steelblue", colour = NA),
    strip.text.x = element_text(size = base_size * 0.75),
    strip.text.y = element_text(colour = NA, size = base_size * 0.8, angle = 90),
    
    plot.background = element_rect(colour = NA, fill = "white"),
    plot.title = element_text(size = base_size * 1.35, color="steelblue4", face = "bold"),
    plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"), complete = TRUE)
}
