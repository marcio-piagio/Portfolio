library(ggplot2)
theme_black <- function(base_size = 12, base_family = "") {
  require(grid)
  theme(
    # Elements in the first block are not used directly, but are inherited by others
    line = element_line(colour = "grey25", size = 0.5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "grey25", size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = "grey25", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text = element_text(size = rel(0.8), colour = "grey50"),
    strip.text = element_text(size = rel(0.8)),
    
    axis.line = element_blank(),
    axis.text.x = element_text(size = base_size * 0.8, lineheight = 0.9, face = "bold", colour = "grey40", vjust = 1),
    axis.text.y = element_text(size = base_size * 0.8, lineheight = 0.9, face = "bold", colour = "grey40", hjust = 1),
    axis.ticks = element_line(colour = "grey25"),
    axis.title.x = element_text(size = base_size, face = "bold", color="black"),
    axis.title.y = element_text(size = base_size, face = "bold", angle = 90, color="black"),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.margin = unit(0.10, "cm"),
    
    # legend.background = element_rect(colour = "steelblue4", size = .8),
    legend.margin = unit(0.2, "cm"),
    # legend.key = element_rect(fill = alpha(colour = "azure", alpha = 0.20), colour = "steelblue4"),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = base_size * 0.8, face = "bold", color="black"),
    legend.text.align = NULL,
    legend.title = element_text(size = base_size * 0.8, face = "bold", hjust = 0, color="black"),
    legend.title.align = NULL,
    legend.position = "bottom",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    
    panel.background = element_rect(fill = alpha(colour = "azure", alpha = 0.2), color="black"),
    panel.border = element_blank(),
    panel.grid.major = element_line(colour = "grey25", linetype = "dashed", size = 0.25),
    panel.grid.minor = element_line(colour = "grey25", linetype = "dotted", size = 0.25),
    panel.margin = unit(0.25, "lines"),
    
    strip.background = element_rect(fill = "grey25", colour = NA),
    strip.text.x = element_text(size = base_size * 0.75),
    strip.text.y = element_text(colour = NA, size = base_size * 0.8, angle = 90),
    
    plot.background = element_rect(colour = NA, fill = "white"),
    plot.title = element_text(size = base_size*1.5, color="black", face = "bold"),
    plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"), complete = TRUE)
}
