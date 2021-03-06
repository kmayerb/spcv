#' theme_basic
#'
#' a ggplot2 theme with minimal contextual ink, for plotting spatial data with geom_points()
#' or geom_tile(). Ticks, axes labels, and legend title are omitted.
#'
#'
#' @return a ggplot2 theme object to produce a clean plot style in ggplot2 for maps
#' @export
#' @import ggplot2
theme_basic <- function() {
  theme_minimal() +
    theme(
      text = element_text(color = "gray25", family = "Helvetica", size=12),
      plot.title = element_text(size = 14, hjust =.5, face = "bold"),
      plot.subtitle = element_text(size = 12, hjust =.5, face = "bold"),
      plot.caption = element_text(color = "gray30"),
      plot.background = element_rect(fill = "gray95"),
      plot.margin = unit(c(5, 10, 5, 10), units = "mm"),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.title = element_blank(),
      panel.grid = element_blank(),
      legend.position	= "bottom",
      legend.direction = "horizontal",
      legend.justification = "center",
      legend.box.spacing = unit(0, "cm")
    )
}
