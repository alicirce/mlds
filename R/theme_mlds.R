#' A minimal theme for plotting
#' @noRd
theme_mlds <- function() {
  theme_classic() %+replace%
    theme(
      strip.background = element_rect(fill = "white", color = NA),
      axis.text = element_text(color = "black"),
      plot.caption = element_text(color = "grey50", hjust = 1, size = 6),
      plot.subtitle = element_text(size = 7.3, color = "grey30", hjust = 0),
      axis.line.y = element_blank(),
      panel.background = element_rect(color = NA)
    )
}
