#' LIU theme
#'
#' A ggplot2 theme that can be used in ggplot2 plots
#'
#'
#'
#' @import ggplot2
#'
#' @examples
#' data(iris)
#'
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#' geom_point(size = 2) +
#' theme_liu() +
#' labs(title = "Iris-mätningar", subtitle = "Exempel med LiU-tema")

theme_liu <- function(...) {
  theme(
    plot.background  = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    plot.title    = element_text(color = "white", hjust = 0.5, size = 20),
    plot.subtitle = element_text(color = "white", hjust = 0.5, size = 10),
    axis.title.x  = element_text(color = "#00BFFF"),
    axis.title.y  = element_text(color = "#00BFFF"),
    axis.text     = element_text(color = "white"),
    legend.background = element_rect(fill = "black", color = NA),
    legend.key    = element_rect(fill = "black", color = NA),
    legend.title  = element_text(color = "#00BFFF"),
    legend.text   = element_text(color = "white"),
    plot.caption  = element_text(color = "white"),
    panel.grid.major = element_line(color = "#00BFFF", linewidth = 0.2),
    panel.grid.minor = element_line(color = "#00BFFF", linewidth = 0.1)
  )
}

