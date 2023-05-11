#' 'ggplot2' Theme Compatible with PAC Rules
#'
#' Complete theme which controls all non-data display. You can use \code{theme()}
#' in a chained expression to tweak the display for this current theme.
#'
#' @param base_size Base font size, given in pts.
#' @param base_family Base font family
#' @param base_line_size Base size forline elements
#' @param base_rect_size Base size for rect elements
#' @param base_background Base background color (white or gray/grey)
#' @param base_x_axis_angle Base angle of the x-axis text (default 45)
#' @param base_col_width Base angle of the column width (default 0.7)
#' @importFrom ggplot2 theme theme_bw element_rect element_line element_text
#'  element_blank margin unit ggsave '%+replace%' scale_y_continuous
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' load_oecd_fonts()
#'
#' p <- ggplot(pta) +
#'   geom_col(
#'     aes(x = country, y = pct_pta, fill = category),
#'     position = "dodge2"
#'   )
#'
#' # both fill and fill direction are optional
#' p + theme_oecd()
#' }
#' @export
theme_oecd <- function(base_size = 7.5,
                       base_family = "Arial Narrow",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22,
                       base_background = "white",
                       base_x_axis_angle = 45,
                       base_col_width = 0.7) {
  base_background <- switch(
    EXPR = base_background,
    white = "white",
    gray = "gray",
    grey = "gray",
    {
      warning(paste0("Option '", !!sym("option"), "' does not exist. Defaulting to 'white'."))
      "white"
    }
  )

  g <- theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
      axis.ticks = element_line(colour = "black", size = 0.3),
      axis.ticks.length = unit(-2.5, units = "pt"),
      axis.text.x = element_text(
        size = base_size,
        color = "black",
        angle = base_x_axis_angle,
        vjust = 1,
        hjust = ifelse(base_x_axis_angle == 0, 0.5, 1),
        margin = margin(7, 0, 0, 0, unit = "pt")
      ),
      axis.text.y = element_text(
        size = base_size, color = "black", hjust = 1,
        margin = margin(0, 7, 5.5, 5.5, unit = "pt")
      ),
      axis.title.x = element_text(
        size = base_size,
        margin = margin(0, 0, 0, 2, unit = "pt"), hjust = 0.5
      ),
      axis.title.y = element_text(
        size = base_size, angle = 90,
        margin = margin(0, 0, 0, 2, unit = "pt"), hjust = 0.5
      ),
      legend.text = element_text(size = base_size),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.background = element_rect(fill = c("#f2f0f2"), colour = NA),
      legend.margin = margin(t = 1, r = 1, b = 1, l = 1),
      # legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "#f2f0f2", colour = NA),
      legend.key.height = unit(0.6, "lines"),
      panel.border = element_blank(),
      panel.spacing = unit(1, "lines"),
      plot.title = element_text(size = base_size),
      plot.subtitle = element_text(size = base_size, hjust = 0),
      plot.caption = element_blank(),
      strip.text.x = element_text(size = base_size),
      strip.text = element_text(
        size = base_size, face = "bold",
        margin = margin(b = 3.0)
      ),
      strip.background = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      complete = T
    )

  if (base_background == "white") {
    g <- g %+replace%
      theme(panel.grid = element_line(colour = "grey92"))
  }

  if (base_background == "gray") {
    g <- g %+replace%
      theme(
        panel.background = element_rect(fill = "#eaeaea", colour = NA),
        panel.grid = element_line(colour = "white")
      )
  }

  return(g)
}
