#' Save 'ggplot' Objects According to PAC Rules
#'
#' Uses predefined sizes in centimeters (cm) to export graphic objects. Besides
#' saving EMF and PNG files, optionally creates a spreadsheet with Statlinks.
#'
#' @param x X-axis coordinate for the watermark (defaults to 20)
#' @param y Y-axis coordinate for the watermark (defaults to 20)
#' @param text The text for the watermark (defaults to "Draft")
#' @importFrom stringr str_c str_to_title
#' @importFrom ggplot2 geom_text .data .pt
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' p <- ggplot(pta) +
#'   geom_col(
#'     aes(x = country, y = pct_pta, fill = category),
#'     position = "dodge2"
#'   )
#'
#' p + oecd_watermark()
#' }
#'
#' @export
oecd_watermark <- function(x = 20, y = 20, text = "Draft") {
  d <- data.frame(
    "x" = x,
    "y" = y,
    "label" = text
  )

  g <- geom_text(
    data = d,
    aes(x = .data$x, y = .data$y, label = .data$label),
    alpha = 0.7,
    hjust = 0.5,
    vjust = 0.5,
    angle = 45,
    size = 100 / .pt,
    color = "lightgray",
    inherit.aes = FALSE
  )

  return(g)
}
