#' OECD Colour Scales
#'
#' The `oecd` scales provide official colour maps for the different OECD's
#' teams. They are based on the official XML especifications for the Excel
#' add-in.
#'
#' @importFrom ggplot2 discrete_scale
#' @inheritParams oecd_pal
#' @inheritParams ggplot2::discrete_scale
#' @param ... Other arguments passed on to [ggplot2::discrete_scale()],
#' [ggplot2::continuous_scale()], or [ggplot2::binned_scale()] to control name, limits, breaks,
#'   labels and so forth.
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
#' @param values if colours should not be evenly positioned along the gradient this
#' vector gives the position (between 0 and 1) for each colour in the colours vector.
#' @family colour scales
#' @export
#' @rdname scale_fill_oecd_d
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
#' p + scale_fill_oecd_d(option = "darkgreen", direction = -1)
#' }
scale_fill_oecd_d <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                              option = "darkblue", aesthetics = "fill") {
  discrete_scale(aesthetics, "oecd_standard", oecd_pal(alpha, begin, end, direction, option), ...)
}

#' @export
#' @inheritParams scale_fill_oecd_d
#' @importFrom scales gradient_n_pal
#' @importFrom ggplot2 continuous_scale
#' @rdname scale_fill_oecd_d
scale_fill_oecd_c <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                              option = "darkblue", values = NULL,
                              na.value = "grey50", guide = "colourbar",
                              aesthetics = "fill") {
  continuous_scale(aesthetics, "oecd_standard",
    gradient_n_pal(
      oecd_pal(alpha, begin, end, direction, option)(10),
      values
    ),
    na.value = na.value,
    guide = guide, ...
  )
}
