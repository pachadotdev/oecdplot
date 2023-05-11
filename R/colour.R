#' @export
#' @inheritParams scale_fill_oecd_d
#' @rdname scale_fill_oecd_d
scale_colour_oecd_d <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                                option = "darkblue", aesthetics = "colour") {
  discrete_scale(aesthetics, "oecd_standard", oecd_pal(alpha, begin, end, direction, option), ...)
}

#' @export
#' @inheritParams scale_fill_oecd_d
#' @importFrom scales gradient_n_pal
#' @importFrom ggplot2 continuous_scale
#' @rdname scale_fill_oecd_d
scale_colour_oecd_c <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                                option = "darkblue", values = NULL,
                                na.value = "grey50", guide = "colourbar",
                                aesthetics = "colour") {
  continuous_scale(aesthetics, "oecd_standard",
    gradient_n_pal(
      oecd_pal(alpha, begin, end, direction, option)(10),
      values
    ),
    na.value = na.value,
    guide = guide, ...
  )
}
