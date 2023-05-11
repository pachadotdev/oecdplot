#' @noRd
#' @importFrom rlang is_null
#' @importFrom ggplot2 expand_limits
wrapper_limits_ <- function(plot, x_include, y_include) {
  if (!is_null(x_include)) {
    plot <- plot +
      expand_limits(x = x_include)
  }
  if (!is_null(y_include)) {
    plot <- plot +
      expand_limits(y = y_include)
  }
  return(plot)
}
