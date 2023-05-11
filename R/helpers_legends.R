#' @noRd
#' @importFrom rlang eval_tidy quo_is_null
#' @importFrom ggplot2 theme
#' @importFrom grid unit
wrapper_legends_ <- function(data, plot, col, col_legend_place) {
  if (col_legend_place %in% c("b", "t")) {
    plot <- plot +
      theme(legend.direction = "horizontal")

    if (is.numeric(eval_tidy(col, data))) {
      plot <- plot +
        theme(legend.key.width = unit(0.66, "cm")) +
        theme(legend.text.align = 0.5)
    }

    if (col_legend_place == "b") {
      plot <- plot +
        theme(legend.position = "bottom")
    } else if (col_legend_place == "t") {
      plot <- plot +
        theme(legend.position = "top")
    }
  } else if (col_legend_place == "n" | quo_is_null(col)) {
    plot <- plot +
      theme(legend.position = "none")
  } else if (col_legend_place == "l") {
    plot <- plot +
      theme(legend.position = "left")
  } else if (col_legend_place == "r") {
    plot <- plot +
      theme(legend.position = "right")
  }

  return(plot)
}
