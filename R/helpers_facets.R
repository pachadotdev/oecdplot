#' @noRd
#' @importFrom rlang quo_is_null
#' @importFrom ggplot2 vars as_labeller facet_wrap facet_grid vars
wrapper_facets_ <- function(plot, facet, facet2, facet_scales, facet_labels,
                            facet_ncol, facet_nrow, facet_space) {
  if (!quo_is_null(facet)) {
    if (quo_is_null(facet2)) {
      plot <- plot +
        facet_wrap(
          vars(!!facet),
          scales = facet_scales,
          labeller = as_labeller(facet_labels),
          ncol = facet_ncol,
          nrow = facet_nrow
        )
    } else {
      plot <- plot +
        facet_grid(
          rows = vars(!!facet2),
          cols = vars(!!facet),
          space = facet_space,
          labeller = as_labeller(facet_labels),
          scales = facet_scales
        )
    }
  }

  return(plot)
}
