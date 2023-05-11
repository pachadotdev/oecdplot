#' @title Wrapper to simplify max-min plots.
#'
#' @description Create a line plot with a wrapper around the geom_linerange function.
#'
#' @inheritParams oecd_col
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' # Regular
#' oecd_maxmin(pta, x = country, y = pct_pta, colour = category, group = category)
#'
#' # Facetting by colour variable
#' oecd_maxmin(pta,
#'   x = country, y = pct_pta, colour = category, group = category,
#'   facet = category
#' )
#'
#' # Facetting by colouring variable and custom ordering
#' oecd_maxmin(pta,
#'   x = country, y = pct_pta, colour = category, group = category,
#'   facet = category, facet_ncol = 1
#' )
#' }
#'
#' @importFrom rlang quo_is_null enquo quo
#' @importFrom dplyr ungroup group_by
#' @importFrom ggplot2 ggplot aes geom_linerange geom_point
#'
#' @export
oecd_maxmin <- function(data = NULL,
                        x = NULL,
                        xmin = NULL,
                        xmax = NULL,
                        y = NULL,
                        ymin = ymin,
                        ymax = ymax,
                        colour = NULL,
                        facet = NULL,
                        facet2 = NULL,
                        group = NULL,
                        text = NULL,
                        palette = "darkblue",
                        palette_na = "#7F7F7F",
                        alpha = 1,
                        title = NULL,
                        subtitle = NULL,
                        coord = NULL,
                        x_breaks = NULL,
                        x_expand = NULL,
                        x_include = NULL,
                        x_labels = NULL,
                        x_limits = NULL,
                        x_sec_axis = ggplot2::waiver(),
                        x_title = NULL,
                        x_trans = "identity",
                        y_breaks = NULL,
                        y_expand = NULL,
                        y_include = NULL,
                        y_labels = NULL,
                        y_limits = NULL,
                        y_sec_axis = ggplot2::waiver(),
                        y_title = NULL,
                        y_trans = "identity",
                        col_breaks = NULL,
                        col_include = NULL,
                        col_intervals = NULL,
                        col_labels = NULL,
                        col_legend_place = "t",
                        col_legend_ncol = NULL,
                        col_legend_nrow = NULL,
                        col_limits = NULL,
                        col_title = NULL,
                        col_continuous = "gradient",
                        facet_labels = NULL,
                        facet_ncol = NULL,
                        facet_nrow = NULL,
                        facet_scales = "fixed",
                        facet_space = "fixed",
                        caption = NULL,
                        theme = oecdplot::theme_oecd(),
                        ...) {
  # Quote ----
  x <- enquo(x)
  y <- enquo(y)
  colour <- enquo(colour)
  facet <- enquo(facet)
  facet2 <- enquo(facet2)
  group <- enquo(group)
  text <- enquo(text)

  ymin <- enquo(ymin)
  ymax <- enquo(ymax)

  xmin <- enquo(xmin)
  xmax <- enquo(xmax)

  # Create internal variables ----

  if (!all(colnames(data) %in% c("ymin", "ymax"))) {
    data <- data %>%
      group_by(!!x) %>%
      mutate(
        ymin = min(!!y, na.rm = T),
        ymax = max(!!y, na.rm = T)
      )
  }

  # Stop, warn or message ----

  wrapper_message_(data, title, x_title, y_title, facet)

  # Ungroup ----

  data <- ungroup(data)

  # Get default NULL values ----

  res <- wrapper_nulls_(data, x, x_title, y, y_title, coord)

  x_title <- res$x_title
  y_title <- res$y_title
  xy_numeric_date <- res$xy_numeric_date
  coord <- res$coord

  # Process plot data ----

  data <- wrapper_data_(data, x, y, col = colour, facet, facet2)

  # Col scale ----

  res <- wrapper_scale_(
    data, x, y, col = colour, col_title, pal = palette, palette_na,
    facet, facet2, col_labels, col_limits, col_include,
    col_breaks, col_legend_place, col_legend_nrow,
    col_legend_ncol, col_intervals, col_continuous
  )

  col_legend_rev <- res$col_legend_rev
  col_breaks <- res$col_breaks
  col_labels <- res$col_labels
  col_scale <- res$col_scale
  col_legend_place <- res$col_legend_place

  # Make plot ----

  plot <- wrapper_plots_(data, x, y, col = colour, group, xmax, xmin, ymax, ymin,
                         case = "maxmin")

  plot <- plot +
    geom_linerange(size = 1, color = "gray") +
    geom_point(
      aes(text = !!text),
      size = 2,
      alpha = alpha,
      ...
    )

  # Add facetting ----

  plot <- wrapper_facets_(
    plot, facet, facet2, facet_scales, facet_labels,
    facet_ncol, facet_nrow, facet_space
  )

  # Expand limits ----

  plot <- wrapper_limits_(plot, x_include, y_include)

  # Get layer plot ----

  ldata <- layer_data(plot)

  # Make x scale based on layer_data ----

  x_scale <- wrapper_xscale(
    plot, x, data, ldata, x_expand, x_labels,
    facet_scales, x_limits, x_include, x_breaks,
    facet, x_trans, xy_numeric_date, x_sec_axis
  )

  plot <- plot +
    x_scale

  # Make y scale based on layer_data ----

  y_scale <- wrapper_yscale(
    plot, y, data, ldata, y_expand, y_labels,
    facet_scales, y_limits, y_include, y_breaks,
    facet, y_trans, xy_numeric_date, y_sec_axis
  )

  plot <- plot +
    y_scale

  # Apply scale ----

  plot <- plot +
    col_scale +
    coord +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_title,
      y = y_title,
      col = col_title,
      fill = col_title,
      caption = caption
    ) +
    theme

  # Adjust legend ----

  plot <- wrapper_legends_(data, plot, col = colour, col_legend_place)

  # Return PAC-compatible plot ----

  return(plot)
}
