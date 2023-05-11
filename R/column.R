#' @title Wrapper to simplify column plots.
#'
#' @description Create a column plot with a wrapper around the ggplot2::geom_col function.
#'
#' @param data A data frame or tibble.
#' @param x Unquoted x aesthetic variable.
#' @param xmin Unquoted xmin aesthetic variable.
#' @param xmax Unquoted xmin aesthetic variable.
#' @param y Unquoted y aesthetic variable.
#' @param ymin Unquoted ymin aesthetic variable. Defaults to "ymin" and it's
#'  computed from "y" if it's not present.
#' @param ymax Unquoted ymin aesthetic variable. Defaults to "ymin" and it's
#'  computed from "y" if it's not present.
#' @param colour Unquoted colour/fill aesthetic variable.
#' @param palette Palette to use. A character vector of hex codes (or names). The default is to use "darkblue". See the documentation for `oecd_clrs()` to see all the options.
#' @param size Unquoted size aesthetic variable.
#' @param stacked Logical value to stack the columns.
#' @param facet Unquoted facet aesthetic variable.
#' @param facet2 Unquoted second facet variable for a facet grid of facet by facet2 variables.
#' @param group Unquoted group aesthetic variable.
#' @param text Unquoted text aesthetic variable, which can be used in combination with plotly::ggplotly(., tooltip = "text").
#' @param palette_na Colour to use for NA values. A character vector of a hex code (or name).
#' @param alpha Opacity. A number between 0 and 1 (the default is 1).
#' @param title Title string.
#' @param subtitle Subtitle string.
#' @param coord Coordinate system.
#' @param x_breaks A function that takes the limits as input (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param x_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param x_include For a numeric or date variable, any values that the scale should include (e.g. 0).
#' @param x_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param x_limits A vector of length 2 to determine the limits of the axis.
#' @param x_sec_axis A secondary axis specified by the ggplot2::sec_axis or ggplot2::dup_axis function.
#' @param x_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param x_trans For a numeric variable, a transformation object (e.g. "log10").
#' @param y_breaks A function that takes the limits as input (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param y_expand Padding to the limits with the ggplot2::expansion function, or a vector of length 2 (e.g. c(0, 0)).
#' @param y_include For a numeric or date variable, any values that the scale should include (e.g. 0).
#' @param y_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels.
#' @param y_limits A vector of length 2 to determine the limits of the axis.
#' @param y_sec_axis A secondary axis specified by the ggplot2::sec_axis or ggplot2::dup_axis function.
#' @param y_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param y_trans For a numeric variable, a transformation object (e.g. "log10").
#' @param col_breaks A function that takes the limits as input (e.g. scales::breaks_pretty()), or a vector of breaks.
#' @param col_include For a numeric or date variable, any values that the scale should include (e.g. 0).
#' @param col_intervals A function to cut or chop the numeric variable into intervals (e.g. ~ santoku::chop_mean_sd(.x, drop = FALSE)).
#' @param col_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a vector of labels. Note this does not affect where col_intervals is not NULL.
#' @param col_limits A vector to determine the limits of the axis.
#' @param col_legend_ncol The number of columns for the legend elements.
#' @param col_legend_nrow The number of rows for the legend elements.
#' @param col_legend_place The place for the legend. "b" for bottom, "r" for right, "t" for top (default), or "l" for left.
#' @param col_title Axis title string. Defaults to converting to sentence case with spaces. Use "" for no title.
#' @param col_continuous Type of colouring for a continuous variable. Either "gradient" or "steps". Defaults to "steps".
#' @param facet_labels A function that takes the breaks as inputs (e.g. scales::label_comma()), or a named vector of labels (e.g. c(value = "label", ...)).
#' @param facet_ncol The number of columns of facetted plots.
#' @param facet_nrow The number of rows of facetted plots.
#' @param facet_scales Whether facet scales should be "fixed" across facets, "free" in both directions, or free in just one direction (i.e. "free_x" or "free_y"). Defaults to "fixed".
#' @param facet_space Whether facet space should be "fixed" across facets, "free" to be proportional in both directions, or free to be proportional in just one direction (i.e. "free_x" or "free_y"). Only applies Where facet2 is provided and facet_scales are not fixed. Defaults to "fixed".
#' @param caption Caption title string.
#' @param theme A ggplot2 theme. The default is to use `theme_oecd()`.
#' @param trend Add a trend line (line plot only). Defaults to `FALSE`.
#' @param band Add a confidence band (line plot only). Defaults to `FALSE`.
#' @param ... Other arguments passed to the relevant ggplot2::geom_* function.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' # Dodge
#' oecd_col(pta, x = country, y = pct_pta, colour = category, stacked = FALSE)
#'
#' # Dodge with different colours
#' oecd_col(pta,
#'   x = country, y = pct_pta, colour = category, stacked = FALSE,
#'   palette = "darkgreen"
#' )
#'
#' # Stacked
#' oecd_col(pta, x = country, y = pct_pta, colour = category, stacked = TRUE)
#'
#' # Dodge with flipped coords (i.e., Excel's Bar Chart in OECD Chart)
#' oecd_col(pta, y = country, x = pct_pta, colour = category, stacked = FALSE)
#'
#' # Stacked with flipped coords (i.e., Excel's Bar Chart in OECD Chart)
#' oecd_col(pta, y = country, x = pct_pta, colour = category, stacked = TRUE)
#'
#' # Facetting by colour variable
#' oecd_col(pta,
#'   x = country, y = pct_pta, colour = category, group = category,
#'   facet = category
#' )
#'
#' # Facetting by colouring variable and custom ordering
#' oecd_col(pta,
#'   x = country, y = pct_pta, colour = category, group = category,
#'   facet = category, facet_ncol = 1
#' )
#' }
#'
#' @importFrom rlang enquo quo_is_null
#' @importFrom dplyr ungroup
#' @importFrom ggplot2 aes layer_data labs geom_col
#'
#' @export
oecd_col <- function(data = NULL, x = NULL, xmin = NULL, xmax = NULL,
                     y = NULL, ymin = NULL, ymax = NULL,
                     colour = NULL, size = NULL, stacked = FALSE, facet = NULL,
                     facet2 = NULL, group = NULL, text = NULL, palette = "darkblue",
                     palette_na = "#7F7F7F", alpha = 1, title = NULL,
                     subtitle = NULL, coord = NULL, x_breaks = NULL,
                     x_expand = NULL, x_include = NULL, x_labels = NULL,
                     x_limits = NULL, x_sec_axis = ggplot2::waiver(),
                     x_title = NULL, x_trans = "identity", y_breaks = NULL,
                     y_expand = NULL, y_include = NULL, y_labels = NULL,
                     y_limits = NULL, y_sec_axis = ggplot2::waiver(),
                     y_title = NULL, y_trans = "identity", col_breaks = NULL,
                     col_include = NULL, col_intervals = NULL,
                     col_labels = NULL, col_legend_place = "t",
                     col_legend_ncol = NULL, col_legend_nrow = NULL,
                     col_limits = NULL, col_title = NULL,
                     col_continuous = "gradient", facet_labels = NULL,
                     facet_ncol = NULL, facet_nrow = NULL,
                     facet_scales = "fixed", facet_space = "fixed",
                     caption = NULL, theme = oecdplot::theme_oecd(),
                     trend = FALSE, band = FALSE, ...) {
  # Quote ----

  x <- enquo(x)
  y <- enquo(y)
  colour <- enquo(colour)
  facet <- enquo(facet)
  facet2 <- enquo(facet2)
  group <- enquo(group)
  text <- enquo(text)

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
    data, x, y, col = colour, col_title, pal = palette, pal_na = palette_na,
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

  plot <- wrapper_plots_(data, x, y, col = colour, group)

  plot <- plot +
    geom_col(
      aes(text = !!text),
      position = ifelse(isTRUE(stacked), "stack", "dodge"),
      alpha = alpha,
      width = 0.7,
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
