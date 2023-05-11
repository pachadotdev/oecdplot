#' @title Wrapper to simplify pie plots.
#'
#' @description Create a pie plot with a wrapper around the ggplot2::geom_col function.
#'
#' @inheritParams oecd_col
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' # THIS TYPE OF PLOT IS NOT RECOMMENDED AS IT PRESENTS IMPORTANT
#' # PERCEPTUAL ISSUES
#'
#' pta_chl <- data.frame(
#'   country = "CHL",
#'   category = c("Protected", "Non-Protected"),
#'   pct_pta = c(20.5, 79.5)
#' )
#'
#' oecd_pie(pta_chl, x = country, y = pct_pta, colour = category, group = category)
#' }
#'
#' @importFrom rlang quo_is_null enquo
#' @importFrom dplyr ungroup desc
#' @importFrom ggplot2 ggplot aes geom_bar coord_polar position_stack
#'
#' @export
oecd_pie <- function(data = NULL,
                     x = NULL,
                     xmin = NULL,
                     xmax = NULL,
                     y = NULL,
                     ymin = NULL,
                     ymax = NULL,
                     colour = NULL,
                     stacked = FALSE,
                     size = NULL,
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
                     trend = FALSE,
                     band = FALSE,
                     ...) {
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

  if (is_null(coord)) coord <- coord_polar("y", start = 0)

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
    geom_bar(
      aes(text = !!text),
      stat = "identity", width = 1,
      alpha = alpha,
      ...
    )

  print(data)

  plot <- plot +
    geom_text(
      data = data,
      aes(label = !!y),
      position = position_stack(vjust = 0.5),
      color = "black"
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
    theme +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )


  # Adjust legend ----

  plot <- wrapper_legends_(data, plot, col = colour, col_legend_place)

  # Return PAC-compatible plot ----

  return(plot)
}
