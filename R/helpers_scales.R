#' @noRd
#' @importFrom dplyr distinct pull mutate across
#' @importFrom ggplot2 scale_colour_manual scale_fill_manual
#'  guide_colourbar scale_colour_gradientn scale_fill_gradientn
#'  scale_colour_stepsn scale_fill_stepsn guide_coloursteps waiver guide_legend
#' @importFrom scales label_comma breaks_pretty
#' @importFrom rlang is_null quo_is_null as_name eval_tidy
#' @importFrom purrr map_chr
wrapper_scale_ <- function(data, x, y, col, col_title, pal, pal_na,
                           facet, facet2, col_labels, col_limits,
                           col_include, col_breaks, col_legend_place,
                           col_legend_nrow, col_legend_ncol, col_intervals,
                           col_continuous) {
  if (quo_is_null(col)) {
    pal <- oecd_clrs(
      option = pal,
      n = data %>% distinct(!!col) %>% nrow()
    )

    col_scale <- list(
      scale_colour_manual(
        values = pal,
        na.value = pal_na,
      ),
      scale_fill_manual(
        values = pal,
        na.value = pal_na,
      )
    )

    col_legend_place <- "n"
  } else {
    if (is_null(col_title)) {
      col_title <- as_name(col)
    }
    col_title_position <- ifelse(col_title == "", "right", "top")

    if (is_null(col_legend_place)) {
      if (!quo_is_null(x) &
        (identical(eval_tidy(col, data), eval_tidy(x, data)))) {
        col_legend_place <- "n"
      } else if (!quo_is_null(y) &
        (identical(eval_tidy(col, data), eval_tidy(y, data)))) {
        col_legend_place <- "n"
      } else if (!quo_is_null(facet) &
        (identical(eval_tidy(col, data), eval_tidy(facet, data)))) {
        col_legend_place <- "n"
      } else if (!quo_is_null(facet2) &
        (identical(eval_tidy(col, data), eval_tidy(facet2, data)))) {
        col_legend_place <- "n"
      } else {
        col_legend_place <- "b"
      }
    }

    if (is.numeric(eval_tidy(col, data))) {
      if (is_null(col_intervals)) {
        # continuous col
        col_min <- data %>%
          pull(!!col) %>%
          min(na.rm = TRUE)
        col_max <- data %>%
          pull(!!col) %>%
          max(na.rm = TRUE)

        if (!is_null(col_limits)) {
          if (is.na(col_limits)[1]) {
            col_limits[1] <- col_min
          }
          if (is.na(col_limits)[2]) {
            col_limits[2] <- col_max
          }
        }

        if (is_null(col_limits)) {
          col_limits <- c(col_min, col_max)
        }
        if (!is_null(col_include)) {
          col_limits <- range(c(col_include, col_limits))
        }

        if (is_null(col_breaks)) {
          col_breaks <- breaks_pretty(n = 4)
        }

        pal <- oecd_clrs(
          option = pal,
          n = data %>% distinct(!!col) %>% nrow()
        )
        if (is_null(col_labels)) {
          col_labels <- label_comma()
        }

        if (col_continuous == "gradient") {
          col_scale <- list(
            scale_colour_gradientn(
              colors = pal,
              labels = col_labels,
              breaks = col_breaks,
              limits = col_limits,
              na.value = pal_na,
              guide = guide_colourbar(
                title.position = col_title_position,
                ticks.colour = "#F1F3F5"
              )
            ),
            scale_fill_gradientn(
              colors = pal,
              labels = col_labels,
              breaks = col_breaks,
              limits = col_limits,
              na.value = pal_na,
              guide = guide_colourbar(
                title.position = col_title_position,
                ticks.colour = "#F1F3F5"
              )
            )
          )
        } else if (col_continuous == "steps") {
          col_scale <- list(
            scale_colour_stepsn(
              colors = pal,
              labels = col_labels,
              breaks = col_breaks,
              limits = col_limits,
              na.value = pal_na,
              guide = guide_coloursteps(title.position = col_title_position)
            ),
            scale_fill_stepsn(
              colors = pal,
              labels = col_labels,
              breaks = col_breaks,
              limits = col_limits,
              na.value = pal_na,
              guide = guide_coloursteps(title.position = col_title_position)
            )
          )
        }
      } else {
        # intervals col ----
        data <- data %>%
          mutate(across(!!col, col_intervals))

        col_levels <- levels(eval_tidy(col, data))
        col_n <- length(col_levels)

        pal <- oecd_clrs(
          option = pal,
          n = data %>% distinct(!!col) %>% nrow()
        )

        if (is.numeric(eval_tidy(y, data)) |
          is.Date(eval_tidy(y, data))) {
          if (col_legend_place %in% c("b", "t")) {
            col_legend_rev <- FALSE
          } else {
            col_legend_rev <- TRUE
          }
        } else if (is.character(eval_tidy(y, data)) |
          is.factor(eval_tidy(y, data))) {
          if (col_legend_place %in% c("b", "t")) {
            col_legend_rev <- TRUE
          } else {
            col_legend_rev <- FALSE
          }
          pal <- rev(pal)
        } else {
          col_legend_rev <- FALSE
        }

        if (is_null(col_breaks)) {
          col_breaks <- waiver()
        }
        if (is_null(col_labels)) {
          col_labels <- waiver()
        }

        col_scale <- list(
          scale_colour_manual(
            values = pal,
            breaks = col_levels,
            limits = col_levels,
            labels = col_labels,
            na.value = pal_na,
            guide = guide_legend(
              reverse = col_legend_rev,
              title.position = col_title_position,
              ncol = col_legend_ncol,
              nrow = col_legend_nrow,
              byrow = TRUE
            )
          ),
          scale_fill_manual(
            values = pal,
            breaks = col_levels,
            limits = col_levels,
            labels = col_labels,
            na.value = pal_na,
            guide = guide_legend(
              reverse = col_legend_rev,
              title.position = col_title_position,
              ncol = col_legend_ncol,
              nrow = col_legend_nrow,
              byrow = TRUE
            )
          )
        )
      }
    } else {
      # categorical col ----
      if (!is_null(col_limits)) {
        col_n <- length(col_limits)
      } else if (!is_null(col_breaks)) {
        col_n <- length(col_breaks)
      } else {
        if (is.factor(eval_tidy(col, data))) {
          col_n <- length(levels(eval_tidy(col, data)))
        } else {
          col_n <- length(unique(eval_tidy(col, data)))
        }
      }

      pal <- oecd_clrs(
        option = pal,
        n = data %>% distinct(!!col) %>% nrow()
      )

      if (is.numeric(eval_tidy(y, data)) |
        is.Date(eval_tidy(y, data))) {
        if (is.character(eval_tidy(col, data)) |
          is.factor(eval_tidy(col, data))) {
          col_legend_rev <- FALSE
        } else if (col_legend_place %in% c("b", "t")) {
          col_legend_rev <- FALSE
        } else {
          col_legend_rev <- TRUE
        }
      } else if (is.character(eval_tidy(y, data)) |
        is.factor(eval_tidy(y, data))) {
        if (is.character(eval_tidy(col, data)) |
          is.factor(eval_tidy(col, data))) {
          col_legend_rev <- TRUE
        } else if (col_legend_place %in% c("b", "t")) {
          col_legend_rev <- TRUE
        } else {
          col_legend_rev <- FALSE
        }
        pal <- rev(pal)
      } else {
        col_legend_rev <- FALSE
      }

      if (is_null(col_breaks)) {
        col_breaks <- waiver()
      }
      if (is_null(col_labels)) {
        col_labels <- waiver()
      }

      col_scale <- list(
        scale_colour_manual(
          values = pal,
          breaks = col_breaks,
          limits = col_limits,
          labels = col_labels,
          na.value = pal_na,
          guide = guide_legend(
            reverse = col_legend_rev,
            title.position = col_title_position,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE
          )
        ),
        scale_fill_manual(
          values = pal,
          breaks = col_breaks,
          limits = col_limits,
          labels = col_labels,
          na.value = pal_na,
          guide = guide_legend(
            reverse = col_legend_rev,
            title.position = col_title_position,
            ncol = col_legend_ncol,
            nrow = col_legend_nrow,
            byrow = TRUE
          )
        )
      )
    }
  }

  if (!exists("col_legend_rev")) {
    col_legend_rev <- NULL
  }

  return(
    list(
      col_legend_place = col_legend_place,
      col_legend_rev = col_legend_rev,
      col_breaks = col_breaks,
      col_labels = col_labels,
      col_scale = col_scale
    )
  )
}

#' @noRd
#' @importFrom rlang eval_tidy quo_is_null is_null
#' @importFrom dplyr select matches everything pull
#' @importFrom tidyr pivot_longer
#' @importFrom stringr regex
#' @importFrom lubridate is.Date
#' @importFrom ggplot2 waiver scale_x_discrete scale_x_continuous scale_x_date
#' @importFrom scales breaks_pretty breaks_log label_comma label_date_short
#'  oob_keep
#' @importFrom purrr map
wrapper_xscale <- function(plot,
           x,
           data,
           ldata,
           x_expand,
           x_labels,
           facet_scales,
           x_limits,
           x_include,
           x_breaks,
           facet,
           x_trans,
           xy_numeric_date,
           x_sec_axis) {
    if (is.character(eval_tidy(x, data)) |
      is.factor(eval_tidy(x, data))) {
      if (is_null(x_expand)) {
        x_expand <- waiver()
      }
      if (is_null(x_labels)) {
        x_labels <- waiver()
      }

      x_scale <-
        scale_x_discrete(expand = x_expand, labels = x_labels)
    } else {
      if (facet_scales %in% c("fixed", "free_y")) {
        x_vctr <- ldata %>%
          select(matches(regex(
            "^x$|^xmin$|^xmax$|^xend$|^xmax_final$"
          ))) %>%
          pivot_longer(cols = everything()) %>%
          pull(.data$value)

        if (is.Date(eval_tidy(x, data))) {
          x_vctr <- as.Date(x_vctr, origin = "1970-01-01")
        }

        x_min <- x_vctr %>% min(na.rm = TRUE)
        x_max <- x_vctr %>% max(na.rm = TRUE)

        if (is_null(x_limits)) {
          x_limits <- range(c(x_min, x_max))
          if (!is_null(x_include)) {
            x_limits <- range(c(x_limits, x_include))
          }

          if (is_null(x_breaks)) {
            x_breaks_n <- ifelse(quo_is_null(facet), 5, 3)
            if (x_trans != c("identity")) {
              x_breaks <-
                breaks_log(n = x_breaks_n, base = 10)(x_limits)
            } else {
              x_breaks <- breaks_pretty(n = x_breaks_n)(x_limits)
            }

            if (xy_numeric_date) {
              x_limits <- NULL
            } else {
              if (x_trans != "identity") {
                x_limits <- NULL
              } else {
                x_limits <- c(min(x_breaks), max(x_breaks))
              }
            }
          } else if (!is_null(x_breaks)) {
            if (xy_numeric_date) {
              x_limits <- NULL
            } else {
              if (is.vector(x_breaks)) {
                if (x_trans != "identity") {
                  x_limits <- NULL
                } else {
                  x_limits <- c(min(x_breaks), max(x_breaks))
                }
              } else {
                if (x_trans != "identity") {
                  x_limits <- NULL
                } else {
                  x_limits <- list(x_limits) %>%
                    map(.f = x_breaks) %>%
                    unlist() %>%
                    range()
                }
              }
            }
          }
        } else if (!is_null(x_limits)) {
          if (is.na(x_limits)[1]) {
            x_limits[1] <- x_min
          }
          if (is.na(x_limits)[2]) {
            x_limits[2] <- x_max
          }
          if (!is_null(x_include)) {
            x_limits <- range(c(x_limits, x_include))
          }

          if (is_null(x_breaks)) {
            x_breaks_n <- ifelse(quo_is_null(facet), 5, 4)
            if (x_trans != "identity") {
              x_breaks <-
                breaks_log(n = x_breaks_n, base = 10)(x_limits)
            } else {
              x_breaks <- breaks_pretty(n = x_breaks_n)(x_limits)
            }
          }
        }
      } else if (facet_scales %in% c("free", "free_x")) {
        if (is_null(x_breaks)) {
          x_breaks <- waiver()
        }
      }

      if (is_null(x_expand)) {
        if (facet_scales %in% c("fixed", "free_y")) {
          if (xy_numeric_date) {
            x_expand <- c(0.05, 0.05)
          } else {
            x_expand <- c(0, 0)
          }
        } else {
          x_expand <- c(0.05, 0.05)
        }
      }

      if (is_null(x_labels)) {
        if (is.numeric(eval_tidy(x, data)) |
          quo_is_null(x)) {
          x_labels <- label_comma()
        } else if (is.Date(eval_tidy(x, data))) {
          x_labels <- label_date_short()
        } else {
          x_labels <- waiver()
        }
      }

      if (is.numeric(eval_tidy(x, data)) | quo_is_null(x)) {
        x_scale <- scale_x_continuous(
          breaks = x_breaks,
          limits = x_limits,
          expand = x_expand,
          labels = x_labels,
          oob = oob_keep,
          sec.axis = x_sec_axis,
          trans = x_trans
        )
      } else if (is.Date(eval_tidy(x, data))) {
        x_scale <- scale_x_date(
          breaks = x_breaks,
          limits = x_limits,
          expand = x_expand,
          labels = x_labels,
          oob = oob_keep,
          sec.axis = x_sec_axis
        )
      }
    }

    return(x_scale)
  }

#' @noRd
#' @importFrom rlang eval_tidy quo_is_null is_null
#' @importFrom dplyr select matches everything pull
#' @importFrom tidyr pivot_longer
#' @importFrom stringr regex
#' @importFrom lubridate is.Date
#' @importFrom ggplot2 waiver scale_y_discrete scale_y_continuous scale_y_date expansion
#' @importFrom scales breaks_pretty breaks_log label_comma label_date_short
#'  oob_keep
#' @importFrom purrr map
wrapper_yscale <- function(plot, y, data, ldata, y_expand, y_labels,
                           facet_scales, y_limits, y_include, y_breaks, facet,
                           y_trans, xy_numeric_date, y_sec_axis) {
  if (is.character(eval_tidy(y, data)) |
    is.factor(eval_tidy(y, data))) {
    if (is_null(y_expand)) {
      y_expand <- waiver()
    }
    if (is_null(y_labels)) {
      y_labels <- waiver()
    }

    y_scale <- scale_y_discrete(expand = y_expand, labels = y_labels)
  } else {
    if (facet_scales %in% c("fixed", "free_x")) {
      y_vctr <- ldata %>%
        select(matches(regex(
          "^y$|^ymin$|^ymax$|^yend$|^ymax_final$"
        ))) %>%
        pivot_longer(cols = everything()) %>%
        pull(.data$value)

      if (is.Date(eval_tidy(y, data))) {
        y_vctr <- as.Date(y_vctr, origin = "1970-01-01")
      }

      y_min <- y_vctr %>% min(na.rm = TRUE)
      y_max <- y_vctr %>% max(na.rm = TRUE)

      if (is_null(y_limits)) {
        y_limits <- range(c(y_min, y_max))
        if (!is_null(y_include)) {
          y_limits <- range(c(y_limits, y_include))
        }

        if (is_null(y_breaks)) {
          y_breaks_n <- ifelse(quo_is_null(facet), 5, 3)
          if (y_trans != c("identity")) {
            y_breaks <- breaks_log(n = y_breaks_n, base = 10)(y_limits)
          } else {
            y_breaks <- breaks_pretty(n = y_breaks_n)(y_limits)
          }

          if (y_trans != "identity") {
            y_limits <- NULL
          } else {
            y_limits <- c(min(y_breaks), max(y_breaks))
          }
        } else if (!is_null(y_breaks)) {
          if (is.vector(y_breaks)) {
            if (y_trans != "identity") {
              y_limits <- NULL
            } else {
              y_limits <- c(min(y_breaks), max(y_breaks))
            }
          } else {
            if (y_trans != "identity") {
              y_limits <- NULL
            } else {
              y_limits <- list(y_limits) %>%
                map(.f = y_breaks) %>%
                unlist() %>%
                range()
            }
          }
        }
      } else if (!is_null(y_limits)) {
        if (is.na(y_limits)[1]) {
          y_limits[1] <- y_min
        }
        if (is.na(y_limits)[2]) {
          y_limits[2] <- y_max
        }
        if (!is_null(y_include)) {
          y_limits <- range(c(y_limits, y_include))
        }

        if (is_null(y_breaks)) {
          y_breaks_n <- ifelse(quo_is_null(facet), 5, 4)
          if (y_trans != "identity") {
            y_breaks <- breaks_log(n = y_breaks_n, base = 10)(y_limits)
          } else {
            y_breaks <- breaks_pretty(n = y_breaks_n)(y_limits)
          }
        }
      }
    } else if (facet_scales %in% c("free", "free_y")) {
      if (is_null(y_breaks)) {
        y_breaks <- waiver()
      }
    }

    if (is_null(y_expand)) {
      if (facet_scales %in% c("fixed", "free_x")) {
        y_expand <- c(0, 0)
      } else if (!is_null(y_include)) {
        if (min(y_include) == 0 |
          max(y_include) == 0) {
          y_expand <- expansion(mult = c(0, 0.05))
        }
      } else {
        y_expand <- c(0.05, 0.05)
      }
    }

    if (is_null(y_labels)) {
      if (is.numeric(eval_tidy(y, data)) |
        quo_is_null(y)) {
        y_labels <- label_comma()
      } else if (is.Date(eval_tidy(y, data))) {
        y_labels <- label_date_short()
      } else {
        y_labels <- waiver()
      }
    }

    if (is.numeric(eval_tidy(y, data)) | quo_is_null(y)) {
      y_scale <- scale_y_continuous(
        breaks = y_breaks,
        limits = y_limits,
        expand = y_expand,
        labels = y_labels,
        oob = oob_keep,
        sec.axis = y_sec_axis,
        trans = y_trans
      )
    } else if (is.Date(eval_tidy(y, data))) {
      y_scale <- scale_y_date(
        breaks = y_breaks,
        limits = y_limits,
        expand = y_expand,
        labels = y_labels,
        oob = oob_keep,
        sec.axis = y_sec_axis,
      )
    }
  }
}
