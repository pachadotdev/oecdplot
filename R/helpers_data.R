#' @noRd
#' @importFrom rlang abort inform
wrapper_message_ <- function(data, title, x_title, y_title, facet) {
  if (is_null(data)) {
    abort("Data is required")
  }
  if (is_null(title) | is_null(x_title) | is_null(y_title)) {
    inform(
      c("i" = "Use title and/or *_title arguments to specify titles.")
    )
  }
  if (!quo_is_null(facet)) {
    inform(c("i" = "Faceting was treated as an aesthetic."))
  }
}

#' @noRd
#' @importFrom rlang is_null as_name quo_is_null eval_tidy
#' @importFrom purrr map_chr
#' @importFrom ggplot2 coord_cartesian
#' @importFrom lubridate is.Date
wrapper_nulls_ <- function(data, x, x_title, y, y_title, coord) {
  if (is_null(x_title)) {
    x_title <- as_name(x)
  }

  if (is_null(y_title)) {
    y_title <- as_name(y)
  }

  x_numeric_date <- is.numeric(eval_tidy(x, data)) |
    quo_is_null(x) |
    is.Date(eval_tidy(x, data))

  y_numeric_date <- is.numeric(eval_tidy(y, data)) |
    quo_is_null(y) |
    is.Date(eval_tidy(y, data))

  xy_numeric_date <-
    ifelse(x_numeric_date & y_numeric_date, TRUE, FALSE)

  if (is_null(coord)) {
    coord <- coord_cartesian(clip = "off")
  }

  return(
    list(
      x_title = x_title,
      y_title = y_title,
      xy_numeric_date = xy_numeric_date,
      coord = coord
    )
  )
}

#' @noRd
#' @importFrom rlang quo_is_null eval_tidy
#' @importFrom dplyr mutate across
#' @importFrom forcats fct_rev
wrapper_data_ <- function(data, x, y, col, facet, facet2) {
  ## Factorise logical, reverse for horizontal, and chop intervals ----
  if (!quo_is_null(x)) {
    if (is.logical(eval_tidy(x, data))) {
      data <- data %>%
        mutate(across(!!x, ~ factor(.x, levels = c(
          "FALSE", "TRUE"
        ))))
    }
  }

  if (!quo_is_null(y)) {
    if (is.logical(eval_tidy(y, data))) {
      data <- data %>%
        mutate(across(!!y, ~ factor(.x, levels = c(
          "FALSE", "TRUE"
        ))))
    }

    if (is.character(eval_tidy(y, data)) |
      is.factor(eval_tidy(y, data))) {
      if (!quo_is_null(col) &
        (identical(eval_tidy(y, data), eval_tidy(col, data)))) {

      } else {
        data <- data %>%
          mutate(across(!!y, ~ fct_rev(.x)))
      }
    }
  }

  if (!quo_is_null(col)) {
    if (is.logical(eval_tidy(col, data))) {
      data <- data %>%
        mutate(across(!!col, ~ factor(.x, levels = c(
          "FALSE", "TRUE"
        ))))
    }

    if (is.character(eval_tidy(col, data)) |
      is.factor(eval_tidy(col, data))) {
      if (is.character(eval_tidy(y, data)) |
        is.factor(eval_tidy(y, data))) {
        data <- data %>%
          mutate(across(!!col, ~ fct_rev(.x)))
      }
    }
  }

  if (!quo_is_null(facet)) {
    if (is.logical(class(eval_tidy(facet, data)))) {
      data <- data %>%
        mutate(across(!!facet, ~ factor(.x, levels = c(
          "FALSE", "TRUE"
        ))))
    }
  }

  if (!quo_is_null(facet2)) {
    if (is.logical(class(eval_tidy(facet2, data)))) {
      data <- data %>%
        mutate(across(!!facet2, ~ factor(.x, levels = c(
          "FALSE", "TRUE"
        ))))
    }
  }

  return(data)
}
