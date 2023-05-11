#' @noRd
#' @importFrom rlang quo_is_null
#' @importFrom ggplot2 ggplot aes
wrapper_plots_ <- function(data, x, y, col, group,
                           xmax = NULL, xmin = NULL,
                           ymax = NULL, ymin = NULL,
                           case = "notmaxmin") {
  if (case != "maxmin") {
    # Not maxmin ----
    if (!quo_is_null(x) & !quo_is_null(y)) {
      if (!quo_is_null(col)) {
        plot <- data %>%
          ggplot(mapping = aes(
            x = !!x,
            y = !!y,
            col = !!col,
            fill = !!col,
            group = !!group
          ))
      } else if (quo_is_null(col)) {
        plot <- data %>%
          ggplot(mapping = aes(
            x = !!x,
            y = !!y,
            col = "",
            fill = "",
            group = !!group
          ))
      }
    } else if (!quo_is_null(x) & quo_is_null(y)) {
      if (!quo_is_null(col)) {
        plot <- data %>%
          ggplot(mapping = aes(
            x = !!x,
            col = !!col,
            fill = !!col,
            group = !!group
          ))
      } else if (quo_is_null(col)) {
        plot <- data %>%
          ggplot(mapping = aes(
            x = !!x,
            col = "",
            fill = "",
            group = !!group
          ))
      }
    } else if (quo_is_null(x) & !quo_is_null(y)) {
      if (!quo_is_null(col)) {
        plot <- data %>%
          ggplot(mapping = aes(
            y = !!y,
            col = !!col,
            fill = !!col,
            group = !!group
          ))
      } else if (quo_is_null(col)) {
        plot <- data %>%
          ggplot(mapping = aes(
            y = !!y,
            col = "",
            fill = "",
            group = !!group
          ))
      }
    } else if (quo_is_null(x) & quo_is_null(y)) {
      if (!quo_is_null(col)) {
        plot <- data %>%
          ggplot(mapping = aes(
            col = !!col,
            fill = !!col,
            group = !!group
          ))
      } else if (quo_is_null(col)) {
        plot <- data %>%
          ggplot(mapping = aes(
            col = "",
            fill = "",
            group = !!group
          ))
      }
    }
  }

  if (case == "maxmin") {
    # Maxmin ----
    if (!quo_is_null(x) & !quo_is_null(y)) {
      if (!quo_is_null(col)) {
        plot <- data %>%
          ggplot(mapping = aes(
            x = !!x,
            y = !!y,
            col = !!col,
            fill = !!col,
            group = !!group,
            shape = !!group,
            xmin = !!xmin,
            xmax = !!xmax,
            ymin = !!ymin,
            ymax = !!ymax
          ))
      } else if (quo_is_null(col)) {
        plot <- data %>%
          ggplot(mapping = aes(
            x = !!x,
            y = !!y,
            col = "",
            fill = "",
            group = !!group,
            shape = !!group,
            xmin = !!xmin,
            xmax = !!xmax,
            ymin = !!ymin,
            ymax = !!ymax
          ))
      }
    } else if (!quo_is_null(x) & quo_is_null(y)) {
      if (!quo_is_null(col)) {
        plot <- data %>%
          ggplot(mapping = aes(
            x = !!x,
            col = !!col,
            fill = !!col,
            group = !!group,
            shape = !!group,
            xmin = !!xmin,
            xmax = !!xmax,
            ymin = !!ymin,
            ymax = !!ymax
          ))
      } else if (quo_is_null(col)) {
        plot <- data %>%
          ggplot(mapping = aes(
            x = !!x,
            col = "",
            fill = "",
            group = !!group,
            shape = !!group,
            xmin = !!xmin,
            xmax = !!xmax,
            ymin = !!ymin,
            ymax = !!ymax
          ))
      }
    } else if (quo_is_null(x) & !quo_is_null(y)) {
      if (!quo_is_null(col)) {
        plot <- data %>%
          ggplot(mapping = aes(
            y = !!y,
            col = !!col,
            fill = !!col,
            group = !!group,
            shape = !!group,
            xmin = !!xmin,
            xmax = !!xmax,
            ymin = !!ymin,
            ymax = !!ymax
          ))
      } else if (quo_is_null(col)) {
        plot <- data %>%
          ggplot(mapping = aes(
            y = !!y,
            col = "",
            fill = "",
            group = !!group,
            shape = !!group,
            xmin = !!xmin,
            xmax = !!xmax,
            ymin = !!ymin,
            ymax = !!ymax
          ))
      }
    } else if (quo_is_null(x) & quo_is_null(y)) {
      if (!quo_is_null(col)) {
        plot <- data %>%
          ggplot(mapping = aes(
            col = !!col,
            fill = !!col,
            group = !!group,
            shape = !!group,
            xmin = !!xmin,
            xmax = !!xmax,
            ymin = !!ymin,
            ymax = !!ymax
          ))
      } else if (quo_is_null(col)) {
        plot <- data %>%
          ggplot(mapping = aes(
            col = "",
            fill = "",
            group = !!group,
            shape = !!group,
            xmin = !!xmin,
            xmax = !!xmax,
            ymin = !!ymin,
            ymax = !!ymax
          ))
      }
    }
  }

  return(plot)
}
