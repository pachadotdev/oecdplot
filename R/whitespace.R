#' Replaces all spaces with newlines for plotting
#'
#' Uses regex to convert whitespaces (i.e. \code{\\s+}, not \code{" "})
#' into new lines (i.e. \code{\\n}). It applies to all factor/character
#' columns in a table.
#'
#' @param x a tibble or data.frame to be used with \code{ggplot2}
#' @export
fct_whitespace_to_newline <- function(x) {
  x <- gsub("\\s+", "\n", as.character(x))
  factor(x) # if it receives a factor, then it should return one
}
