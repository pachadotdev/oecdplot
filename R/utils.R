#' Load Official OECD Fonts for Charts
#'
#' Loads a modified version of Arial Narrow from P: drive that works with R
#' (and also Python).
#'
#' @importFrom systemfonts register_font
#'
#' @export
load_oecd_fonts <- function() {
  if (isTRUE(file.exists("//OECDMAIN/em_apps/R/Fonts"))) {
    register_font(
      "Arial Narrow",
      plain = "//OECDMAIN/em_apps/R/Fonts/ArialNarrow.ttf"
    )
  } else {
    if (file.exists("~/Downloads/ArialNarrow.ttf")) {
      register_font(
        "Arial Narrow",
        plain = "~/Downloads/ArialNarrow.ttf"
      )
    } else {
      message("Your account can't access OECDMAIN. You can download Arial Narrow from https://algobank.oecd.org:4430/r-oecd-graphs/oecdplot/-/raw/main/fonts/ArialNarrow.zip. Extract the zip in Downloads.")
    }
  }
}

# #' Options for SF
# #' @importFrom sf sf_use_s2
# load_sf_options <- function() {
#   sf_use_s2(FALSE) # because we are working with plain coordinates
# }

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
