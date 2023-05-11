#' @keywords internal
"_PACKAGE"

#' Protected Terrestrial Areas
#'
#' Regional disparities in protected terrestrial areas (2017) expressed as
#' protected terrestrial areas as a percentage of the total area for large
#' regions. See the Stat link https://doi.org/10.1787/888934190039.
#'
#' \itemize{
#'   \item{\code{country}}{ ISO-3 code of for the countries}
#'   \item{\code{region}}{ Geographical region for the observations}
#'   \item{\code{category}}{ Category for the observations (minimum, country
#'   value or maximum)}
#'   \item{\code{pct_pta}}{ Protected terrestrial areas a percentage of the
#'   total area (large regions)}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name pta
#' @usage pta
#' @source OECD Stats
#' @examples
#' pta
#' @format A data frame with 99 rows and 4 variables
NULL

#' OECD Colours
#'
#' A data frame with 72 observations on the following 7 variables.
#' The variables are as follows:
#'
#' \itemize{
#'   \item{\code{section}}{ section from the Add In (i.e. 'Full Colours')}
#'   \item{\code{palette}}{ the name of the palette (i.e. 'Dark Blue', 'Orange', etc.)}
#'   \item{\code{order}}{ the order of the colour (i.e. first, second, etc.)}
#'   \item{\code{red}}{ the amount of red of the colour (i.e. 131 in a 0-255 scale)}
#'   \item{\code{green}}{ the amount of green of the colour (i.e. 210 in a 0-255 scale)}
#'   \item{\code{blue}}{ the amount of blue of the colour (i.e. 227 in a 0-255 scale)}
#'   \item{\code{hex}}{ the hex colour for the colour (i.e. \code{#83D2E3})}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name oecd_colours
#' @usage oecd_colours
#' @source OECD Chart Excel Add In
#' @format A data frame with 72 rows and 7 variables
NULL
