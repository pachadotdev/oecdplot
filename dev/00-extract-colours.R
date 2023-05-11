library(xml2)
library(dplyr)
library(purrr)

parse_palette <- function(node) {
  rgb <- node %>%
    xml_find_all("PaletteColor/Fill/ForeColor") %>%
    xml_attrs() %>%
    bind_rows()

  rgbn <- node %>%
    xml_find_all("PaletteColor") %>%
    xml_attrs() %>%
    unlist()
  rgb$colour <- rgbn

  rgb
}

rgb2hex  <- function(r,g,b) grDevices::rgb(r, g, b, maxColorValue = 255)

showcol <- function (n = NULL, type = "all", exact.n = TRUE) {
  gaplist <- ""

  colorlist <- colorlist <- list(
    darkblue = c(oecdplot::oecd_colours$hex[oecdplot::oecd_colours$palette == "Dark Blue"], "#ffffff"),
    orange = c(oecdplot::oecd_colours$hex[oecdplot::oecd_colours$palette == "Orange"], "#ffffff"),
    darkgreen = c(oecdplot::oecd_colours$hex[oecdplot::oecd_colours$palette == "Dark Green"], "#ffffff"),
    blue = c(oecdplot::oecd_colours$hex[oecdplot::oecd_colours$palette == "Blue"], "#ffffff"),
    green = c(oecdplot::oecd_colours$hex[oecdplot::oecd_colours$palette == "Green"], "#ffffff"),
    pink = c(oecdplot::oecd_colours$hex[oecdplot::oecd_colours$palette == "Pink"], "#ffffff"),
    purple = c(oecdplot::oecd_colours$hex[oecdplot::oecd_colours$palette == "Purple"], "#ffffff"),
    red = c(oecdplot::oecd_colours$hex[oecdplot::oecd_colours$palette == "Red"], "#ffffff"),
    ppt = c(oecdplot::oecd_colours$hex[oecdplot::oecd_colours$palette == "OECD Corporate Colors"], "#ffffff")
  )

  maxnum <- 8

  palattr <- switch(type, qual = "qualitative", div = "divergent",
                    seq = "sequential", all = "qualitative+divergent+sequential")
  if (is.null(n))
    n <- maxnum

  if (length(n) == 1)
    n <- rep(n, length(colorlist))

  n[n < 3] <- 3
  n[n > maxnum] <- maxnum[n > maxnum]
  nr <- length(colorlist)
  nc <- max(n)
  ylim <- c(0, nr)
  oldpar <- par(mgp = c(2, 0.25, 0))
  on.exit(par(oldpar))
  plot(1, 1, xlim = c(0, nc), ylim = ylim, type = "n", axes = FALSE,
       bty = "n", xlab = "", ylab = "")
  for (i in 1:nr) {
    nj <- n[i]
    if (colorlist[i] == "")
      next
    shadi <- colorlist[[i]]
    shadi <- shadi[shadi != "#ffffff"]
    rect(xleft = 0:(nj - 1), ybottom = i - 1, xright = 1:nj,
         ytop = i - 0.2, col = shadi, border = "light grey")
    text(x = 1:nj - 0.1, y = i - 0.6, labels = shadi, xpd = TRUE, adj = 1)
  }
  text(rep(-0.1, nr), (1:nr) - 0.6, labels = names(colorlist), xpd = TRUE,
       adj = 1)
}

# url <- "https://algobank.oecd.org:4430/Bo.WERTH/oecdplotr/-/raw/master/inst/Templates_Definition/OECD%20Standard.xml"
# xml <- "dev/oecd_standard.xml"
# if (!file.exists(xml)) try(download.file(url, xml))

path_templates <- r"(\\main.oecd.org\winapps\Office2016\OECDGraph\Templates Definition)"

# # Standard ----
#
# clrs_std <- read_xml(fs::path(path_templates, "OECD Standard.xml"))
#
# pal_std <- clrs_std %>%
#   xml_find_all("//Palettes/Palette")
#
# names(pal_std) <- pal_std %>%
#   xml_attr("Name")
#
# pal_std <- pal_std %>%
#   map_dfr(parse_palette, .id = "palette") %>%
#   rename(red = R, green = G, blue = B) %>%
#   rowwise() %>%
#   mutate(hex = rgb2hex(red, green, blue)) %>%
#   ungroup() %>%
#   mutate(section = "OECD Standard") %>%
#   group_by(palette) %>%
#   mutate(order = row_number()) %>%
#   ungroup() %>%
#   select(section, palette, order, red, green, blue, hex)

# Full ----

clrs_full <- read_xml(fs::path(path_templates, "Full-colour.xml"))

pal_full <- clrs_full %>%
  xml_find_all("//Palettes/Palette")

names(pal_full) <- pal_full %>%
  xml_attr("Name")

pal_full <- pal_full %>%
  map_dfr(parse_palette, .id = "palette") %>%
  rename(red = R, green = G, blue = B) %>%
  rowwise() %>%
  mutate(hex = rgb2hex(red, green, blue)) %>%
  ungroup() %>%
  mutate(section = "Full colour") %>%
  group_by(palette) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  select(section, palette, order, red, green, blue, hex)

# PPT ----

clrs_ppt <- read_xml(fs::path(path_templates, "ppt.xml"))

pal_ppt <- clrs_ppt %>%
  xml_find_all("//Palettes/Palette")

names(pal_ppt) <- pal_ppt %>%
  xml_attr("Name")

pal_ppt <- pal_ppt %>%
  map_dfr(parse_palette, .id = "palette") %>%
  rename(red = R, green = G, blue = B) %>%
  rowwise() %>%
  mutate(hex = rgb2hex(red, green, blue)) %>%
  ungroup() %>%
  mutate(section = "PPT") %>%
  group_by(palette) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  select(section, palette, order, red, green, blue, hex)

pal_ppt <- pal_ppt %>%
  filter(palette == "OECD Corporate Colors")

# Combine ----

oecd_colours <- pal_full %>%
  bind_rows(pal_ppt)

use_data(oecd_colours, overwrite = T)

# Image for documentation ----

grDevices::png("man/figures/oecd-scales.png", width = 700, height = 700)
showcol(8)
dev.off()
