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
    rect(xleft = 0:(nj - 1), ybottom = i - 1, xright = 1:nj,
         ytop = i - 0.2, col = shadi, border = "light grey")
  }
  text(rep(-0.1, nr), (1:nr) - 0.6, labels = names(colorlist), xpd = TRUE,
       adj = 1)
}

grDevices::png("man/figures/oecd-scales.png")
showcol()
dev.off()
