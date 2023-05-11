test_that("oecd_col class = ggplot", {
  # Dodge
  p <- oecd_col(pta, x = country, y = pct_pta, colour = category, stacked = FALSE)
  expect_s3_class(p, c("gg","ggplot"))

  # Dodge with different colours
  p <- oecd_col(pta,
    x = country, y = pct_pta, colour = category, stacked = FALSE,
    palette = "darkgreen"
  )
  expect_s3_class(p, c("gg","ggplot"))

  # Stacked
  p <- oecd_col(pta, x = country, y = pct_pta, colour = category, stacked = TRUE)
  expect_s3_class(p, c("gg","ggplot"))

  # Dodge with flipped coords (i.e., Excel's Bar Chart in OECD Chart)
  p <- oecd_col(pta, y = country, x = pct_pta, colour = category, stacked = FALSE)
  expect_s3_class(p, c("gg","ggplot"))

  # Stacked with flipped coords (i.e., Excel's Bar Chart in OECD Chart)
  p <- oecd_col(pta, y = country, x = pct_pta, colour = category, stacked = TRUE)
  expect_s3_class(p, c("gg","ggplot"))

  # Facetting by colour variable
  p <- oecd_col(pta,
    x = country, y = pct_pta, colour = category, group = category,
    facet = category
  )
  expect_s3_class(p, c("gg","ggplot"))

  # Facetting by colouring variable and custom ordering
  p <- oecd_col(pta,
    x = country, y = pct_pta, colour = category, group = category,
    facet = category, facet_ncol = 1
  )
  expect_s3_class(p, c("gg","ggplot"))
})

test_that("oecd_point class = ggplot", {
  # Regular
  p <- oecd_point(pta, x = country, y = pct_pta, colour = category, group = category)
  expect_s3_class(p, c("gg","ggplot"))

  # Facetting by colour variable
  p <- oecd_point(pta,
    x = country, y = pct_pta, colour = category, group = category,
    facet = category
  )
  expect_s3_class(p, c("gg","ggplot"))

  # Facetting by colouring variable and custom ordering
  p <- oecd_point(pta,
    x = country, y = pct_pta, colour = category, group = category,
    facet = category, facet_ncol = 1
  )
  expect_s3_class(p, c("gg","ggplot"))

  # Options available only for point plot
  p <- oecd_point(pta,
    x = country, y = pct_pta, colour = category, group = category,
    facet = category, facet_ncol = 1, size = log(pct_pta)
  )
  expect_s3_class(p, c("gg","ggplot"))
})

test_that("oecd_line class = ggplot", {
  # Regular
  p <- oecd_line(pta, x = country, y = pct_pta, colour = category, group = category)
  expect_s3_class(p, c("gg","ggplot"))

  # Facetting by colour variable
  p <- oecd_line(pta,
    x = country, y = pct_pta, colour = category, group = category,
    facet = category
  )
  expect_s3_class(p, c("gg","ggplot"))

  # Facetting by colouring variable and custom ordering
  p <- oecd_line(pta,
    x = country, y = pct_pta, colour = category, group = category,
    facet = category, facet_ncol = 1
  )
  expect_s3_class(p, c("gg","ggplot"))
})

test_that("oecd_maxmin class = ggplot", {
  # Regular
  p <- oecd_maxmin(pta, x = country, y = pct_pta, colour = category, group = category)
  expect_s3_class(p, c("gg","ggplot"))

  # Facetting by colour variable
  p <- oecd_maxmin(pta,
    x = country, y = pct_pta, colour = category, group = category,
    facet = category
  )
  expect_s3_class(p, c("gg","ggplot"))

  # Facetting by colouring variable and custom ordering
  p <- oecd_maxmin(pta,
    x = country, y = pct_pta, colour = category, group = category,
    facet = category, facet_ncol = 1
  )
  expect_s3_class(p, c("gg","ggplot"))
})

test_that("oecd_pie class = ggplot", {
  pta_chl <- data.frame(
    country = "CHL",
    category = c("Protected", "Non-Protected"),
    pct_pta = c(20.5, 79.5)
  )

  p <- oecd_pie(pta_chl, x = country, y = pct_pta, colour = category, group = category)

  expect_s3_class(p, c("gg","ggplot"))
})
