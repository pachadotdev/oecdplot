test_that("emf works", {
  load_oecd_fonts()

  p <- oecd_col(pta, x = country, y = pct_pta, colour = category, stacked = FALSE)

  dout <- tempdir()

  expect_message(save_oecd_chart("testemf", p, format = "emf", folder = dout),
                 regexp = "No Statlinks generated")
})

test_that("png works", {
  load_oecd_fonts()

  p <- oecd_col(pta, x = country, y = pct_pta, colour = category, stacked = FALSE)

  dout <- tempdir()

  expect_message(save_oecd_chart("testemf", p, format = "png", folder = dout),
                 regexp = "No Statlinks generated")
})

test_that("svg works", {
  load_oecd_fonts()

  p <- oecd_col(pta, x = country, y = pct_pta, colour = category, stacked = FALSE)

  dout <- tempdir()

  expect_message(save_oecd_chart("testemf", p, format = "svg", folder = dout),
                 regexp = "No Statlinks generated")
})
