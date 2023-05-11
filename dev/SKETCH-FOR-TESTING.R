oecd_col(pta, x = country, y = pct_pta, colour = category)

oecd_line(pta, x = country, y = pct_pta, colour = category)

oecd_maxmin(pta, x = country, y = pct_pta, colour = category)

oecd_point(pta, x = country, y = pct_pta, colour = category)

pta_chl <- data.frame(
  country = "CHL",
  category = c("Protected", "Non-Protected"),
  pct_pta = c(20.5, 79.5)
)

oecd_pie(pta_chl, x = country, y = pct_pta, colour = category, group = category)

# oecd_col(pta, x = country, y = pct_pta, colour = category,
#          facet = category, palette = "darkgreen", facet_nrow = 3,
#          facet_ncol = 1)

# oecd_line(pta, x = country, y = pct_pta, colour = category,
#          facet = category, palette = "darkgreen", facet_nrow = 3,
#          facet_ncol = 1, group = 1)

# oecd_point(pta, x = country, y = pct_pta, colour = category,
#            facet = category, palette = "darkgreen", facet_nrow = 3,
#            facet_ncol = 1, group = 1, trend = T, band = T)

pta2 <- pta
pta2$pct_pta <- pta2$pct_pta / 100

oecd_col(pta2, x = country, y = pct_pta, colour = category,
         facet = category, facet_nrow = 3, facet_ncol = 1) +
  scale_y_continuous(labels = scales::percent)

save_oecd_chart("demo_2022_09_15", folder = "dev")
