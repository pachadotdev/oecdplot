#' Save 'ggplot' Objects According to PAC Rules
#'
#' Uses predefined sizes in centimeters (cm) to export graphic objects. Besides
#' saving EMF and PNG files, optionally creates a spreadsheet with Statlinks.
#'
#' @param file_name Name for the file
#' @param plot Graphic object to export (defaults to the last plot in the
#' Viewer)
#' @param folder Golder where the plot will be saved
#' @param size Chart size according to official PAC dimensions in centimeters
#' | Parameter                       | Width  | Height |
#' |---------------------------------|--------|--------|
#' | 1/3                             | 15.42  |  7.08  |
#' | 1/3                             | 15.42  |  9.66  |
#' | 2/3                             | 15.42  | 11.93  |
#' | 1/3 (2 graphs in the same row)  |  7.54  |  7.08  |
#' | 1/2 (2 graphs in the same row)  |  7.54  |  9.66  |
#' | 2/3 (2 graphs in the same row)  |  7.54  | 11.93  |
#' | Full footnotes                  | 13.22  | 10.49  |
#' | Full page                       | 13.22  | 20.8   |
#' | Slides                          | 26     | 12.2   |
#' | Half slides                     | 14     | 12.2   |
#' @param dpi Chart dots per inch (DPI). The default is NULL.
#' @param plot_title Title of the plot
#' @param plot_subtitle Subtitle of the plot
#' @param plot_note Note for the plot
#' @param plot_source Source of the plot
#' @param statlink_create \code{TRUE} or \code{FALSE} (default)
#' @param statlink_dataframe Data used to create figure that will be added to
#' Statlink
#' @param statlink_filename the filename with no extension
#' @param box Logical parameter to rescale plots for boxes (reduced plot width)
#' @param custom_size (Optional) A vector such as `c(20.8, 13.22, 49)` (i.e.
#' 20.8 cm width, 13.22 cm height, data starts in row 49 in the exported Excel)
#' @param format a string or vector indicating the formats. The options are
#' "png", "emf", "ps" and the default is NULL, which saves as PNG and EMF
#' @param emfPlus Totally optional parameter for EMF rendering
#' @param emfPlusFontToPath Totally optional parameter for EMF rendering
#' @importFrom dplyr tibble mutate mutate_if select row_number rename_all
#'  everything
#' @importFrom showtext showtext_auto showtext_opts
#' @importFrom stringr str_c str_to_title str_sub
#' @importFrom ggplot2 last_plot theme unit
#' @importFrom grDevices dev.off png jpeg cairo_ps cairo_pdf
#' @importFrom openxlsx createWorkbook addWorksheet insertImage createStyle
#'  writeData addStyle conditionalFormatting setColWidths saveWorkbook
#' @importFrom devEMF emf
#' @importFrom svglite svglite
#' @importFrom rlang inform sym
#' @examples
#' \dontrun{
#' save_oecd_chart(
#'   file_name = "fig1",
#'   plot = f1,
#'   folder = "dev/pac_demo/",
#'   size = "2/3",
#'   plot_title = "Life Expectancy in 2002 and 2007",
#'   plot_subtitle = "Source: Gapminder",
#'   plot_note = "Note: Random sample of countries",
#'   plot_source = "R 4.1",
#'   statlink_create = T,
#'   statlink_dataframe = d1,
#'   statlink_filename = "f1"
#' )
#' }
#' @md
#' @export
save_oecd_chart <- function(file_name = "graph name",
                            plot = last_plot(),
                            folder = "publication/",
                            size = "1/2",
                            dpi = NULL,
                            plot_title = NULL,
                            plot_subtitle = NULL,
                            plot_note = NULL,
                            plot_source = NULL,
                            statlink_create = FALSE,
                            statlink_dataframe = NULL,
                            statlink_filename = NULL,
                            box = FALSE,
                            custom_size = NULL,
                            format = c("png","emf"),
                            emfPlus = NULL,
                            emfPlusFontToPath  = NULL) {
  if (is.null(plot)) {
    stop(
      "No plot was detected. Run the code to produce the plot you would like to save and then re-run `save_oecd_chart()`."
    )
  }

  if (is.null(statlink_dataframe)) {
    statlink_dataframe <- plot$data
  }

  if (str_sub(folder, -1, -1) != "/") { folder <- paste0(folder, "/") }

  if (is.null(emfPlus)) {
    emfPlus <- FALSE
  }

  if (is.null(emfPlusFontToPath)) {
    emfPlusFontToPath <- FALSE
  }

  # for emf device
  cm_to_in <- 0.3937

  # Select size
  if (is.null(custom_size)) {
    chart_height <- switch(size,
      "1/3" = 7.08,
      "1/2" = 9.66,
      "2/3" = 11.93,
      "1" = 20.8,
      "Slides" = 12.12,
      "Half slides" = 12.12,
      "Full page footnotes" = 10.49,
      "Full page" = 20.8,
      "1/3 (2 graphs in the same row)" = 7.08,
      "1/2 (2 graphs in the same row)" = 7.99,
      "2/3 (2 graphs in the same row)" = 11.93
    )

    chart_width <- switch(size,
      "1/3" = 15.42,
      "1/2" = 15.42,
      "2/3" = 15.42,
      "1" = 13.22,
      "Slides" = 26,
      "Half slides" = 14,
      "Full page footnotes" = 13.22,
      "Full page" = 13.22,
      "1/3 (2 graphs in the same row)" = 7.54,
      "1/2 (2 graphs in the same row)" = 7.54,
      "2/3 (2 graphs in the same row)" = 7.54
    )

    chart_nrows <- switch(size,
      "1/3" = 22,
      "1/2" = 27,
      "2/3" = 44,
      "1" = 49,
      "Slides" = 22,
      "Half slides" = 22,
      "Full page footnotes" = 27,
      "Full page" = 27,
      "1/3 (2 graphs in the same row)" = 27,
      "1/2 (2 graphs in the same row)" = 27,
      "2/3 (2 graphs in the same row)" = 27
    )
  } else {
    chart_width <- custom_size[1]
    chart_height <- custom_size[2]
    chart_nrows <- custom_size[3]
  }

  chart_height <- ifelse(isTRUE(box), chart_height * (16 / 16.4), chart_height)

  # set width as in PAC indication
  # plot <- plot + theme(legend.key.width = unit(chart_width, 'cm'))

  # Define folder and name
  file_name <- paste0(folder, "/", file_name)

  # Check if folder exists
  suppressWarnings({
    try(dir.create(folder, recursive = T))
  })

  # Save in different formats ----

  if (is.null(format)) format <- c("png", "emf")
  stopifnot(any(format %in% c("png", "emf", "svg", "eps", "pdf")))

  # HACK: set fonts (adapted from rgc2022)
  # TODO: the fonts appear really small unless showtext is disabled. no idea
  # how this affects the fonts itself. visually, it looks like Arial Arrow is
  # still used
  showtext_auto(enable = FALSE)

  # PNG ----
  if (any("png" %in% format)) {
    ggsave(
      paste0(file_name, ".png"),
      plot = plot,
      device = png,
      type = "cairo",
      height = chart_height,
      width = chart_width,
      units = "cm",
      dpi = ifelse(is.null(dpi), 300, dpi)
    )
  }

  # EMF ----
  if (any("emf" %in% format)) {
    emf(
      file = paste0(file_name, ".emf"),
      width = chart_width * cm_to_in,
      height = chart_height * cm_to_in,
      family = "Arial Narrow",
      coordDPI = ifelse(is.null(dpi), 300, dpi),
      emfPlus = emfPlus,
      emfPlusFontToPath = emfPlusFontToPath
    )
    plot(plot)
    dev.off()
  }

  # SVG ----
  if (any("svg" %in% format)) {
    ggsave(
      paste0(file_name, ".svg"),
      plot = plot,
      device = svglite,
      height = chart_height * cm_to_in,
      width = chart_width * cm_to_in
    )
  }

  # PDF ----
  if (any("pdf" %in% format)) {
    ggsave(
      paste0(file_name, ".pdf"),
      plot = plot,
      device = cairo_pdf,
      height = chart_height * cm_to_in,
      width = chart_width * cm_to_in
    )
  }

  # EPS ----
  if (any("eps" %in% format)) {
    ggsave(
      paste0(file_name, ".eps"),
      plot = plot,
      device = cairo_ps,
      height = chart_height * cm_to_in,
      width = chart_width * cm_to_in
    )
  }

  # STATLINKS ----

  if (statlink_create) {
    # TODO: MAKE 2ND EMF, SAME NAME AS THE STATLINK
    # I.E. EXCEL FIG1.1 AND EMF FIG1.1
    # BOTH IN PUBLICATIONS

    ## TXT
    writeLines(
      c(
        plot_title,
        plot_subtitle,
        "",
        "",
        "",
        ifelse(plot_note == "", "", paste0("Note: ", plot_note)),
        ifelse(plot_source == "", "", paste0("Source: ", plot_source))
      ),
      paste0(file_name, ".txt")
    )

    # Set up
    startrow_data <- chart_nrows

    suppressWarnings(
      try(dir.create(paste0(folder, "/statlinks/")))
    )

    if (is.null(statlink_filename)) {
      statlink_filename <- gsub(folder, "", file_name)
    }
    plot_path <- paste0(folder, "/statlinks", statlink_filename, ".png")
    ggsave(
      plot_path,
      plot = plot,
      device = png,
      type = "cairo",
      height = chart_height,
      width = chart_width,
      units = "cm",
      dpi = ifelse(is.null(dpi), 96, dpi)
    )

    # Create excel and add image
    wb <- createWorkbook()
    addWorksheet(wb, statlink_filename)
    insertImage(wb, statlink_filename, plot_path,
      height = chart_height,
      width = chart_width, startRow = 4, startCol = 2, units = "cm"
    )

    # Create styles for data formatting
    format_title <- createStyle(fontSize = 12, textDecoration = "bold")
    format_header <- createStyle(
      fontName = "Calibri", fontSize = 11,
      valign = "center", wrapText = TRUE,
      border = "TopBottomLeftRight",
      textDecoration = "bold"
    )
    format_body <- createStyle(
      fontName = "Calibri", fontSize = 11,
      valign = "center", wrapText = TRUE,
      border = "LeftRight", numFmt = "COMMA"
    )
    format_body_odd <- createStyle(
      fontName = "Calibri", fontSize = 11,
      valign = "center", wrapText = TRUE,
      border = "LeftRight", numFmt = "COMMA",
      bgFill = "#dbe5f1"
    )

    # Prepare data
    statlink_dataframe <- statlink_dataframe %>%
      mutate_if(is.numeric, function(x) round(x, 4)) %>%
      mutate_if(is.double, function(x) round(x, 4)) %>%
      mutate(`Row number` = row_number()) %>%
      select(!!sym("Row number"), everything()) %>%
      rename_all(str_to_title)

    # Add data
    writeData(wb, statlink_filename, statlink_dataframe,
      xy = c(1, startrow_data), colNames = TRUE, rowNames = FALSE,
      headerStyle = format_header, borders = "columns"
    )
    writeData(wb, statlink_filename, c(plot_title, plot_subtitle),
      startRow = 1, headerStyle = createStyle(textDecoration = "bold")
    )
    writeData(wb, statlink_filename, c(
      str_c("Note: ", plot_note),
      str_c("Source: ", plot_source)
    ), startRow = startrow_data - 3)

    # Add formatting
    addStyle(wb, statlink_filename, format_title, cols = 1, rows = 1)
    addStyle(wb, statlink_filename, format_body,
      cols = 1:ncol(statlink_dataframe),
      rows = (1 + startrow_data):(startrow_data + nrow(statlink_dataframe)),
      gridExpand = TRUE
    )
    conditionalFormatting(wb, statlink_filename,
      cols = 1:ncol(statlink_dataframe),
      rows = 1:(startrow_data + nrow(statlink_dataframe)),
      rule = "MOD($A1,2)=1", style = format_body_odd
    )
    setColWidths(wb, sheet = 1, cols = 1, widths = 10)
    setColWidths(wb,
      sheet = 1, cols = 2:ncol(statlink_dataframe),
      widths = "auto"
    )

    # Save
    saveWorkbook(wb, paste0(
      folder, "/statlinks/",
      statlink_filename, ".xlsx"
    ), overwrite = TRUE)

    inform(c("i" = "Statlinks successfully generated and saved to produced_data/statlinks."))
  } else {
    inform(c("i" = "No Statlinks generated."))
  }

  orig_dpi <- showtext_opts(dpi = 300)$dpi
  on.exit(showtext_opts(dpi = orig_dpi), add = TRUE)
  on.exit(showtext_auto(enable = TRUE), add = TRUE)
}
