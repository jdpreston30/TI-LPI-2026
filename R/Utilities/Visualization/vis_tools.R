#* Visualization Utility: vis_tools
# Shared visualization helpers. Auto-loaded by 00b_setup.R.

#+ Grid Guide for Positioning (cowplot coordinates)
#' Grid Guide for Positioning (cowplot coordinates)
#' Creates a grid overlay with dashed lines and coordinate labels to assist with
#' precise positioning of plot elements in cowplot figure layouts.
#'
#' @param x_max Maximum x-coordinate for the grid (default: 8.5)
#' @param y_max Maximum y-coordinate for the grid (default: 11)
#' @param interval Spacing between grid lines (default: 0.25)
#' @param label_interval Spacing between coordinate labels (default: 0.5)
#' @param margins Distance from edges for margin lines in inches (default: 0.5)
#'
#' @return List of ggplot2 geom objects for grid overlay
#' @export
grdgd <- function(x_max = 8.5, y_max = 11, interval = 0.25, label_interval = 0.5, margins = 0.5) {
  guide_elements <- list(
    ggplot2::geom_vline(xintercept = seq(0, x_max, interval), color = "red", alpha = 0.3, linetype = "dashed"),
    ggplot2::geom_hline(yintercept = seq(0, y_max, interval), color = "red", alpha = 0.3, linetype = "dashed"),
    ggplot2::annotate("text", x = seq(0, x_max, label_interval), y = 0.2, label = seq(0, x_max, label_interval), size = 3, color = "red"),
    ggplot2::annotate("text", x = 0.2, y = seq(0, y_max, label_interval), label = seq(0, y_max, label_interval), size = 3, color = "red"),
    ggplot2::annotate("text", x = seq(0, x_max, label_interval), y = y_max - 0.2, label = seq(0, x_max, label_interval), size = 3, color = "red"),
    ggplot2::annotate("text", x = x_max - 0.2, y = seq(0, y_max, label_interval), label = seq(0, y_max, label_interval), size = 3, color = "red")
  )
  if (!is.null(margins)) {
    guide_elements[[length(guide_elements) + 1]] <- ggplot2::geom_vline(xintercept = margins, color = "black", linewidth = 1)
    guide_elements[[length(guide_elements) + 1]] <- ggplot2::geom_vline(xintercept = x_max - margins, color = "black", linewidth = 1)
    guide_elements[[length(guide_elements) + 1]] <- ggplot2::geom_hline(yintercept = margins, color = "black", linewidth = 1)
    guide_elements[[length(guide_elements) + 1]] <- ggplot2::geom_hline(yintercept = y_max - margins, color = "black", linewidth = 1)
  }
  return(guide_elements)
}
#+ Figure Labels Generator for Cowplot Layouts
#' Figure Labels Generator for Cowplot Layouts
#' Generates figure panel labels (A, B, C, etc.) at specified coordinates
#' for multi-panel figure layouts using cowplot's coordinate system.
#'
#' @param labels Named list where names are label text and values are coordinate vectors c(x, y)
#' @param size Font size for labels (default: 14)
#' @param fontface Font face for labels (default: "bold")
#' @param fontfamily Font family for labels (default: "Arial")
#' @param hjust Horizontal justification (default: 0)
#'
#' @return List of cowplot::draw_label objects
#' @export
figure_labels <- function(labels, size = 14, fontface = "bold", fontfamily = "Arial", hjust = 0) {
  if (is.character(labels)) {
    stop("Please provide labels as a named list with x and y coordinates, e.g., list(A = c(0.8, 9.7), B = c(3.7, 9.7))")
  }
  label_layers <- list()
  for (name in names(labels)) {
    coords <- labels[[name]]
    label_layers[[length(label_layers) + 1]] <-
      cowplot::draw_label(name, x = coords[1], y = coords[2],
                         size = size, fontface = fontface,
                         fontfamily = fontfamily, hjust = hjust)
  }
  return(label_layers)
}
#+ Print plot to PNG with auto-refresh for macOS Preview
#' Print plot to PNG with auto-refresh for macOS Preview
#'
#' @param plot The plot object to print
#' @param filename Name of the PNG file (with or without .png extension)
#' @param width Width in inches (default: 8.5)
#' @param height Height in inches (default: 11)
#' @param dpi Resolution in DPI (default: 600)
#' @param output_dir Directory to save the PNG (default: "Outputs/Figures")
#' @param auto_open Whether to automatically open in Preview on first run (default: TRUE)
#' @param background Background color for the plot (default: "white")
#'
#' @return Invisible path to the created PNG file
#' @export
print_to_png <- function(plot, filename, width = 8.5, height = 11, dpi = 600,
                         output_dir = "Outputs/Figures", auto_open = TRUE, background = "white") {
  if (!grepl("\\.png$", filename, ignore.case = TRUE)) filename <- paste0(filename, ".png")
  filepath <- file.path(output_dir, filename)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  file_exists <- file.exists(filepath)
  ggplot2::ggsave(filename = filepath, plot = plot, width = width, height = height,
                  dpi = dpi, units = "in", device = "png", bg = background)
  if (auto_open && !file_exists) {
    system(paste("open", shQuote(filepath)))
    cat("PNG saved and opened in Preview:", filepath, "\n")
    cat("Preview will auto-refresh when you re-run this function!\n")
  } else {
    cat("PNG updated:", filepath, "\n")
  }
  invisible(filepath)
}
#+ Print plot to TIFF with auto-refresh for macOS Preview
#' Print plot to TIFF with auto-refresh for macOS Preview
#'
#' @param plot The plot object to print
#' @param filename Name of the TIFF file (with or without .tiff extension)
#' @param width Width in inches (default: 8.5)
#' @param height Height in inches (default: 11)
#' @param dpi Resolution in DPI (default: 600)
#' @param output_dir Directory to save the TIFF (default: "Outputs/Figures")
#' @param auto_open Whether to automatically open in Preview on first run (default: TRUE)
#' @param background Background color for the plot (default: "white")
#'
#' @return Invisible path to the created TIFF file
#' @export
print_to_tiff <- function(plot, filename, width = 8.5, height = 11, dpi = 600,
                          output_dir = "Outputs/Figures", auto_open = TRUE, background = "white") {
  if (!grepl("\\.tiff?$", filename, ignore.case = TRUE)) filename <- paste0(filename, ".tiff")
  filepath <- file.path(output_dir, filename)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  file_exists <- file.exists(filepath)
  ggplot2::ggsave(filename = filepath, plot = plot, width = width, height = height,
                  dpi = dpi, units = "in", device = ragg::agg_tiff,
                  compression = "lzw", bg = background)
  if (auto_open && !file_exists) {
    system(paste("open", shQuote(filepath)))
    cat("TIFF saved and opened in Preview:", filepath, "\n")
    cat("Preview will auto-refresh when you re-run this function!\n")
  } else {
    cat("TIFF updated:", filepath, "\n")
  }
  invisible(filepath)
}
#+ Spacing Helper for Vertical Centering
#' Spacing Helper for Vertical Centering
#' Calculates the new y position needed to center an element when top and bottom
#' spacing is unequal.
#'
#' @param top_space Space from top edge in pixels
#' @param bottom_space Space from bottom edge in pixels
#' @param current_y Current y position in inches
#' @param dpi DPI conversion factor (default: 300)
#'
#' @return Invisibly returns the new centered y position
#' @export
sy <- function(top_space, bottom_space, current_y, dpi = 300) {
  total_space      <- top_space + bottom_space
  target_spacing   <- total_space / 2
  adjustment_pixels <- target_spacing - top_space
  adjustment_inches <- -adjustment_pixels / dpi
  new_y <- current_y + adjustment_inches
  cat("\033[1;31mNew y position = ", format(new_y, nsmall = 9), "\033[0m\n", sep = "")
  invisible(new_y)
}

