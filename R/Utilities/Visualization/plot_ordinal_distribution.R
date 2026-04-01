#* Visualization: Ordinal Distribution Bar Chart
# Bar chart of percentage distribution for an ordinal/categorical variable.
# Auto-loaded by 00b_setup.R.

#' Bar chart of percentage distribution for an ordinal/categorical variable
#'
#' Each bar represents one level of the variable; bars show % of total patients.
#' Levels are mutually exclusive so bars sum to 100%.
#'
#' @param data A data frame.
#' @param var_name Character. Name of the factor column to plot.
#' @param y_label Character. Y-axis title (default: "% of Patients").
#' @param bar_colors Character vector of border/base colors, one per level. Ignored
#'   if uniform_color is set.
#' @param uniform_color Single color string. If set, all bars use this color (border)
#'   with a lightened fill. Overrides bar_colors. Use this when displaying alongside
#'   another chart to avoid implying cross-chart color meaning.
#' @param lighten_amount Amount to lighten fills (0 = original, 1 = white; default 0.55)
#' @param x_title Character. Optional x-axis title (default: NULL = no title).
#' @param roman_numerals Logical. If TRUE, extracts trailing integers from level names
#'   and converts them to Roman numerals for tick labels (default: FALSE).
#' @param for_slides Logical. If TRUE, uses larger text and thicker axes for projection (default: FALSE).
#' @param base_family Font family (default: "Arial").
#' @param label_size Size of percentage labels above bars (default: 4).
#'
#' @return A ggplot object.
plot_ordinal_distribution <- function(data,
                                      var_name,
                                      y_label        = "% of Patients",
                                      bar_colors     = NULL,
                                      uniform_color  = NULL,
                                      lighten_amount = 0.55,
                                      x_title        = NULL,
                                      roman_numerals = FALSE,
                                      for_slides     = FALSE,
                                      base_family    = "Arial",
                                      label_size     = if (for_slides) 5.5 else 4) {
  library(dplyr)
  library(ggplot2)
  library(colorspace)

  col_data <- data[[var_name]]
  if (!is.factor(col_data)) col_data <- factor(col_data)

  lvls    <- levels(col_data)
  n_total <- sum(!is.na(col_data))

  default_palette <- c("#113d6a", "#800017", "#b45309", "#e65100", "#6a1b9a")
  if (!is.null(uniform_color)) {
    bar_colors <- rep(uniform_color, length(lvls))
  } else if (is.null(bar_colors)) {
    bar_colors <- default_palette[seq_along(lvls)]
  }
  fill_colors   <- colorspace::lighten(bar_colors, amount = lighten_amount)
  border_colors <- bar_colors

  # Derive x-axis tick labels
  if (roman_numerals) {
    nums    <- suppressWarnings(as.integer(stringr::str_extract(lvls, "\\d+$")))
    x_labels <- ifelse(!is.na(nums), as.character(as.roman(nums)), lvls)
  } else {
    x_labels <- lvls
  }

  plot_data <- tibble::tibble(
    level = factor(lvls, levels = lvls),
    pct   = as.numeric(table(col_data)[lvls]) / n_total * 100
  )

  sz_text   <- if (for_slides) 16 else 11
  sz_title  <- if (for_slides) 18 else 12
  lw_axis   <- if (for_slides) 1.1 else 0.6
  lw_bar    <- if (for_slides) 0.8 else 0.6

  theme_pub <- function() {
    theme_minimal(base_family = base_family) +
      theme(
        panel.grid.major   = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.background   = element_blank(),
        axis.line.x.bottom = element_line(color = "black", linewidth = lw_axis),
        axis.line.y.left   = element_line(color = "black", linewidth = lw_axis),
        axis.ticks         = element_line(color = "black", linewidth = lw_axis),
        axis.ticks.length  = unit(0.15, "cm"),
        axis.text          = element_text(size = sz_text,  face = "bold", color = "black"),
        axis.title.x       = if (is.null(x_title)) element_blank() else
                               element_text(size = sz_title, face = "bold", color = "black",
                                            margin = margin(t = 5)),
        axis.title.y       = element_text(size = sz_title, face = "bold", color = "black",
                                          margin = margin(r = 5)),
        plot.title         = element_blank(),
        legend.position    = "none"
      )
  }

  ggplot(plot_data, aes(x = level, y = pct, fill = level, color = level)) +
    geom_col(width = 0.55, linewidth = lw_bar) +
    scale_fill_manual(values  = setNames(fill_colors,   lvls)) +
    scale_color_manual(values = setNames(border_colors, lvls)) +
    scale_x_discrete(labels = setNames(x_labels, lvls)) +
    geom_text(
      aes(label = paste0(round(pct), "%")),
      vjust    = -0.5,
      size     = label_size,
      fontface = "bold",
      family   = base_family,
      color    = "black"
    ) +
    scale_y_continuous(
      limits = c(0, max(plot_data$pct) * 1.18),
      breaks = seq(0, 100, 25),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(y = y_label, x = x_title) +
    theme_pub()
}
