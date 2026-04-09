#* Visualization: Volume Comparison Bar + Scatter Plot
# Side-by-side mean bar + scatter comparing HTX volume vs irrigation volume.
# Auto-loaded by 00b_setup.R.

#' Side-by-side mean bar + scatter plot comparing HTX volume vs irrigation volume
#'
#' Plots a mean bar (lightened fill, dark border) with individual data points
#' (dark solid circles) overlaid — matching house bar+scatter style.
#' Places both volumes on a shared mL y-axis to visually contrast the
#' hemothorax burden with the irrigation volume delivered.
#'
#' @param data A data frame containing total_htx_volume and irrigation_volume.
#' @param htx_color Border/point color for HTX volume (default: "#800017").
#' @param irr_color Border/point color for irrigation volume (default: "#113d6a").
#' @param lighten_amount Amount to lighten bar fills (default: 0.55).
#' @param for_slides Logical. If TRUE, larger text and thicker axes (default: FALSE).
#' @param base_family Font family (default: "Arial").
#' @param jitter_width Width of jitter for individual points (default: 0.15).
#'
#' @return A ggplot object.
plot_volume_comparison <- function(data,
                                   htx_color      = "#800017",
                                   irr_color      = "#113d6a",
                                   lighten_amount = 0.55,
                                   for_slides     = FALSE,
                                   base_family    = "Arial",
                                   jitter_width   = 0.15) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(colorspace)

  sz_text  <- if (for_slides) 16 else 11
  sz_title <- if (for_slides) 18 else 12
  lw_axis  <- if (for_slides) 1.1 else 0.6
  lw_bar   <- if (for_slides) 0.8 else 0.6

  border_colors <- c(
    "HTX Volume"        = htx_color,
    "Irrigation Volume" = irr_color
  )
  fill_colors <- c(
    "HTX Volume"        = colorspace::lighten(htx_color, lighten_amount),
    "Irrigation Volume" = colorspace::lighten(irr_color, lighten_amount)
  )

  plot_data <- data %>%
    select(total_htx_volume, irrigation_volume) %>%
    rename(
      "HTX Volume"        = total_htx_volume,
      "Irrigation Volume" = irrigation_volume
    ) %>%
    pivot_longer(everything(), names_to = "group", values_to = "volume_ml") %>%
    filter(!is.na(volume_ml)) %>%
    mutate(group = factor(group, levels = c("HTX Volume", "Irrigation Volume")))

  medians <- plot_data %>%
    group_by(group) %>%
    summarise(median_val = median(volume_ml, na.rm = TRUE), .groups = "drop")

  # Fixed y-axis: ticks every 1000 from 0 to 6000
  y_breaks <- seq(0, 6000, by = 1000)
  y_limit  <- 6000

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
        axis.title.x       = element_blank(),
        axis.title.y       = element_text(size = sz_title, face = "bold", color = "black",
                                          margin = margin(r = 5)),
        plot.title         = element_blank(),
        legend.position    = "none"
      )
  }

  # Bar colors need to be looked up per-point for geom_jitter (no aes mapping)
  point_colors <- ifelse(
    plot_data$group == "HTX Volume", htx_color, irr_color
  )

  ggplot() +
    geom_col(
      data = medians,
      aes(x = group, y = median_val, fill = group, color = group),
      width = 0.7, linewidth = lw_bar, alpha = 0.8
    ) +
    geom_jitter(
      data  = plot_data,
      aes(x = group, y = volume_ml),
      color = point_colors,
      width = jitter_width,
      size  = 1.9,
      alpha = 1,
      shape = 16,
      show.legend = FALSE
    ) +
    scale_fill_manual(values  = fill_colors) +
    scale_color_manual(values = border_colors) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      limits = c(0, y_limit),
      breaks = y_breaks,
      labels = scales::comma
    ) +
    labs(y = "Volume (mL)") +
    theme_pub()
}
