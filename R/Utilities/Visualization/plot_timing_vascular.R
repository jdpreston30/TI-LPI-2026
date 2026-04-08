#* Visualization: EMS-to-TI Timing with Vascular Injury Overlay
# Dot strip plot (log-scale x) of time from EMS to thoracic irrigation.
# Vascular injury patients highlighted with color; procedure timing shown
# as a diamond above with a connecting segment where data are available.
# Auto-loaded by 00b_setup.R.

#' EMS-to-TI timing strip plot with vascular injury overlay
#'
#' Plots each patient's time from EMS arrival to thoracic irrigation on
#' a log-scaled x-axis. Non-vascular patients appear as small grey dots
#' with vertical jitter. Vascular injury patients are plotted as larger
#' colored circles (color = injury type) at y = 0. When
#' ems_to_vessel_procedure_or_dx_calc is available, a colored diamond is
#' drawn above at y = 0.65 and a dashed segment connects the two points
#' to show the temporal gap between TI and the vascular event.
#' Injury type labels are printed below each colored dot.
#'
#' @param data A data frame (typically test_dedup) containing:
#'   ems_to_ti_calc (numeric, minutes),
#'   ems_to_vessel_procedure_or_dx_calc (numeric, minutes; NA when absent),
#'   chest_vascular_injury (character; NA for non-vascular patients).
#' @param for_slides Logical. If TRUE, uses larger text and thicker axes (default: FALSE).
#' @param version Integer controlling sequential reveal layers (default: 2).
#'   0 = gray dots + median only (legend present, cover w/ white in PPT);
#'   1 = + colored diamonds; 2 = full plot (+ colored TI dots + dashed connections).
#' @param base_family Font family (default: "Arial").
#' @param dot_color_main Color for non-vascular patient dots (default: "#aaaaaa").
#' @param vasc_colors Named character vector mapping injury type to color.
#'   If NULL (default), an internal palette is used.
#' @param seed Random seed for reproducible jitter (default: 2026).
#'
#' @return A ggplot object
plot_timing_vascular <- function(data,
                                 for_slides     = FALSE,
                                 version        = 2,
                                 base_family    = "Arial",
                                 dot_color_main = "#aaaaaa",
                                 vasc_colors    = NULL,
                                 seed           = 2026) {
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(stringr)

  sz_text  <- if (for_slides) 14 else 11
  sz_title <- if (for_slides) 16 else 12
  sz_label <- if (for_slides) 3.2 else 2.8
  lw_axis  <- if (for_slides) 1.1 else 0.6
  pt_main  <- if (for_slides) 2.0 else 1.6
  pt_vasc  <- if (for_slides) 4.0 else 3.2
  pt_diam  <- if (for_slides) 4.5 else 3.8

  #- Convert minutes to hours; flag vascular patients; remap cardiac variants
  df <- data %>%
    dplyr::mutate(
      ti_hr   = as.numeric(ems_to_ti_calc) / 60,
      vasc_hr = as.numeric(ems_to_vessel_procedure_or_dx_calc) / 60,
      chest_vascular_injury = dplyr::case_when(
        stringr::str_detect(tolower(chest_vascular_injury), "ventricle|pericardium") ~ "Cardiac",
        TRUE ~ stringr::str_to_title(chest_vascular_injury)
      ),
      has_vasc = !is.na(chest_vascular_injury) & nchar(chest_vascular_injury) > 0
    ) %>%
    #- Factor with fixed display order
    dplyr::mutate(
      chest_vascular_injury = factor(
        chest_vascular_injury,
        levels = c("Pulmonary Vasculature", "Cardiac", "Intercostal Artery", "Subclavian Artery")
      )
    )

  df_bg   <- df %>% dplyr::filter(!has_vasc)
  df_vasc <- df %>% dplyr::filter(has_vasc)
  df_segs <- df_vasc %>% dplyr::filter(!is.na(vasc_hr))

  #- Default color palette — keyed to fixed level order
  inj_levels <- c("Pulmonary Vasculature", "Cardiac", "Intercostal Artery", "Subclavian Artery")
  if (is.null(vasc_colors)) {
    palette <- c("Pulmonary Vasculature" = "#8e44ad", "Cardiac" = "#c0392b",
                 "Intercostal Artery" = "#e67e22", "Subclavian Artery" = "#2980b9")
    vasc_colors <- palette
  }

  #- Pre-compute reproducible jitter positions
  median_ti_hr <- median(df$ti_hr, na.rm = TRUE)
  median_label <- dplyr::case_when(
    median_ti_hr < 1  ~ paste0("Median: ", round(median_ti_hr * 60), " min"),
    median_ti_hr < 24 ~ paste0("Median: ", round(median_ti_hr, 1), "h"),
    TRUE              ~ paste0("Median: ", round(median_ti_hr / 24, 1), "d")
  )
  set.seed(seed)
  df_bg$y_jit   <- runif(nrow(df_bg),   -0.28, 0.28)
  df_vasc$y_jit <- runif(nrow(df_vasc), -0.04, 0.04)
  #! Merge jitter back to df_segs so segments start at correct y
  df_segs <- df_vasc %>% dplyr::filter(!is.na(vasc_hr))

  #- x-axis break labels: sub-hour in minutes, hour+ rounded
  hr_breaks  <- c(0.5, 1, 2, 6, 12, 24, 48, 120, 200)
  hr_labels  <- dplyr::case_when(
    hr_breaks < 1  ~ paste0(round(hr_breaks * 60), " min"),
    hr_breaks < 24 ~ paste0(round(hr_breaks), "h"),
    TRUE           ~ paste0(round(hr_breaks / 24, 1), "d")
  )

  p <- ggplot2::ggplot() +
    #_ Non-vascular patients
    ggplot2::geom_point(
      data = df_bg,
      ggplot2::aes(x = ti_hr, y = y_jit),
      color = dot_color_main, size = pt_main, alpha = 0.55, shape = 16
    ) +
    #_ Vascular patients: TI dot (alpha=0 in v0/v1 to anchor legend without showing dots)
    ggplot2::geom_point(
      data = df_vasc,
      ggplot2::aes(x = ti_hr, y = y_jit, color = chest_vascular_injury),
      size = pt_vasc, alpha = if (version >= 2) 0.92 else 0, shape = 16
    ) +
    #_ Segments: TI dot → vessel procedure/dx diamond (v2 only)
    (if (version >= 2) ggplot2::geom_segment(
      data = df_segs,
      ggplot2::aes(
        x = ti_hr, xend = vasc_hr,
        y = y_jit, yend = 0.65,
        color = chest_vascular_injury
      ),
      linetype = "dashed", linewidth = 0.45, alpha = 0.65
    ) else NULL) +
    #_ Vessel procedure/dx: diamond at y = 0.65 (v1+)
    (if (version >= 1) ggplot2::geom_point(
      data = df_segs,
      ggplot2::aes(x = vasc_hr, y = 0.65, color = chest_vascular_injury),
      size = pt_diam, alpha = 0.92, shape = 18
    ) else NULL) +
    #- Scales
    ggplot2::scale_x_continuous(
      trans   = "log10",
      breaks  = hr_breaks,
      labels  = hr_labels,
      name    = "Time from EMS Arrival (log scale)"
    ) +
    ggplot2::scale_color_manual(
      values = vasc_colors,
      name   = "Chest Vascular Injury"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-0.65, 1.0),
      breaks = NULL
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    #_ Median TI dashed vertical line
    ggplot2::geom_vline(
      xintercept = median_ti_hr,
      linetype = "dashed", color = "#444444", linewidth = 0.55
    ) +
    ggplot2::annotate(
      "text", x = median_ti_hr, y = 0.85,
      label = median_label,
      hjust = -0.1, size = sz_label + 0.2,
      color = "#444444", fontface = "bold", family = base_family
    ) +
    ggplot2::labs() +
    ggplot2::theme_classic(base_size = sz_text, base_family = base_family) +
    ggplot2::theme(
      axis.line.y        = ggplot2::element_blank(),
      axis.ticks.y       = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_blank(),
      axis.title.y       = ggplot2::element_blank(),
      axis.title.x       = ggplot2::element_text(size = sz_title, face = "bold", color = "black"),
      axis.text.x        = ggplot2::element_text(size = sz_text,  face = "bold", color = "black"),
      axis.line.x        = ggplot2::element_line(linewidth = lw_axis, color = "black"),
      axis.ticks.x       = ggplot2::element_line(linewidth = lw_axis, color = "black"),
      axis.ticks.length  = ggplot2::unit(0.15, "cm"),
      legend.position    = "bottom",
      legend.direction   = "horizontal",
      legend.text        = ggplot2::element_text(size = sz_text - 2, face = "bold"),
      legend.title       = ggplot2::element_text(size = sz_text - 1, face = "bold"),
      panel.grid.major.x = ggplot2::element_line(color = "#e8e8e8", linewidth = 0.4),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.margin        = ggplot2::margin(8, 12, 8, 60, unit = "pt")
    )

  return(p)
}
