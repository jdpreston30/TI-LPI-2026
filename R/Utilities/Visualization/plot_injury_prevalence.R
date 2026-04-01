#* Visualization: Injury Prevalence Bar Chart
# Bar chart of pulmonary injury prevalence (non-mutually exclusive).
# Auto-loaded by 00b_setup.R.

#' Bar chart of pulmonary injury prevalence (non-mutually exclusive)
#'
#' Plots the percentage of patients with each of three pulmonary injury types:
#' lung contusion, lung laceration, and intraparenchymal hemorrhage.
#' Injuries are not mutually exclusive — each bar is computed independently.
#'
#' @param data A data frame containing pulm_contusion, pulmonary_laceration,
#'   and iph columns as factors with levels "N"/"Y".
#' @param base_family Font family (default: "Arial")
#' @param bar_colors Named or unnamed character vector of three border/base colors.
#'   Ignored if uniform_color is set.
#' @param uniform_color Single color string. If set, all bars use this color (border)
#'   with a lightened fill. Overrides bar_colors. Use this when displaying alongside
#'   another chart to avoid implying cross-chart color meaning.
#' @param lighten_amount Amount to lighten fills (0 = original, 1 = white; default 0.55)
#' @param x_title Character. X-axis title (default: "Injury Type").
#' @param for_slides Logical. If TRUE, uses larger text and thicker axes for projection (default: FALSE).
#' @param label_size Size of percentage labels above bars (default: 4)
#'
#' @return A ggplot object
plot_injury_prevalence <- function(data,
                                   base_family    = "Arial",
                                   bar_colors     = c("#113d6a", "#800017", "#b45309"),
                                   uniform_color  = NULL,
                                   lighten_amount = 0.55,
                                   x_title        = "Injury Type",
                                   for_slides     = FALSE,
                                   label_size     = if (for_slides) 5.5 else 4) {
  library(dplyr)
  library(ggplot2)
  library(colorspace)

  n_total <- nrow(data)

  prev_data <- tibble::tibble(
    injury = factor(
      c("Lung\nContusion", "Lung\nLaceration", "Intrapulmonary\nHemorrhage"),
      levels = c("Lung\nContusion", "Lung\nLaceration", "Intrapulmonary\nHemorrhage")
    ),
    pct = c(
      sum(data$pulm_contusion      == "Y", na.rm = TRUE) / n_total * 100,
      sum(data$pulmonary_laceration == "Y", na.rm = TRUE) / n_total * 100,
      sum(data$iph                  == "Y", na.rm = TRUE) / n_total * 100
    )
  )

  sz_text   <- if (for_slides) 16 else 11
  sz_title  <- if (for_slides) 18 else 12
  lw_axis   <- if (for_slides) 1.1 else 0.6
  lw_bar    <- if (for_slides) 0.8 else 0.6

  # Resolve colors: uniform overrides per-bar
  resolved_border <- if (!is.null(uniform_color)) rep(uniform_color, 3) else bar_colors
  fill_colors     <- colorspace::lighten(resolved_border, amount = lighten_amount)
  border_colors   <- resolved_border

  theme_pub <- function() {
    theme_minimal(base_family = base_family) +
      theme(
        panel.grid.major    = element_blank(),
        panel.grid.minor    = element_blank(),
        panel.background    = element_blank(),
        axis.line.x.bottom  = element_line(color = "black", linewidth = lw_axis),
        axis.line.y.left    = element_line(color = "black", linewidth = lw_axis),
        axis.ticks          = element_line(color = "black", linewidth = lw_axis),
        axis.ticks.length   = unit(0.15, "cm"),
        axis.text           = element_text(size = sz_text,  face = "bold", color = "black"),
        axis.title.x        = if (is.null(x_title)) element_blank() else
                                element_text(size = sz_title, face = "bold", color = "black",
                                             margin = margin(t = 5)),
        axis.title.y        = element_text(size = sz_title, face = "bold", color = "black",
                                           margin = margin(r = 5)),
        plot.title          = element_blank(),
        legend.position     = "none"
      )
  }

  ggplot(prev_data, aes(x = injury, y = pct, fill = injury, color = injury)) +
    geom_col(width = 0.55, linewidth = lw_bar) +
    scale_fill_manual(values  = setNames(fill_colors,   levels(prev_data$injury))) +
    scale_color_manual(values = setNames(border_colors, levels(prev_data$injury))) +
    geom_text(
      aes(label = paste0(round(pct), "%")),
      vjust = -0.5,
      size  = label_size,
      fontface = "bold",
      family   = base_family,
      color    = "black"
    ) +
    scale_y_continuous(
      limits = c(0, 105),
      breaks = seq(0, 100, 25),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(y = "% of Patients", x = x_title) +
    theme_pub()
}
