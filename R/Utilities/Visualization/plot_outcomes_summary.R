#* Visualization: Outcomes Summary Bar Chart
# Two-bar chart of surgery-free rates (no emergent surgery and no post-TI rebleeding).
# Auto-loaded by 00b_setup.R.

#' Two-bar outcomes summary: both bars show % surgery-free (same axis direction)
#'
#' Bar 1 (amber): 93.5% — no emergent surgery before TI
#' Bar 2 (blue):  100%  — no reoperation for post-TI rebleeding
#'
#' Both bars measure the same thing (surgery-free rate), making the y-axis
#' consistent. The step from 93.5% → 100% tells the safety story visually.
#'
#' @param surgery_color  Border/base color for the emergent surgery bar (default: "#b45309").
#' @param rebleed_color  Border/base color for the rebleeding-free bar (default: "#113d6a").
#' @param lighten_amount Amount to lighten bar fills (default: 0.55).
#' @param for_slides     Logical. If TRUE, larger text and thicker axes (default: FALSE).
#' @param base_family    Font family (default: "Arial").
#' @param label_size     Size of % annotation above bars.
#'
#' @return A ggplot object.
plot_outcomes_summary <- function(surgery_color  = "#b45309",
                                  rebleed_color  = "#113d6a",
                                  lighten_amount = 0.55,
                                  for_slides     = FALSE,
                                  base_family    = "Arial",
                                  label_size     = if (for_slides) 6 else 4.5) {
  library(ggplot2)
  library(colorspace)

  sz_text  <- if (for_slides) 16 else 11
  sz_title <- if (for_slides) 18 else 12
  lw_axis  <- if (for_slides) 1.1 else 0.6
  lw_bar   <- if (for_slides) 0.8 else 0.6

  bar_data <- tibble::tibble(
    outcome = factor(
      c("No Emergent Surgery
Before TI",
        "No Reoperation for
Post-TI Rebleeding"),
      levels = c("No Emergent Surgery
Before TI",
                 "No Reoperation for
Post-TI Rebleeding")
    ),
    pct    = c(93.5, 100),
    color  = c(surgery_color, rebleed_color),
    fill   = c(
      colorspace::lighten(surgery_color, lighten_amount),
      colorspace::lighten(rebleed_color, lighten_amount)
    ),
    label  = c("93.5%", "100%")
  )

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

  ggplot(bar_data, aes(x = outcome, y = pct, fill = outcome, color = outcome)) +
    geom_col(width = 0.55, linewidth = lw_bar, alpha = 0.8) +
    geom_text(
      aes(label = label),
      vjust    = -0.5,
      size     = label_size,
      fontface = "bold",
      family   = base_family,
      color    = "black"
    ) +
    scale_fill_manual(values  = setNames(bar_data$fill,  levels(bar_data$outcome))) +
    scale_color_manual(values = setNames(bar_data$color, levels(bar_data$outcome))) +
    scale_y_continuous(
      limits = c(0, 110),
      breaks = seq(0, 100, 25),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(y = "% of Patients") +
    theme_pub()
}
