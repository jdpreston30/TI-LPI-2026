#* Visualization: Operative Procedures Pie Chart
# Pie chart of operative procedures preceding thoracic irrigation (n = 6, manual data).
# Auto-loaded by 00b_setup.R.

#' Pie chart of operative procedures preceding thoracic irrigation (n = 6)
#'
#' Hardcoded manual data: among the 6 patients who underwent emergent surgery
#' before TI — intercostal artery ligation (n=3), subclavian artery stent (n=2),
#' lung wedge resection (n=1).
#'
#' @param colors Named character vector of slice colors (order: ligation, stent, wedge).
#' @param for_slides Logical. If TRUE, larger text (default: FALSE).
#' @param base_family Font family (default: "Arial").
#'
#' @return A ggplot object.
plot_operative_procedures <- function(colors      = c("#113d6a", "#800017", "#b45309"),
                                      for_slides  = FALSE,
                                      base_family = "Arial") {
  library(dplyr)
  library(ggplot2)

  sz_label  <- if (for_slides) 5.5 else 4
  sz_legend <- if (for_slides) 14 else 10

  pie_data <- tibble::tibble(
    procedure = factor(
      c("Intercostal Artery Ligation",
        "Subclavian Artery Endovascular Stent",
        "Lung Wedge Resection"),
      levels = c("Intercostal Artery Ligation",
                 "Subclavian Artery Endovascular Stent",
                 "Lung Wedge Resection")
    ),
    n = c(3L, 2L, 1L)
  ) %>%
    mutate(
      pct        = n / sum(n) * 100,
      slice_label = paste0("n=", n)
    )

  ggplot(pie_data, aes(x = "", y = pct, fill = procedure)) +
    geom_col(
      width     = 1,
      color     = "white",
      linewidth = 0.8
    ) +
    geom_text(
      aes(label = slice_label),
      position   = position_stack(vjust = 0.5),
      size       = sz_label,
      fontface   = "bold",
      family     = base_family,
      color      = "white",
      lineheight = 1.1
    ) +
    coord_polar(theta = "y", start = 0) +
    scale_fill_manual(
      values = setNames(colors, levels(pie_data$procedure)),
      name   = NULL
    ) +
    guides(fill = guide_legend(
      override.aes = list(size = 5),
      keywidth     = unit(0.9, "cm"),
      keyheight    = unit(0.9, "cm")
    )) +
    theme_void(base_family = base_family) +
    theme(
      legend.position  = "right",
      legend.text      = element_text(size = sz_legend, face = "bold",
                                      color = "black", family = base_family),
      legend.spacing.y = unit(0.3, "cm"),
      plot.background  = element_rect(fill = "white", color = NA)
    )
}
