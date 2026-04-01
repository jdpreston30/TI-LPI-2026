#* 1: Visualization Setup
#+ 1.1: Output directory
out_dir <- config$paths$figures
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
#* 2: Build Visualization Panels
#+ 2.1: Individual injury panels
p_injury_prev <- plot_injury_prevalence(test_dedup, for_slides = TRUE,
                                        uniform_color = "#113d6a")
p_aast <- plot_ordinal_distribution(
  test_dedup, "aast_lung_grade",
  x_title        = "Pulmonary AAST-OIS",
  roman_numerals = TRUE,
  for_slides     = TRUE,
  uniform_color  = "#800017"
)
#+ 2.2: Slide 1 — LPI characterization (aligned panel heights)
aligned <- cowplot::align_plots(p_aast, p_injury_prev, align = "h", axis = "tb")
slide1  <- cowplot::plot_grid(aligned[[1]], aligned[[2]], ncol = 2, rel_widths = c(1, 1))
print_to_png(slide1, "slide1_lpi_characterization", width = 12, height = 5, output_dir = out_dir)
#+ 2.3: Individual exports
print_to_png(p_injury_prev, "fig1_injury_prevalence", output_dir = out_dir)
print_to_png(p_aast,        "fig3_aast_lung_grade",   output_dir = out_dir)
#+ 2.4: Slide 2 — volume comparison (HTX vs irrigation)
p_volumes <- plot_volume_comparison(test_dedup, for_slides = TRUE)
print_to_png(p_volumes, "slide2_volume_comparison", width = 6, height = 5, output_dir = out_dir)
#+ 2.5: Slide 3 — operative procedures preceding TI (n = 6, manual data)
p_ops <- plot_operative_procedures(for_slides = TRUE)
print_to_png(p_ops, "slide3_operative_procedures", width = 8, height = 5, output_dir = out_dir)
#+ 2.6: Slide 4 — outcomes summary (emergent surgery rate vs. rebleeding-free rate)
p_outcomes <- plot_outcomes_summary(for_slides = TRUE)
print_to_png(p_outcomes, "slide4_outcomes_summary", width = 6, height = 5, output_dir = out_dir)

