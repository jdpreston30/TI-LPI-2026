#* Analysis Utility: Descriptive Table Builder
# Assembles a formatted descriptive statistics table from ternG output.
# Auto-loaded by 00b_setup.R.

#' Section header row helper
#'
#' Creates a single-row tibble used as a section divider in the descriptive table.
#'
#' @param title Character. Section header text.
#' @return A one-row tibble with Variable = title and Total = "".
.section <- function(title) tibble(Variable = title, Total = "")

#' Assemble formatted descriptive table from ternG output
#'
#' Relabels variables, groups rows into clinical sections, and optionally
#' prepends cohort-level count rows. Designed for TI-LPI cohort output from ternG().
#'
#' @param descriptive_norm Data frame. Output of ternG() after label cleaning.
#' @param n_total Integer. Total screened patients (optional).
#' @param n_met Integer. Patients meeting inclusion criteria (optional).
#'
#' @return A tibble with Variable and Total columns, ready for export.
build_descriptive_table <- function(descriptive_norm,
                                    n_total = NULL,
                                    n_met = NULL) {
  # 1) Relabel to match your table text exactly
  labeled <- descriptive_norm %>%
    mutate(Variable = case_when(
      Variable == "age" ~ "Age",
      Variable == "sex: M" ~ "Sex (Male)",
      Variable == "iss" ~ "Injury Severity Score",
      Variable == "mechanism: Penetrating" ~ "Penetrating Mechanism",
      Variable == "pulm_contusion: Y" ~ "Lung Contusion",
      Variable == "pulmonary_laceration: Y" ~ "Lung Laceration",
      Variable == "iph: Y" ~ "Intraparenchymal Hemorrhage",
      Variable == "ems_to_ti_calc" ~ "Injury to Irrigation Time (min)*",
      Variable == "total_htx_volume" ~ "Total Hemothorax Volume (cc)",
      Variable == "irrigation_volume" ~ "Irrigation Volume (cc)",
      # Pull out the level number for AIS chest and AAST lung
      str_detect(Variable, "^ais_chest:") ~ str_replace(Variable, "^ais_chest:\\s*", ""),
      str_detect(Variable, "^aast_lung_grade:") ~ str_replace(Variable, "^aast_lung_grade:\\s*", ""),
      str_detect(Variable, "^location_of_irrigation:") ~ str_replace(Variable, "^location_of_irrigation:\\s*", ""),
      TRUE ~ Variable
    ))
  # Convenience pickers for each section
  demo_rows <- labeled %>%
    filter(Variable %in% c("Age", "Sex (Male)"))
  inj_char_rows <- labeled %>%
    filter(Variable %in% c("Injury Severity Score", "Penetrating Mechanism"))
  thor_injury_rows <- labeled %>%
    filter(Variable %in% c("Lung Contusion", "Lung Laceration", "Intraparenchymal Hemorrhage"))
  ais_rows <- labeled %>%
    filter(Variable %in% c("AIS chest: 3", "AIS chest: 4", "AIS chest: 5")) %>%
    arrange(as.numeric(Variable))
  aast_rows <- labeled %>%
    filter(Variable %in% c("AAST lung grade: 2", "AAST lung grade: 3", "AAST lung grade: 4")) %>%
    arrange(as.numeric(Variable))
  irr_rows <- labeled %>%
    filter(Variable %in% c(
      "Injury to Irrigation Time (min)*",
      "Total Hemothorax Volume (cc)",
      "Irrigation Volume (cc)"
    ))
  loc_rows <- labeled %>%
    filter(str_detect(Variable, "^[A-Z]") &
             Variable %in% setdiff(unique(labeled$Variable),
               c("Age", "Sex (Male)", "Injury Severity Score", "Penetrating Mechanism",
                 "Lung Contusion", "Lung Laceration", "Intraparenchymal Hemorrhage",
                 "AIS chest: 3", "AIS chest: 4", "AIS chest: 5",
                 "AAST lung grade: 2", "AAST lung grade: 3", "AAST lung grade: 4",
                 "Injury to Irrigation Time (min)*", "Total Hemothorax Volume (cc)",
                 "Irrigation Volume (cc)")))
  # Optional topline counts (only added if provided)
  toplines <- bind_rows(
    if (!is.null(n_total)) tibble(Variable = "Total Patients", Total = as.character(n_total)) else NULL,
    if (!is.null(n_met)) tibble(Variable = "Met Inclusion Criteria (Had LPI)", Total = as.character(n_met)) else NULL
  )
  # 2) Assemble with headers as blank/dummy rows
  out <- bind_rows(
    .section("Cohort Features and Demographics*"),
    toplines,
    demo_rows,
    .section("Injury Characteristics"),
    inj_char_rows,
    .section("Thoracic Injuries"),
    thor_injury_rows,
    .section("Chest Abbreviated Injury Scale"),
    ais_rows,
    .section("AAST Organ Injury Scale (Lung)"),
    aast_rows,
    .section("Thoracic Irrigation Features"),
    irr_rows,
    .section("Location of Irrigation"),
    loc_rows
  ) %>%
    # Ensure missing totals for headers are empty strings (not NA)
    mutate(Total = replace_na(Total, ""))
  out
}
