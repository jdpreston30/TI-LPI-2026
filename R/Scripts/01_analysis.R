#* 1: Data Import and Cleaning
#+ 1.1: Import Raw Data
#! Must include double rows (bilateral injury patients appear twice)
raw <- read_xlsx(config$paths$primary_data, sheet = "SWSC_data") %>%
  janitor::clean_names() %>%
  select(
    number,
    iss,
    ais_chest,
    aast_lung_grade,
    age,
    sex,
    race,
    mechanism,
    los_days_at_grady_does_not_include_any_ohs,
    diagnosis,
    rib_fx,
    emergent_or,
    operation,
    major_chest_vascular_injury,
    iph,
    pulm_contusion,
    pulmonary_laceration,
    diaphragm_repair,
    failed_thoracic_irrigation,
    hx_of_chest_tube_before_irrigation_no_immediate_tube_then_ti_4hr,
    had_a_vats_despite,
    side_of_irrigation,
    location_of_irrigation,
    ems_to_ti_calc,
    ems_to_vessel_procedure_or_dx_calc,
    vascular_injury_procedure,
    injury_to_chest_tube_time_min,
    chest_tube_size,
    late_irrigation_from_date_of_injury_4,
    early_y_irrigation_4_hours,
    late_irrigator_4_hours,
    total_volume_htx_at_time_of_chest_tube,
    volume_chest_irrigation,
    volume_blood_evacuated,
    total_volume_evcauated,
    hr,
    sbp,
    active_bleeding
  ) %>%
  rename(
    los_days         = los_days_at_grady_does_not_include_any_ohs,
    hx_ct_before_ti  = hx_of_chest_tube_before_irrigation_no_immediate_tube_then_ti_4hr,
    late_irrigation  = late_irrigation_from_date_of_injury_4,
    early_irrigation = early_y_irrigation_4_hours,
    late_irrigator   = late_irrigator_4_hours,
    total_htx_volume = total_volume_htx_at_time_of_chest_tube,
    irrigation_volume = volume_chest_irrigation,
    blood_evacuated  = volume_blood_evacuated,
    total_evacuated  = total_volume_evcauated
  )
#+ 1.2: Pare Down Cohort
raw_pared <- raw %>%
  filter(pulm_contusion == "Y" | pulmonary_laceration == "Y" | iph == "Y") %>%
  mutate(side_of_irrigation = if_else(number == 45, "Bilateral", side_of_irrigation)) %>%
  mutate(side_of_irrigation = if_else(number == 105, "Bilateral", side_of_irrigation)) %>%
  mutate(side_of_irrigation = if_else(number == 106, "Bilateral", side_of_irrigation)) %>%
  mutate(side_of_irrigation = if_else(number == 46, "Bilateral", side_of_irrigation)) %>%
  filter(number != 106) %>%
  filter(number != 46) %>%
  filter(!str_detect(diagnosis, regex("(?<!hemo)pneumothorax", ignore_case = TRUE))) %>%
  #! Patients who had two entries for bilateral, keeping one
  select(number:los_days, sbp, hr, ems_to_ti_calc, ems_to_vessel_procedure_or_dx_calc, vascular_injury_procedure, operation, iss, failed_thoracic_irrigation, had_a_vats_despite, side_of_irrigation, location_of_irrigation, chest_tube_size, total_htx_volume, irrigation_volume, total_evacuated, blood_evacuated, major_chest_vascular_injury, iph, pulm_contusion, pulmonary_laceration, active_bleeding)
#* 2: Data Preparation
#+ 2.1: Structure Variables
test <- raw_pared %>%
  mutate(
    mechanism = str_to_title(mechanism),
    mechanism = factor(mechanism, levels = c("Penetrating", "Blunt"))
  ) %>%
  # derive Y/N for VATS and then make it a factor
  mutate(
    had_a_vats_thor_despite = case_when(
      is.na(had_a_vats_despite) ~ "N",
      str_detect(had_a_vats_despite, regex("^y", ignore_case = TRUE)) ~ "Y",
      str_detect(had_a_vats_despite, regex("^left", ignore_case = TRUE)) ~ "Y",
      str_detect(had_a_vats_despite, regex("^no$", ignore_case = TRUE)) ~ "N",
      had_a_vats_despite == "N" ~ "N",
      TRUE ~ "N"
    ),
    had_a_vats_thor_despite = factor(had_a_vats_thor_despite, levels = c("N", "Y"))
  ) %>%
  # numeric coercions
  mutate(across(
    c(iss, total_htx_volume, blood_evacuated, total_evacuated, irrigation_volume, age),
    ~ suppressWarnings(as.numeric(.))
  )) %>%
  mutate(
    sex = case_when(
      str_detect(sex, regex("^m", ignore_case = TRUE)) ~ "M",
      str_detect(sex, regex("^f", ignore_case = TRUE)) ~ "F",
      TRUE ~ NA_character_
    ),
    sex = factor(sex, levels = c("M", "F"))
  ) %>%
  mutate(across(
    c(
      pulm_contusion, pulmonary_laceration, iph,
      ais_chest, aast_lung_grade, active_bleeding
    ),
    as.factor
  )) %>%
  mutate(
    location_of_irrigation = stringr::str_to_title(location_of_irrigation),
    location_of_irrigation = factor(location_of_irrigation)
  ) %>%
  select(number, active_bleeding, age, sex, iss, mechanism, pulm_contusion, pulmonary_laceration, iph, ais_chest, aast_lung_grade, location_of_irrigation, ems_to_ti_calc, ems_to_vessel_procedure_or_dx_calc, vascular_injury_procedure, total_htx_volume, irrigation_volume, failed_thoracic_irrigation, had_a_vats_thor_despite)
  # filter(is.na(major_chest_vascular_injury)) %>%
  print(test, n = Inf)
#+ 2.2: Deduplicate Bilateral Records
test_dedup <- test %>%
  # map bilateral record numbers to a single patient id
  mutate(patient_id = dplyr::case_when(
    number %in% c(45, 46) ~ 45L,
    number %in% c(105, 106) ~ 105L,
    TRUE ~ as.integer(number)
  )) %>%
  # (optional) choose which row to keep as the "first"
  arrange(patient_id, number) %>% # or arrange(patient_id, desc(total_htx_volume)), etc.
  group_by(patient_id) %>%
  mutate(across(all_of(c("age", "sex", "iss", "mechanism", "ais_chest", "location_of_irrigation", "ems_to_ti_calc", "failed_thoracic_irrigation", "had_a_vats_thor_despite")), ~ replace(.x, dplyr::row_number() > 1, NA))) %>%
  ungroup() %>%
  select(-patient_id, -number) %>%
  mutate(
    # make factors and relabel levels with a prefix
    ais_chest = as.factor(ais_chest) |>
      fct_relabel(~ paste0("AIS chest: ", .x)),
    aast_lung_grade = as.factor(aast_lung_grade) |>
      fct_relabel(~ paste0("AAST lung grade: ", .x))
  )
#+ 2.3: Split Semicolon-Delimited Vascular Injury Column
test_dedup <- test_dedup %>%
  mutate(
    chest_vascular_injury    = str_trim(str_extract(vascular_injury_procedure, "^[^;]+")),
    chest_vascular_procedure = str_trim(str_extract(vascular_injury_procedure, "(?<=;).*$"))
  ) %>%
  select(-vascular_injury_procedure)
#* 3: Descriptive Analysis
#+ 3.1: Run TernG
descriptive_norm <- ternG(
  data = test_dedup,
  group_var = "active_bleeding",
  consider_normality = TRUE,
  open_doc = FALSE
)
#+ 3.2: Clean Table
headers <- c(
  "Cohort Features and Demographics*",
  "Injury Characteristics",
  "Thoracic Injuries",
  "Chest Abbreviated Injury Scale",
  "AAST Organ Injury Scale (Lung)",
  "Thoracic Irrigation Features"
)
descriptive_norm_fixed <- descriptive_norm %>%
  mutate(.section = ifelse(Variable %in% headers, Variable, NA_character_)) %>%
  tidyr::fill(.section) %>%
  mutate(
    # For rows that are just a number under the AIS/AAST sections, add a clean label
    Variable = case_when(
      .section == "Chest Abbreviated Injury Scale" & str_detect(Variable, "^\\s*\\d+\\s*$") ~
        paste0("AIS chest: ", str_trim(Variable)),
      .section == "AAST Organ Injury Scale (Lung)" & str_detect(Variable, "^\\s*\\d+\\s*$") ~
        paste0("AAST lung grade: ", str_trim(Variable)),
      TRUE ~ Variable
    ),
    # Remove any leftover original prefixes that ternG added (prevents "ais_chest: AIS chest: 3")
    Variable = str_replace(Variable, "^ais_chest:\\s*", ""),
    Variable = str_replace(Variable, "^aast_lung_grade:\\s*", "")
  ) %>%
  select(-.section)
#+ 3.4: Export Table
table_disp <- build_descriptive_table(descriptive_norm_fixed, n_total = 122, n_met = 92)
write.xlsx(table_disp, file.path(config$paths$tables, "descriptive_table.xlsx"), overwrite = TRUE)
str(test_dedup)
#* 4: Outcome Rates
#+ 4.1: Failed TI and VATS rates in final cohort
n_pts      <- sum(!is.na(test_dedup$failed_thoracic_irrigation))
n_failed   <- sum(test_dedup$failed_thoracic_irrigation == "Y", na.rm = TRUE)
n_vats     <- sum(test_dedup$had_a_vats_thor_despite == "Y", na.rm = TRUE)
cat("\n─── Outcome Rates (test_dedup) ───\n")
cat(sprintf("Failed Thoracic Irrigation : %d / %d (%.1f%%)\n", n_failed, n_pts, n_failed / n_pts * 100))
cat(sprintf("Had VATS Despite TI        : %d / %d (%.1f%%)\n", n_vats,   n_pts, n_vats   / n_pts * 100))
#+ 4.2: Median volumes — overall cohort
cat("\n─── Median Volumes (test_dedup, all patients) ───\n")
cat(sprintf("Median HTX Volume       : %.0f cc\n", median(test_dedup$total_htx_volume,  na.rm = TRUE)))
cat(sprintf("Median Irrigation Volume: %.0f cc\n", median(test_dedup$irrigation_volume, na.rm = TRUE)))


