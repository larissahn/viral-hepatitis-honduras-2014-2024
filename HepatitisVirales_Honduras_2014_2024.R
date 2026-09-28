# ==============================================================================
# Viral Hepatitis in Honduras, 2014–2024 — Reproducible analytical script
# Author: Larissa Acosta Salgado (ORCID 0000-0002-3583-5070)
#
# Extended-abstract preprint: https://doi.org/10.6084/m9.figshare.34007166
# Consult the README file for reproducibility & data availability information.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------
options(stringsAsFactors = FALSE, scipen = 999)
rm(list = ls())

required_packages <- c(
  "readxl", "dplyr", "stringr", "ggplot2", "tidyr", "readr",
  "forcats", "scales", "patchwork"
)
invisible(lapply(required_packages, library, character.only = TRUE))

PERIOD_START <- 2014
PERIOD_END   <- 2024
PERIOD_LABEL <- paste0(PERIOD_START, "–", PERIOD_END)

FIGURE_LANGUAGES <- c("EN", "ES")
FIGURE_FORMATS   <- c("pdf", "png", "tiff")   # print-ready PDF, high-res PNG, TIFF 300 dpi

SAVE_FIGS <- TRUE   # write figure files (PDF/PNG/TIFF); figures are visualised inline

# ------------------------------------------------------------------------------
# 1. Paths and file inputs
# ------------------------------------------------------------------------------
find_root <- function() {
  marker <- "poblacion_departamental_INE_2014_2024.csv"
  has_marker <- function(d) !is.na(d) && nzchar(d) && file.exists(file.path(d, marker))
  walk_up <- function(start) {
    if (is.null(start) || is.na(start) || !nzchar(start)) return(NA_character_)
    d <- normalizePath(start, mustWork = FALSE)
    repeat {
      if (has_marker(d)) return(d)
      parent <- dirname(d); if (parent == d) return(NA_character_); d <- parent
    }
  }
  if (exists("ROOT", inherits = TRUE) && nzchar(get("ROOT", inherits = TRUE)))
    return(normalizePath(get("ROOT", inherits = TRUE), mustWork = FALSE))
  args <- commandArgs(trailingOnly = FALSE); hit <- grep("^--file=", args)
  starts <- character(0)
  if (length(hit)) starts <- c(starts, dirname(sub("^--file=", "", args[hit[1]])))
  of <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
  if (!is.null(of)) starts <- c(starts, dirname(of))
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))) {
    for (getter in c("getSourceEditorContext", "getActiveDocumentContext")) {
      rs_path <- tryCatch(get(getter, envir = asNamespace("rstudioapi"))()$path,
                          error = function(e) "")
      if (length(rs_path) && nzchar(rs_path)) starts <- c(starts, dirname(rs_path))
    }
  }
  starts <- c(starts, getwd())
  for (s in starts) { r <- walk_up(s); if (!is.na(r)) return(r) }
  stop("Could not locate the project folder (the one containing '", marker,
       "'). Set it manually, e.g.  ROOT <- \"path/to/SHEI Hepatitis 2026\"", call. = FALSE)
}
root <- find_root()
cat(sprintf("[paths] root -> %s\n", root))

path_figures <- file.path(root, "outputs", "figures")
path_tables  <- file.path(root, "outputs", "tables")

for (p in c(path_figures, path_tables)) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE)
}

file_main       <- file.path(root, "2014-2024 Enfermedades Hepaticas.xlsx")
file_2024       <- file.path(root, "2024 Egresos_ Defunciones Enfermedades del Higado.xlsx")
file_population <- file.path(root, "poblacion_departamental_INE_2014_2024.csv")

# ------------------------------------------------------------------------------
# 2. Constants, helpers and visual style
# ------------------------------------------------------------------------------
ICD10_LABELS <- c(
  "B15: Acute hepatitis A",
  "B16: Acute hepatitis B",
  "B17: Other acute viral hepatitis",
  "B18: Chronic viral hepatitis",
  "B19: Viral hepatitis, unspecified"
)

palette_hepatitis <- c(
  "B15" = "#F4A261",
  "B16" = "#E76F51",
  "B17" = "#6D597A",
  "B18" = "#355070",
  "B19" = "#B5838D"
)

normalize_sex <- function(x) {
  dplyr::case_when(
    x == "1 Hombre" ~ "Male",
    x == "2 Mujer"  ~ "Female",
    is.na(x)        ~ NA_character_,
    TRUE            ~ as.character(x)
  )
}

clean_department <- function(name) {
  name |>
    as.character() |>
    gsub("^\\d+\\s+", "", x = _) |>
    trimws()
}

assign_region <- function(dept) {
  dplyr::case_when(
    dept %in% c("Francisco Morazán", "Comayagua", "La Paz")                       ~ "CENTRO",
    dept %in% c("Choluteca", "Valle")                                                 ~ "SUR",
    dept %in% c("Cortés", "Atlántida", "Colón", "Yoro", "Islas de la Bahía") ~ "NORTE",
    dept %in% c("Santa Bárbara", "Copán", "Ocotepeque", "Lempira", "Intibucá") ~ "OCCIDENTE",
    dept %in% c("El Paraíso", "Olancho", "Gracias a Dios")                       ~ "ORIENTE",
    TRUE                                                                              ~ "UNCLASSIFIED"
  )
}

theme_hepatitis_poster <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title         = element_text(face = "bold", size = rel(1.25),
                                        hjust = 0, margin = margin(b = 6)),
      plot.subtitle      = element_text(size = rel(0.95), color = "grey30",
                                        margin = margin(b = 12)),
      plot.caption       = element_text(size = rel(0.72), color = "grey40",
                                        hjust = 0, lineheight = 1.2,
                                        margin = margin(t = 14)),
      axis.title         = element_text(face = "bold", size = rel(0.95)),
      axis.title.x       = element_text(margin = margin(t = 8)),
      axis.title.y       = element_text(margin = margin(r = 8)),
      axis.text          = element_text(size = rel(0.9), color = "grey20"),
      axis.text.x        = element_text(margin = margin(t = 4)),
      legend.position    = "bottom",
      legend.title       = element_text(face = "bold", size = rel(0.9)),
      legend.text        = element_text(size = rel(0.85)),
      legend.key.size    = unit(0.5, "cm"),
      legend.margin      = margin(t = 6),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.margin        = margin(t = 16, r = 16, b = 14, l = 16)
    )
}

wrap_caption <- function(text, width = 105) {
  stringr::str_wrap(text, width = width)
}

header <- function(title) {
  cat("\n", strrep("─", 70), "\n", title, "\n", strrep("─", 70), "\n", sep = "")
}

# Byar's approximation to the exact Poisson CI for the SDR (Breslow & Day, 1987)
add_byar_ci <- function(df, alpha = 0.05) {
  stopifnot(all(df$EXPECTED > 0))
  z <- qnorm(1 - alpha / 2)
  observed <- df$OBSERVED
  expected <- df$EXPECTED
  df$CI_LOW <- ifelse(
    observed == 0, 0,
    (observed * (1 - 1 / (9 * observed) - z / (3 * sqrt(observed)))^3) / expected
  )
  df$CI_HIGH <- ifelse(
    observed == 0,
    -log(alpha / 2) / expected,
    ((observed + 1) * (1 - 1 / (9 * (observed + 1)) + z / (3 * sqrt(observed + 1)))^3) / expected
  )
  df
}

# ------------------------------------------------------------------------------
# 3. Data ingestion
# ------------------------------------------------------------------------------
# Public denominators ship with the repository; a missing file here is a clone problem.
if (!file.exists(file_population))
  stop("Required file not found: ", file_population,
       " (public INE denominators; it ships with the repository).", call. = FALSE)

# SESAL microdata are restricted and are NOT distributed with this repository;
# give the informative message before any generic check can pre-empt it.
if (!file.exists(file_main) || !file.exists(file_2024)) {
  stop("SESAL microdata not found in the project root. These records are ",
       "restricted and are not distributed with this repository; request them ",
       "from SESAL (see README, Data availability). Expected files:\n  - ",
       basename(file_main), "\n  - ", basename(file_2024), call. = FALSE)
}

discharges_2014_2017 <- read_excel(file_main, sheet = "2014-2017 B15-B16.9", skip = 4)
discharges_2018_2024 <- read_excel(file_main, sheet = "2018-2024 Egresos  B15-B19", skip = 4)
deaths_main          <- read_excel(file_main, sheet = "2014-2024 Def. B15-B19", skip = 4)

discharges_2024_full <- read_excel(file_2024, sheet = "2024 Hepatitis Viral ", skip = 4)
deaths_2024_full     <- read_excel(file_2024, sheet = "2024 DEF. B15-B19", skip = 4) |>
  select(-starts_with("..."))

discharges_2018_2023 <- discharges_2018_2024 |> filter(AÑO  < 2024)
deaths_2014_2023     <- deaths_main          |> filter(Año < 2024)

# ------------------------------------------------------------------------------
# 4. Wrangling
# ------------------------------------------------------------------------------
discharges_hepatitis <- bind_rows(
  discharges_2014_2017, discharges_2018_2023, discharges_2024_full
)
deaths_hepatitis <- bind_rows(deaths_2014_2023, deaths_2024_full)

# QC: validate the SESAL age-unit coding (EDAD TIPO) 
edad_tipo_qc <- discharges_hepatitis |>
  filter(str_detect(DIAGNOSTICO, "^B1[5-9]")) |>
  mutate(NUMERO_EDAD = as.numeric(`NUMERO EDAD`)) |>
  group_by(`EDAD TIPO`) |>
  summarise(n = n(),
            min_value = min(NUMERO_EDAD, na.rm = TRUE),
            max_value = max(NUMERO_EDAD, na.rm = TRUE),
            .groups = "drop") |>
  mutate(
    unit = case_when(`EDAD TIPO` == 1 ~ "hours",  `EDAD TIPO` == 2 ~ "days",
                     `EDAD TIPO` == 3 ~ "months", `EDAD TIPO` == 4 ~ "years",
                     TRUE ~ "unknown"),
    max_plausible = case_when(`EDAD TIPO` == 1 ~ 24, `EDAD TIPO` == 2 ~ 31,
                              `EDAD TIPO` == 3 ~ 12, `EDAD TIPO` == 4 ~ 120,
                              TRUE ~ NA_real_),
    within_range = is.na(max_plausible) | max_value < max_plausible
  ) |>
  relocate(`EDAD TIPO`, unit)

header("§4-QC Age-unit coding — EDAD TIPO (1=hours, 2=days, 3=months, 4=years)")
print(edad_tipo_qc)
if (!all(edad_tipo_qc$within_range))
  message("[QC WARNING] An EDAD TIPO unit has a max above its definitional ceiling; ",
          "inspect before trusting the age conversion.")

discharges_2014_2024 <- discharges_hepatitis |>
  filter(str_detect(DIAGNOSTICO, "^B1[5-9]")) |>
  mutate(
    SEX             = factor(normalize_sex(SEXO), levels = c("Female", "Male")),
    DIAGNOSIS_GROUP = factor(str_extract(DIAGNOSTICO, "^B1[5-9]"),
                             levels = c("B15", "B16", "B17", "B18", "B19")),
    AGE_YEARS = case_when(
      `EDAD TIPO` == 4 ~ as.numeric(`NUMERO EDAD`),
      `EDAD TIPO` == 3 ~ as.numeric(`NUMERO EDAD`) / 12,
      `EDAD TIPO` == 2 ~ as.numeric(`NUMERO EDAD`) / 365,
      `EDAD TIPO` == 1 ~ as.numeric(`NUMERO EDAD`) / 8760,
      TRUE             ~ NA_real_
    ),
    DEPT_CLEAN = clean_department(DEPTO_PACIENTE),
    REGION     = assign_region(DEPT_CLEAN)
  )

deaths_2014_2024 <- deaths_hepatitis |>
  filter(str_detect(Causa_Basica, "^B1[5-9]")) |>
  mutate(
    SEX         = factor(normalize_sex(Sexo), levels = c("Female", "Male")),
    CAUSE_GROUP = factor(str_extract(Causa_Basica, "^B1[5-9]"),
                         levels = c("B15", "B16", "B17", "B18", "B19")),
    AGE_YEARS = case_when(
      Tipo_Edad == 4 ~ as.numeric(Numero_Edad),
      Tipo_Edad == 3 ~ as.numeric(Numero_Edad) / 12,
      Tipo_Edad == 2 ~ as.numeric(Numero_Edad) / 365,
      Tipo_Edad == 1 ~ as.numeric(Numero_Edad) / 8760,
      TRUE           ~ NA_real_
    ),
    DEPT_CLEAN = clean_department(Depto_Difunto),
    REGION     = assign_region(DEPT_CLEAN)
  )

# Exclude EDAD TIPO 1 (hours): See README file.
implausible_age_records <- discharges_2014_2024 |> filter(`EDAD TIPO` == 1)
stopifnot(nrow(implausible_age_records) == 4)
discharges_2014_2024 <- discharges_2014_2024 |> filter(`EDAD TIPO` != 1 | is.na(`EDAD TIPO`))

stopifnot(
  nrow(discharges_2014_2024) == 2488,
  nrow(deaths_2014_2024)     == 35
)

header("§4 Wrangling — analytical dataset built")
cat(sprintf("Discharges (B15–B19, EDAD TIPO 1 excluded): %s\n",
            format(nrow(discharges_2014_2024), big.mark = ",")))
cat(sprintf("Deaths     (B15–B19, underlying cause):     %s\n",
            format(nrow(deaths_2014_2024), big.mark = ",")))
cat(sprintf("Records excluded (biologically implausible): %d\n",
            nrow(implausible_age_records)))

# QC (composition / reconciliation): make the case definition auditable at a glance
composition_reconciled <- discharges_hepatitis |>
  filter(str_detect(DIAGNOSTICO, "^B1[5-9]")) |>
  mutate(
    DIAGNOSIS_GROUP = factor(str_extract(DIAGNOSTICO, "^B1[5-9]"),
                             levels = c("B15", "B16", "B17", "B18", "B19")),
    STATUS = if_else(`EDAD TIPO` == 1 & !is.na(`EDAD TIPO`),
                     "Excluded (EDAD TIPO 1 = hours)", "Counted (analytical)")
  ) |>
  count(STATUS, DIAGNOSIS_GROUP, name = "n", .drop = FALSE) |>
  pivot_wider(names_from = DIAGNOSIS_GROUP, values_from = n, values_fill = 0) |>
  mutate(TOTAL = rowSums(across(-STATUS))) |>
  arrange(desc(TOTAL))

# Freeze the reconciliation: counted (2,488) + excluded (4) = raw B15–B19 (2,492).
counted_total  <- composition_reconciled$TOTAL[composition_reconciled$STATUS == "Counted (analytical)"]
excluded_total <- composition_reconciled$TOTAL[composition_reconciled$STATUS == "Excluded (EDAD TIPO 1 = hours)"]
stopifnot(counted_total == 2488, excluded_total == 4,
          counted_total + excluded_total == sum(composition_reconciled$TOTAL))

header("§4-QC Composition — counted vs excluded B15–B19 discharges, by subtype")
print(composition_reconciled, n = Inf)
cat(sprintf("Reconciliation: %d counted + %d excluded = %d raw B15–B19 rows.\n",
            counted_total, excluded_total, counted_total + excluded_total))

composition_reconciled |>
  filter(STATUS == "Counted (analytical)") |>
  pivot_longer(B15:B19, names_to = "ICD", values_to = "n") |>
  mutate(pct = round(n / TOTAL * 100, 1)) |>
  select(ICD, n, pct)

# ------------------------------------------------------------------------------
# 5. Reference data: INE population and AES-SIVAC coverage
# ------------------------------------------------------------------------------
population_department <- read_csv(file_population, comment = "#", show_col_types = FALSE) |>
  mutate(REGION = assign_region(DEPT))

stopifnot(
  nrow(population_department) == 198,
  sum(population_department$POPULATION[population_department$YEAR == 2024]) == 9892632
)

population_national <- population_department |>
  group_by(YEAR) |>
  summarise(POPULATION_NAC = sum(POPULATION), .groups = "drop")

person_years_regional <- population_department |>
  group_by(REGION) |>
  summarise(PERSON_YEARS = sum(POPULATION), .groups = "drop") |>
  arrange(desc(PERSON_YEARS))

vaccination_coverage <- tibble(
  YEAR   = 2014:2024,
  HEPB   = c(68.9, 72.1, 83.1, 80.8, 81.7, 77.5, 71.0, 72.4, 69.4, 64.3, 65.6),
  PENTA3 = c(84.8, 85.3, 103.7, 90.0, 91.0, 88.0, 80.3, 77.3, 77.8, 72.5, 75.1),
  # HEPA series kept for documentation only; not used in analysis (introduced
  # in Honduras in 2020, too short relative to the 11-year burden series).
  HEPA   = c(  NA,   NA,    NA,   NA,   NA,   NA, 77.3, 78.7, 76.9, 77.1, 78.5)
)

header("§5 Reference data — INE population and PAI coverage")
cat(sprintf("National population 2024:   %s\n",
            format(population_national$POPULATION_NAC[population_national$YEAR == 2024], big.mark = ",")))
cat(sprintf("Cumulative person-years:    %s\n",
            format(sum(population_national$POPULATION_NAC), big.mark = ",")))
cat("\nHepB neonatal coverage (national, %):\n")
print(vaccination_coverage |> select(YEAR, HEPB))

# ------------------------------------------------------------------------------
# 6. Descriptive statistics
# ------------------------------------------------------------------------------
total_discharges <- nrow(discharges_2014_2024)
total_deaths     <- nrow(deaths_2014_2024)

stopifnot(
  total_discharges == 2488,
  total_deaths     == 35
)

discharges_by_type <- discharges_2014_2024 |>
  count(DIAGNOSIS_GROUP, name = "n") |>
  mutate(percent = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

age_by_type <- discharges_2014_2024 |>
  filter(!is.na(AGE_YEARS), !is.na(DIAGNOSIS_GROUP)) |>
  group_by(DIAGNOSIS_GROUP) |>
  summarise(
    n      = n(),
    median = median(AGE_YEARS),
    Q1     = quantile(AGE_YEARS, 0.25, names = FALSE),
    Q3     = quantile(AGE_YEARS, 0.75, names = FALSE),
    min    = min(AGE_YEARS),
    max    = max(AGE_YEARS),
    .groups = "drop"
  )

b15_paediatric <- discharges_2014_2024 |>
  filter(DIAGNOSIS_GROUP == "B15", !is.na(AGE_YEARS)) |>
  summarise(
    n_B15       = n(),
    n_5_to_15   = sum(AGE_YEARS >= 5 & AGE_YEARS <= 15),
    pct_5_to_15 = round(n_5_to_15 / n_B15 * 100, 1),
    median      = median(AGE_YEARS),
    Q1          = quantile(AGE_YEARS, 0.25, names = FALSE),
    Q3          = quantile(AGE_YEARS, 0.75, names = FALSE)
  )

# Check if concentration of the unspecified group (B19) mirrors B15
b19_paediatric <- discharges_2014_2024 |>
  filter(DIAGNOSIS_GROUP == "B19", !is.na(AGE_YEARS)) |>
  summarise(
    n_B19       = n(),
    n_5_to_15   = sum(AGE_YEARS >= 5 & AGE_YEARS <= 15),
    pct_5_to_15 = round(n_5_to_15 / n_B19 * 100, 1),
    median      = median(AGE_YEARS),
    Q1          = quantile(AGE_YEARS, 0.25, names = FALSE),
    Q3          = quantile(AGE_YEARS, 0.75, names = FALSE)
  )

sex_by_type <- discharges_2014_2024 |>
  filter(!is.na(SEX), !is.na(DIAGNOSIS_GROUP)) |>
  count(DIAGNOSIS_GROUP, SEX, name = "n") |>
  group_by(DIAGNOSIS_GROUP) |>
  mutate(percent = round(n / sum(n) * 100, 1)) |>
  ungroup()

# Age-structure comparison, B15 (hepatitis A) vs B19 (unspecified viral hepatitis)
age_profile_b15_vs_b19 <- discharges_2014_2024 |>
  filter(DIAGNOSIS_GROUP %in% c("B15", "B19"), !is.na(AGE_YEARS)) |>
  group_by(DIAGNOSIS_GROUP) |>
  summarise(
    n           = n(),
    median      = median(AGE_YEARS),
    Q1          = quantile(AGE_YEARS, 0.25, names = FALSE),
    Q3          = quantile(AGE_YEARS, 0.75, names = FALSE),
    pct_5_to_15 = round(mean(AGE_YEARS >= 5 & AGE_YEARS <= 15) * 100, 1),
    pct_le_15   = round(mean(AGE_YEARS <= 15) * 100, 1),
    pct_le_18   = round(mean(AGE_YEARS <= 18) * 100, 1),
    .groups = "drop"
  ) |>
  left_join(
    sex_by_type |>
      filter(DIAGNOSIS_GROUP %in% c("B15", "B19"), SEX == "Female") |>
      transmute(DIAGNOSIS_GROUP, pct_female = percent),
    by = "DIAGNOSIS_GROUP"
  )

top_departments <- discharges_2014_2024 |>
  filter(!is.na(DEPT_CLEAN)) |>
  count(DEPT_CLEAN, name = "n") |>
  mutate(percent = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

discharges_by_region <- discharges_2014_2024 |>
  filter(REGION != "UNCLASSIFIED", !is.na(REGION)) |>
  count(REGION, name = "n") |>
  mutate(percent = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

deaths_by_type <- deaths_2014_2024 |>
  count(CAUSE_GROUP, name = "n") |>
  mutate(percent = round(n / sum(n) * 100, 1)) |>
  arrange(desc(n))

# Etiological concentration of mortality (reviewer comment)
deaths_concentration <- deaths_by_type |>
  mutate(cum_percent = round(cumsum(percent), 1))
top2_deaths     <- head(deaths_concentration, 2)
top2_deaths_pct <- round(sum(top2_deaths$percent), 1)

deaths_by_sex <- deaths_2014_2024 |>
  filter(!is.na(SEX)) |>
  count(SEX, name = "n") |>
  mutate(percent = round(n / sum(n) * 100, 1))

# Supplementary Table S2 — ICD-10 subcode distribution (discharges and deaths)
icd_subcode_lookup <- tibble::tribble(
  ~ICD_FULL, ~DESCRIPTION_WHO,
  "B15.0", "Acute hepatitis A with hepatic coma",
  "B15.9", "Acute hepatitis A without hepatic coma",
  "B16.0", "Acute hepatitis B with delta-agent (coinfection) with hepatic coma",
  "B16.1", "Acute hepatitis B with delta-agent (coinfection) without hepatic coma",
  "B16.2", "Acute hepatitis B without delta-agent with hepatic coma",
  "B16.9", "Acute hepatitis B without delta-agent and without hepatic coma",
  "B17.0", "Acute delta-(super)infection of hepatitis B carrier",
  "B17.1", "Acute hepatitis C",
  "B17.2", "Acute hepatitis E",
  "B17.8", "Other specified acute viral hepatitis",
  "B18.0", "Chronic viral hepatitis B with delta-agent",
  "B18.1", "Chronic viral hepatitis B without delta-agent",
  "B18.2", "Chronic viral hepatitis C",
  "B18.8", "Other chronic viral hepatitis",
  "B18.9", "Chronic viral hepatitis, unspecified",
  "B19.0", "Unspecified viral hepatitis with hepatic coma",
  "B19.9", "Unspecified viral hepatitis without hepatic coma"
)

subcodes_discharges <- discharges_2014_2024 |>
  mutate(ICD_FULL = str_extract(DIAGNOSTICO, "^B\\d{2}\\.\\d")) |>
  count(DIAGNOSIS_GROUP, ICD_FULL, name = "n_discharges") |>
  group_by(DIAGNOSIS_GROUP) |>
  mutate(pct_within_group = round(n_discharges / sum(n_discharges) * 100, 1)) |>
  ungroup() |>
  mutate(pct_overall = round(n_discharges / total_discharges * 100, 2))

subcodes_deaths <- deaths_2014_2024 |>
  mutate(ICD_FULL = str_extract(Causa_Basica, "^B\\d{2}\\.\\d")) |>
  count(ICD_FULL, name = "n_deaths")

supplementary_table_S2 <- icd_subcode_lookup |>
  left_join(subcodes_discharges, by = "ICD_FULL") |>
  left_join(subcodes_deaths,     by = "ICD_FULL") |>
  mutate(
    n_discharges     = tidyr::replace_na(n_discharges, 0L),
    n_deaths         = tidyr::replace_na(n_deaths,     0L),
    pct_within_group = tidyr::replace_na(pct_within_group, 0),
    pct_overall      = tidyr::replace_na(pct_overall,      0)
  ) |>
  arrange(ICD_FULL)

# Supplementary Table S3 — Sensitivity analysis on age-coding exclusions.
# Three definitions of the discharges denominator are compared so that
# reviewers can assess robustness to the EDAD TIPO 1 (hours) exclusion.
sensitivity_age_exclusion <- tibble::tribble(
  ~definition,                                  ~n_total, ~n_B15, ~n_B16, ~n_B17, ~n_B18, ~n_B19,
  "Inclusive (all EDAD TIPO 1-4)",                  2492,   1457,    189,    152,     67,    627,
  "Primary (exclude EDAD TIPO 1 = hours)",          2488,   1455,    189,    152,     67,    625,
  "Sensitivity (exclude EDAD TIPO 1 and 2)",        2481,   1450,    189,    152,     67,    623
)

header("§6 Descriptive statistics — ICD-10 distribution and age profile")
cat("\nDischarges by ICD-10 subtype:\n")
print(discharges_by_type)
cat("\nAge by ICD-10 subtype (years):\n")
print(age_by_type)
cat(sprintf("\nB15 paediatric concentration (5–15 years): %d/%d (%.1f%%)\n",
            b15_paediatric$n_5_to_15, b15_paediatric$n_B15, b15_paediatric$pct_5_to_15))
cat(sprintf("B19 paediatric concentration (5–15 years): %d/%d (%.1f%%)\n",
            b19_paediatric$n_5_to_15, b19_paediatric$n_B19, b19_paediatric$pct_5_to_15))
cat("\nAge-structure comparison — B15 (hepatitis A) vs B19 (unspecified):\n")
print(age_profile_b15_vs_b19)
cat("\nDischarges by macroregion:\n")
print(discharges_by_region)
cat("\nTop 5 departments (absolute counts):\n")
print(head(top_departments, 5))
cat("\nDeaths by underlying cause:\n")
print(deaths_by_type)
cat("\nMortality concentration by ICD-10 group (deaths, cumulative %):\n")
print(deaths_concentration)
cat(sprintf("Two leading groups (%s + %s) concentrate %.1f%% of the %d deaths.\n",
            top2_deaths$CAUSE_GROUP[1], top2_deaths$CAUSE_GROUP[2],
            top2_deaths_pct, total_deaths))

# ------------------------------------------------------------------------------
# 7. Annual crude rates
# ------------------------------------------------------------------------------
national_rates_discharges <- discharges_2014_2024 |>
  count(AÑO, name = "DISCHARGES") |>
  right_join(population_national |> rename(AÑO = YEAR), by = "AÑO") |>
  mutate(
    DISCHARGES     = tidyr::replace_na(DISCHARGES, 0L),
    RATE_CRUDE_RAW = (DISCHARGES / POPULATION_NAC) * 100000,
    RATE_CRUDE     = round(RATE_CRUDE_RAW, 3)
  ) |>
  arrange(AÑO)

national_rates_deaths <- deaths_2014_2024 |>
  count(Año, name = "DEATHS") |>
  rename(AÑO = Año) |>
  right_join(population_national |> rename(AÑO = YEAR), by = "AÑO") |>
  mutate(
    DEATHS         = tidyr::replace_na(DEATHS, 0L),
    RATE_CRUDE_RAW = (DEATHS / POPULATION_NAC) * 100000,
    RATE_CRUDE     = round(RATE_CRUDE_RAW, 3)
  ) |>
  arrange(AÑO)

stopifnot(
  sum(national_rates_discharges$DISCHARGES) == nrow(discharges_2014_2024),
  sum(national_rates_deaths$DEATHS) == nrow(deaths_2014_2024)
)

national_rates_by_type <- discharges_2014_2024 |>
  count(AÑO, DIAGNOSIS_GROUP, name = "DISCHARGES") |>
  left_join(population_national |> rename(AÑO = YEAR), by = "AÑO") |>
  mutate(RATE_CRUDE = round((DISCHARGES / POPULATION_NAC) * 100000, 3)) |>
  arrange(DIAGNOSIS_GROUP, AÑO)

geographic_concentration <- discharges_2014_2024 |>
  filter(!is.na(DEPT_CLEAN)) |>
  count(DEPT_CLEAN, name = "DISCHARGES") |>
  mutate(PERCENT = round(DISCHARGES / sum(DISCHARGES) * 100, 2)) |>
  arrange(desc(DISCHARGES))

peak_year   <- national_rates_discharges |> slice_max(RATE_CRUDE_RAW, n = 1) |> pull(AÑO)
peak_rate   <- national_rates_discharges |> slice_max(RATE_CRUDE_RAW, n = 1) |> pull(RATE_CRUDE)
trough_year <- national_rates_discharges |> slice_min(RATE_CRUDE_RAW, n = 1) |> pull(AÑO)
trough_rate <- national_rates_discharges |> slice_min(RATE_CRUDE_RAW, n = 1) |> pull(RATE_CRUDE)
rate_2024   <- national_rates_discharges |> filter(AÑO == 2024) |> pull(RATE_CRUDE)
rate_2014   <- national_rates_discharges |> filter(AÑO == 2014) |> pull(RATE_CRUDE)

header("§7 Annual crude rates — key narrative metrics")
cat("\nNational discharge rate per year (/100,000):\n")
print(national_rates_discharges |> select(AÑO, DISCHARGES, RATE_CRUDE))
cat(sprintf("\nPeak rate:       %.2f/100,000 in %d\n", peak_rate,   peak_year))
cat(sprintf("Trough rate:     %.2f/100,000 in %d  (%+.1f%% vs peak)\n",
            trough_rate, trough_year, (trough_rate - peak_rate) / peak_rate * 100))
cat(sprintf("Rate 2024:       %.2f/100,000          (%+.1f%% vs trough)\n",
            rate_2024, (rate_2024 - trough_rate) / trough_rate * 100))

# QC (spot-check): 2021 counts
qc_2021_by_subtype <- discharges_2014_2024 |>
  filter(AÑO == 2021) |>
  mutate(ICD_FULL = str_extract(DIAGNOSTICO, "^B\\d{2}\\.\\d")) |>
  count(DIAGNOSIS_GROUP, ICD_FULL, name = "n") |>
  arrange(desc(n))

header("§7-QC Spot-check — 2021 (pandemic trough) discharges by ICD-10 subcode")
print(qc_2021_by_subtype, n = Inf)
cat(sprintf("2021 total: %d discharges across %d distinct subcodes; national rate %.2f/100,000.\n",
            sum(qc_2021_by_subtype$n), nrow(qc_2021_by_subtype),
            national_rates_discharges$RATE_CRUDE[national_rates_discharges$AÑO == 2021]))

# ------------------------------------------------------------------------------
# 8. Hepatitis Delta (HDV) — stratified by clinical presentation
# ------------------------------------------------------------------------------
delta_codes <- c("B16.0", "B16.1", "B17.0", "B18.0")

delta_cases <- discharges_2014_2024 |>
  mutate(ICD_FULL = str_extract(DIAGNOSTICO, "^B\\d{2}\\.\\d")) |>
  filter(ICD_FULL %in% delta_codes) |>
  mutate(
    DELTA_PRESENTATION = factor(case_when(
      ICD_FULL %in% c("B16.0", "B16.1") ~ "Acute coinfection (B16.0/B16.1)",
      ICD_FULL == "B17.0"               ~ "Superinfection in HBV carrier (B17.0)",
      ICD_FULL == "B18.0"               ~ "Chronic HBV with delta (B18.0)"
    ), levels = c("Acute coinfection (B16.0/B16.1)",
                  "Superinfection in HBV carrier (B17.0)",
                  "Chronic HBV with delta (B18.0)"))
  )

stopifnot(nrow(delta_cases) == 15)

delta_summary <- delta_cases |>
  group_by(DELTA_PRESENTATION) |>
  summarise(
    n                = n(),
    n_female         = sum(SEX == "Female", na.rm = TRUE),
    n_male           = sum(SEX == "Male",   na.rm = TRUE),
    median_age       = median(AGE_YEARS, na.rm = TRUE),
    min_age          = min(AGE_YEARS, na.rm = TRUE),
    max_age          = max(AGE_YEARS, na.rm = TRUE),
    n_in_hosp_deaths = sum(`CONDICION SALIDA` == "4 Fallecido", na.rm = TRUE),
    .groups = "drop"
  )

delta_case_list <- delta_cases |>
  transmute(
    YEAR              = AÑO,
    MONTH             = MES,
    PRESENTATION      = DELTA_PRESENTATION,
    ICD_FULL,
    SEX,
    AGE_YEARS         = round(AGE_YEARS, 1),
    DEPARTMENT        = DEPT_CLEAN,
    REGION,
    DIAGNOSIS_ORDER   = CODIGO_ORDEN_AFECCION,
    DISCHARGE_OUTCOME = `CONDICION SALIDA`
  ) |>
  arrange(YEAR, MONTH)

# QC: HDV component counted per calendar year and clinical presentation
delta_by_year <- delta_cases |>
  mutate(YEAR = factor(AÑO, levels = PERIOD_START:PERIOD_END)) |>
  count(YEAR, DELTA_PRESENTATION, name = "n", .drop = FALSE) |>
  mutate(YEAR = as.integer(as.character(YEAR)))

delta_by_year_wide <- delta_by_year |>
  pivot_wider(names_from = DELTA_PRESENTATION, values_from = n, values_fill = 0) |>
  mutate(TOTAL = rowSums(across(-YEAR))) |>
  arrange(YEAR)

# Reconcile the annual grid against the case count
stopifnot(sum(delta_by_year_wide$TOTAL) == nrow(delta_cases))

header("§8 Hepatitis Delta (HDV) — stratified summary")
print(delta_summary)
cat(sprintf("\nTotal HDV-related episodes 2014–2024: %d (of which %d in-hospital deaths)\n",
            nrow(delta_cases),
            sum(delta_cases$`CONDICION SALIDA` == "4 Fallecido", na.rm = TRUE)))

# QC (console): delta episodes by year × presentation, with delta-free years shown
header("§8-QC Delta component by year × presentation (0 = no HDV episode)")
print(delta_by_year_wide, n = Inf)
cat(sprintf("Delta-active years: %d of %d (none in %s); grid reconciles to %d episodes.\n",
            sum(delta_by_year_wide$TOTAL > 0), nrow(delta_by_year_wide),
            paste(delta_by_year_wide$YEAR[delta_by_year_wide$TOTAL == 0], collapse = ", "),
            sum(delta_by_year_wide$TOTAL)))

# ------------------------------------------------------------------------------
# 8b. Hepatitis C (HCV) — acute (B17.1) + chronic (B18.2), aggregated across groups
# ------------------------------------------------------------------------------
hcv_codes <- c("B17.1", "B18.2")

hcv_discharges <- discharges_2014_2024 |>
  mutate(ICD_FULL = str_extract(DIAGNOSTICO, "^B\\d{2}\\.\\d")) |>
  filter(ICD_FULL %in% hcv_codes)

hcv_deaths <- deaths_2014_2024 |>
  mutate(ICD_FULL = str_extract(Causa_Basica, "^B\\d{2}\\.\\d")) |>
  filter(ICD_FULL %in% hcv_codes)

hepatitis_c <- tibble::tibble(
  HCV_PRESENTATION = c("Acute (B17.1)", "Chronic (B18.2)"),
  discharges = c(sum(hcv_discharges$ICD_FULL == "B17.1"),
                 sum(hcv_discharges$ICD_FULL == "B18.2")),
  deaths     = c(sum(hcv_deaths$ICD_FULL == "B17.1"),
                 sum(hcv_deaths$ICD_FULL == "B18.2"))
)

stopifnot(sum(hepatitis_c$discharges) == 49, sum(hepatitis_c$deaths) == 4)

header("§8b Hepatitis C (HCV) — acute B17.1 + chronic B18.2")
print(hepatitis_c)
cat(sprintf("\nHepatitis C 2014–2024: %d discharges (%d acute + %d chronic), %d deaths.\n",
            sum(hepatitis_c$discharges), hepatitis_c$discharges[1],
            hepatitis_c$discharges[2], sum(hepatitis_c$deaths)))

# ------------------------------------------------------------------------------
# 9. Department-level epidemiological profile (SDR for discharges)
# ------------------------------------------------------------------------------
person_years_dept <- population_department |>
  group_by(DEPT) |>
  summarise(PERSON_YEARS = sum(POPULATION), .groups = "drop")

national_rate_disc <- total_discharges / sum(person_years_dept$PERSON_YEARS)

sdr_discharges <- discharges_2014_2024 |>
  filter(!is.na(DEPT_CLEAN)) |>
  count(DEPT_CLEAN, name = "OBSERVED") |>
  rename(DEPT = DEPT_CLEAN) |>
  right_join(person_years_dept, by = "DEPT") |>
  mutate(
    OBSERVED = tidyr::replace_na(OBSERVED, 0L),
    EXPECTED = PERSON_YEARS * national_rate_disc,
    SDR      = OBSERVED / EXPECTED
  ) |>
  add_byar_ci() |>
  mutate(
    SDR     = round(SDR, 2),
    CI_LOW  = round(CI_LOW, 2),
    CI_HIGH = round(CI_HIGH, 2),
    PROFILE = case_when(
      CI_LOW > 1  ~ "Above expected",
      CI_HIGH < 1 ~ "Below expected",
      TRUE        ~ "Within expected"
    )
  ) |>
  arrange(desc(SDR))

# Transparency of the SDR base: the right_join keeps only the 18 INE departments,
# so any discharge whose (cleaned) department is not among them is not counted here.
sdr_observed_total <- sum(sdr_discharges$OBSERVED)
sdr_outside        <- total_discharges - sdr_observed_total
if (sdr_outside != 0)
  message("[QC] SDR base: ", sdr_observed_total, " of ", total_discharges,
          " discharges mapped to the 18 INE departments; ", sdr_outside,
          " fall outside them (unmatched/foreign department) and are excluded ",
          "from the departmental SDR.")

header("§9 Department-level SDR — epidemiological profile")
print(sdr_discharges |> select(DEPT, OBSERVED, EXPECTED, SDR, CI_LOW, CI_HIGH, PROFILE))
cat("\nAbove expected:\n  ")
cat(paste(sdr_discharges$DEPT[sdr_discharges$PROFILE == "Above expected"], collapse = ", "), "\n")
cat("Below expected:\n  ")
cat(paste(sdr_discharges$DEPT[sdr_discharges$PROFILE == "Below expected"], collapse = ", "), "\n")
cortes <- sdr_discharges |> filter(DEPT == "Cortés")
cat(sprintf("\nCortés paradox: n=%d (rank #%d) but SDR=%.2f [%.2f–%.2f] (%s)\n",
            cortes$OBSERVED,
            which(sdr_discharges$DEPT[order(-sdr_discharges$OBSERVED)] == "Cortés"),
            cortes$SDR, cortes$CI_LOW, cortes$CI_HIGH, cortes$PROFILE))

# ------------------------------------------------------------------------------
# 10. Bilingual figures (EN/ES)
# ------------------------------------------------------------------------------
i18n <- list(
  EN = list(
    pandemic_band  = "Pandemic period",
    who_target     = "WHO target: 90%",
    fig1_title     = "Hospital discharges due to viral hepatitis",
    fig1_subtitle  = "Honduras, 2014–2024 (cumulative count by ICD-10 code)",
    fig1_x         = "Number of discharges",
    fig1_caption   = "Source: SESAL, hospital discharges, ICD-10. B15: acute hepatitis A; B16: acute hepatitis B; B17: other acute viral; B18: chronic; B19: unspecified.",
    fig2_title     = "Age distribution by viral hepatitis subtype",
    fig2_subtitle  = "Honduras, 2014–2024 (hospital discharges, B15–B19)",
    fig2_x         = "Age (years)",
    fig2_caption   = "Source: SESAL, hospital discharges. Age in fractional years. ICD-10 codes B15–B19.",
    fig3_title     = "Temporal trend of viral hepatitis in Honduras",
    fig3_subtitle  = "Hospital discharge rate per 100,000 inhabitants, 2014–2024",
    fig3_x         = "Year",
    fig3_y         = "Discharge rate per 100,000 inhabitants",
    fig3_caption   = "Source: SESAL (B15–B19 discharges) and INE (mid-year population). Grey band: COVID-19 pandemic (2020–2022).",
    fig4_title     = "EPI vaccination coverage in Honduras, 2014–2024",
    fig4_y_cov     = "Coverage",
    fig4_vac_hepb  = "HepB birth dose",
    fig4_vac_penta = "Pentavalent 3rd dose",
    fig4_caption   = "Sources: AES-SIVAC/SESAL (coverage), INE (denominators). Descriptive figure.",
    fig5_title     = "Department-level epidemiological profile",
    fig5_subtitle  = "Standardized Discharge Ratio for viral hepatitis, Honduras 2014–2024",
    fig5_x         = "SDR (95% CI, Byar)",
    fig5_caption   = "Source: SESAL (B15–B19 discharges) and INE (mid-year population). 95% CI by Byar's approximation. Dashed line at SDR=1.",
    fig5_legend    = c("Above expected" = "Above expected",
                       "Within expected" = "Within expected",
                       "Below expected"  = "Below expected"),
    fig6_title     = "HBsAg vaccine-escape map (genotype-F Honduras)",
    fig6_subtitle  = "adw2 vaccine vs genotype-F adw4 and the 5 Honduran TEG",
    fig6_x         = "HBsAg residue position (SHBs protein, Met1 numbering)",
    fig6_same      = "Same as vaccine",
    fig6_diff      = "Differs from vaccine",
    fig6_caption   = "In silico, sequence-level comparison; n = 5 Honduran sequences (1997, partial S gene); not an immunological assay.",
    fig6_rows      = c(vaccine_adw2 = "Vaccine adw2 (A2)", F_prototype_adw4 = "F prototype adw4",
                       F_consensus_adw4 = "F consensus adw4", TEG_adw4 = "TEG adw4 (Honduras)")
  ),
  ES = list(
    pandemic_band  = "Periodo pandémico",
    who_target     = "Meta OMS: 90%",
    fig1_title     = "Egresos hospitalarios por Hepatitis Viral",
    fig1_subtitle  = "Honduras, 2014–2024 (total acumulado por código CIE-10)",
    fig1_x         = "Número de egresos",
    fig1_caption   = "Fuente: SESAL, egresos hospitalarios CIE-10. B15: hepatitis A aguda; B16: hepatitis B aguda; B17: otras virales agudas; B18: crónica; B19: no especificada.",
    fig2_title     = "Distribución de edad por Hepatitis Viral",
    fig2_subtitle  = "Honduras, 2014–2024 (egresos hospitalarios, B15–B19)",
    fig2_x         = "Edad (años)",
    fig2_caption   = "Fuente: SESAL, egresos hospitalarios. Edad en años fraccionales. Códigos CIE-10 B15–B19.",
    fig3_title     = "Tendencia temporal de Hepatitis Viral en Honduras",
    fig3_subtitle  = "Tasa de egresos hospitalarios por 100,000 habitantes, 2014–2024",
    fig3_x         = "Año",
    fig3_y         = "Tasa de egresos por 100,000 hab.",
    fig3_caption   = "Fuente: SESAL (egresos B15–B19) e INE (población a mitad de año). Banda gris: pandemia COVID-19 (2020–2022).",
    fig4_title     = "Coberturas del PAI en Honduras, 2014–2024",
    fig4_y_cov     = "Cobertura",
    fig4_vac_hepb  = "HepB recién nacido",
    fig4_vac_penta = "Pentavalente 3ras dosis",
    fig4_caption   = "Fuentes: AES-SIVAC/SESAL (coberturas), INE (denominadores). Figura descriptiva.",
    fig5_title     = "Perfil epidemiológico departamental",
    fig5_subtitle  = "Razón estandarizada de egresos por hepatitis viral, Honduras 2014–2024",
    fig5_x         = "SDR (IC 95%, Byar)",
    fig5_caption   = "Fuente: SESAL (egresos B15–B19) e INE (población a mitad de año). IC 95% por aproximación de Byar. Línea punteada en SDR=1.",
    fig5_legend    = c("Above expected" = "Por encima de lo esperado",
                       "Within expected" = "Dentro de lo esperado",
                       "Below expected"  = "Por debajo de lo esperado"),
    fig6_title     = "Mapa de escape vacunal del HBsAg (genotipo F, Honduras)",
    fig6_subtitle  = "Vacuna adw2 vs genotipo F adw4 y los 5 TEG hondureños",
    fig6_x         = "Posición de residuo del HBsAg (proteína SHBs, numeración Met1)",
    fig6_same      = "Igual que la vacuna",
    fig6_diff      = "Difiere de la vacuna",
    fig6_caption   = "Comparación in silico a nivel de secuencia; n = 5 secuencias hondureñas (1997, gen S parcial); no es un ensayo inmunológico.",
    fig6_rows      = c(vaccine_adw2 = "Vacuna adw2 (A2)", F_prototype_adw4 = "Prototipo F adw4",
                       F_consensus_adw4 = "Consenso F adw4", TEG_adw4 = "TEG adw4 (Honduras)")
  )
)

fig_path <- function(stem, lang, ext) {
  file.path(path_figures, sprintf("%s_2014_2024_%s.%s", stem, tolower(lang), ext))
}

save_figure <- function(plot, stem, lang, width, height) {
  if (!isTRUE(SAVE_FIGS)) return(invisible(plot))
  pdf_device <- if (isTRUE(capabilities("cairo"))) cairo_pdf else grDevices::pdf
  if ("pdf" %in% FIGURE_FORMATS) {
    tryCatch(
      ggsave(fig_path(stem, lang, "pdf"), plot, width = width, height = height,
             units = "in", device = pdf_device),
      error = function(e) warning("PDF not written for ", stem, " (", lang,
                                  "): ", conditionMessage(e),
                                  " - is the file open in a viewer?"))
  }
  if ("png" %in% FIGURE_FORMATS) {
    tryCatch(
      ggsave(fig_path(stem, lang, "png"), plot, width = width, height = height,
             units = "in", dpi = 600),
      error = function(e) warning("PNG not written for ", stem, " (", lang,
                                  "): ", conditionMessage(e)))
  }
  if ("tiff" %in% FIGURE_FORMATS) {
    tryCatch(
      ggsave(fig_path(stem, lang, "tiff"), plot, width = width, height = height,
             units = "in", dpi = 300, compression = "lzw"),
      error = function(e) warning("TIFF not written for ", stem, " (", lang,
                                  "): ", conditionMessage(e)))
  }
}

make_figure1 <- function(lang) {
  L <- i18n[[lang]]
  datos <- discharges_2014_2024 |>
    count(DIAGNOSIS_GROUP, name = "DISCHARGES") |>
    mutate(
      PERCENT         = DISCHARGES / sum(DISCHARGES) * 100,
      LABEL           = paste0(format(DISCHARGES, big.mark = ","),
                               " (", sprintf("%.1f", PERCENT), "%)"),
      DIAGNOSIS_GROUP = fct_reorder(DIAGNOSIS_GROUP, DISCHARGES)
    )

  p <- ggplot(datos, aes(x = DISCHARGES, y = DIAGNOSIS_GROUP, fill = DIAGNOSIS_GROUP)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = LABEL), hjust = -0.08, size = 4,
              color = "grey15", fontface = "bold") +
    scale_fill_manual(values = palette_hepatitis, guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.18)), labels = label_comma()) +
    labs(title = L$fig1_title, subtitle = L$fig1_subtitle,
         x = L$fig1_x, y = NULL, caption = wrap_caption(L$fig1_caption)) +
    theme_hepatitis_poster() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
          axis.text.y        = element_text(face = "bold", size = rel(0.95)))

  p
}

print(make_figure1("EN"))

make_figure2 <- function(lang) {
  L <- i18n[[lang]]
  datos <- discharges_2014_2024 |>
    filter(!is.na(AGE_YEARS), !is.na(DIAGNOSIS_GROUP))

  n_per_type <- datos |>
    count(DIAGNOSIS_GROUP, name = "n") |>
    mutate(Y_LABEL = paste0(DIAGNOSIS_GROUP, "\n(n = ", format(n, big.mark = ","), ")"))

  datos <- datos |>
    left_join(n_per_type, by = "DIAGNOSIS_GROUP") |>
    mutate(Y_LABEL = factor(Y_LABEL, levels = n_per_type$Y_LABEL))

  p <- ggplot(datos, aes(x = AGE_YEARS, y = Y_LABEL, fill = DIAGNOSIS_GROUP)) +
    geom_violin(alpha = 0.55, color = NA, scale = "width", trim = FALSE) +
    geom_boxplot(width = 0.18, outlier.size = 0.6, outlier.alpha = 0.4,
                 color = "grey20", alpha = 0.95) +
    stat_summary(fun = median, geom = "point", shape = 21, size = 2.4,
                 fill = "white", color = "grey15") +
    scale_fill_manual(values = palette_hepatitis, guide = "none") +
    scale_x_continuous(breaks = seq(0, 100, by = 10),
                       expand = expansion(mult = c(0.01, 0.03))) +
    labs(title = L$fig2_title, subtitle = L$fig2_subtitle,
         x = L$fig2_x, y = NULL, caption = wrap_caption(L$fig2_caption)) +
    theme_hepatitis_poster() +
    theme(axis.text.y        = element_text(face = "bold", size = rel(0.95),
                                            lineheight = 1),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3))

  p
}

print(make_figure2("EN"))

make_figure3 <- function(lang) {
  L <- i18n[[lang]]

  p <- ggplot(national_rates_discharges, aes(x = AÑO, y = RATE_CRUDE)) +
    annotate("rect", xmin = 2019.5, xmax = 2022.5, ymin = -Inf, ymax = Inf,
             fill = "grey50", alpha = 0.13) +
    annotate("text", x = 2021,
             y = max(national_rates_discharges$RATE_CRUDE) * 0.97,
             label = L$pandemic_band, color = "grey30",
             size = 3.6, fontface = "italic") +
    geom_line(color = "#355070", linewidth = 1.3) +
    geom_point(size = 2.8, color = "#355070") +
    geom_text(aes(label = sprintf("%.2f", RATE_CRUDE)),
              vjust = -1.1, size = 3.3, color = "grey15", fontface = "bold") +
    scale_x_continuous(breaks = 2014:2024) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(title = L$fig3_title, subtitle = L$fig3_subtitle,
         x = L$fig3_x, y = L$fig3_y, caption = wrap_caption(L$fig3_caption)) +
    theme_hepatitis_poster()

  p
}

print(make_figure3("EN"))

make_figure4 <- function(lang) {
  L <- i18n[[lang]]

  pandemic_band <- list(
    annotate("rect", xmin = 2019.5, xmax = 2022.5, ymin = -Inf, ymax = Inf,
             fill = "grey50", alpha = 0.13)
  )

  coverage_long <- vaccination_coverage |>
    select(YEAR, HEPB, PENTA3) |>
    pivot_longer(c(HEPB, PENTA3), names_to = "VACCINE", values_to = "COVERAGE") |>
    mutate(VACCINE = factor(VACCINE, levels = c("HEPB", "PENTA3"),
                            labels = c(L$fig4_vac_hepb, L$fig4_vac_penta)))

  panel_A <- ggplot(coverage_long, aes(x = YEAR, y = COVERAGE, color = VACCINE)) +
    pandemic_band +
    geom_hline(yintercept = 90, linetype = "dashed", color = "grey60", linewidth = 0.4) +
    annotate("text", x = 2014.2, y = 91.8, label = L$who_target,
             hjust = 0, size = 3, color = "grey45", fontface = "italic") +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.6) +
    scale_color_manual(values = setNames(c("#E76F51", "#355070"),
                                         c(L$fig4_vac_hepb, L$fig4_vac_penta)),
                       name = NULL) +
    scale_x_continuous(breaks = 2014:2024) +
    scale_y_continuous(limits = c(55, 115), breaks = seq(60, 110, by = 10),
                       labels = function(x) paste0(x, "%")) +
    labs(title = L$fig4_title, x = NULL, y = L$fig4_y_cov,
         caption = wrap_caption(L$fig4_caption)) +
    theme_hepatitis_poster(base_size = 13) +
    theme(legend.position = "top")

  p <- panel_A
  p
}

print(make_figure4("EN"))

make_figure5 <- function(lang) {
  L <- i18n[[lang]]
  datos <- sdr_discharges |>
    mutate(
      DEPT_F        = fct_reorder(DEPT, SDR),
      PROFILE_LABEL = L$fig5_legend[as.character(PROFILE)]
    )

  fill_colors <- setNames(
    c("#C9483F", "#9C9C9C", "#3A6B7E"),
    c(L$fig5_legend["Above expected"],
      L$fig5_legend["Within expected"],
      L$fig5_legend["Below expected"])
  )

  p <- ggplot(datos, aes(y = DEPT_F, x = SDR, color = PROFILE_LABEL)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey60", linewidth = 0.4) +
    geom_errorbar(aes(xmin = CI_LOW, xmax = CI_HIGH), orientation = "y",
                  width = 0, linewidth = 0.7) +
    geom_point(size = 3) +
    geom_text(aes(label = sprintf("%.2f [%.2f–%.2f]", SDR, CI_LOW, CI_HIGH),
                  x = CI_HIGH),
              hjust = -0.12, size = 3, color = "grey20") +
    scale_color_manual(values = fill_colors, name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.35))) +
    labs(title = L$fig5_title, subtitle = L$fig5_subtitle,
         x = L$fig5_x, y = NULL, caption = wrap_caption(L$fig5_caption)) +
    theme_hepatitis_poster(base_size = 13) +
    theme(axis.text.y        = element_text(face = "bold", size = rel(0.92)),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
          legend.position    = "top")

  p
}

print(make_figure5("EN"))

# ------------------------------------------------------------------------------
# 10b. Figure 6 — bioinformatics-derived poster panel
# ------------------------------------------------------------------------------
make_figure6 <- function(lang) {
  L   <- i18n[[lang]]
  csv <- file.path(root, "bioinformatics", "06_tables", "hbsag_mhr_substitutions.csv")
  if (!file.exists(csv)) { warning("Fig6 skipped: missing ", csv); return(invisible(NULL)) }
  mhr <- readr::read_csv(csv, show_col_types = FALSE)

  # Focus the map on the informative window: the "a" determinant (aa 124-147)
  # plus all canonical escape positions and short flanks.
  FIG6_WINDOW <- c(116, 152)
  mhr <- mhr |> filter(position >= FIG6_WINDOW[1], position <= FIG6_WINDOW[2])

  ref_cols <- intersect(c("vaccine_adw2", "F_prototype_adw4", "F_consensus_adw4", "TEG_adw4"),
                        names(mhr))
  long <- mhr |>
    select(position, all_of(ref_cols)) |>
    tidyr::pivot_longer(all_of(ref_cols), names_to = "ref", values_to = "residue") |>
    left_join(mhr |> select(position, vax = vaccine_adw2), by = "position") |>
    mutate(
      state = case_when(ref == "vaccine_adw2" ~ "vaccine",
                        residue != vax        ~ "diff",
                        TRUE                  ~ "same"),
      ref   = factor(L$fig6_rows[ref], levels = rev(unname(L$fig6_rows[ref_cols])))
    )

  esc <- mhr |> filter(canonical_escape) |> pull(position)
  ny  <- length(ref_cols)

  p <- ggplot(long, aes(x = position, y = ref)) +
    geom_tile(aes(fill = state), color = "white", linewidth = 0.3) +
    geom_text(aes(label = residue, color = state), size = 3, show.legend = FALSE) +
    annotate("rect", xmin = 123.5, xmax = 147.5, ymin = 0.5, ymax = ny + 0.5,
             fill = NA, color = "#1F3A5F", linewidth = 0.7) +
    annotate("point", x = esc, y = ny + 0.7, shape = 25, size = 2,
             fill = "#9B2226", color = "#9B2226") +
    scale_fill_manual(values = c(vaccine = "#E8EEF4", same = "#F4F7FA", diff = "#F4C9C9"),
                      breaks = c("same", "diff"),
                      labels = c(L$fig6_same, L$fig6_diff), name = NULL) +
    scale_color_manual(values = c(vaccine = "#33414F", same = "#33414F", diff = "#9B2226"),
                       guide = "none") +
    scale_x_continuous(breaks = seq(120, 150, by = 5), expand = expansion(mult = 0.01)) +
    coord_cartesian(ylim = c(0.5, ny + 1), clip = "off") +
    labs(title    = stringr::str_wrap(L$fig6_title, 50),
         subtitle = stringr::str_wrap(L$fig6_subtitle, 62),
         x = L$fig6_x, y = NULL,
         caption = wrap_caption(L$fig6_caption)) +
    theme_hepatitis_poster(base_size = 13) +
    theme(panel.grid    = element_blank(),
          legend.position = "top",
          axis.text.y   = element_text(face = "bold"))

  p
}

print(make_figure6("EN"))

# ------------------------------------------------------------------------------
# 10c. Molecular component — provenance and genotype QC (reproducibility)
# ------------------------------------------------------------------------------
# Fig 6 rests on the only Honduran HBV sequences in GenBank: 5 partial small-S
# sequences (681 bp), TEG isolates, Tegucigalpa 1997, accessions U91811.1-U91815.1
# (Arauz-Ruiz et al. 1997).
HBV_HN_ACCESSIONS <- c("U91811.1", "U91812.1", "U91813.1", "U91814.1", "U91815.1")

geno_csv <- file.path(root, "bioinformatics", "06_tables", "teg_genotype.csv")
g2p_csv  <- file.path(root, "bioinformatics", "notes", "08_geno2pheno_TEG.csv")
ml_tree  <- file.path(root, "bioinformatics", "05_figures", "panel_S_ML.nwk")
if (file.exists(geno_csv)) {
  teg_geno <- readr::read_csv(
    geno_csv, show_col_types = FALSE,
    col_types = readr::cols(best_genotype = readr::col_character(),
                            best_pct_id   = readr::col_double()))
  stopifnot(nrow(teg_geno) == 5,
            all(teg_geno$best_genotype == "F"),
            all(teg_geno$best_pct_id >= 98))

  # Orthogonal subgenotype check: Geno2pheno[hbv] resolves all five to F1
  # (F1 vs F1a is not separable by Geno2pheno; F1a is fixed phylogenetically).
  if (file.exists(g2p_csv)) {
    g2p <- readr::read_csv(g2p_csv, show_col_types = FALSE,
                           col_types = readr::cols(.default = readr::col_character()))
    stopifnot(nrow(g2p) == 5, all(g2p$subgenotype_sp == "F1"))
  }
  # Provenance of the F1a placement: maximum-likelihood tree with Central-American
  # F1a anchors (see bioinformatics/notes/09_phylo_confirmation.md).
  f1a_tree_present <- file.exists(ml_tree)

  message("Molecular QC: ", nrow(teg_geno),
          " Honduran HBV sequences, all genotype F (min. identity ",
          sprintf("%.1f%%", min(teg_geno$best_pct_id)),
          "), subgenotype F1 by Geno2pheno[hbv]; ",
          if (f1a_tree_present)
            "F1a placement supported by the ML tree (see bioinformatics/notes/09)."
          else
            "F1a placement: see bioinformatics/notes/09 (ML tree not bundled).")
} else {
  message("Molecular QC skipped: ", basename(geno_csv),
          " not present (bioinformatics component not bundled).")
}

header("§10 Saving figures")

fig_specs <- list(
  list(fn = make_figure1, stem = "Fig1_discharges_by_icd", w = 9,  h = 5.5),
  list(fn = make_figure2, stem = "Fig2_age_by_icd",        w = 9,  h = 6),
  list(fn = make_figure3, stem = "Fig3_annual_trend",      w = 9,  h = 6),
  list(fn = make_figure4, stem = "Fig4_pai_coverage",      w = 9,  h = 5.5),
  list(fn = make_figure5, stem = "Fig5_sdr_departmental",  w = 9,  h = 7),
  list(fn = make_figure6, stem = "Fig6_escape_map",        w = 9,  h = 5)
)
for (fs in fig_specs) {
  for (lang in FIGURE_LANGUAGES) {
    p <- tryCatch(fs$fn(lang),
                  error = function(e) { warning("Figure skipped (", lang, "): ",
                                                conditionMessage(e)); NULL })
    if (!is.null(p)) save_figure(p, fs$stem, lang, fs$w, fs$h)   # write only
  }
}

# ------------------------------------------------------------------------------
# 11. Write tables and session info
# ------------------------------------------------------------------------------
write_csv(discharges_by_type,        file.path(path_tables, "table1_discharges_by_type.csv"))
write_csv(age_by_type,               file.path(path_tables, "table2_age_by_type.csv"))
write_csv(sex_by_type,               file.path(path_tables, "table3_sex_by_type.csv"))
write_csv(top_departments,           file.path(path_tables, "table4_discharges_by_department.csv"))
write_csv(discharges_by_region,      file.path(path_tables, "table5_discharges_by_region.csv"))
write_csv(deaths_by_type,            file.path(path_tables, "table6_deaths_by_type.csv"))
write_csv(national_rates_discharges, file.path(path_tables, "table7_national_rates_discharges.csv"))
write_csv(national_rates_deaths,     file.path(path_tables, "table8_national_rates_deaths.csv"))
write_csv(national_rates_by_type,    file.path(path_tables, "table9_national_rates_by_type.csv"))
write_csv(geographic_concentration,  file.path(path_tables, "table10_geographic_concentration.csv"))
write_csv(sdr_discharges,            file.path(path_tables, "table11_sdr_discharges_by_dept.csv"))
write_csv(delta_summary,             file.path(path_tables, "table12_delta_summary.csv"))
write_csv(delta_case_list,           file.path(path_tables, "table13_delta_case_list.csv"))
write_csv(delta_by_year_wide,        file.path(path_tables, "table14_delta_by_year.csv"))
write_csv(supplementary_table_S2,    file.path(path_tables, "tableS2_icd_subcode_distribution.csv"))
write_csv(sensitivity_age_exclusion, file.path(path_tables, "tableS3_sensitivity_age_exclusion.csv"))
write_csv(composition_reconciled,    file.path(path_tables, "tableS4_composition_reconciled.csv"))
write_csv(age_profile_b15_vs_b19,    file.path(path_tables, "tableS5_age_profile_b15_vs_b19.csv"))
write_csv(deaths_concentration,      file.path(path_tables, "tableS6_mortality_concentration.csv"))
write_csv(hepatitis_c,               file.path(path_tables, "tableS7_hepatitis_c.csv"))

writeLines(capture.output(sessionInfo()),
           file.path(path_tables, "sessionInfo.txt"))

header("Run summary")
cat(sprintf("Period:                    %s\n", PERIOD_LABEL))
cat(sprintf("Total discharges:          %s\n", format(total_discharges, big.mark = ",")))
cat(sprintf("Total deaths:              %s\n", format(total_deaths,     big.mark = ",")))
cat(sprintf("Total person-years:        %s\n", format(sum(population_national$POPULATION_NAC), big.mark = ",")))
cat(sprintf("Tables written to:         %s\n", path_tables))
cat(sprintf("Figures written to:        %s\n", path_figures))
cat(sprintf("Run completed at:          %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))

