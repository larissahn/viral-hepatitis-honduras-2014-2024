# Viral hepatitis in Honduras, 2014–2024

A decade-long national analysis of hospitalizations and mortality from viral hepatitis (ICD-10 B15–B19) in Honduras, linked to the temporal performance of Expanded Program on Immunization (EPI) coverage, with a molecular characterization of the only Honduran hepatitis B virus (HBV) sequences available. This repository holds the **reproducible analytical script** that generates the figures, tables, and statistics of the work accepted as an **e-poster at the XXIV National Course and V National Congress of Infectious Diseases (SHEI), Tegucigalpa, Honduras, 14–16 October 2026**, and of the extended abstract deposited as a preprint.

## Authorship

**Larissa Acosta Salgado, MSc** (presenting author and repository maintainer)
ORCID: [0000-0002-3583-5070](https://orcid.org/0000-0002-3583-5070) · Email: larissa.acosta@galileo.edu · Web: [www.larissaacosta.com](https://www.larissaacosta.com)

Co-authors: Daniela Sofía Cruz Betanco, MSc (FUNHEPA); Zonia Reyes, MD (UNITEC).

## Associated outputs

- **E-poster** — XXIV National Course and V National Congress of Infectious Diseases (SHEI) 2026 (digital-poster format).
- **Extended abstract (preprint)** — Figshare, [doi:10.6084/m9.figshare.34007166](https://doi.org/10.6084/m9.figshare.34007166).
- **Methodological reference** — hepatocellular carcinoma *Brief Report*, *Annals of Hepatology* (Acosta et al., 2026; [doi:10.1016/j.aohep.2026.102230](https://doi.org/10.1016/j.aohep.2026.102230)), from which this analysis inherits the rate and macro-region architecture.

## Data availability

| Source | Content | Access | In the repo? |
|--------|---------|--------|--------------|
| SESAL (Honduras Ministry of Health) | Hospital discharges and deaths, ICD-10 B15–B19, 2014–2024 | Institutional request | **No** (restricted) |
| INE (National Institute of Statistics) | Departmental and national population projections | [Public](https://www.ine.gob.hn) | **Yes** (`poblacion_departamental_INE_2014_2024.csv`) |
| AES-SIVAC / SESAL | HepB birth dose, Pentavalent 3rd dose and HepA coverage | Public bulletin | **Yes** (entered in the script, source cited in the code) |

The SESAL microdata (`2014-2024 Enfermedades Hepaticas.xlsx` and `2024 Egresos_ Defunciones Enfermedades del Higado.xlsx`) are secondary, de-identified, aggregable records under **restricted access**; they are **not redistributed** in this repository and can be requested from SESAL. The derived **aggregated tables** (`outputs/tables/*.csv`) are published, under a CC BY 4.0 licence.

**Ethics.** The study relied on secondary, aggregated, de-identified surveillance data without individual identifiers; by its nature it does not constitute research on human subjects and required no informed consent. It was conducted in accordance with the WMA Declaration of Helsinki and the WMA Declaration of Taipei on health databases.

## Repository structure

```
.
├── HepatitisVirales_Honduras_2014_2024.R   Single reproducible analytical script
├── README.md                               This file
├── LICENSE                                 MIT (code)
├── LICENSE-DATA.md                         CC BY 4.0 (derived tables and figures)
├── CITATION.cff                            Citation metadata (GitHub / Zenodo / Figshare)
├── .gitignore                              Excludes SESAL microdata (*.xlsx/*.xls) and figure binaries
├── poblacion_departamental_INE_2014_2024.csv   Public INE denominators
├── bioinformatics/                         HBV molecular component (feeds Fig 6 and the §10c QC)
│   ├── 05_figures/panel_S_ML.nwk           Maximum-likelihood S-gene tree (F1a placement)
│   ├── 06_tables/hbsag_mhr_substitutions.csv   HBsAg residue map (Fig 6 input)
│   ├── 06_tables/teg_genotype.csv          Genotype call per sequence (all F)
│   └── notes/08_geno2pheno_TEG.csv         Geno2pheno[hbv] subgenotype call (F1)
└── outputs/
    ├── tables/                             CSV tables (table1–table14, tableS2–tableS7, sessionInfo.txt)
    └── figures/                            EN/ES figures in PDF + PNG + TIFF (regenerated on run)
```

The SESAL microdata are placed locally but are **not** versioned (covered by `.gitignore`).

## Reproducing the analysis

### Requirements

- **R ≥ 4.3** (tested on 4.3.x and 4.4.x).
- Packages: `readxl`, `dplyr`, `tidyr`, `stringr`, `readr`, `forcats`, `ggplot2`, `scales`, `patchwork`. Install them before running (`install.packages(...)`); the script loads them with `library()`, it does not install them for you.
- An R build with `cairo_pdf` (renders accents and dashes in the PDF; if absent, the script falls back to `pdf()` with a warning).

### Steps

1. Clone the repository and open it as an R Project.
2. Place the two SESAL files in the project root (next to the script). If you do not have them, request them from SESAL.
3. The project root is detected automatically via `source()` or `Rscript`; only for line-by-line console use, set `ROOT` to the project folder first. All other paths are relative.
4. Open `HepatitisVirales_Honduras_2014_2024.R` in RStudio and run it (`source`), or from the terminal: `Rscript HepatitisVirales_Honduras_2014_2024.R`.

### Figure controls

As the script runs, each figure is **printed to the graphics device** so you can inspect it before moving on to the next; the files are written separately, in a single export step at the end. One toggle at the top of the script governs saving:

- `SAVE_FIGS <- TRUE` — write the figure files; set it to `FALSE` to review in the console only.

Figures are written as **PDF (vector), PNG (600 dpi) and TIFF (300 dpi, LZW)**; tables as CSV; and the exact environment to `outputs/tables/sessionInfo.txt`.

## Generated figures

1. **Fig 1 — Discharges by ICD-10**: hospital burden by type (B15 predominant).
2. **Fig 2 — Age by type**: age distribution by code (median, IQR).
3. **Fig 3 — Annual trend**: national rate per 100,000, with the 2020–2022 pandemic band.
4. **Fig 4 — EPI vaccination coverage**: HepB birth dose and third Pentavalent dose over time, single descriptive panel, no causal claim.
5. **Fig 5 — Departmental SDR**: standardized discharge ratio (Byar 95% CI), forest plot.
6. **Fig 6 — HBsAg "a" determinant**: adw2 vaccine antigen against the regional adw4 subtype and the Honduran sequences.

Fig 6 draws on the bioinformatics component; if its input table (`bioinformatics/06_tables/hbsag_mhr_substitutions.csv`) is absent, the script skips it with a warning and continues.

## Methodological conventions

- **ICD-10**: B15 (acute hepatitis A), B16 (acute hepatitis B), B17 (other acute), B18 (chronic), B19 (unspecified).
- **Fractional age**: `EDAD TIPO` codes the unit (1=hours, 2=days, 3=months, 4=years); the script converts it to years in a single variable and **excludes `EDAD TIPO` = 1 (hours)** for biological implausibility (documented sensitivity analysis).
- **Crude rates per 100,000**: numerator, annual count; denominator, mid-year population (INE). For cumulative rates, the sum of person-years over the period.
- **Departmental SDR**: standardized discharge ratio (observed vs expected discharges) with a 95% CI by Byar's approximation. Discharges are not incident cases, hence "discharge" rather than "incidence".
- **Mortality by aetiology**: with 35 deaths over the period, mortality is reported by type (not geographically, given the scarcity of events).
- **Cross-cutting hepatitis Delta and C**: Delta aggregates B16.0/B16.1/B17.0/B18.0; hepatitis C aggregates B17.1 (acute) + B18.2 (chronic); both cross groups, so they are reported as counts, not as a percentage of the B15–B19 partition.
- **COVID-19 pandemic band** (2020–2022): marked on the temporal figures as context, without imposing causality.
- **Reproducibility**: the analysis is fully deterministic (no sampling or bootstrap, hence no random seed); save-time controls with `stopifnot()` freeze the key counts, and `sessionInfo()` is exported to `outputs/tables/sessionInfo.txt`.

## Licence

- **R code**: [MIT](LICENSE).
- **Derived tables and figures**: [CC BY 4.0](LICENSE-DATA.md).
- **SESAL microdata**: not licensed by this repository; consult SESAL for their terms.

## How to cite

See `CITATION.cff` (citable by GitHub, Zenodo and Figshare). Suggested citation:

> Acosta Salgado, L., Cruz Betanco, D. S., & Reyes, Z. (2026). *Disease burden and gaps in elimination: a decade-long analysis of hospitalizations and mortality from viral hepatitis and genomic characterization of HBV in Honduras, 2014–2024* [R script]. XXIV National Course and V National Congress of Infectious Diseases (SHEI), Tegucigalpa, Honduras.

## Contact

For methodological questions, code issues or collaboration, open an *issue* or write to the lead author.
