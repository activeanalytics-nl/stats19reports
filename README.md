# stats19reports

Reproducible road safety reports for any Great Britain Local Authority,
built from DfT [STATS19](https://www.data.gov.uk/dataset/road-accidents-safety-data)
collision, casualty and vehicle data via the
[stats19](https://github.com/ropensci/stats19) package.

The package summarises casualties by Local Authority, LSOA, MSOA, Index of
Multiple Deprivation, OpenStreetMap road link and speed limit; values
collisions with DfT TAG / RAS4001 costs; produces static and interactive
tmap maps, ggplot2 charts and gt / reactable tables; and renders everything
into a parameterised Quarto report.

## Installation

```r
# install.packages("remotes")
remotes::install_github("BlaiseKelly/stats19reports")
```

Note the `Remotes` dependencies (`nptscot/osmactive`,
`humaniverse/geographr`) are installed automatically by `remotes`.

## Usage

The workflow is two steps — build the data, then render the report:

```r
library(stats19reports)

# 1. download data, compute all summaries, write plots/maps/tables
#    and save report objects to outputs/Bristol/data/la_report_data.rds
build_la_report_data("Bristol", base_year = 2021, upper_year = 2025)

# 2. render the packaged Quarto template against those outputs
render_la_report("Bristol")
```

Or both at once:

```r
la_report("Bristol")
```

`build_la_report_data()` takes a `quick = TRUE` argument that skips the
slow per-street map loops and the national choropleth grid — useful when
testing changes.

Any Local Authority name matched against `LAD22NM` works, e.g.
`la_report("Wiltshire")`, `la_report("Leeds")`.

## Package structure

| File | Contents |
|---|---|
| `R/get.R` | Boundary/population downloads (ONS, Eurostat, NOMIS) |
| `R/lookups.R` | Shared vehicle/casualty type lookups, DfT age bands, basemap options |
| `R/match.R` | Joins: TAG/RAS4001 costs, LSOA 2021, MSOA IMD, OSM links |
| `R/summary.R` | Casualty/collision summaries at all geographies |
| `R/plots.R` | ggplot2 chart functions |
| `R/map.R` | tmap static and interactive map functions |
| `R/tabulate.R` | gt and reactable table builders |
| `R/report_text.R` | Inline prose/stat helpers for the report template |
| `R/pipeline.R` | `build_la_report_data()` and its stage functions |
| `R/render.R` | `render_la_report()`, `la_report()` |
| `inst/report/LA_report.qmd` | Parameterised Quarto report template |

## Notes on the 2026 refactor

This package was refactored from a collection of scripts. Main changes:

- **`report.R` script → `build_la_report_data()`**, decomposed into stage
  functions (STATS19 load, IMD/LSOA, OSM network, interactive maps, MSOA,
  costs, conditions, national maps). All hardcoded local paths replaced
  with parameters/URLs. Outputs are saved as a single named-list RDS
  rather than an `.Rdata` of loose objects.
- **`pavements.R` was dropped** — it was a stale near-duplicate fork of
  `report.R`; its unique e-scooter flag handling was merged into the
  STATS19 loading stage.
- **The Quarto template is now parameterised** (`params$authority`,
  `params$output_dir`) rather than rendered via text substitution of an
  `AUTHORITY` placeholder. All hidden calculation chunks and long
  `ifelse()/paste0()` prose chains moved into tested package functions
  (`report_*_stats()`, `la_summary_paragraph()`, `five_year_sentence()`,
  `change_phrase()`).
- **Inline table code moved into `tabulate_*()` functions** sharing one
  `reactable_report_theme()`.
- **Bug fixes**, including: `match_tag()` was called throughout but no
  longer existed (now `match_tag_costs()`, which also uses its argument
  instead of a global `cra_2024`); `tabulate_la_cost_ranking()` used `=`
  instead of `==` in a filter, sorted by a non-existent column and called
  a non-existent function; a missing pipe in `plots.R`; the `la_8.png`
  MSOA plot saved the wrong plot object; `get_nomis_populationulation`
  typo; duplicated `st_read_retry()` and vehicle/casualty lookup tables
  (now defined once in `R/lookups.R`); map functions silently depending
  on global variables (`city_shp`, `drive_net`, `la_name`) now take
  explicit arguments; an empty broken `plot_waffle()` stub removed and
  its stolen documentation returned to `plot_ksi_pavement()`.

## Regenerating documentation

`NAMESPACE` was generated from the `@export` tags. After edits run:

```r
devtools::document()
devtools::check()
```
