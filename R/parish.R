# Parish council report pipeline. A lighter sibling of the LA report using
# parish boundaries from planning.data.gov.uk, DfT-styled charts, and long
# year ranges (parishes have few collisions so more history is needed).

#' Parish report pipeline section names
#'
#' @return A named character vector of section ids and descriptions.
#' @examples
#' parish_sections()
#' @export
parish_sections <- function() {
  c(boundary     = "Parish boundary map",
    index        = "Casualty index chart (relative to base year)",
    demographics = "Casualties by age and sex",
    conditions   = "Crash condition charts",
    costs        = "Annual TAG cost table and chart",
    maps         = "Casualty location and type maps",
    lsoa         = "Casualty and driver home LSOA maps",
    osm          = "Cost and casualties per OSM road link")
}

#' Build all data, plots and tables for a parish report
#'
#' Runs the analysis pipeline for one parish council area, using parish
#' boundaries from the national planning data platform. Follows the same
#' section/caching model as [build_la_report_data()]: each section saves to
#' \code{output_dir/data/sections/} and is skipped when already built.
#'
#' Because parish areas have few collisions, the default period is much
#' longer than the LA report (2010 onwards).
#'
#' @param parish Character. Parish name matched exactly, e.g.
#'   \code{"Winsley"}.
#' @param sections Character. \code{"all"} or a subset of
#'   \code{names(parish_sections())}.
#' @param base_year,upper_year Integers. Analysis period. Default 2010 to
#'   2025.
#' @param output_dir Character. Root output directory.
#' @param overwrite Logical. Re-run requested sections even if cached.
#' @return Invisibly, the assembled list of report objects.
#' @examples
#' \dontrun{
#' build_parish_report_data("Winsley")
#' build_parish_report_data("Winsley", sections = "osm", overwrite = TRUE)
#' }
#' @export
build_parish_report_data <- function(
    parish,
    sections = "all",
    base_year = 2010,
    upper_year = 2025,
    output_dir = file.path("outputs", gsub(" ", "_", parish)),
    overwrite = FALSE) {

  all_sections <- names(parish_sections())
  if (identical(sections, "all")) sections <- all_sections
  bad <- setdiff(sections, all_sections)
  if (length(bad) > 0) {
    stop("Unknown section(s): ", paste(bad, collapse = ", "),
         ". Valid sections: ", paste(all_sections, collapse = ", "))
  }

  create_report_dirs(output_dir)

  core <- load_report_core(parish, base_year, upper_year, la_url = NULL,
                           output_dir, area_type = "parish")

  runners <- list(
    boundary     = function() psec_boundary(core, output_dir),
    index        = function() psec_index(core, output_dir),
    demographics = function() psec_demographics(core, output_dir),
    conditions   = function() sec_conditions(core, output_dir),
    costs        = function() psec_costs(core, output_dir),
    maps         = function() psec_maps(core, output_dir),
    lsoa         = function() psec_lsoa(core, output_dir),
    osm          = function() psec_osm(core, output_dir)
  )

  run_report_sections(runners, intersect(all_sections, sections),
                      output_dir, overwrite)

  invisible(assemble_parish_data(output_dir))
}

#' Merge cached parish section outputs into the report data file
#'
#' @param output_dir Character. Root output directory used by
#'   [build_parish_report_data()].
#' @return Invisibly, the assembled named list.
#' @examples
#' \dontrun{
#' assemble_parish_data("outputs/Winsley")
#' }
#' @export
assemble_parish_data <- function(output_dir) {

  core_file <- file.path(output_dir, "data", "cache", "core.rds")
  if (!file.exists(core_file)) {
    stop("No core cache found at ", core_file,
         ". Run build_parish_report_data() first.")
  }
  core <- readRDS(core_file)

  report_dat <- list(
    parish_name = core$la_name,
    base_year = core$base_year, upper_year = core$upper_year
  )

  done <- character(0)
  for (sec in names(parish_sections())) {
    f <- section_file(output_dir, sec)
    if (file.exists(f)) {
      report_dat <- utils::modifyList(report_dat, readRDS(f))
      done <- c(done, sec)
    }
  }

  missing <- setdiff(names(parish_sections()), done)
  if (length(missing) > 0) {
    warning("Sections not yet built (report may not render fully): ",
            paste(missing, collapse = ", "))
  }

  saveRDS(report_dat,
          file.path(output_dir, "data", "parish_report_data.rds"))
  invisible(report_dat)
}

#' Render the parish report
#'
#' Renders the packaged parish Quarto template against outputs built by
#' [build_parish_report_data()].
#'
#' @param parish Character. Parish name.
#' @param output_dir Character. Root output directory.
#' @param output_file Character. Output HTML filename.
#' @param template Character. Path to a Quarto template.
#' @return Invisibly, the path to the rendered file.
#' @examples
#' \dontrun{
#' build_parish_report_data("Winsley")
#' render_parish_report("Winsley")
#' }
#' @export
render_parish_report <- function(
    parish,
    output_dir = file.path("outputs", gsub(" ", "_", parish)),
    output_file = paste0(gsub(" ", "_", parish), "_report.html"),
    template = system.file("report", "parish_report.qmd",
                           package = "stats19reports")) {

  if (!nzchar(template) || !file.exists(template)) {
    stop("Parish report template not found. Is the package installed?")
  }

  data_file <- file.path(output_dir, "data", "parish_report_data.rds")
  core_file <- file.path(output_dir, "data", "cache", "core.rds")
  if (!file.exists(data_file) && file.exists(core_file)) {
    assemble_parish_data(output_dir)
  }
  if (!file.exists(data_file)) {
    stop("No report data found at ", data_file,
         ". Run build_parish_report_data('", parish, "') first.")
  }

  local_qmd <- file.path(getwd(), basename(template))
  file.copy(template, local_qmd, overwrite = TRUE)
  on.exit(unlink(local_qmd), add = TRUE)

  quarto::quarto_render(
    input = local_qmd,
    output_file = output_file,
    execute_params = list(parish = parish, output_dir = output_dir)
  )

  invisible(file.path(getwd(), output_file))
}

#' Map costs or casualties per OSM road link
#'
#' Static tmap of the parish road network with links coloured by total TAG
#' cost (or another link-level variable) over a basemap.
#'
#' @param link_costs An \code{sf} data frame of OSM links with the variable
#'   to map (e.g. from [summarise_osm_link_costs()]).
#' @param area_sf An \code{sf} polygon of the area boundary.
#' @param variable Character. Column to colour by. Default
#'   \code{"total_cost"}.
#' @param legend_title Character. Legend title.
#' @param palette Character. cols4all palette name.
#' @param save_path Character or \code{NULL}. PNG path.
#' @return A tmap object, invisibly.
#' @examples
#' \dontrun{
#' map_osm_link_costs(link_costs, parish_sf)
#' }
#' @export
map_osm_link_costs <- function(link_costs, area_sf,
                               variable = "total_cost",
                               legend_title = "Value (\u00a3)",
                               palette = "carto.sunset",
                               save_path = NULL) {

  bm <- basemaps::basemap_raster(ext = area_sf, map_service = "carto",
                                 map_type = "light")

  tm <- tmap::tm_shape(bm) +
    tmap::tm_rgb() +
    tmap::tm_shape(sf::st_transform(area_sf, 27700)) +
    tmap::tm_polygons(fill_alpha = 0, col = "#ff7733", lwd = 4) +
    tmap::tm_shape(sf::st_transform(link_costs, 27700)) +
    tmap::tm_lines(col = variable,
                   col.scale = tmap::tm_scale_intervals(values = palette),
                   col.legend = tmap::tm_legend(legend_title),
                   lwd = 4) +
    tmap::tm_scalebar()

  if (!is.null(save_path)) {
    tmap::tmap_save(tm, save_path, width = 7500, height = 7000, dpi = 800)
  }
  invisible(tm)
}

# --------------------------------------------------------------------------
# parish section runners
# --------------------------------------------------------------------------

#' Parish boundary map
#' @noRd
psec_boundary <- function(core, output_dir) {
  map_city_boundary(city_shape = sf::st_transform(core$city_shp, 27700),
                    city_name = core$la_name,
                    map_type = "osm",
                    plot_dir = file.path(output_dir, "plots"))
  list()
}

#' Casualty index and year totals
#' @noRd
psec_index <- function(core, output_dir) {

  cas_summary <- core$casualties |>
    dplyr::group_by(collision_index, collision_year) |>
    dplyr::summarise(
      Fatal = sum(fatal_count),
      Serious = sum(casualty_adjusted_severity_serious, na.rm = TRUE),
      Slight = sum(casualty_adjusted_severity_slight, na.rm = TRUE),
      .groups = "drop")

  cas_rates <- cas_summary |>
    dplyr::group_by(collision_year) |>
    dplyr::summarise(Fatal = sum(Fatal),
                     Serious = sum(Serious, na.rm = TRUE),
                     Slight = sum(Slight, na.rm = TRUE))

  bm_vals <- dplyr::filter(cas_rates, collision_year == core$base_year)

  indexes <- cas_rates |>
    dplyr::transmute(year = collision_year,
                     Fatal = Fatal / bm_vals$Fatal * 100,
                     Serious = Serious / bm_vals$Serious * 100,
                     Slight = Slight / bm_vals$Slight * 100)

  plot_casualty_index(indexes, base_year = core$base_year,
                      end_year = core$upper_year, city = core$la_name,
                      plot_dir = file.path(output_dir, "plots"))

  plot_casualties_by_year(casualties = core$casualties,
                          city = core$la_name,
                          plot_dir = file.path(output_dir, "plots"))

  list(cas_rates = cas_rates, indexes = indexes)
}

#' Casualties by age and sex
#' @noRd
psec_demographics <- function(core, output_dir) {
  plot_casualty_demographics(casualties = core$casualties,
                             city = core$la_name, severity = "ksi",
                             plot_dir = output_dir)
  list(age_sex = summarise_casualties_by_demog(casualties = core$casualties))
}

#' Annual TAG costs table and chart
#' @noRd
psec_costs <- function(core, output_dir) {

  tabulate_summarise_tag_costs(core$crashes, city = core$la_name,
                               agg_level = "severity",
                               tab_dir = file.path(output_dir, "tables"),
                               file_type = ".png")

  plot_summarise_tag_costs(core$crashes, agg_level = "severity",
                           plot_dir = file.path(output_dir, "plots",
                                                "costs"),
                           city = core$la_name)

  tag_costs <- summarise_tag_costs(crashes_df = core$crashes,
                                   agg_level = "severity")

  list(tag_costs = tag_costs)
}

#' Casualty location and type maps
#' @noRd
psec_maps <- function(core, output_dir) {
  for (c in c("Year", "Casualty IMD", "Speed limit")) {
    tm_cas <- map_casualties_interactive2(crashes = core$crashes,
                                          casualties = core$casualties,
                                          colour_by = c,
                                          extent_geo = core$city_shp)
    tmap::tmap_save(tm_cas,
                    filename = file.path(output_dir, "maps",
                                         paste0("cas_location_", c,
                                                ".html")),
                    selfcontained = TRUE)
  }
  list()
}

#' Casualty and driver home LSOA maps
#' @noRd
psec_lsoa <- function(core, output_dir) {

  lsoa_geo <- get_lsoa21_boundaries(provider = "geographr")

  tm_cas <- map_lsoa_home(casualty_df = core$casualties,
                          lsoa_geo = lsoa_geo, city_shp = core$city_shp,
                          bgd_map_buff = 5000, bgd_map = TRUE,
                          base_year = core$base_year,
                          end_year = core$upper_year,
                          info_position = c(0.55, 0.29))
  tmap::tmap_save(tm_cas,
                  file.path(output_dir, "plots", "lsoa",
                            "casualty_lsoa.png"))

  tm_veh <- map_lsoa_home(vehicle_df = core$vehicles,
                          lsoa_geo = lsoa_geo, city_shp = core$city_shp,
                          bgd_map_buff = 5000, bgd_map = TRUE,
                          base_year = core$base_year,
                          end_year = core$upper_year,
                          info_position = c(0.1, 0.28))
  tmap::tmap_save(tm_veh,
                  file.path(output_dir, "plots", "lsoa", "driver_lsoa.png"))
  list()
}

#' Cost and casualties per OSM road link
#' @noRd
psec_osm <- function(core, output_dir) {

  net <- get_drive_net_boundary(core$city_shp, core$la_name, output_dir)

  link_costs <- summarise_osm_link_costs(osm_data = net,
                                         crash_sf = core$crashes,
                                         casualties = core$casualties,
                                         by_year = FALSE)

  map_osm_link_costs(link_costs, core$city_shp,
                     save_path = file.path(output_dir, "plots",
                                           "cost_osm_links.png"))

  list(link_costs = sf::st_set_geometry(link_costs, NULL))
}

#' Load (or build and cache) the OSM driving network for an arbitrary
#' boundary (used by the parish pipeline, where the area is not an LA)
#' @noRd
get_drive_net_boundary <- function(area_sf, area_name, output_dir) {

  cache <- file.path(output_dir, "data", "cache", "drive_net.rds")
  if (file.exists(cache)) return(readRDS(cache))

  message("[osm] downloading OSM network (cached for next time)...")

  osm_data <- osmactive::get_travel_network(place = area_name,
                                            boundary = area_sf)

  drive_net <- osmactive::get_driving_network(osm_data) |>
    sf::st_transform(sf::st_crs(area_sf)) |>
    sf::st_intersection(area_sf)

  saveRDS(drive_net, cache)
  drive_net
}
