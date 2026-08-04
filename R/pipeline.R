# The LA report data pipeline. This replaces the old R/report.R script (and
# its stale fork R/pavements.R): everything is now parameterised through
# build_la_report_data() and decomposed into internal stage functions.

#' Build all data, plots, maps and tables for an LA report
#'
#' Runs the full analysis pipeline for one Local Authority: downloads
#' STATS19 collision, casualty and vehicle data, computes LA / LSOA / MSOA /
#' IMD / OSM / speed-limit / pavement / cost summaries, writes all static
#' plots, interactive maps and tables to \code{output_dir}, and saves the
#' summary objects needed by the Quarto report template to
#' \code{output_dir/data/la_report_data.rds}.
#'
#' @param authority Character. Local Authority name (matched against
#'   \code{LAD22NM} with \code{grepl()}), e.g. \code{"Bristol"}.
#' @param base_year,upper_year Integers. Analysis period. Note
#'   \code{stats19::get_stats19("5 years")} limits how far back data goes.
#' @param output_dir Character. Root output directory. Default
#'   \code{"outputs/<authority>"}.
#' @param imd_url Character or \code{NULL}. URL (or local path) of the LSOA
#'   IMD 2025 GeoPackage; \code{NULL} uses the packaged default.
#' @param la_url Character. URL of the LA boundaries GeoPackage.
#' @param pop_url Character. URL of the LSOA population CSV.
#' @param quick Logical. If \code{TRUE}, skips the slow per-street map loops
#'   and national choropleth grid (useful for testing). Default
#'   \code{FALSE}.
#' @return Invisibly, the named list of report objects (also saved as RDS).
#' @examples
#' \dontrun{
#' build_la_report_data("Bristol", base_year = 2021, upper_year = 2025)
#' }
#' @export
build_la_report_data <- function(
    authority,
    base_year = 2021,
    upper_year = 2025,
    output_dir = file.path("outputs", gsub(" ", "_", authority)),
    imd_url = NULL,
    la_url = NULL,
    pop_url = NULL,
    quick = FALSE) {

  # default data URLs (kept out of the signature for readable Rd usage)
  if (is.null(imd_url)) {
    imd_url <- paste0("https://github.com/BlaiseKelly/IMD/releases/download/",
                      "LSOA_IMD2025/LSOA_IMD2025_WGS84_-4854136717238973930.gpkg")
  }
  if (is.null(la_url)) {
    la_url <- paste0("https://github.com/BlaiseKelly/stats19_stats/releases/",
                     "download/LA_boundaries/LA.gpkg")
  }
  if (is.null(pop_url)) {
    pop_url <- paste0("https://github.com/BlaiseKelly/lsoa_ons_population/",
                      "releases/download/v0.1.1/lsoa21_pop_tot_2011_2024.csv")
  }

  la_name <- authority
  create_report_dirs(output_dir)

  # ---- geography -----------------------------------------------------------
  LAs <- st_read_retry(la_url)

  city_shp <- dplyr::filter(LAs, grepl(la_name, LAD22NM)) |>
    sf::st_transform(4326)

  if (NROW(city_shp) == 0) {
    stop("No Local Authority matched '", la_name, "' in LAD22NM.")
  }

  message("Gathering data for LA '", la_name, "', matched with ",
          city_shp$LAD22NM[1])

  city_shp_m <- sf::st_transform(city_shp, 27700)

  # ---- STATS19 data --------------------------------------------------------
  s19 <- load_report_stats19(base_year, upper_year)

  crashes <- s19$crashes_gb[city_shp_m, ]
  casualties <- dplyr::filter(s19$casualties_gb,
                              collision_index %in% crashes$collision_index)
  vehicles <- dplyr::filter(s19$vehicles_gb,
                            collision_index %in% crashes$collision_index)

  # ---- LA rankings ---------------------------------------------------------
  LA_casualties <- summarise_casualties_per_la(
    casualties = s19$casualties_gb, crashes = s19$crashes_gb, la_geo = LAs,
    per_capita = TRUE, casualty_types = "All")
  LA_casualties_cycling <- summarise_casualties_per_la(
    casualties = s19$casualties_gb, crashes = s19$crashes_gb, la_geo = LAs,
    per_capita = TRUE, casualty_types = "Cyclist")
  LA_casualties_pedestrian <- summarise_casualties_per_la(
    casualties = s19$casualties_gb, crashes = s19$crashes_gb, la_geo = LAs,
    per_capita = TRUE, casualty_types = "Pedestrian")

  YBLY <- upper_year - 1

  la_year <- function(df, yr) {
    dplyr::filter(df, collision_year == yr & LAD22NM == city_shp$LAD22NM[1])
  }

  LA_LY <- la_year(LA_casualties, upper_year)
  LA_YBLY <- la_year(LA_casualties, YBLY)
  LA_5Y <- la_year(LA_casualties, base_year)
  LA_LY_CYC <- la_year(LA_casualties_cycling, upper_year)
  LA_YBLY_CYC <- la_year(LA_casualties_cycling, YBLY)
  LA_5Y_CYC <- la_year(LA_casualties_cycling, base_year)
  LA_LY_PED <- la_year(LA_casualties_pedestrian, upper_year)
  LA_YBLY_PED <- la_year(LA_casualties_pedestrian, YBLY)
  LA_5Y_PED <- la_year(LA_casualties_pedestrian, base_year)

  # ---- IMD / LSOA stage ----------------------------------------------------
  imd_stage <- build_imd_lsoa_stage(
    crashes = crashes, casualties = casualties, vehicles = vehicles,
    city_shp = city_shp, imd_url = imd_url, pop_url = pop_url,
    base_year = base_year, upper_year = upper_year,
    output_dir = output_dir)

  # ---- OSM network stage ---------------------------------------------------
  osm_stage <- build_osm_stage(
    la_name = la_name, crashes = crashes, casualties = casualties,
    vehicles = vehicles, city_shp = city_shp,
    base_year = base_year, upper_year = upper_year,
    output_dir = output_dir, quick = quick)

  # ---- interactive maps ----------------------------------------------------
  save_interactive_maps(crashes, casualties, city_shp, output_dir)

  # ---- MSOA / IMD relationship stage ---------------------------------------
  msoa_stage <- build_msoa_stage(
    la_name = la_name, casualties = casualties,
    casualties_gb = s19$casualties_gb, IMD_2025 = imd_stage$IMD_2025,
    decile_match = imd_stage$decile_match, output_dir = output_dir)

  # ---- pavements, costs, conditions, demographics --------------------------
  sing_veh_pave <- summarise_casualties_pavements(
    crashes_df = crashes, casualties_df = casualties, vehicles_df = vehicles,
    base_year = base_year, upper_year = upper_year)

  sing_veh_pave_gb <- summarise_casualties_pavements(
    crashes_df = s19$crashes_gb, casualties_df = msoa_stage$casualties_simp,
    vehicles_df = s19$vehicles_gb,
    base_year = base_year, upper_year = upper_year)

  plot_ksi_pavement(
    crashes = crashes, casualties = casualties, vehicles = vehicles,
    base_year = base_year, upper_year = upper_year, plot_rows = 2,
    plot_width = 200, plot_height = 100,
    title = paste0("Pedestrians KSI whilst on a pavement or verge, coloured",
                   " by driven\nvehicle that collided with them between ",
                   base_year, " and ", upper_year),
    plot_dir = file.path(output_dir, "plots", "streets"))

  costs <- build_costs_stage(crashes, vehicles, la_name, output_dir)

  save_condition_plots(crashes, casualties, la_name, output_dir)

  if (!quick) {
    save_national_plots(s19$crashes_gb, s19$casualties_gb, LAs, la_name,
                        base_year, upper_year, output_dir)
  }

  plot_casualty_demographics(casualties = casualties, city = la_name,
                             severity = "ksi", plot_dir = output_dir)

  age_sex <- summarise_casualties_by_demog(casualties = casualties)

  # ---- collect and save ----------------------------------------------------
  report_dat <- list(
    LAs = LAs, la_name = la_name,
    base_year = base_year, upper_year = upper_year, YBLY = YBLY,
    n_local_authorities = NROW(LAs),
    LA_LY = LA_LY, LA_YBLY = LA_YBLY, LA_5Y = LA_5Y,
    LA_LY_CYC = LA_LY_CYC, LA_YBLY_CYC = LA_YBLY_CYC, LA_5Y_CYC = LA_5Y_CYC,
    LA_LY_PED = LA_LY_PED, LA_YBLY_PED = LA_YBLY_PED, LA_5Y_PED = LA_5Y_PED,
    pop_least_imd = imd_stage$pop_least_imd,
    cas_least_imd = imd_stage$cas_least_imd,
    cas_imd_data = imd_stage$cas_imd_data,
    imd_casualties = msoa_stage$imd_casualties,
    slope_dat_ew = msoa_stage$slope_dat_ew,
    slope_dat_la = msoa_stage$slope_dat_la,
    cas_df_all_LA = msoa_stage$cas_df_all_LA,
    csl = osm_stage$csl,
    cas_osm_period = osm_stage$cas_osm_period,
    cas_osm_year = osm_stage$cas_osm_year,
    cas_osm_type = osm_stage$cas_osm_type,
    sing_veh_pave = sing_veh_pave,
    sing_veh_pave_gb = sing_veh_pave_gb,
    cc = costs$cc, cc_mv = costs$cc_mv, cc_spd = costs$cc_spd,
    tag_costs_col = costs$tag_costs_col,
    tag_costs_road = costs$tag_costs_road,
    age_sex = age_sex,
    cas_type = casualty_type_lookup[, c("casualty_type", "short_name")]
  )

  saveRDS(report_dat,
          file.path(output_dir, "data", "la_report_data.rds"))

  invisible(report_dat)
}

#' Create the output directory tree for an LA report
#' @param output_dir Character. Root output directory.
#' @return Invisibly, \code{output_dir}.
#' @noRd
create_report_dirs <- function(output_dir) {
  subdirs <- c("data", "plots", "plots/lsoa", "plots/msoa",
               "plots/osm_links", "plots/streets", "plots/costs",
               "plots/conditions", "plots/demog", "plots/imd",
               "plots/national", "tables", "maps")
  for (d in subdirs) {
    dir.create(file.path(output_dir, d), recursive = TRUE,
               showWarnings = FALSE)
  }
  invisible(output_dir)
}

#' Download and prepare STATS19 collision, vehicle and casualty data
#'
#' Adds e-scooter vehicle types (from \code{escooter_flag} where present),
#' a \code{fatal_count} column, DfT age bands, and fills missing junction
#' detail / vehicle type values.
#'
#' @param base_year,upper_year Integers. Analysis period.
#' @return A list with \code{crashes_gb}, \code{vehicles_gb},
#'   \code{casualties_gb}.
#' @noRd
load_report_stats19 <- function(base_year, upper_year) {

  crashes_gb <- stats19::get_stats19("5 years", type = "collision") |>
    dplyr::filter(collision_year >= base_year &
                    collision_year <= upper_year) |>
    dplyr::mutate(junction_detail = ifelse(is.na(junction_detail),
                                           "unknown (self reported)",
                                           junction_detail)) |>
    stats19::format_sf()

  vehicles_gb <- stats19::get_stats19("5 years", type = "vehicle")

  if ("escooter_flag" %in% names(vehicles_gb)) {
    vehicles_gb <- vehicles_gb |>
      dplyr::mutate(vehicle_type = dplyr::if_else(
        escooter_flag == "Vehicle was an e-scooter",
        "e-scooter", vehicle_type))
  }

  vehicles_gb <- vehicles_gb |>
    dplyr::mutate(vehicle_type = ifelse(is.na(vehicle_type),
                                        "Other vehicle", vehicle_type))

  e_scooter_collisions <- dplyr::filter(vehicles_gb,
                                        vehicle_type == "e-scooter")

  casualties_gb <- stats19::get_stats19("5 years", type = "casualty") |>
    dplyr::mutate(fatal_count = dplyr::if_else(
      casualty_severity == "Fatal", 1, 0)) |>
    dplyr::mutate(casualty_type = ifelse(
      collision_index %in% e_scooter_collisions$collision_index &
        casualty_type == "Data missing or out of range",
      "E-scooter rider", casualty_type)) |>
    dplyr::mutate(dft_age_band = cut(as.numeric(age_of_casualty),
                                     breaks = dft_age_breaks,
                                     labels = dft_age_labels))

  list(crashes_gb = crashes_gb, vehicles_gb = vehicles_gb,
       casualties_gb = casualties_gb)
}

#' IMD / LSOA stage: population and casualty deprivation summaries and plots
#' @return A list with IMD_2025, decile_match, pop_least_imd, cas_least_imd,
#'   cas_imd_data.
#' @noRd
build_imd_lsoa_stage <- function(crashes, casualties, vehicles, city_shp,
                                 imd_url, pop_url, base_year, upper_year,
                                 output_dir) {

  # casualties by IMD decile
  casualties_imd <- casualties |>
    dplyr::filter(collision_year >= base_year) |>
    dplyr::group_by(casualty_imd_decile) |>
    dplyr::summarise(all = dplyr::n()) |>
    dplyr::mutate(pc = round(all / sum(all) * 100, 1))

  casualties_imd_more_less <- casualties_imd |>
    dplyr::mutate(ML = stringr::str_sub(casualty_imd_decile, 1, 1)) |>
    dplyr::group_by(ML) |>
    dplyr::summarise(pc = sum(pc))

  # IMD 2025 (includes high-res LSOA shapes)
  IMD_2025 <- st_read_retry(imd_url)

  lsoa_boundaries_21 <- dplyr::select(IMD_2025, LSOA21CD, LSOA21NM,
                                      geom = SHAPE) |>
    sf::st_transform(4326)
  sf::st_geometry(lsoa_boundaries_21) <- lsoa_boundaries_21$geom

  lsoa_centroids <- sf::st_centroid(lsoa_boundaries_21)
  city_lsoa <- lsoa_centroids[city_shp, ]

  # population
  git_pop <- utils::read.csv(pop_url)
  latest_pop_year <- max(as.numeric(gsub("X", "",
    grep("^X\\d{4}$", names(git_pop), value = TRUE))))
  pop_yr <- paste0("X", min(upper_year, latest_pop_year))

  city_pop_lsoa <- git_pop |>
    dplyr::select("LSOA.2021.Code" = lsoa21cd, "Total" = !!rlang::sym(pop_yr)) |>
    dplyr::filter(LSOA.2021.Code %in% city_lsoa$LSOA21CD)

  # LSOA maps
  tm_lsoa_pop <- map_lsoa_pop(city_sf = city_shp,
                              lsoa_geo = lsoa_boundaries_21,
                              base_year = base_year, end_year = upper_year)
  tmap::tmap_save(tm_lsoa_pop,
                  file.path(output_dir, "plots", "lsoa", "lsoa_pop.png"))

  tm_lsoa_cas <- map_lsoa_home(casualty_df = casualties,
                               lsoa_geo = lsoa_boundaries_21,
                               palette = "wes.zissou1", city_shp = city_shp,
                               base_year = base_year, end_year = upper_year,
                               info_position = c(0.08, 0.4))
  tmap::tmap_save(tm_lsoa_cas,
                  file.path(output_dir, "plots", "lsoa", "lsoa_casualties.png"))

  tm_lsoa_veh <- map_lsoa_home(vehicle_df = vehicles,
                               lsoa_geo = lsoa_boundaries_21,
                               city_shp = city_shp, palette = "wes.zissou1",
                               base_year = base_year, end_year = upper_year,
                               info_position = c(0.08, 0.4))
  tmap::tmap_save(tm_lsoa_veh,
                  file.path(output_dir, "plots", "lsoa", "lsoa_vehicles.png"))

  tm_lsoa_cra <- map_lsoa_crashes(crashes_df = crashes,
                                  lsoa_geo = lsoa_boundaries_21,
                                  palette = "wes.zissou1",
                                  city_shp = city_shp,
                                  info_position = c(0.08, 0.4),
                                  base_year = base_year,
                                  end_year = upper_year)
  tmap::tmap_save(tm_lsoa_cra,
                  file.path(output_dir, "plots", "lsoa", "lsoa_crashes.png"))

  all_lsoa <- tmap::tmap_arrange(tm_lsoa_pop, tm_lsoa_cra, tm_lsoa_cas,
                                 tm_lsoa_veh, ncol = 2, nrow = 2)
  tmap::tmap_save(all_lsoa,
                  file.path(output_dir, "plots", "lsoa", "lsoa_all.png"),
                  width = 8000, height = 8000, dpi = 700)

  # deprivation split of city population
  city_imd <- dplyr::filter(IMD_2025, LSOA21CD %in% city_lsoa$LSOA21CD)

  decile_match <- data.frame(
    imd_decile = casualties_imd$casualty_imd_decile[-1],
    IMDDecil = rev(seq(1, 10, 1)))

  city_imd_pop <- city_imd |>
    dplyr::left_join(city_pop_lsoa, by = c("LSOA21CD" = "LSOA.2021.Code")) |>
    sf::st_set_geometry(NULL) |>
    dplyr::group_by(IMDDecil) |>
    dplyr::summarise(pop = sum(Total, na.rm = TRUE)) |>
    dplyr::mutate(imd_pc = pop / sum(pop) * 100) |>
    dplyr::left_join(decile_match, by = "IMDDecil")

  city_imd_ML <- city_imd_pop |>
    dplyr::mutate(ML = stringr::str_sub(imd_decile, 1, 1)) |>
    dplyr::group_by(ML) |>
    dplyr::summarise(pc = sum(imd_pc), pop = sum(pop))

  list(
    IMD_2025 = IMD_2025,
    decile_match = decile_match,
    pop_least_imd = round(city_imd_ML$pc[city_imd_ML$ML == "L"], 1),
    cas_least_imd = round(
      casualties_imd_more_less$pc[casualties_imd_more_less$ML == "L"], 1),
    cas_imd_data = 100 - round(
      casualties_imd_more_less$pc[casualties_imd_more_less$ML == "D"], 1)
  )
}

#' OSM network stage: driving network, speed limit rates, link summaries,
#' street-level plots
#' @return A list with csl, cas_osm_period, cas_osm_year, cas_osm_type,
#'   drive_net.
#' @noRd
build_osm_stage <- function(la_name, crashes, casualties, vehicles,
                            city_shp, base_year, upper_year, output_dir,
                            quick = FALSE) {

  city_shp_osm <- get_la_boundaries(city_name = la_name, source = "ons") |>
    sf::st_buffer(100) |>
    sf::st_buffer(-100) |>
    sf::st_transform(4326)

  area_bb_sf <- sf::st_bbox(city_shp_osm) |>
    sf::st_as_sfc() |>
    sf::st_as_sf()

  osm_data <- osmactive::get_travel_network(
    place = area_bb_sf, boundary = area_bb_sf,
    boundary_type = "clipsrc", max_file_size = 9e999)

  drive_net <- osmactive::get_driving_network(osm_data) |>
    dplyr::filter(!service %in% c("alley", "driveway", "parking_aisle",
                                  "garages", "drive-through", "lay-by",
                                  "private", "emergency_access", "yard") &
                    !access %in% c("private", "customers", "emergency",
                                   "delivery", "permit") &
                    highway != "service") |>
    dplyr::mutate(name = ifelse(is.na(name) & highway == "motorway",
                                ref, name)) |>
    sf::st_intersection(city_shp_osm)

  # road length by speed limit
  speed_limit_length <- drive_net |>
    dplyr::ungroup() |>
    dplyr::mutate(maxspeed = gsub("mph", "", gsub(" mph", "", maxspeed)),
                  length = as.numeric(sf::st_length(geometry))) |>
    sf::st_set_geometry(NULL) |>
    dplyr::group_by(maxspeed) |>
    dplyr::summarise(tot_length = round(sum(length) / 1000, 1)) |>
    dplyr::mutate(pc_length = round(tot_length / sum(tot_length) * 100, 1))

  cas_sum <- summarise_casualties_per_collision(casualties)

  csl <- dplyr::inner_join(crashes, cas_sum) |>
    sf::st_set_geometry(NULL) |>
    dplyr::group_by(speed_limit) |>
    dplyr::summarise(collisions = dplyr::n(),
                     fatal = sum(Fatal),
                     serious = round(sum(Serious)),
                     ksi = round(sum(Fatal, Serious)),
                     slight = round(sum(Slight))) |>
    dplyr::left_join(speed_limit_length,
                     by = c("speed_limit" = "maxspeed")) |>
    dplyr::mutate(coll_km = round(collisions / tot_length, 2),
                  fatal_km = round(fatal / tot_length, 2),
                  serious_km = round(serious / tot_length, 2),
                  ksi_km = round(ksi / tot_length, 2),
                  slight_km = round(slight / tot_length, 2),
                  fatal_col = round(fatal / collisions, 3),
                  serious_col = round(serious / collisions, 3),
                  ksi_col = round(ksi / collisions, 3),
                  slight_col = round(slight / collisions, 3))

  # link-level summaries
  cas_osm_period <- summarise_osm_link_casualties(
    crashes = crashes, casualties = casualties, ranking = TRUE,
    group = "total", osm_data = drive_net)
  cas_osm_year <- summarise_osm_link_casualties(
    crashes = crashes, casualties = casualties, ranking = TRUE,
    group = "year", osm_data = drive_net)
  cas_osm_type <- summarise_osm_link_casualties(
    crashes = crashes, casualties = casualties, ranking = TRUE,
    group = "casualty_type", osm_data = drive_net)

  if (!quick) {
    save_osm_static_maps(crashes, casualties, drive_net, city_shp_osm,
                         la_name, base_year, upper_year, output_dir)
    save_street_maps(cas_osm_period, drive_net, crashes, casualties,
                     vehicles, base_year, upper_year, output_dir)
  }

  list(csl = csl, cas_osm_period = cas_osm_period,
       cas_osm_year = cas_osm_year, cas_osm_type = cas_osm_type,
       drive_net = drive_net)
}

#' Save the static OSM road maps for the report
#' @noRd
save_osm_static_maps <- function(crashes, casualties, drive_net,
                                 city_shp_osm, la_name, base_year,
                                 upper_year, output_dir) {

  colour_by <- c("number_of_collisions", "ksi")
  casualty_types <- c("Cyclist", "Pedestrian")

  errors <- list()
  for (c in colour_by) {
    tryCatch({
      map_dir <- file.path(tempdir(), "basemaps_fresh")
      dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)
      basemaps::set_defaults(map_dir = map_dir)

      tm_stat <- map_osm_roads_static(
        crashes = crashes, casualties = casualties, osm_data = drive_net,
        casualty_type = casualty_types, year = NULL,
        group = "casualty_type", colour_by = c, city_shape = city_shp_osm,
        basemap_bgd_colour = "dark", legend_pos = c(0.03, 0.48),
        area_name = la_name)
      tmap::tmap_save(tm_stat, file.path(output_dir, "plots", "osm_links",
                                         paste0("osm_all_cas_", c, ".png")))

      for (cas_type in casualty_types) {
        tryCatch({
          cas_nam <- tolower(gsub(" ", "_", cas_type))
          tm_stat <- map_osm_roads_static(
            crashes = crashes, casualties = casualties,
            osm_data = drive_net, casualty_type = cas_type, year = NULL,
            group = "casualty_type", colour_by = c,
            city_shape = city_shp_osm, basemap_bgd_colour = "dark",
            legend_pos = c(0.03, 0.48), area_name = la_name)
          tmap::tmap_save(tm_stat,
                          file.path(output_dir, "plots", "osm_links",
                                    paste0("osm_", cas_nam, "_", c, ".png")))
        }, error = function(e) errors[[c]] <<- e$message)
      }

      for (y in seq(base_year, upper_year)) {
        tryCatch({
          tm_stat <- map_osm_roads_static(
            crashes = crashes, casualties = casualties,
            osm_data = drive_net, casualty_type = NULL, year = y,
            group = "casualty_type", colour_by = c,
            city_shape = city_shp_osm, basemap_bgd_colour = "dark",
            legend_pos = c(0.03, 0.48), area_name = la_name)
          tmap::tmap_save(tm_stat,
                          file.path(output_dir, "plots", "osm_links",
                                    paste0("osm_", y, "_", c, ".png")))
        }, error = function(e) errors[[c]] <<- e$message)
      }
    }, error = function(e) errors[[c]] <<- e$message)
  }

  if (length(errors) > 0) {
    warning("Some OSM static maps failed: ",
            paste(names(errors), unlist(errors), collapse = "; "))
  }
  invisible(NULL)
}

#' Save per-street casualty and vehicle maps for the top 10 roads
#' @noRd
save_street_maps <- function(cas_osm_period, drive_net, crashes, casualties,
                             vehicles, base_year, upper_year, output_dir) {

  cas_osm_plot <- cas_osm_period |>
    dplyr::arrange(dplyr::desc(number_of_collisions)) |>
    dplyr::slice(1:10)

  for (o in cas_osm_plot$osm_id) {

    street2analyse <- dplyr::filter(drive_net, osm_id == o)

    map_osm_street_casualties(
      osm_links = street2analyse, casualties = casualties,
      crashes = crashes, year_from = base_year, year_to = upper_year,
      bgd_map_buff = "street", casualties_buffer = 10, plot_buffer = 20,
      plot_dir = file.path(output_dir, "plots", "streets"))

    map_osm_street_vehicles(
      osm_links = street2analyse, vehicles = vehicles,
      casualties = casualties, crashes = crashes, casualty_types = "All",
      year_from = base_year, year_to = upper_year,
      bgd_map_buff = "street", casualties_buffer = 10, plot_buffer = 20,
      plot_dir = file.path(output_dir, "plots", "streets"))
  }
  invisible(NULL)
}

#' Save the interactive casualty and OSM-road maps
#' @noRd
save_interactive_maps <- function(crashes, casualties, city_shp,
                                  output_dir) {

  for (c in c("Day", "Month", "Year", "Hour", "Sex of casualty",
              "Age group", "Casualty IMD", "Speed limit")) {
    tm_cas <- map_casualties_interactive2(crashes = crashes,
                                          casualties = casualties,
                                          colour_by = c,
                                          extent_geo = city_shp)
    tmap::tmap_save(tm_cas,
                    filename = file.path(output_dir, "maps",
                                         paste0("cas_location_", c, ".html")),
                    selfcontained = TRUE)
  }

  for (c in c("Sex of casualty", "Age group", "Fatal", "KSI", "Serious",
              "Slight", "Total", "Speed limit")) {
    for (g in c("casualty_type", "year")) {
      tm_rds <- map_osm_roads_interactive(crashes = crashes,
                                          casualties = casualties,
                                          group = g, colour_by = c,
                                          area_name = NULL)
      tmap::tmap_save(tm_rds,
                      filename = file.path(output_dir, "maps",
                                           paste0("osm_", g, "_", c, ".html")),
                      selfcontained = TRUE)
    }
  }
  invisible(NULL)
}

#' MSOA stage: IMD relationships, slope charts, national comparisons
#' @return A list with imd_casualties, slope_dat_ew, slope_dat_la,
#'   cas_df_all_LA, casualties_simp.
#' @noRd
build_msoa_stage <- function(la_name, casualties, casualties_gb, IMD_2025,
                             decile_match, output_dir) {

  msoa_imd <- match_msoa_imd(IMD_lsoa_data = IMD_2025)

  casualties_simp <- summarise_casualty_types(casualties_gb,
                                              summary_type = "short_name") |>
    dplyr::mutate(casualty_type = short_name)

  cas_df_msoa_all <- casualties_per_MSOA(
    casualties = casualties_simp, per_capita = TRUE, by_year = FALSE,
    by_casualty = FALSE, casualty_sexes = c("Male", "Female")) |>
    dplyr::inner_join(msoa_imd)

  cas_df_all <- cas_df_msoa_all |>
    sf::st_set_geometry(NULL) |>
    dplyr::ungroup() |>
    dplyr::select(imd_weighted, msoa21hclnm, fatal_pcap, serious_pcap,
                  slight_pcap, ksi_pcap, total_pcap) |>
    tidyr::pivot_longer(-c(imd_weighted, msoa21hclnm),
                        names_to = "severity",
                        values_to = "casualties_pcap") |>
    dplyr::mutate(severity = gsub("_pcap", "", severity))

  # England & Wales, all severities
  p0 <- openair::scatterPlot(
    cas_df_all, x = "imd_weighted", y = "casualties_pcap",
    type = "severity", linear = TRUE,
    xlab = "IMD score (population-weighted MSOA)",
    ylab = "Casualty rate (per 1,000 population of MSOA)",
    main = paste0("Relationship between pcap casualty rate for all casualty",
                  " types and home MSOA IMD decile for England and Wales"))
  save_png(p0, file.path(output_dir, "plots", "msoa", "EngWal_all.png"))

  msoa_nm <- utils::read.csv(
    "https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-2.2.csv") |>
    dplyr::distinct(msoa21hclnm, localauthorityname)

  LA_cas_all <- cas_df_all |>
    dplyr::left_join(msoa_nm, by = "msoa21hclnm") |>
    dplyr::filter(grepl(la_name, localauthorityname))

  cas_df_all_LA <- dplyr::filter(cas_df_msoa_all,
                                 grepl(la_name, localauthorityname))

  p1 <- openair::scatterPlot(
    LA_cas_all, x = "imd_weighted", y = "casualties_pcap",
    type = "severity", linear = TRUE,
    xlab = "IMD score (population-weighted MSOA)",
    ylab = "Casualty rate (per 1,000 population of MSOA)",
    main = paste0("Relationship between pcap casualty rate for all casualty",
                  " types and home MSOA IMD for LA: ",
                  LA_cas_all$localauthorityname[1]))
  save_png(p1, file.path(output_dir, "plots", "msoa", "la_all.png"))

  # per casualty type and severity
  cas_df <- casualties_per_MSOA(
    casualties = casualties_simp, per_capita = TRUE, by_year = FALSE,
    by_casualty = TRUE, casualty_sexes = c("Male", "Female"))

  casualty_types <- c("Pedestrian", "Cyclist", "Motorcyclist",
                      "Car occupant", "Goods vehicle occupant",
                      "Mobility scooter rider", "Taxi occupant",
                      "E-scooter rider")

  msoa_casualties <- cas_df |>
    dplyr::filter(casualty_type %in% casualty_types) |>
    dplyr::inner_join(msoa_imd) |>
    sf::st_set_geometry(NULL) |>
    dplyr::left_join(msoa_nm, by = "msoa21hclnm")

  rates <- gsub("_rank", "",
                unique(names(msoa_casualties)[
                  grepl("_pcap_rank", names(msoa_casualties))]))

  fit_slopes <- function(df, rate) {
    df |>
      dplyr::group_by(casualty_type) |>
      dplyr::summarise(
        r2 = summary(stats::lm(!!rlang::sym(rate) ~ imd_weighted))$r.squared,
        slope = stats::coef(
          stats::lm(!!rlang::sym(rate) ~ imd_weighted))["imd_weighted"],
        .groups = "drop") |>
      dplyr::mutate(
        label = paste0("R\u00b2 = ", round(r2, 2),
                       "  |  slope = +", round(slope, 3)),
        severity = rate)
  }

  slope_list_ew <- list()
  slope_list_la <- list()
  LA_cas_home <- dplyr::filter(msoa_casualties,
                               grepl(la_name, localauthorityname))

  for (r in rates) {

    slope_list_ew[[r]] <- fit_slopes(msoa_casualties, r)

    p_ew <- openair::scatterPlot(
      msoa_casualties, x = "imd_weighted", y = r, type = "casualty_type",
      linear = TRUE,
      xlab = "IMD score (population-weighted MSOA)",
      ylab = "Casualty rate (per 1,000 population of MSOA)",
      main = paste0("Relationship between ", gsub("_pcap", "", r),
                    " casualty rate for most common casualty types and home",
                    " MSOA IMD decile for England and Wales"))
    save_png(p_ew, file.path(output_dir, "plots", "msoa",
                             paste0("EngWal_", r, ".png")))

    slope_list_la[[r]] <- fit_slopes(LA_cas_home, r)

    p_la <- openair::scatterPlot(
      LA_cas_home, x = "imd_weighted", y = r, type = "casualty_type",
      linear = TRUE,
      xlab = "IMD score (population-weighted MSOA)",
      ylab = "Casualty rate (per 1,000 population of MSOA)",
      main = paste0("Relationship between ", gsub("_pcap", "", r),
                    " casualty rate for most common casualty types and home",
                    " MSOA IMD for LA: ",
                    LA_cas_home$localauthorityname[1]))
    save_png(p_la, file.path(output_dir, "plots", "msoa",
                             paste0("la_", r, ".png")))
  }

  slope_dat_ew <- do.call(rbind, slope_list_ew)
  slope_dat_la <- do.call(rbind, slope_list_la)

  # 8 strongest LA correlations
  lcs <- slope_dat_la |>
    dplyr::arrange(dplyr::desc(r2)) |>
    dplyr::slice(1:8) |>
    dplyr::mutate(cas_sev = paste(casualty_type, severity))

  la_top8 <- LA_cas_home |>
    dplyr::ungroup() |>
    dplyr::filter(casualty_type %in% lcs$casualty_type) |>
    dplyr::select(casualty_type, imd_weighted,
                  dplyr::all_of(unique(lcs$severity))) |>
    tidyr::pivot_longer(-c(casualty_type, imd_weighted),
                        names_to = "severity", values_to = "casualties") |>
    dplyr::mutate(cas_sev = paste(casualty_type, severity)) |>
    dplyr::filter(cas_sev %in% lcs$cas_sev) |>
    dplyr::mutate(cas_sev = gsub("_pcap", "", cas_sev))

  p3 <- openair::scatterPlot(
    la_top8, x = "imd_weighted", y = "casualties", type = "cas_sev",
    linear = TRUE,
    xlab = "IMD score (population-weighted MSOA)",
    ylab = "Casualty rate (per 1,000 population of MSOA)",
    main = paste0("Relationship between casualty rate and home MSOA IMD for",
                  " the 8 strongest correlation combinations for LA: ",
                  la_name))
  # NB: the old script saved p2 here by mistake; this saves the la_8 plot
  save_png(p3, file.path(output_dir, "plots", "msoa", "la_8.png"))

  # IMD casualty breakdown for the local area
  casualties_la_simp <- casualties |>
    summarise_casualty_types(summary_type = "short_name") |>
    dplyr::mutate(casualty_type = short_name)

  imd_casualties <- casualties_la_simp |>
    dplyr::left_join(decile_match,
                     by = c("casualty_imd_decile" = "imd_decile")) |>
    dplyr::group_by(casualty_type, dft_age_band, sex_of_casualty,
                    IMDDecil) |>
    dplyr::summarise(
      fatal = sum(fatal_count),
      serious = sum(casualty_adjusted_severity_serious),
      slight = sum(casualty_adjusted_severity_slight),
      .groups = "drop") |>
    dplyr::mutate(ksi = fatal + serious,
                  total = fatal + serious + slight) |>
    dplyr::filter(!is.na(IMDDecil))

  for (s in c("fatal", "serious", "ksi", "slight", "total")) {
    for (d in c("age", "sex")) {
      plot_casualty_type_demographics(
        casualties = casualties_la_simp, demog_param = d,
        la_name = la_name, decile_match = decile_match, stat2plot = s,
        plot_dir = file.path(output_dir, "plots", "imd"))
    }
  }

  list(imd_casualties = imd_casualties, slope_dat_ew = slope_dat_ew,
       slope_dat_la = slope_dat_la, cas_df_all_LA = cas_df_all_LA,
       casualties_simp = casualties_simp)
}

#' Costs stage: TAG cost summaries, plots and tables
#' @return A list with cc, cc_mv, cc_spd, tag_costs_col, tag_costs_road.
#' @noRd
build_costs_stage <- function(crashes, vehicles, la_name, output_dir) {

  veh_col <- summarise_vehicles_per_collision(vehicles)

  crashes_costed <- crashes |>
    sf::st_set_geometry(NULL) |>
    match_tag_costs(match_with = "severity")

  cc <- crashes_costed |>
    dplyr::select(collision_index, cost_per_collision) |>
    dplyr::inner_join(veh_col)

  cc_mv <- cc |>
    dplyr::filter(Car > 0 | Motorcycle > 0 | Bus > 0 |
                    `Goods vehicle` > 0 | Taxi > 0)

  cc_spd <- crashes_costed |>
    dplyr::select(collision_index, speed_limit, cost_per_collision,
                  cost_per_casualty) |>
    dplyr::mutate(total_cost = cost_per_collision,
                  cost_per_collision = cost_per_collision -
                    cost_per_casualty) |>
    dplyr::group_by(speed_limit) |>
    dplyr::summarise(col_cost = sum(cost_per_collision),
                     cas_cost = sum(cost_per_casualty),
                     total_cost = sum(total_cost),
                     ncols = dplyr::n()) |>
    dplyr::mutate(pc_cost = total_cost / sum(total_cost),
                  tot_cost_per_col = total_cost / ncols,
                  col_cost_per_col = col_cost / ncols,
                  cas_cost_per_col = cas_cost / ncols)

  plot_summarise_tag_costs_speed(crashes = crashes, agg_level = "severity",
                                 plot_param = "total_cost", city = la_name,
                                 plot_dir = output_dir)
  plot_summarise_tag_costs_speed(crashes = crashes, agg_level = "severity",
                                 plot_param = "cost_per_col", city = la_name,
                                 plot_dir = output_dir)

  tabulate_summarise_tag_costs(crashes, city = la_name,
                               agg_level = "severity",
                               tab_dir = file.path(output_dir, "tables"),
                               file_type = ".html")

  tag_costs_col <- summarise_tag_costs(crashes_df = crashes,
                                       agg_level = "severity")

  tag_costs_road <- summarise_tag_costs(crashes_df = crashes,
                                        agg_level = "severity_road") |>
    tidyr::pivot_longer(-c(collision_year, collision_severity),
                        names_to = "road_type", values_to = "cost") |>
    dplyr::group_by(collision_year) |>
    dplyr::summarise(cost = sum(cost, na.rm = TRUE))

  plot_summarise_tag_costs(crashes, agg_level = "severity_road",
                           plot_dir = file.path(output_dir, "plots", "costs"),
                           city = la_name)
  plot_summarise_tag_costs(crashes, agg_level = "severity",
                           plot_dir = file.path(output_dir, "plots", "costs"),
                           city = la_name)

  list(cc = cc, cc_mv = cc_mv, cc_spd = cc_spd,
       tag_costs_col = tag_costs_col, tag_costs_road = tag_costs_road)
}

#' Save crash condition bar charts for the report
#' @noRd
save_condition_plots <- function(crashes, casualties, la_name, output_dir) {

  crash_vars <- c("road_surface_conditions", "junction_detail",
                  "speed_limit", "light_conditions", "weather_conditions")

  for (v in crash_vars) {
    plot_crash_conditions(crashes = crashes, casualties = casualties,
                          severities = c("Fatal", "Serious", "Slight"),
                          city = la_name, plot_width = 10, parameter = v,
                          plot_dir = file.path(output_dir, "plots",
                                               "conditions"))
    plot_crash_conditions(crashes = crashes, casualties = casualties,
                          severities = c("Fatal", "Serious"),
                          plot_width = 10, city = la_name, parameter = v,
                          plot_dir = file.path(output_dir, "plots",
                                               "conditions"))
    plot_crash_conditions(crashes = crashes, casualties = casualties,
                          severities = "Fatal", plot_width = 9,
                          city = la_name, parameter = v,
                          plot_dir = file.path(output_dir, "plots",
                                               "conditions"))
    message("conditions plotted: ", v)
  }
  invisible(NULL)
}

#' Save national LA ranking plots and choropleth maps
#' @noRd
save_national_plots <- function(crashes_gb, casualties_gb, LAs, la_name,
                                base_year, upper_year, output_dir) {

  for (ct in c("Pedestrian", "Cyclist", "All")) {
    plot_la_ranking(crashes = crashes_gb, casualties = casualties_gb,
                    casualty_types = ct, LA = la_name, la_geo = LAs,
                    severities = c("Fatal", "KSI", "Total"),
                    plot_dir = file.path(output_dir, "plots", "national"),
                    base_year = base_year, end_year = upper_year)
  }

  for (cas in c("All", "Cyclist", "Pedestrian", "Car occupant")) {

    LA_casualties <- summarise_casualties_per_la(
      casualties = casualties_gb, crashes = crashes_gb, la_geo = LAs,
      casualty_types = cas)

    cas_nam <- if (cas == "All") "All casualties" else gsub("_", " ", cas)

    for (v in c("ksi_cas", "total_cas", "fatal_cas", "serious_cas")) {

      v_nam <- gsub("_cas", "", v)

      tm1 <- map_la_casualties(region_sf = LA_casualties, variable = v,
                               home_LA = la_name, start_year = upper_year,
                               end_year = upper_year,
                               palette = "wes.zissou1",
                               breaks_style = "kmeans",
                               title = paste(v_nam, cas_nam))
      tmap::tmap_save(tm1,
                      file.path(output_dir, "plots", "national",
                                paste0("la_casualties_", cas, "_", v,
                                       "_LY.png")),
                      width = 4500, height = 5000, dpi = 650)

      tm2 <- map_la_casualties(region_sf = LA_casualties, variable = v,
                               home_LA = la_name, start_year = base_year,
                               end_year = upper_year,
                               palette = "wes.zissou1",
                               breaks_style = "kmeans",
                               title = paste(v_nam, cas_nam))
      tmap::tmap_save(tm2,
                      file.path(output_dir, "plots", "national",
                                paste0("la_casualties_", cas, "_", v,
                                       "_ALL.png")),
                      width = 4500, height = 5000, dpi = 650)
    }
  }
  invisible(NULL)
}

#' Save an openair/lattice plot to PNG
#' @noRd
save_png <- function(p, filename, width = 2300, height = 1500, res = 190) {
  grDevices::png(filename, width = width, height = height, units = "px",
                 res = res)
  print(p)
  grDevices::dev.off()
  invisible(filename)
}
