
#' Match crashes to TAG collision cost data
#'
#' Downloads the DfT RAS4001 cost tables (value of prevention) and joins them
#' to a crash data frame by severity and year, optionally split by road type,
#' adding per-casualty and per-collision cost columns.
#'
#' This is the function previously referred to as \code{match_tag()} in
#' several call sites; all internal callers now use this name and signature.
#'
#' @param crashes An \code{sf} or plain data frame of crash records with
#'   \code{collision_year} and \code{collision_severity} columns. For
#'   \code{match_with = "severity_road"} it must also contain
#'   \code{first_road_class}, \code{speed_limit} and
#'   \code{urban_or_rural_area}.
#' @param match_with Character. One of \code{"severity"} (costs by severity
#'   and year) or \code{"severity_road"} (costs also split by built-up /
#'   non-built-up / motorway). Default \code{"severity"}.
#' @param ras4001_url Character. URL of the published RAS4001 ODS file.
#' @return The crash data enriched with TAG cost columns
#'   (\code{cost_per_casualty} and \code{cost_per_collision} for
#'   \code{"severity"}; a long-format \code{cost} column for
#'   \code{"severity_road"}).
#' @examples
#' \dontrun{
#' crashes_costed <- match_tag_costs(crashes, match_with = "severity")
#' }
#' @export
match_tag_costs <- function(
    crashes,
    match_with = c("severity", "severity_road"),
    ras4001_url = "https://assets.publishing.service.gov.uk/media/68d421cc275fc9339a248c8e/ras4001.ods") {

  match_with <- match.arg(match_with)

  tmpfile <- tempfile(fileext = ".ods")
  utils::download.file(ras4001_url, destfile = tmpfile, mode = "wb", quiet = TRUE)

  if (match_with == "severity") {

    # average value sheet: cost per casualty/collision by year and severity
    ras4001 <- readODS::read_ods(tmpfile, sheet = "Average_value",
                                 skip = 5, col_names = FALSE) |>
      dplyr::transmute(
        collision_year = ...1,
        collision_severity = ...3,
        cost_per_casualty = as.numeric(...4),
        cost_per_collision = as.numeric(...5)
      )

    out <- crashes |>
      dplyr::left_join(ras4001, by = c("collision_year", "collision_severity"))

    return(out)
  }

  if (match_with == "severity_road") {

    ras4001 <- readODS::read_ods(tmpfile, sheet = "Average_value_road_type",
                                 skip = 3)

    # the cost columns contain a GBP symbol, which can't be written portably
    # in R source, so rename by pattern match instead
    nm <- names(ras4001)
    names(ras4001)[grepl("^Built-up roads", nm)] <- "built_up"
    names(ras4001)[grepl("^Non built-up roads", nm)] <- "not_built_up"
    names(ras4001)[grepl("^Motorways", nm)] <- "Motorway"

    ras4001 <- ras4001 |>
      dplyr::transmute(
        collision_year = `Collision data year`,
        collision_severity = Severity,
        built_up,
        not_built_up,
        Motorway
      ) |>
      dplyr::filter(collision_severity %in% c("Fatal", "Serious", "Slight")) |>
      tidyr::pivot_longer(-c(collision_year, collision_severity),
                          names_to = "ons_road", values_to = "cost")

    # classify each crash as motorway / built-up / not built-up, falling back
    # to urban/rural where speed limit is missing
    out <- crashes |>
      dplyr::mutate(speed_limit = as.numeric(speed_limit)) |>
      dplyr::mutate(ons_road = dplyr::case_when(
        first_road_class == "Motorway" ~ "Motorway",
        !is.na(speed_limit) & speed_limit <= 40 ~ "built_up",
        !is.na(speed_limit) & speed_limit > 40 ~ "not_built_up",
        urban_or_rural_area == "Urban" ~ "built_up",
        urban_or_rural_area == "Rural" ~ "not_built_up",
        TRUE ~ NA_character_
      )) |>
      dplyr::left_join(ras4001,
                       by = c("collision_year", "collision_severity", "ons_road"))

    return(out)
  }
}

#' Match LSOA codes to 2021 equivalents
#'
#' Matches casualty or vehicle LSOA codes to 2021 LSOA codes using official
#' lookup tables from the `geographr` package.
#'
#' @param casualties Optional casualty data frame with \code{lsoa_of_casualty}.
#' @param vehicles Optional vehicle data frame with \code{lsoa_of_driver}.
#' @return The input data frame with added 2021 LSOA codes and names.
#' @examples
#' \dontrun{
#' cas_lsoa21 <- match_lsoa_2021(casualties = my_casualties)
#' veh_lsoa21 <- match_lsoa_2021(vehicles = my_vehicles)
#' }
#' @export
match_lsoa_2021 <- function(casualties = NULL,
                            vehicles = NULL) {
  if (!is.null(casualties)) {
    df2match <- casualties
    col_nam <- "lsoa_of_casualty"
  } else {
    df2match <- vehicles
    col_nam <- "lsoa_of_driver"
  }
  
  # lookup tables
  lsoa_lookup_01 <- geographr::lookup_lsoa01_lsoa11 %>%
    dplyr::select(lsoa01_code, lsoa11_name, lsoa11_code) %>%
    dplyr::distinct(lsoa11_code, .keep_all = TRUE)
  
  lsoa_lookup_21 <- geographr::lookup_lsoa11_lsoa21_ltla22 %>%
    dplyr::select(lsoa11_code, lsoa21_name, lsoa21_code)
  
  # stage 1: 01 -> 11 -> 21
  lsoas_1 <- df2match %>%
    dplyr::select(dplyr::all_of(col_nam)) %>%
    dplyr::left_join(lsoa_lookup_01,
                     by = setNames("lsoa01_code", col_nam)) %>%
    dplyr::filter(!is.na(lsoa11_code)) %>%
    dplyr::select(dplyr::all_of(col_nam), lsoa11_code) %>%
    dplyr::left_join(lsoa_lookup_21, by = "lsoa11_code") %>%
    dplyr::select(dplyr::all_of(col_nam), lsoa21_code, lsoa21_name)
  
  # stage 2: 11 -> 21
  lsoas_2 <- df2match %>%
    dplyr::select(dplyr::all_of(col_nam)) %>%
    dplyr::left_join(lsoa_lookup_21,
                     by = setNames("lsoa11_code", col_nam)) %>%
    dplyr::filter(!is.na(lsoa21_code)) %>%
    dplyr::select(dplyr::all_of(col_nam), lsoa21_code, lsoa21_name)
  
  # stage 3: already 21
  lsoas_3 <- df2match %>%
    dplyr::select(dplyr::all_of(col_nam)) %>%
    dplyr::left_join(lsoa_lookup_21,
                     by = setNames("lsoa21_code", col_nam)) %>%
    dplyr::filter(!is.na(lsoa21_name)) %>%
    dplyr::select(dplyr::all_of(col_nam), lsoa21_name) %>%
    dplyr::mutate(lsoa21_code = !!rlang::sym(col_nam))
  
  # combine
  lsoas <- dplyr::bind_rows(lsoas_1, lsoas_2, lsoas_3) %>%
    dplyr::distinct(!!rlang::sym(col_nam), .keep_all = TRUE)
  
  df_lsoa <- df2match %>%
    dplyr::left_join(lsoas, by = col_nam)
  
  df_lsoa
}

#' Compute population-weighted IMD decile for MSOAs
#'
#' Aggregates LSOA-level Index of Multiple Deprivation (IMD 2025) data to MSOA
#' level using a population-weighted mean of IMD deciles. Downloads the IMD
#' GeoPackage from GitHub if not supplied.
#'
#' @param IMD_lsoa_data Optional \code{sf} data frame of LSOA-level IMD data.
#'   If \code{NULL} (the default), the function downloads it from GitHub.
#' @return A data frame with columns \code{MSOA21CD},
#'   \code{imd_weighted} (population-weighted mean IMD decile), and
#'   \code{n_lsoa} (number of LSOAs in each MSOA).
#' @examples
#' \dontrun{
#' msoa_imd <- match_msoa_imd()
#' msoa_imd <- match_msoa_imd(IMD_lsoa_data = my_imd_sf)
#' }
#' @export
match_msoa_imd = function(IMD_lsoa_data = NULL){
  
  if(is.null(IMD_lsoa_data)){
    
    IMD_lsoa_data = st_read("https://github.com/BlaiseKelly/IMD/releases/download/LSOA_IMD2025/LSOA_IMD2025_WGS84_-4854136717238973930.gpkg")
    
  }
  
  lsoa_geo = get_lsoa21_boundaries(provider = "geographr") |> 
    st_transform(27700)
  
  lsoa_cent = st_centroid(lsoa_geo)
  
  lsoa_pop = read.csv("https://github.com/BlaiseKelly/lsoa_ons_population/releases/download/v0.1.1/lsoa21_pop_tot_2011_2024.csv") |> 
    select(lsoa21cd,pop = X2024)
  
  msoa_geo = st_read("https://github.com/BlaiseKelly/stats19_stats/releases/download/msoa_boundaries-v1.0/msoa.gpkg") |> 
    st_transform(27700) |> 
    select(MSOA21CD,geom)
  
  msoa_lsoa = st_join(msoa_geo,lsoa_cent) |> 
    select(MSOA21CD,lsoa21_code) |> 
    st_set_geometry(NULL)
  
  msoa_imd <- IMD_lsoa_data |> 
    st_set_geometry(NULL) |> 
    left_join(lsoa_pop, by = c("LSOA21CD" = "lsoa21cd")) |> 
    left_join(msoa_lsoa, by = c("LSOA21CD" = "lsoa21_code")) |> 
    group_by(MSOA21CD) %>%
    summarise(
      imd_weighted = weighted.mean(IMDDecil, w = pop, na.rm = TRUE),
      n_lsoa = n()
    )
  
  return(msoa_imd)
  
}

#' Match crashes to nearest OSM road segment
#'
#' Assigns each crash record to the nearest driving network segment from OSM.
#'
#' @param osm_network_sf An \code{sf} object of OSM network data.
#' @param crash_sf An \code{sf} object of crash points.
#' @return The crash \code{sf} object with an added \code{osm_id} column.
#' @examples
#' \dontrun{
#' crashes_matched <- match_crashes_to_osm(osm_network_sf = osm_data,
#'                                         crash_sf = crashes)
#' }
#' @export
match_crashes_to_osm <- function(osm_network_sf, crash_sf) {
  drive_net <- osmactive::get_driving_network(osm_network_sf)
  
  crs_osm <- sf::st_crs(drive_net)
  crash_sf <- sf::st_transform(crash_sf, crs_osm)
  
  crash_sf$osm_id <- drive_net$osm_id[sf::st_nearest_feature(crash_sf, drive_net)]
  
  crash_sf
}
