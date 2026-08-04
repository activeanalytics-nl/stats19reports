#' stats19reports: Local Authority Road Safety Reports from STATS19 Data
#'
#' Builds reproducible road safety reports for any Great Britain Local
#' Authority from DfT STATS19 collision, casualty and vehicle data.
#'
#' The typical workflow is:
#' \enumerate{
#'   \item \code{\link{build_la_report_data}("Bristol")} - downloads data,
#'     computes all summaries and writes plots/maps/tables plus an RDS of
#'     report objects to \code{outputs/Bristol/}.
#'   \item \code{\link{render_la_report}("Bristol")} - renders the packaged
#'     Quarto template against those outputs.
#' }
#' Or both in one call with \code{\link{la_report}("Bristol")}.
#'
#' @keywords internal
#' @import dplyr
#' @import ggplot2
#' @import gt
#' @import tmap
#' @import sf
#' @importFrom rlang sym .data :=
#' @importFrom stats coef lm setNames weighted.mean runif
#' @importFrom utils download.file read.csv
#' @importFrom grDevices dev.off png graphics.off
"_PACKAGE"

# suppress R CMD check notes for NSE column names used throughout
utils::globalVariables(c(
  ".", "...1", "...3", "...4", "...5", ".env", "All", "Bus",
  "C2021_RESTYPE_3_NAME", "CNTR_CODE", "Car", "Casualty type",
  "Collision data year", "Collisions", "Fatal", "GEOGRAPHY_CODE",
  "Goods vehicle", "IMDDecil", "KSI", "LAD22CD", "LAD22NM", "LADCD",
  "LADNM", "LSOA.2021.Code", "LSOA21CD", "LSOA21NM", "ML", "MSOA21CD",
  "Motorcycle", "Motorway", "NUTS_NAME", "OBS_VALUE", "SHAPE", "Serious",
  "Severity", "Slight", "Taxi", "Total", "X", "X2024", "Year", "access",
  "age_band", "age_of_casualty", "area", "area_city_km2", "built_up",
  "cas_cost", "cas_sev", "casualties", "casualties_pcap",
  "casualty_adjusted_severity_serious", "casualty_adjusted_severity_slight",
  "casualty cost", "casualty_cost", "casualty_imd_decile",
  "casualty_severity", "casualty_type", "col_cost", "collision cost",
  "collision_cost", "collision_hr", "collision_index", "collision_rank",
  "collision_severity", "collision_year", "collisions", "collisions_pcap",
  "collisions_rank", "cost", "cost category", "cost_per_casualty",
  "cost_per_collision", "crashes", "cycle_paths", "datetime",
  "day_of_week", "detailed_segregation", "dft_age_band", "dist2city_km",
  "distances", "dow", "driving_routes", "escooter_flag", "fatal",
  "fatal_cas", "fatal_count", "fatal_km", "fatal_pcap", "fatal_pcap_rank",
  "fatal_rank", "first_road_class", "first_road_number", "geom",
  "geometry", "highway", "imd_decile", "imd_pc", "imd_weighted",
  "is_serious", "junction_detail", "ksi", "ksi_cas", "ksi_pcap",
  "ksi_pcap_rank", "ksi_rank", "length", "light_conditions",
  "localauthorityname", "lsoa01_code", "lsoa11_code", "lsoa11_name",
  "lsoa21_code", "lsoa21_code.x", "lsoa21_name", "lsoa21cd", "lsoa21nm",
  "lsoa_of_casualty", "lsoa_of_driver", "maxspeed", "mean_age",
  "msoa21cd", "msoa21hclnm", "n_fatal", "n_serious", "n_slight", "name",
  "ncols", "not_built_up", "number_of_casualties",
  "number_of_collisions", "number_of_vehicles", "number_vehicles",
  "ons_road", "osm_id", "pc", "pc_all", "pc_ksi", "pc_length", "pc_total",
  "pedestrian_location", "persons", "pop", "population", "r2", "ref",
  "road type", "road_surface_conditions", "second_road_class",
  "second_road_number", "seg_cycle", "serious", "serious_cas",
  "serious_pcap", "serious_pcap_rank", "serious_rank", "service",
  "severity", "sex_of_casualty", "short_name", "slight", "slight_cas",
  "slight_pcap", "slight_rank", "slope", "speed_limit", "time",
  "tot_length", "total", "total_cas", "total_casualties", "total_cost",
  "total_pcap", "total_pcap_rank", "total_rank", "value", "variable",
  "vehicle_type", "weather_conditions", "y", "year", "# collisions"
))
