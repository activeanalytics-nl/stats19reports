#' stats19stats: Local Authority Road Safety Reports from STATS19 Data
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
#' @importFrom rlang sym .data
#' @importFrom stats coef lm setNames weighted.mean
#' @importFrom utils download.file read.csv
#' @importFrom grDevices dev.off png
"_PACKAGE"

# suppress R CMD check notes for NSE column names used throughout
utils::globalVariables(c(
  ".", "...1", "...3", "...4", "...5", "Collisions", "Fatal", "IMDDecil",
  "KSI", "LAD22NM", "LSOA.2021.Code", "LSOA21CD", "LSOA21NM", "Motorway",
  "Serious", "Slight", "Total", "access", "age_band", "age_of_casualty",
  "built_up", "cas_sev", "casualties_pcap", "casualty_adjusted_severity_serious",
  "casualty_adjusted_severity_slight", "casualty_imd_decile",
  "casualty_severity", "casualty_type", "collision_index",
  "collision_severity", "collision_year", "cost", "cost_per_casualty",
  "cost_per_collision", "dft_age_band", "fatal", "fatal_count", "fatal_km",
  "fatal_pcap", "geom", "geometry", "highway", "imd_weighted",
  "junction_detail", "ksi", "ksi_pcap", "length", "localauthorityname",
  "lsoa21cd", "lsoa21nm", "maxspeed", "msoa21hclnm", "name",
  "not_built_up", "number_of_collisions", "ons_road", "osm_id", "pc",
  "pc_all", "pc_ksi", "pc_length", "pop", "r2", "ref", "serious",
  "serious_pcap", "service", "severity", "sex_of_casualty", "short_name",
  "slight", "slight_pcap", "speed_limit", "tot_length", "total",
  "total_cas", "total_pcap", "vehicle_type", "year"
))
