# Helpers that generate the inline text and summary statistics used by the
# LA report Quarto template. These replace the long ifelse()/paste0() chains
# and hidden "calc" chunks that previously lived in LA_report_default.qmd.

#' Describe a change between two values in words
#'
#' Returns phrases like \code{"up by 12"} / \code{"down by 3"} (style
#' \code{"by"}), \code{"an increase"} / \code{"a decrease"} (style
#' \code{"increase"}), or \code{"up on 2023"} / \code{"down on 2023"}
#' (style \code{"on"}, using \code{ref}).
#'
#' @param new,old Numeric. The later and earlier values.
#' @param style Character. One of \code{"by"}, \code{"increase"},
#'   \code{"on"}.
#' @param ref Character or numeric. Reference label (e.g. a year) for
#'   \code{style = "on"}.
#' @return A character string.
#' @examples
#' change_phrase(120, 100)
#' change_phrase(90, 100, style = "increase")
#' change_phrase(90, 100, style = "on", ref = 2023)
#' @export
change_phrase <- function(new, old, style = c("by", "increase", "on"),
                          ref = NULL) {
  style <- match.arg(style)
  diff <- round(new - old)
  switch(style,
    by = if (diff >= 0) paste0("up by ", diff) else paste0("down by ", abs(diff)),
    increase = if (diff >= 0) "an increase" else "a decrease",
    on = if (diff >= 0) paste0("up on ", ref) else paste0("down on ", ref)
  )
}

#' Generate the LA summary paragraph for a casualty type
#'
#' Produces the markdown paragraph summarising an LA's latest-year casualty
#' figures, national rankings and year-on-year change, for a given casualty
#' type. Replaces three near-identical hand-written paragraphs in the report
#' template.
#'
#' @param ly,ybly One-row data frames for the latest year and the year
#'   before (from [summarise_casualties_per_la()], filtered).
#' @param n_las Integer. Number of Local Authorities ranked against.
#' @param ybly_year Integer. The comparison year (for "up on YYYY" phrases).
#' @param la_name Character. Local Authority name.
#' @param casualty_label Character. \code{""} for all casualties, otherwise
#'   e.g. \code{"cyclist"} or \code{"pedestrian"} (lower case).
#' @return A character string of markdown/prose.
#' @examples
#' \dontrun{
#' la_summary_paragraph(LA_LY, LA_YBLY, n_las = 206, ybly_year = 2024,
#'                      la_name = "Bristol", casualty_label = "")
#' }
#' @export
la_summary_paragraph <- function(ly, ybly, n_las, ybly_year, la_name,
                                 casualty_label = "") {

  cas_lab <- if (nzchar(casualty_label)) paste0(casualty_label, " ") else ""
  subject <- if (nzchar(casualty_label)) {
    paste0("For ", casualty_label, "s there were ")
  } else {
    paste0("In ", la_name, " there were ")
  }

  rank_sentence <- function(rank, pcap_rank, n, label, unit = "per capita") {
    paste0("For ", label, " it ranked ", scales::ordinal(rank), " (",
           scales::ordinal(pcap_rank), " ", unit, ") with ",
           round(n), " ", cas_lab, "casualties ",
           change_phrase(n, NA, style = "by"))
  }

  paste0(
    subject, ly$collisions, " collisions resulting in ",
    round(ly$total_cas), " ", cas_lab,
    "casualties reported to the police. This is the ",
    scales::ordinal(ly$total_rank), " highest of ", n_las,
    " Local Authorities in Great Britain (",
    scales::ordinal(ly$total_pcap_rank), " per capita) and ",
    change_phrase(ly$collisions, ybly$collisions, style = "increase"),
    " in collisions of ", abs(round(ly$collisions - ybly$collisions)),
    if (ly$total_cas >= ybly$total_cas) " and an increase" else " but a decrease",
    " in total casualties of ", abs(round(ly$total_cas - ybly$total_cas)),
    ". For fatal it ranked ", scales::ordinal(ly$fatal_rank), " (",
    scales::ordinal(ly$fatal_pcap_rank), " per capita) with ",
    round(ly$fatal_cas), " ", cas_lab, "casualties ",
    change_phrase(ly$fatal_cas, ybly$fatal_cas, style = "on", ref = ybly_year),
    ". For KSIs it ranked ", scales::ordinal(ly$ksi_rank), " (",
    scales::ordinal(ly$ksi_pcap_rank), " per capita) with ",
    round(ly$ksi_cas), " casualties ",
    change_phrase(ly$ksi_cas, ybly$ksi_cas, style = "on", ref = ybly_year),
    ". For serious injuries it ranked ", scales::ordinal(ly$serious_rank),
    " (", scales::ordinal(ly$serious_pcap_rank), " per capita) with ",
    round(ly$serious_cas), " ", cas_lab, "casualties ",
    change_phrase(ly$serious_cas, ybly$serious_cas, style = "on",
                  ref = ybly_year),
    ". For slight injuries it ranked ", scales::ordinal(ly$slight_rank),
    " (", scales::ordinal(ly$slight_pcap_rank), " per capita) with ",
    round(ly$slight_cas), " ", cas_lab, "casualties ",
    change_phrase(ly$slight_cas, ybly$slight_cas, style = "on",
                  ref = ybly_year), "."
  )
}

#' Generate the five-year comparison sentence for a casualty type
#'
#' Produces the "Compared to 5 years ago..." sentence comparing collisions
#' and casualty counts against the base year.
#'
#' @param ly,base One-row data frames for the latest year and the base year.
#' @param casualty_label Character. \code{""} for all casualties, otherwise
#'   e.g. \code{"cyclists"} / \code{"pedestrians"} (plural, lower case).
#' @return A character string of prose.
#' @examples
#' \dontrun{
#' five_year_sentence(LA_LY, LA_5Y)
#' five_year_sentence(LA_LY_CYC, LA_5Y_CYC, casualty_label = "cyclists")
#' }
#' @export
five_year_sentence <- function(ly, base, casualty_label = "") {

  involving <- if (nzchar(casualty_label)) {
    paste0(" involving ", casualty_label)
  } else {
    ""
  }

  paste0(
    "Compared to 5 years ago collisions", involving, " are ",
    change_phrase(ly$collisions, base$collisions),
    ", total casualties ", change_phrase(ly$total_cas, base$total_cas),
    ", fatalities ", change_phrase(ly$fatal_cas, base$fatal_cas),
    ", KSI casualties ", change_phrase(ly$ksi_cas, base$ksi_cas),
    ", serious injuries ", change_phrase(ly$serious_cas, base$serious_cas),
    " and slight injuries ", change_phrase(ly$slight_cas, base$slight_cas),
    "."
  )
}

#' Summary statistics for the OSM road network section
#'
#' Extracts the headline numbers (top roads, peak year, dominant casualty
#' type) used in the "Road Network Analysis" prose.
#'
#' @param cas_osm_period,cas_osm_year,cas_osm_type Data frames from
#'   [summarise_osm_link_casualties()] grouped by total, year and casualty
#'   type respectively.
#' @return A named list of scalars used for inline text.
#' @examples
#' \dontrun{
#' osm <- report_osm_stats(cas_osm_period, cas_osm_year, cas_osm_type)
#' osm$top_road_name
#' }
#' @export
report_osm_stats <- function(cas_osm_period, cas_osm_year, cas_osm_type) {

  osm_sorted <- dplyr::arrange(cas_osm_period,
                               dplyr::desc(number_of_collisions))

  yearly_totals <- cas_osm_year |>
    dplyr::group_by(year) |>
    dplyr::summarise(n = sum(number_of_collisions, na.rm = TRUE))

  type_totals <- cas_osm_type |>
    dplyr::group_by(casualty_type) |>
    dplyr::summarise(n = sum(number_of_collisions, na.rm = TRUE)) |>
    dplyr::arrange(dplyr::desc(n))

  list(
    n_links = NROW(cas_osm_period),
    total_collisions = sum(cas_osm_period$number_of_collisions, na.rm = TRUE),
    top_road_name = osm_sorted$name[1],
    top_road_collisions = osm_sorted$number_of_collisions[1],
    second_road_name = osm_sorted$name[2],
    second_road_collisions = osm_sorted$number_of_collisions[2],
    n_years = length(unique(cas_osm_year$year)),
    peak_year = yearly_totals$year[which.max(yearly_totals$n)],
    peak_year_n = max(yearly_totals$n),
    top_type = type_totals$casualty_type[1],
    top_type_n = type_totals$n[1]
  )
}

#' Summary statistics for the IMD casualty section
#'
#' Extracts decile 1 vs decile 10 shares and the peak decile from the IMD
#' casualty summary, plus KSI breakdowns by casualty type, age and sex.
#'
#' @param imd_casualties A data frame of casualties grouped by casualty
#'   type, age band, sex and \code{IMDDecil} (built by
#'   [build_la_report_data()]).
#' @param main_types Character vector of casualty types to include in the
#'   KSI breakdowns.
#' @return A named list of scalars and small data frames for inline text.
#' @examples
#' \dontrun{
#' imd <- report_imd_stats(imd_casualties)
#' imd$d1_pc
#' }
#' @export
report_imd_stats <- function(imd_casualties,
                             main_types = c("Car occupant", "Cyclist",
                                            "E-scooter rider", "Motorcyclist",
                                            "Pedestrian")) {

  imd_total <- sum(imd_casualties$total, na.rm = TRUE)

  d1_n <- sum(imd_casualties$total[imd_casualties$IMDDecil == 1], na.rm = TRUE)
  d10_n <- sum(imd_casualties$total[imd_casualties$IMDDecil == 10], na.rm = TRUE)

  decile_totals <- imd_casualties |>
    dplyr::group_by(IMDDecil) |>
    dplyr::summarise(total = sum(total, na.rm = TRUE))

  df <- dplyr::filter(imd_casualties, casualty_type %in% main_types)

  top_imd <- df |>
    dplyr::group_by(IMDDecil) |>
    dplyr::summarise(ksi = sum(ksi, na.rm = TRUE)) |>
    dplyr::slice_max(ksi, n = 1)

  top_imd_type <- df |>
    dplyr::group_by(casualty_type, IMDDecil) |>
    dplyr::summarise(ksi = sum(ksi, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(casualty_type) |>
    dplyr::slice_max(ksi, n = 1, with_ties = FALSE)

  top_age_type <- df |>
    dplyr::group_by(casualty_type, dft_age_band) |>
    dplyr::summarise(ksi = sum(ksi, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(casualty_type) |>
    dplyr::slice_max(ksi, n = 1, with_ties = FALSE)

  sex_split <- df |>
    dplyr::group_by(sex_of_casualty) |>
    dplyr::summarise(ksi = sum(ksi, na.rm = TRUE)) |>
    dplyr::arrange(dplyr::desc(ksi))

  top_type <- df |>
    dplyr::group_by(casualty_type) |>
    dplyr::summarise(ksi = sum(ksi, na.rm = TRUE)) |>
    dplyr::slice_max(ksi, n = 1)

  list(
    total = imd_total,
    d1_n = d1_n,
    d1_pc = round(d1_n / imd_total * 100, 1),
    d10_n = d10_n,
    d10_pc = round(d10_n / imd_total * 100, 1),
    ratio = round(d1_n / d10_n, 1),
    peak_decile = decile_totals$IMDDecil[which.max(decile_totals$total)],
    peak_n = max(decile_totals$total, na.rm = TRUE),
    peak_pc = round(max(decile_totals$total, na.rm = TRUE) / imd_total * 100, 1),
    top_imd = top_imd,
    top_imd_type = top_imd_type,
    top_age_type = top_age_type,
    sex_split = sex_split,
    top_type = top_type
  )
}

#' Summary statistics for the casualty demographics section
#'
#' Extracts male/female shares, peak age bands and KSI splits from the
#' age/sex summary for inline text.
#'
#' @param age_sex A data frame from [summarise_casualties_by_demog()].
#' @return A named list of scalars for inline text.
#' @examples
#' \dontrun{
#' demog <- report_demog_stats(age_sex)
#' demog$male_pc
#' }
#' @export
report_demog_stats <- function(age_sex) {

  sex_totals <- age_sex |>
    dplyr::group_by(sex_of_casualty) |>
    dplyr::summarise(
      n_fatal = sum(Fatal, na.rm = TRUE),
      n_serious = sum(Serious, na.rm = TRUE),
      n_slight = sum(Slight, na.rm = TRUE),
      n_ksi = sum(KSI, na.rm = TRUE),
      n_all = n_fatal + n_serious + n_slight
    )

  total_all <- sum(sex_totals$n_all)
  male_n <- sex_totals$n_all[sex_totals$sex_of_casualty == "Male"]
  female_n <- sex_totals$n_all[sex_totals$sex_of_casualty == "Female"]
  male_ksi <- sex_totals$n_ksi[sex_totals$sex_of_casualty == "Male"]

  age_totals <- age_sex |>
    dplyr::group_by(age_band) |>
    dplyr::summarise(n = sum(Fatal + Serious + Slight, na.rm = TRUE)) |>
    dplyr::mutate(pc = round(n / sum(n) * 100, 1))

  age_ksi <- age_sex |>
    dplyr::group_by(age_band) |>
    dplyr::summarise(ksi = sum(KSI, na.rm = TRUE)) |>
    dplyr::mutate(pc = round(ksi / sum(ksi) * 100, 1))

  list(
    male_pc = round(male_n / total_all * 100, 1),
    female_pc = round(female_n / total_all * 100, 1),
    male_ksi_pc = round(male_ksi / sum(sex_totals$n_ksi) * 100, 1),
    peak_age = age_totals$age_band[which.max(age_totals$n)],
    peak_age_pc = age_totals$pc[which.max(age_totals$n)],
    peak_ksi_age = age_ksi$age_band[which.max(age_ksi$ksi)],
    peak_ksi_age_pc = age_ksi$pc[which.max(age_ksi$ksi)]
  )
}

#' Summary statistics for the pavement collisions section
#'
#' Extracts totals and vehicle-group shares (car/goods/taxi/bus vs pedal
#' cycle/e-scooter) for the "Single Vehicle Pavement Collisions" prose, for
#' either the LA or national data.
#'
#' @param sing_veh_pave A data frame from
#'   [summarise_casualties_pavements()].
#' @return A named list of scalars for inline text.
#' @examples
#' \dontrun{
#' pave_la <- report_pavement_stats(sing_veh_pave)
#' pave_gb <- report_pavement_stats(sing_veh_pave_gb)
#' }
#' @export
report_pavement_stats <- function(sing_veh_pave) {

  motor <- dplyr::filter(sing_veh_pave,
                         vehicle_type %in% c("Car", "Goods vehicle",
                                             "Taxi", "Bus"))
  micro <- dplyr::filter(sing_veh_pave,
                         vehicle_type %in% c("Pedal cycle", "e-scooter"))

  sorted <- dplyr::arrange(sing_veh_pave, dplyr::desc(Total))

  tot_coll <- sum(sing_veh_pave$Collisions, na.rm = TRUE)
  tot_fatal <- sum(sing_veh_pave$Fatal, na.rm = TRUE)
  tot_serious <- sum(sing_veh_pave$Serious, na.rm = TRUE)

  pc <- function(x, tot) if (tot > 0) round(x / tot * 100) else NA_real_

  list(
    total_collisions = tot_coll,
    total_cas = sum(sing_veh_pave$Total, na.rm = TRUE),
    total_fatal = tot_fatal,
    total_serious = round(tot_serious),
    top_type = sorted$vehicle_type[1],
    top_n = sorted$Total[1],
    top_coll = sorted$Collisions[1],
    motor_coll_pc = pc(sum(motor$Collisions, na.rm = TRUE), tot_coll),
    micro_coll_pc = pc(sum(micro$Collisions, na.rm = TRUE), tot_coll),
    motor_fatal_pc = pc(sum(motor$Fatal, na.rm = TRUE), tot_fatal),
    micro_fatal_pc = pc(sum(micro$Fatal, na.rm = TRUE), tot_fatal),
    motor_serious_pc = pc(sum(motor$Serious, na.rm = TRUE), tot_serious),
    micro_serious_pc = pc(sum(micro$Serious, na.rm = TRUE), tot_serious)
  )
}

#' Summary statistics for the MSOA casualty-home section
#'
#' Extracts the top MSOA by casualty count, most/least deprived MSOAs and
#' best-ranked per-capita MSOAs for the "MSOA" prose.
#'
#' @param cas_df_all_LA An \code{sf} data frame of MSOA-level casualty
#'   summaries for the LA.
#' @return A named list of scalars for inline text.
#' @examples
#' \dontrun{
#' msoa <- report_msoa_stats(cas_df_all_LA)
#' msoa$top_msoa
#' }
#' @export
report_msoa_stats <- function(cas_df_all_LA) {

  list(
    n_msoa = NROW(unique(cas_df_all_LA$msoa21hclnm)),
    top_msoa = cas_df_all_LA$msoa21hclnm[which.max(cas_df_all_LA$total_cas)],
    top_msoa_n = max(cas_df_all_LA$total_cas, na.rm = TRUE),
    most_deprived_msoa =
      cas_df_all_LA$msoa21hclnm[which.min(cas_df_all_LA$imd_weighted)],
    most_deprived_imd = round(min(cas_df_all_LA$imd_weighted, na.rm = TRUE), 1),
    least_deprived_msoa =
      cas_df_all_LA$msoa21hclnm[which.max(cas_df_all_LA$imd_weighted)],
    least_deprived_imd = round(max(cas_df_all_LA$imd_weighted, na.rm = TRUE), 1),
    top_fatal_pcap_msoa = cas_df_all_LA$msoa21hclnm[
      cas_df_all_LA$fatal_pcap_rank == min(cas_df_all_LA$fatal_pcap_rank)][1],
    top_fatal_pcap_rank = min(cas_df_all_LA$fatal_pcap_rank),
    top_ksi_pcap_msoa = cas_df_all_LA$msoa21hclnm[
      cas_df_all_LA$ksi_pcap_rank == min(cas_df_all_LA$ksi_pcap_rank)][1],
    top_ksi_pcap_rank = min(cas_df_all_LA$ksi_pcap_rank),
    top_serious_pcap_msoa = cas_df_all_LA$msoa21hclnm[
      cas_df_all_LA$serious_pcap_rank == min(cas_df_all_LA$serious_pcap_rank)][1],
    top_serious_pcap_rank = min(cas_df_all_LA$serious_pcap_rank)
  )
}

#' Summary statistics for the speed limit section
#'
#' Extracts the headline shares and rates used in the "Speed Limits" prose,
#' with safe handling where a speed limit category is missing.
#'
#' @param csl A data frame of casualties/collisions by speed limit including
#'   per-km and per-collision rates.
#' @return A named list of scalars for inline text.
#' @examples
#' \dontrun{
#' spd <- report_speed_stats(csl)
#' spd$majority_limit
#' }
#' @export
report_speed_stats <- function(csl) {

  at <- function(col, limit) {
    v <- csl[[col]][csl$speed_limit == limit]
    if (length(v) == 0) NA_real_ else v[1]
  }

  list(
    majority_limit = csl$speed_limit[which.max(csl$pc_length)],
    majority_pc = round(max(csl$pc_length, na.rm = TRUE)),
    max_coll = round(max(csl$collisions, na.rm = TRUE)),
    max_coll_limit = csl$speed_limit[which.max(csl$collisions)],
    fatal_pc_at_max = round(max(csl$fatal, na.rm = TRUE) /
                              sum(csl$fatal, na.rm = TRUE) * 100),
    max_fatal_limit = csl$speed_limit[which.max(csl$fatal)],
    pc_length_30 = round(at("pc_length", 30)),
    max_serious = round(max(csl$serious, na.rm = TRUE)),
    max_serious_limit = csl$speed_limit[which.max(csl$serious)],
    max_coll_km_limit = csl$speed_limit[which.max(csl$coll_km)],
    max_fatal_km_limit = csl$speed_limit[which.max(csl$fatal_km)],
    max_serious_km_limit = csl$speed_limit[which.max(csl$serious_km)],
    max_slight_km_limit = csl$speed_limit[which.max(csl$slight_km)],
    max_ksi_km_limit = csl$speed_limit[which.max(csl$ksi_km)],
    coll_km_20_vs_max = round(at("coll_km", 20) /
                                max(csl$coll_km, na.rm = TRUE) * 100, 1),
    fatal_km_20_vs_30 = round(at("fatal_km", 20) / at("fatal_km", 30) * 100, 1),
    fatal_km_20_vs_max = round(at("fatal_km", 20) /
                                 max(csl$fatal_km, na.rm = TRUE) * 100, 1),
    ksi_km_30_vs_20 = round(at("ksi_km", 30) / at("ksi_km", 20), 1),
    fatal_col_30 = at("fatal_col", 30),
    fatal_km_30 = at("fatal_km", 30),
    ksi_col_30 = at("ksi_col", 30),
    coll_km_30 = at("coll_km", 30),
    collisions_20 = at("collisions", 20),
    tot_length_20 = at("tot_length", 20)
  )
}
