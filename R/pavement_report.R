# National pavement collision analysis: pedestrians struck on a footway or
# verge across Great Britain, categorised by the vehicle involved. Produces
# BBC-styled charts (via bbplot), treemaps of where fatalities happen, and
# the data behind the national pavement report.

#' Categorise vehicles into coarse pavement-analysis groups
#'
#' Classifies vehicle records into Motor vehicle / Bicycle-E-scooter-
#' Mobility Scooter / Tram / Unknown using [vehicle_category_lookup].
#'
#' @param vehicles A STATS19 vehicle data frame.
#' @return The input with \code{short_name} and \code{vehicle_cat} columns,
#'   one row per vehicle, keeping \code{collision_index} and
#'   \code{vehicle_type}.
#' @examples
#' \dontrun{
#' categorise_pavement_vehicles(vehicles_gb)
#' }
#' @export
categorise_pavement_vehicles <- function(vehicles) {
  vehicles |>
    summarise_vehicle_types("short_name") |>
    dplyr::left_join(vehicle_category_lookup, by = "short_name") |>
    dplyr::mutate(vehicle_cat = dplyr::if_else(
      is.na(vehicle_cat) | short_name == "Other vehicle",
      "Unknown", vehicle_cat)) |>
    dplyr::select(collision_index, vehicle_type, short_name, vehicle_cat)
}

#' Pedestrian pavement casualties, one row per collision
#'
#' Filters casualties to pedestrians recorded as "On footway or verge" and
#' summarises to one row per collision with Fatal/Serious/Slight counts.
#'
#' @param casualties A STATS19 casualty data frame.
#' @return A data frame, one row per collision.
#' @examples
#' \dontrun{
#' pavement_casualties(casualties_gb)
#' }
#' @export
pavement_casualties <- function(casualties) {
  casualties |>
    dplyr::filter(pedestrian_location == "On footway or verge" &
                    casualty_type == "Pedestrian") |>
    summarise_casualties_per_collision()
}

#' BBC-styled stacked bar of pavement casualties by year
#'
#' Builds the bbplot-styled stacked bar chart of vehicles involved in
#' pavement collisions per year, for a chosen severity, and saves it via
#' \code{bbplot::finalise_plot()}.
#'
#' @param crashes_gb An \code{sf} data frame of GB crashes.
#' @param casualties_pv Output of [pavement_casualties()].
#' @param veh_summary Wide per-collision vehicle-category counts (see
#'   [build_pavement_report_data()]).
#' @param severity Character. One of \code{"fatal"}, \code{"ksi"},
#'   \code{"all"}.
#' @param base_year,upper_year Integers. Analysis period.
#' @param save_path Character or \code{NULL}. PNG path for
#'   \code{finalise_plot()}.
#' @return The ggplot object, invisibly.
#' @examples
#' \dontrun{
#' plot_pavement_vehicles_by_year(crashes_gb, cas_pv, veh_summary, "fatal",
#'                                2021, 2025, "plots/av_fatalities.png")
#' }
#' @export
plot_pavement_vehicles_by_year <- function(crashes_gb, casualties_pv,
                                           veh_summary,
                                           severity = c("fatal", "ksi", "all"),
                                           base_year, upper_year,
                                           save_path = NULL,
                                           theme = "remains") {

  
  severity <- match.arg(severity)

  dat <- crashes_gb |>
    dplyr::filter(collision_year >= base_year &
                    collision_year <= upper_year) |>
    dplyr::inner_join(veh_summary, by = "collision_index") |>
    dplyr::inner_join(casualties_pv, by = c("collision_index",
                                            "collision_year"))

  dat <- switch(severity,
    fatal = dplyr::filter(dat, Fatal > 0),
    ksi = dplyr::filter(dat, Fatal > 0 | Serious > 0),
    all = dat)

  title <- switch(severity,
    fatal = "Vehicles involved in pavement fatalities",
    ksi = "Vehicles involved in pavement KSI",
    all = "Vehicles involved in all pavement collisions")

  cat_cols <- intersect(c("Motor vehicle", "Unknown",
                          "Bicycle/E-scooter/Mobility Scooter", "Tram"),
                        names(dat))
  
  if(theme == "bbc"){
    pal = rev(c("#1380A1", "grey", "#FAAB18", "#9a1101"))
  } else {
    pal = rev(ltc::ltc("pantone23", 4))
  }

  plot_dat <- dat |>
    sf::st_set_geometry(NULL) |>
    dplyr::select(collision_year, dplyr::all_of(cat_cols)) |>
    tidyr::pivot_longer(-collision_year, names_to = "variable",
                        values_to = "value") |>
    dplyr::group_by(collision_year, variable) |>
    dplyr::summarise(n_veh = sum(value, na.rm = TRUE), .groups = "drop")

  p <- ggplot2::ggplot(plot_dat,
                       ggplot2::aes(x = collision_year, y = n_veh,
                                    fill = variable)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::scale_fill_manual(
      values = pal) +
    ggplot2::labs(
      title = title,
      subtitle = paste0("Number of vehicles by category between ",
                        base_year, " and ", upper_year)) +
    bbplot::bbc_style()+
    theme(
      legend.text = element_text(size = 16),
      #legend.justification = "left"   # or legend.justification.top = "left" if ggplot2 >= 3.5.0
    )

  if (!is.null(save_path)) {
    bbplot::finalise_plot(plot_name = p, source_name = "Source: STATS19",
                          save_filepath = save_path)
  }
  invisible(p)
}

#' BBC-styled stacked bar of pavement casualties by severity
#'
#' @param casualties_pv Output of [pavement_casualties()].
#' @param base_year,upper_year Integers. Analysis period.
#' @param theme either bbc or the package ltc in which case enter the name of the theme https://github.com/loukesio/ltc-color-palettes.
#' @param save_path Character or \code{NULL}. PNG path.
#' @return The ggplot object, invisibly.
#' @examples
#' \dontrun{
#' plot_pavement_severity_by_year(cas_pv, 2021, 2025)
#' }
#' @export
plot_pavement_severity_by_year <- function(casualties_pv, base_year, 
                                           upper_year, save_path = NULL,
                                           theme = "ltc") {

  plot_dat <- casualties_pv |>
    dplyr::filter(collision_year >= base_year &
                    collision_year <= upper_year) |>
    dplyr::select(collision_year, Fatal, Serious, Slight) |>
    tidyr::pivot_longer(-collision_year, names_to = "variable",
                        values_to = "number_of_casualties") |>
    dplyr::group_by(collision_year, variable) |>
    dplyr::summarise(number_of_casualties = sum(number_of_casualties,
                                                na.rm = TRUE),
                     .groups = "drop")
  
  if(theme == "bbc"){
    pal = rev(c("#B0B2B4", "#141414","#DC2878"))
  } else {
    pal = ltc::ltc(trio3, 3)
  }
  
  

  p <- ggplot2::ggplot(plot_dat,
                       ggplot2::aes(x = collision_year,
                                    y = number_of_casualties,
                                    fill = variable)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::labs(
      title = "Pedestrian pavement casualties",
      subtitle = paste0("Between ", base_year, " and ", upper_year,
                        " by severity")) +
    bbplot::bbc_style()

  if (!is.null(save_path)) {
    bbplot::finalise_plot(plot_name = p, source_name = "Source: STATS19",
                          save_filepath = save_path)
  }
  invisible(p)
}

#' BBC-styled bar of pavement fatalities by IMD decile or age band
#'
#' @param casualties_gb A STATS19 casualty data frame with
#'   \code{fatal_count} and \code{dft_age_band} columns (as produced by the
#'   pipeline loader).
#' @param by Character. \code{"imd"} or \code{"age"}.
#' @param base_year,upper_year Integers. For the subtitle.
#' @param save_path Character or \code{NULL}. PNG path.
#' @param theme either bbc or the package ltc in which case enter the name of the theme https://github.com/loukesio/ltc-color-palettes.

#' @return The ggplot object, invisibly.
#' @examples
#' \dontrun{
#' plot_pavement_fatalities_by(casualties_gb, "imd", 2021, 2025)
#' }
#' @export
plot_pavement_fatalities_by <- function(casualties_gb, by = c("imd", "age"),
                                        base_year, upper_year,
                                        save_path = NULL,
                                        theme = "ltc") {

  by <- match.arg(by)
  group_col <- if (by == "imd") "casualty_imd_decile" else "dft_age_band"
  if(theme == "bbc"){
  fill <- if (by == "imd") "#9a1101" else "#1380A1"
  } else{
    cols = ltc::ltc("remains", 4)[3:4]
    fill <- if (by == "imd") cols[1] else cols[2]
  }
  sub <- if (by == "imd") "by IMD (index of multiple deprivation)" else "by age"

  plot_dat <- casualties_gb |>
    dplyr::filter(pedestrian_location == "On footway or verge") |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::summarise(Fatal = sum(fatal_count, na.rm = TRUE)) |>
    dplyr::filter(!is.na(.data[[group_col]]))

  p <- ggplot2::ggplot(plot_dat,
                       ggplot2::aes(x = .data[[group_col]], y = Fatal)) +
    ggplot2::geom_bar(stat = "identity", show.legend = FALSE,
                      position = "identity", fill = fill) +
    ggplot2::labs(
      title = "Pedestrian pavement fatalities: all vehicles",
      subtitle = paste0("Between ", base_year, " and ", upper_year, " ",
                        sub)) +
    bbplot::bbc_style() +
    ggplot2::coord_flip() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb",
                                                 linewidth = 0.1),
      panel.grid.major.y = ggplot2::element_blank())

  if (!is.null(save_path)) {
    bbplot::finalise_plot(plot_name = p, source_name = "Source: STATS19",
                          height_pixels = if (by == "imd") 800 else 800,
                          width_pixels = 700, save_filepath = save_path)
  }
  invisible(p)
}

#' Treemap of pavement fatalities by area
#'
#' Builds the bbplot-styled treemap of pavement fatalities by MSOA (with
#' Scottish wards appended when supplied) or by Local Authority, for areas
#' with more than \code{min_fatal} deaths.
#'
#' @param pav_area A data frame with \code{name}, \code{localauthorityname}
#'   and \code{Fatal} columns (see [build_pavement_report_data()]).
#' @param level Character. \code{"msoa"} (labels area + LA) or \code{"la"}.
#' @param min_fatal Integer. Only areas with more than this many fatalities
#'   are shown. Default \code{1}.
#' @param title Character. Plot title.
#' @param base_year,upper_year Integers. For the subtitle.
#' @param save_path Character or \code{NULL}. PNG path.
#' @return The ggplot object, invisibly.
#' @examples
#' \dontrun{
#' plot_pavement_treemap(svp_msoa, "msoa", 1,
#'                       "Pedestrian pavement fatalities: single vehicle",
#'                       2021, 2025)
#' }
#' @export
plot_pavement_treemap <- function(pav_area, level = c("msoa", "la"), 
                                  min_fatal = 1, title, base_year,
                                  upper_year, save_path = NULL,theme = "ltc") {

  level <- match.arg(level)

  plot_dat <- dplyr::filter(pav_area, Fatal > min_fatal)

  if (level == "msoa") {
    plot_dat <- dplyr::mutate(plot_dat,
                              label = paste0(name, "\n", localauthorityname))
    sub_area <- "Mid-size regions"
  } else {
    plot_dat <- dplyr::mutate(plot_dat, label = localauthorityname)
    sub_area <- "Local Authorities"
  }

  n_levels <- length(unique(plot_dat$Fatal))
  if(theme == "bbc"){
    pal = rev(c("#1380A1", "grey", "#FAAB18", "#9a1101"))
  } else {
    pal = rev(ltc::ltc("minou", n_levels))
  }
  #pal <- c("#13809f", "#f89c15", "#37601e", "#ad3025", "#510d09")

  p <- ggplot2::ggplot(plot_dat,
                       ggplot2::aes(area = Fatal,
                                    fill = as.factor(Fatal),
                                    label = label)) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(colour = "white") +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::labs(
      title = title,
      subtitle = paste0(sub_area, " with more than ", min_fatal,
                        " death", if (min_fatal != 1) "s" else "",
                        " between ", base_year, " and ", upper_year)) +
    bbplot::bbc_style() +
    ggplot2::theme(legend.position = "bottom")

  if (!is.null(save_path)) {
    bbplot::finalise_plot(plot_name = p, source_name = "Source: STATS19",
                          save_filepath = save_path)
  }
  invisible(p)
}

#' Build the national pavement collision report
#'
#' Runs the GB-wide analysis of pedestrians struck on a footway or verge:
#' vehicle categorisation, yearly casualty/vehicle charts, treemaps of
#' where single- and all-vehicle fatalities happened (by MSOA and LA), and
#' IMD/age breakdowns. Charts are bbplot-styled and saved to
#' \code{output_dir/plots}; the summary data is saved to
#' \code{output_dir/data/pavement_report_data.rds}.
#'
#' Scottish geography: police Scotland casualty locations align with
#' multi-member ward boundaries rather than MSOAs. Supply
#' \code{scottish_wards_path} (a GeoPackage with Name/Council/SHAPE
#' columns) to include Scotland in the area treemaps; without it the
#' treemaps cover England and Wales only.
#'
#' @param base_year,upper_year Integers. Analysis period.
#' @param output_dir Character. Output directory. Default
#'   \code{"outputs/pavements"}.
#' @param msoa_url Character. URL of the MSOA boundaries GeoPackage.
#' @param scottish_wards_path Character or \code{NULL}. Path/URL to the
#'   Scottish wards GeoPackage.
#' @return Invisibly, the list of summary objects (also saved as RDS).
#' @examples
#' \dontrun{
#' build_pavement_report_data(2021, 2025)
#' }
#' @export
build_pavement_report_data <- function(
    base_year = 2021,
    upper_year = 2025,
    output_dir = file.path("outputs", "pavements"),
    msoa_url = paste0("https://github.com/BlaiseKelly/stats19_stats/",
                      "releases/download/msoa_boundaries-v1.0/msoa.gpkg"),
    scottish_wards_path = NULL) {

  if (!requireNamespace("bbplot", quietly = TRUE)) {
    stop("The pavement report charts need the 'bbplot' package: ",
         "remotes::install_github('bbc/bbplot')")
  }

  dir.create(file.path(output_dir, "plots"), recursive = TRUE,
             showWarnings = FALSE)
  dir.create(file.path(output_dir, "data"), recursive = TRUE,
             showWarnings = FALSE)
  plot_path <- function(...) file.path(output_dir, "plots", ...)

  # ---- data ---------------------------------------------------------------
  s19 <- load_report_stats19(base_year, upper_year)
  crashes_gb <- s19$crashes_gb
  casualties_gb <- s19$casualties_gb

  casualties_pv <- pavement_casualties(casualties_gb)

  vehicles_categorised <- s19$vehicles_gb |>
    dplyr::filter(collision_index %in% casualties_pv$collision_index) |>
    categorise_pavement_vehicles()

  veh_summary <- vehicles_categorised |>
    dplyr::group_by(collision_index, vehicle_cat) |>
    dplyr::summarise(number_vehicles = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "vehicle_cat",
                       values_from = "number_vehicles")

  # single-vehicle fatal collisions (vehicle that struck is unambiguous)
  single_vehicle_pavement <- crashes_gb |>
    dplyr::filter(number_of_vehicles == 1 &
                    collision_year >= base_year &
                    collision_year <= upper_year) |>
    dplyr::inner_join(vehicles_categorised, by = "collision_index") |>
    dplyr::inner_join(casualties_pv,
                      by = c("collision_index", "collision_year")) |>
    dplyr::filter(Fatal > 0)

  all_vehicle_pavement <- crashes_gb |>
    dplyr::filter(collision_year >= base_year &
                    collision_year <= upper_year) |>
    dplyr::inner_join(casualties_pv,
                      by = c("collision_index", "collision_year")) |>
    dplyr::filter(Fatal > 0)

  # ---- yearly charts ------------------------------------------------------
  plot_pavement_vehicles_by_year(crashes_gb, casualties_pv, veh_summary,
                                 "fatal", base_year, upper_year,
                                 plot_path("av_pavement_fatalities.png"))
  plot_pavement_vehicles_by_year(crashes_gb, casualties_pv, veh_summary,
                                 "ksi", base_year, upper_year,
                                 plot_path("av_pavement_ksi.png"))
  plot_pavement_vehicles_by_year(crashes_gb, casualties_pv, veh_summary,
                                 "all", base_year, upper_year,
                                 plot_path("av_pavement_all.png"))
  plot_pavement_severity_by_year(casualties_pv, base_year, upper_year,
                                 plot_path("sev_pavement_casualties.png"))
  plot_pavement_fatalities_by(casualties_gb, "imd", base_year, upper_year,
                              plot_path("av_pavement_fatalities_imd.png"))
  plot_pavement_fatalities_by(casualties_gb, "age", base_year, upper_year,
                              plot_path("av_pavement_fatalities_age.png"))

  # ---- geography ----------------------------------------------------------
  msoa_names <- utils::read.csv(paste0(
    "https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-2.2.csv"))

  area_geo <- st_read_retry(msoa_url) |>
    sf::st_transform(27700) |>
    dplyr::left_join(msoa_names, by = c("MSOA21CD" = "msoa21cd")) |>
    dplyr::select(name = msoa21hclnm, localauthorityname, geom)

  if (!is.null(scottish_wards_path)) {
    scottish <- st_read_retry(scottish_wards_path) |>
      dplyr::select(name = Name, localauthorityname = Council,
                    geom = SHAPE) |>
      sf::st_transform(27700)
    area_geo <- rbind(area_geo, scottish)
  } else {
    message("[pavements] no Scottish wards supplied - treemaps cover ",
            "England and Wales only")
  }

  summarise_by_area <- function(pav_sf) {
    pav_sf |>
      sf::st_join(area_geo) |>
      sf::st_set_geometry(NULL) |>
      dplyr::group_by(name, localauthorityname) |>
      dplyr::summarise(dplyr::across(c("Fatal", "Serious", "Slight"), sum),
                       .groups = "drop") |>
      dplyr::filter(!is.na(name)) |>
      dplyr::mutate(ksi = Fatal + Serious,
                    total = Fatal + Serious + Slight)
  }

  svp_msoa <- summarise_by_area(single_vehicle_pavement)
  avp_msoa <- summarise_by_area(all_vehicle_pavement)

  svp_la <- svp_msoa |>
    dplyr::group_by(localauthorityname) |>
    dplyr::summarise(dplyr::across(c("Fatal", "Serious", "Slight"), sum),
                     .groups = "drop")
  avp_la <- avp_msoa |>
    dplyr::group_by(localauthorityname) |>
    dplyr::summarise(dplyr::across(c("Fatal", "Serious", "Slight"), sum),
                     .groups = "drop")

  plot_pavement_treemap(svp_msoa, "msoa", 1,
                        "Pedestrian pavement fatalities: single vehicle",
                        base_year, upper_year,
                        plot_path("sv_pavement_fatalities_msoa.png"))
  plot_pavement_treemap(svp_la, "la", 1,
                        "Pedestrian pavement fatalities: single vehicle",
                        base_year, upper_year,
                        plot_path("sv_pavement_fatalities_la.png"))
  plot_pavement_treemap(avp_msoa, "msoa", 1,
                        "Pedestrian pavement fatalities: all vehicles",
                        base_year, upper_year,
                        plot_path("av_pavement_fatalities_msoa.png"))
  plot_pavement_treemap(avp_la, "la", 1,
                        "Pedestrian pavement fatalities: all vehicles",
                        base_year, upper_year,
                        plot_path("av_pavement_fatalities_la.png"))

  # ---- combined figures ---------------------------------------------------
  if (requireNamespace("magick", quietly = TRUE)) {
    grid <- stitch_grid(
      plot_path(c("av_pavement_all.png", "av_pavement_fatalities.png",
                  "av_pavement_ksi.png", "sev_pavement_casualties.png")),
      captions = c(
        "(a) All pavement collisions by vehicle involved",
        "(b) Vehicles involved in fatal collisions",
        "(c) Vehicles involved in KSI collisions",
        "(d) All pavement casualties by severity"),
      tile_width = 700, caption_size = 20, ncol = 2)
    magick::image_write(grid, plot_path("main_dat.png"))

    grid2 <- stitch_grid(
      plot_path(c("av_pavement_fatalities_age.png",
                  "av_pavement_fatalities_imd.png")),
      captions = c("(a) Fatalities by age band",
                   "(b) Fatalities by IMD decile"),
      tile_width = 700, caption_size = 24, ncol = 2, nrow = 1)
    magick::image_write(grid2, plot_path("demo_dat.png"))
  }

  # ---- summary data -------------------------------------------------------
  sing_veh_pave <- summarise_casualties_pavements(
    crashes_df = crashes_gb,
    casualties_df = casualties_gb,
    vehicles_df = s19$vehicles_gb,
    base_year = base_year, upper_year = upper_year)

  vehicle_cat_table <- vehicles_categorised |>
    dplyr::select(-collision_index) |>
    dplyr::distinct(vehicle_type, .keep_all = TRUE)

  out <- list(
    base_year = base_year, upper_year = upper_year,
    sing_veh_pave_gb = sing_veh_pave,
    svp_msoa = svp_msoa, svp_la = svp_la,
    avp_msoa = avp_msoa, avp_la = avp_la,
    vehicle_cat_table = vehicle_cat_table,
    n_single_vehicle = NROW(single_vehicle_pavement),
    n_all_vehicle = NROW(all_vehicle_pavement)
  )

  saveRDS(out, file.path(output_dir, "data", "pavement_report_data.rds"))
  invisible(out)
}
