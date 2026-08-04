
#' Create a gt table of TAG collision costs
#'
#' Computes TAG collision and casualty costs from crash data, formats values
#' with thousand separators, and produces a styled \code{gt} table. The table
#' is saved to disk as a PNG (or other format) and returned as a \code{gt}
#' object.
#'
#' @param crashes An \code{sf} or regular data frame of crash records with
#'   \code{collision_year} and \code{collision_severity} columns.
#' @param city Character. City name used in the table title and filename.
#' @param agg_level Character. One of \code{"severity"} (costs by severity)
#'   or \code{"severity_road"} (costs by severity and road type).
#' @param tab_dir Character. Directory where the output file will be saved.
#'   Default \code{"tables/"}.
#' @param file_type Character. File extension for the saved table (e.g.
#'   \code{".png"}, \code{".html"}). Default \code{".png"}.
#' @return A \code{gt} table object.
#' @examples
#' \dontrun{
#' t1 <- tabulate_summarise_tag_costs(crashes, city = "Bristol",
#'                          agg_level = "severity")
#' t1 <- tabulate_summarise_tag_costs(crashes, city = "Bristol",
#'                          agg_level = "severity_road")
#' }
#' @export
tabulate_summarise_tag_costs <- function(crashes, city, agg_level, tab_dir = "tables/", file_type = ".png"){

  
  crash_geo = inherits(crashes,"sf")
  if(crash_geo){
    st_geometry(crashes) = NULL
  }
  
  cwc <- summarise_tag_costs(crashes,agg_level)
  
  if(agg_level == "severity"){
  
# format values with commas
cwc_tot <- cwc |>
  ungroup() |>
  rowwise() |>
  mutate(casualty_cost = prettyNum(casualty_cost, big.mark = ",", scientific = FALSE),
         collision_cost = prettyNum(collision_cost, big.mark = ",", scientific = FALSE),
         total = prettyNum(total_cost, big.mark = ",", scientific = FALSE),
         total_casualties = round(total_casualties)) |>
  select(-total_cost)

cc_tot_all <- sum(as.numeric(gsub(",","", cwc_tot$total)))

start_year <- min(cwc$collision_year)
end_year <- max(cwc$collision_year)

# country table
t1 <- gt(cwc_tot,auto_align = TRUE) |>
  cols_width(collision_year ~px(60)) |>
  cols_label(collision_year = md("**Year**"),
             collision_severity = md("**Severity**"),
             total_casualties = md("**Casualties**"),
             casualty_cost = md("**Casualty cost (\u00a3)**"),
             collision_cost = md("**Collision cost (\u00a3)**"),
             total = md("**Total (\u00a3)**")) |>
  tab_footnote(md("**Source: DfT STATS19 and TAG**")) |>
  tab_header(
    title = md(paste0("**Number of reported road casualties and value of prevention by year, ",city,": ",start_year, " to ", end_year,"**"))) |>
  tab_options(heading.align = "left",
              column_labels.border.top.style = "none",
              table.border.top.style = "none",
              column_labels.border.bottom.style = "none",
              column_labels.border.bottom.width = 1,
              column_labels.border.bottom.color = "black",
              table_body.border.top.style = "none",
              table_body.border.bottom.color = "white",
              heading.border.bottom.style = "none",
              table.border.bottom.style = "none") |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_labels(columns = c(collision_year)),
      cells_body(columns = c(collision_year))
    )) |>
  tab_style(
    style = cell_fill(color = "white"),
    locations = cells_body(columns = everything())
  )

gtsave(t1, paste0(tab_dir, "/", city,"_costs", file_type))

return(t1)

  }
  
  if(agg_level == "severity_road"){
    
    # format values with commas
    cwc_tot <- cwc |>
      ungroup() |>
      rowwise() |>
      mutate(total = prettyNum(round(sum(built_up,not_built_up,Motorway,na.rm = TRUE)), big.mark = ",", scientific = FALSE)) |> 
      mutate(built_up = prettyNum(round(built_up), big.mark = ",", scientific = FALSE),
             Motorway = prettyNum(round(Motorway), big.mark = ",", scientific = FALSE),
             not_built_up = prettyNum(round(not_built_up), big.mark = ",", scientific = FALSE)) 
    
   # cc_tot_all <- sum(as.numeric(gsub(",","", cwc_tot$total)))
    
    start_year <- min(cwc$collision_year)
    end_year <- max(cwc$collision_year)
    
    # country table
    t1 <- gt(cwc_tot,auto_align = TRUE) |>
      cols_width(collision_year ~px(60)) |>
      cols_label(collision_year = md("**Year**"),
                 collision_severity = md("**Severity**"),
                 built_up = md("**Built up (\u00a3)**"),
                 not_built_up = md("**Not built up (\u00a3)**"),
                 Motorway = md("**Motorway (\u00a3)**"),
                 total = md("**Total (\u00a3)**")) |>
      tab_footnote(md("**Source: DfT STATS19 and TAG**")) |>
      tab_header(
        title = md(paste0("**Number of reported road casualties and value of prevention by year, ",city,": ",start_year, " to ", end_year,"**"))) |>
      tab_options(heading.align = "left",
                  column_labels.border.top.style = "none",
                  table.border.top.style = "none",
                  column_labels.border.bottom.style = "none",
                  column_labels.border.bottom.width = 1,
                  column_labels.border.bottom.color = "black",
                  table_body.border.top.style = "none",
                  table_body.border.bottom.color = "white",
                  heading.border.bottom.style = "none",
                  table.border.bottom.style = "none") |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = list(
          cells_column_labels(columns = c(collision_year)),
          cells_body(columns = c(collision_year))
        )) |>
      tab_style(
        style = cell_fill(color = "white"),
        locations = cells_body(columns = everything())
      )
    
    gtsave(t1, paste0(tab_dir, "/", city,"_costs", file_type))
    
    return(t1)
    
    
  }
 
}

#' Create a gt table of Local Authority cost rankings
#'
#' Computes collision costs per Local Authority, sorts by the chosen metric,
#' and produces a styled \code{gt} table of the top \code{n} LAs. The table
#' is saved to disk.
#'
#' @param crashes An \code{sf} data frame of crash records.
#' @param severities Character vector of severity levels to include. Default
#'   \code{c("Fatal", "Serious", "Slight")}.
#' @param sort_by Character. Column to sort LAs by. One of
#'   \code{"casualties"}, \code{"cost"}, or \code{"collisions"}.
#' @param table_year Integer. Year to filter to. Default \code{2024}.
#' @param rows Integer. Number of rows (top LAs) to show. Default \code{10}.
#' @param tab_dir Character or \code{NULL}. If supplied, a PNG of the table
#'   is also saved into this directory.
#' @return A \code{gt} table object.
#' @examples
#' \dontrun{
#' t1 <- tabulate_la_cost_ranking(crashes, sort_by = "cost",
#'                                table_year = 2024, rows = 10)
#' }
#' @export
tabulate_la_cost_ranking <- function(crashes, severities = c("Fatal", "Serious", "Slight"), sort_by = c("casualties", "cost", "collisions"), table_year = 2024, rows = 10, tab_dir = NULL){
  
  sort_by <- match.arg(sort_by)

  sort_col <- c(casualties = "total_casualties",
                cost = "total_cost",
                collisions = "total_collisions")[[sort_by]]

  LA_sum <- summarise_costs_per_la(crashes, severities) |>
    st_set_geometry(NULL) |>
    filter(collision_year == table_year) |>
    arrange(desc(.data[[sort_col]])) |>
    slice(1:rows)
  
  # country table
  t1 <- gt(LA_sum,auto_align = TRUE) |>
    cols_width(LAD22NM ~px(60)) |>
    cols_label(LAD22NM = md("**Local Authority**"),
               total_collisions = md("**Total collisions**"),
               total_casualties = md("**Total casualties**"),
               total_cost = md("**Total cost (\u00a3mn)**"),
               annual_coll_rank = md("**Collision ranking**"),
               annual_cas_rank = md("**Casualty ranking**"),
               annual_cost_rank = md("**Cost ranking**")) |>
    tab_footnote(md("**Source: DfT STATS19 and TAG**")) |>
    tab_header(
      title = md(paste0("**Number of collisions, casualties and the cost for top 10 Local Authorities in ", table_year,"**"))) |>
    tab_options(heading.align = "left",
                column_labels.border.top.style = "none",
                table.border.top.style = "none",
                column_labels.border.bottom.style = "none",
                column_labels.border.bottom.width = 1,
                column_labels.border.bottom.color = "black",
                table_body.border.top.style = "none",
                table_body.border.bottom.color = "white",
                heading.border.bottom.style = "none",
                table.border.bottom.style = "none") |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(
        cells_column_labels(columns = c(collision_year)),
        cells_body(columns = c(collision_year))
      )) |>
    tab_style(
      style = cell_fill(color = "white"),
      locations = cells_body(columns = everything())
    )
  
  if (!is.null(tab_dir)) {
    gtsave(t1, file.path(tab_dir, paste0("la_cost_ranking_", table_year, ".png")))
  }

  return(t1)

}


# ---------------------------------------------------------------------------
# Report tables previously built inline in LA_report_default.qmd
# ---------------------------------------------------------------------------

#' Default reactable theme for report tables
#'
#' Shared \code{reactable} theme and column defaults used by the report
#' tables, so styling is defined once.
#'
#' @return A list with elements \code{theme} (a
#'   \code{reactable::reactableTheme}) and \code{defaultColDef} (a
#'   \code{reactable::colDef}).
#' @examples
#' \dontrun{
#' th <- reactable_report_theme()
#' reactable(mtcars, theme = th$theme, defaultColDef = th$defaultColDef)
#' }
#' @export
reactable_report_theme <- function() {
  list(
    theme = reactable::reactableTheme(
      style = list(fontFamily = "Geist, system-ui, sans-serif",
                   fontSize = "13px"),
      headerStyle = list(fontWeight = 600),
      cellStyle = list(lineHeight = "1.5")
    ),
    defaultColDef = reactable::colDef(
      header = function(value) gsub(".", " ", value, fixed = TRUE),
      cell = function(value) format(value, nsmall = 1),
      align = "center",
      minWidth = 30,
      headerStyle = list(background = "grey")
    )
  )
}

#' Interactive table of casualties per OSM road link
#'
#' Builds the sortable \code{reactable} of collision counts per road used in
#' the "Road Network Analysis" section of the LA report.
#'
#' @param cas_osm_period An \code{sf} data frame from
#'   [summarise_osm_link_casualties()] with \code{group = "total"}.
#' @param save_path Character or \code{NULL}. If supplied, the widget is also
#'   saved as a self-contained HTML file at this path.
#' @param page_size Integer. Rows per page. Default \code{10}.
#' @return A \code{reactable} htmlwidget.
#' @examples
#' \dontrun{
#' tabulate_osm_roads(cas_osm_period)
#' }
#' @export
tabulate_osm_roads <- function(cas_osm_period, save_path = NULL,
                               page_size = 10) {

  th <- reactable_report_theme()

  tbl <- cas_osm_period |>
    sf::st_set_geometry(NULL) |>
    dplyr::transmute(
      `Road name` = name,
      `Mean age` = round(age_of_casualty, 1),
      `Mean sex` = round(sex_of_casualty, 1),
      `Speed limit` = round(speed_limit),
      `# collisions` = number_of_collisions,
      Fatal = round(fatal),
      Serious = round(serious),
      KSI = round(ksi),
      Slight = round(slight),
      Total = round(total)
    ) |>
    dplyr::arrange(dplyr::desc(`# collisions`)) |>
    reactable::reactable(
      theme = th$theme,
      defaultColDef = th$defaultColDef,
      columns = list(`Road name` = reactable::colDef(minWidth = 80)),
      bordered = TRUE,
      highlight = TRUE,
      defaultPageSize = page_size
    )

  if (!is.null(save_path)) {
    htmlwidgets::saveWidget(tbl, save_path, selfcontained = TRUE)
  }

  tbl
}

#' Interactive table of MSOA casualty rankings
#'
#' Builds the \code{reactable} of national MSOA rankings for casualties'
#' home areas used in the "MSOA" section of the LA report.
#'
#' @param cas_df_all_LA An \code{sf} data frame of MSOA-level casualty
#'   summaries for the LA (see [casualties_per_MSOA()]).
#' @param save_path Character or \code{NULL}. If supplied, the widget is also
#'   saved as a self-contained HTML file at this path.
#' @param page_size Integer. Rows per page. Default \code{20}.
#' @return A \code{reactable} htmlwidget.
#' @examples
#' \dontrun{
#' tabulate_msoa_ranks(cas_df_all_LA)
#' }
#' @export
tabulate_msoa_ranks <- function(cas_df_all_LA, save_path = NULL,
                                page_size = 20) {

  th <- reactable_report_theme()

  tbl <- cas_df_all_LA |>
    sf::st_set_geometry(NULL) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      `MSOA name` = msoa21hclnm,
      `Fatal rank` = fatal_rank,
      `Serious rank` = serious_rank,
      `Slight rank` = slight_rank,
      `KSI rank` = ksi_rank,
      `Total rank` = total_rank,
      Population = round(pop),
      `Fatal per capita rank` = fatal_pcap_rank,
      `Serious per capita rank` = serious_pcap_rank,
      `KSI per capita rank` = ksi_pcap_rank,
      `Total per capita rank` = total_pcap_rank
    ) |>
    reactable::reactable(
      theme = th$theme,
      defaultColDef = th$defaultColDef,
      columns = list(`MSOA name` = reactable::colDef(minWidth = 80)),
      bordered = TRUE,
      highlight = TRUE,
      defaultPageSize = page_size
    )

  if (!is.null(save_path)) {
    htmlwidgets::saveWidget(tbl, save_path, selfcontained = TRUE)
  }

  tbl
}

#' Interactive table of casualties by age band and sex
#'
#' Builds the \code{reactable} used in the "Casualty Demographics" section of
#' the LA report.
#'
#' @param age_sex A data frame from [summarise_casualties_by_demog()].
#' @param save_path Character or \code{NULL}. If supplied, the widget is also
#'   saved as a self-contained HTML file at this path.
#' @param page_size Integer. Rows per page. Default \code{25}.
#' @return A \code{reactable} htmlwidget.
#' @examples
#' \dontrun{
#' tabulate_age_sex(age_sex)
#' }
#' @export
tabulate_age_sex <- function(age_sex, save_path = NULL, page_size = 25) {

  th <- reactable_report_theme()

  tbl <- age_sex |>
    dplyr::filter(sex_of_casualty != "Data missing or out of range") |>
    dplyr::ungroup() |>
    dplyr::transmute(
      `Sex of casualty` = sex_of_casualty,
      `Age band` = age_band,
      Fatal = round(Fatal),
      Serious = round(Serious),
      Slight = round(Slight),
      KSI = round(KSI),
      All = round(All),
      `% KSI` = round(pc_ksi, 1),
      `% All` = round(pc_all, 1)
    ) |>
    reactable::reactable(
      theme = th$theme,
      defaultColDef = th$defaultColDef,
      columns = list(`Sex of casualty` = reactable::colDef(minWidth = 30)),
      bordered = TRUE,
      highlight = TRUE,
      defaultPageSize = page_size
    )

  if (!is.null(save_path)) {
    htmlwidgets::saveWidget(tbl, save_path, selfcontained = TRUE)
  }

  tbl
}

#' gt table of casualties and collision rates by speed limit
#'
#' Builds the speed-limit summary \code{gt} table from the "Speed Limits"
#' section of the LA report, optionally saving HTML and PNG copies.
#'
#' @param csl A data frame of casualties/collisions by speed limit including
#'   per-km and per-collision rate columns (built by
#'   [build_la_report_data()]).
#' @param la_name Character. Local Authority name for the title.
#' @param base_year,upper_year Integers. Analysis period for the title.
#' @param save_path Character or \code{NULL}. Base path (without extension);
#'   \code{.html} and \code{.png} versions are saved when supplied.
#' @return A \code{gt} table object.
#' @examples
#' \dontrun{
#' tabulate_speed_limits(csl, "Bristol", 2020, 2024)
#' }
#' @export
tabulate_speed_limits <- function(csl, la_name, base_year, upper_year,
                                  save_path = NULL) {

  speed_table <- gt(csl, auto_align = TRUE) |>
    cols_label(speed_limit = md("**Speed Limit (mph)**"),
               tot_length = md("**Total length (km)**"),
               pc_length = md("**% of LA roads**"),
               collisions = md("**Collisions**"),
               fatal = md("**Fatal**"),
               serious = md("**Serious**"),
               ksi = md("**KSI**"),
               slight = md("**Slight**"),
               coll_km = md("**Collisions per km**"),
               fatal_km = md("**Fatal per km**"),
               serious_km = md("**Serious per km**"),
               ksi_km = md("**KSI per km**"),
               slight_km = md("**Slight per km**"),
               fatal_col = md("**Fatal per collision**"),
               serious_col = md("**Serious per collision**"),
               ksi_col = md("**KSI per collision**"),
               slight_col = md("**Slight per collision**")) |>
    tab_footnote(md("**Source: DfT STATS19 and OSM**")) |>
    opt_table_font(size = 11) |>
    opt_row_striping(row_striping = FALSE) |>
    tab_header(title = md(paste0(
      "**Casualties and collisions by speed limit and road length, ",
      la_name, ": ", base_year, " to ", upper_year, "**"))) |>
    tab_options(heading.align = "left",
                column_labels.border.top.style = "none",
                table.border.top.style = "none",
                column_labels.border.bottom.style = "none",
                column_labels.border.bottom.width = 1,
                column_labels.border.bottom.color = "black",
                table_body.border.top.style = "none",
                table_body.border.bottom.color = "white",
                heading.border.bottom.style = "none",
                table.border.bottom.style = "none") |>
    tab_style(style = cell_text(weight = "bold"),
              locations = list(
                cells_column_labels(columns = c(speed_limit)),
                cells_body(columns = c(speed_limit))
              ))

  if (!is.null(save_path)) {
    gtsave(speed_table, paste0(save_path, ".html"))
    gtsave(speed_table, paste0(save_path, ".png"))
  }

  speed_table
}

#' gt table of casualty type groupings
#'
#' Builds the appendix table showing how detailed STATS19 casualty types are
#' grouped into the categories used in the report.
#'
#' @param cas_type A data frame with \code{casualty_type} and
#'   \code{short_name} columns. Defaults to the package
#'   [casualty_type_lookup].
#' @return A \code{gt} table object.
#' @examples
#' \dontrun{
#' tabulate_casualty_groupings()
#' }
#' @export
tabulate_casualty_groupings <- function(cas_type = casualty_type_lookup) {

  cas_type <- dplyr::select(cas_type, casualty_type, short_name)

  gt(cas_type, auto_align = TRUE) |>
    cols_width(casualty_type ~ px(500)) |>
    cols_label(casualty_type = md("**STATS19 Name**"),
               short_name = md("**Categorised**")) |>
    tab_header(title = md("**Table showing casualty groupings used in report**")) |>
    tab_options(heading.align = "left",
                column_labels.border.top.style = "none",
                table.border.top.style = "none",
                column_labels.border.bottom.style = "none",
                column_labels.border.bottom.width = 1,
                column_labels.border.bottom.color = "black",
                table_body.border.top.style = "none",
                table_body.border.bottom.color = "white",
                heading.border.bottom.style = "none",
                table.border.bottom.style = "none") |>
    tab_style(style = cell_text(weight = "bold"),
              locations = list(
                cells_column_labels(columns = c(casualty_type)),
                cells_body(columns = c(casualty_type))
              )) |>
    tab_style(style = cell_fill(color = "white"),
              locations = cells_body(columns = everything()))
}
