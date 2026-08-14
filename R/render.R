#' Render the LA report for an authority
#'
#' Renders the packaged Quarto report template
#' (\code{inst/report/LA_report.qmd}) for a given Local Authority, using
#' Quarto parameters rather than text substitution. Assumes
#' [build_la_report_data()] has already been run for the authority so that
#' \code{<output_dir>/data/la_report_data.rds} and the plot/map outputs
#' exist.
#'
#' @param authority Character. Local Authority name, e.g. \code{"Bristol"}.
#' @param output_dir Character. Root output directory used by
#'   [build_la_report_data()]. Default \code{"outputs/<authority>"}.
#' @param output_file Character. Output HTML filename. Default
#'   \code{"<authority>_report.html"}.
#' @param template Character. Path to a Quarto template. Defaults to the
#'   template shipped with the package.
#' @return Invisibly, the path to the rendered file.
#' @examples
#' \dontrun{
#' build_la_report_data("Bristol")
#' render_la_report("Bristol")
#' }
#' @export
render_la_report <- function(
    authority,
    output_dir = file.path("outputs", gsub(" ", "_", authority)),
    output_file = paste0(gsub(" ", "_", authority), "_report.html"),
    template = system.file("report", "LA_report.qmd",
                           package = "stats19reports")) {

  if (!nzchar(template) || !file.exists(template)) {
    stop("Report template not found. Is the package installed correctly?")
  }

  data_file <- file.path(output_dir, "data", "la_report_data.rds")
  core_file <- file.path(output_dir, "data", "cache", "core.rds")
  if (!file.exists(data_file) && file.exists(core_file)) {
    # sections have been built but never merged - do it now
    assemble_report_data(output_dir)
  }
  if (!file.exists(data_file)) {
    stop("No report data found at ", data_file,
         ". Run build_la_report_data('", authority, "') first.")
  }

  # quarto renders relative to the qmd location, so copy the template to the
  # working directory where the outputs/ tree lives
  local_qmd <- file.path(getwd(), basename(template))
  file.copy(template, local_qmd, overwrite = TRUE)
  on.exit(unlink(local_qmd), add = TRUE)

  quarto::quarto_render(
    input = local_qmd,
    output_file = output_file,
    execute_params = list(
      authority = authority,
      output_dir = output_dir
    )
  )

  invisible(file.path(getwd(), output_file))
}

#' Build data and render the report in one call
#'
#' Convenience wrapper that runs [build_la_report_data()] followed by
#' [render_la_report()].
#'
#' @inheritParams build_la_report_data
#' @param ... Further arguments passed to [build_la_report_data()].
#' @return Invisibly, the path to the rendered file.
#' @examples
#' \dontrun{
#' la_report("Bristol", base_year = 2021, upper_year = 2025)
#' }
#' @export
la_report <- function(authority, base_year = 2021, upper_year = 2025, ...) {
  build_la_report_data(authority, base_year = base_year,
                       upper_year = upper_year, ...)
  render_la_report(authority)
}
