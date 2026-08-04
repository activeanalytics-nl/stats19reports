#' Read spatial data with retry on failure
#'
#' Wraps \code{sf::st_read()} in a retry loop, useful for unreliable remote
#' servers (e.g. the ONS Open Geography Portal). Retries up to
#' \code{max_attempts} times with a configurable wait between attempts.
#'
#' @param url Character. URL or file path to read.
#' @param wait Numeric. Seconds to wait between retry attempts. Default
#'   \code{5}.
#' @param max_attempts Integer. Maximum number of attempts before erroring.
#'   Default \code{20}.
#' @return An \code{sf} object returned by \code{sf::st_read()}.
#' @examples
#' \dontrun{
#' la_sf <- st_read_retry("https://example.com/boundaries.gpkg")
#' }
#' @export
st_read_retry <- function(url, wait = 5, max_attempts = 20) {
  for (i in seq_len(max_attempts)) {
    out <- tryCatch(sf::st_read(url, quiet = TRUE),
                    error = function(e) NULL)
    if (!is.null(out)) {
      if (i > 1) message("Success after ", i, " attempts")
      return(out)
    }
    message("Attempt ", i, " failed - waiting ", wait, "s...")
    Sys.sleep(wait)
  }
  stop("st_read_retry: failed to read ", url, " after ", max_attempts,
       " attempts")
}
