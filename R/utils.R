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

#' Stitch images into a captioned grid
#'
#' Combines any number of image files into a single grid image, with
#' optional captions below each panel. Useful for building multi-panel
#' figures from separately saved plots. Requires the \pkg{magick} package.
#'
#' @param images Character vector of file paths (or list of magick images).
#' @param captions Optional character vector of captions, same length as
#'   \code{images}.
#' @param ncol,nrow Grid dimensions. If \code{NULL}, computed automatically.
#' @param tile_width Width each image is resized to (px), keeping aspect.
#' @param caption_size,caption_font Caption text size and font family.
#' @param padding Padding (px) around each tile.
#' @param bg Background colour (also fills empty cells).
#' @param byrow Fill row-by-row (default) or column-by-column.
#' @return A magick image object (also viewable with \code{print()}).
#' @examples
#' \dontrun{
#' grid <- stitch_grid(c("p1.png", "p2.png", "p3.png", "p4.png"),
#'                     captions = c("(a)", "(b)", "(c)", "(d)"), ncol = 2)
#' magick::image_write(grid, "grid.png")
#' }
#' @export
stitch_grid <- function(images,
                        captions = NULL,
                        ncol = NULL,
                        nrow = NULL,
                        tile_width = 600,
                        caption_size = 28,
                        caption_font = "sans",
                        padding = 20,
                        bg = "white",
                        byrow = TRUE) {

  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("stitch_grid() requires the 'magick' package: install.packages('magick')")
  }

  n <- length(images)
  if (n == 0) stop("Need at least 1 image")
  if (!is.null(captions) && length(captions) != n) {
    stop("captions must be the same length as images (or NULL)")
  }

  if (is.null(ncol) && is.null(nrow)) {
    ncol <- ceiling(sqrt(n)); nrow <- ceiling(n / ncol)
  } else if (is.null(ncol)) {
    ncol <- ceiling(n / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(n / ncol)
  }
  if (nrow * ncol < n) stop("nrow x ncol is too small to fit all images")

  imgs <- lapply(images, function(x)
    if (is.character(x)) magick::image_read(x) else x)
  imgs <- lapply(imgs, magick::image_resize,
                 geometry = paste0(tile_width, "x"))

  if (!is.null(captions)) {
    imgs <- Map(function(img, cap) {
      cap_height <- caption_size * 2
      caption_img <- magick::image_blank(
        width = magick::image_info(img)$width,
        height = cap_height, color = bg) |>
        magick::image_annotate(cap, gravity = "center", size = caption_size,
                               font = caption_font, color = "black")
      magick::image_append(c(img, caption_img), stack = TRUE)
    }, imgs, captions)
  }

  imgs <- lapply(imgs, magick::image_border, color = bg,
                 geometry = paste0(padding, "x", padding))

  n_cells <- nrow * ncol
  if (n_cells > n) {
    ref_info <- magick::image_info(imgs[[n]])
    blank <- magick::image_blank(width = ref_info$width,
                                 height = ref_info$height, color = bg)
    imgs <- c(imgs, rep(list(blank), n_cells - n))
  }

  idx_matrix <- matrix(seq_len(n_cells), nrow = nrow, ncol = ncol,
                       byrow = byrow)

  rows <- lapply(seq_len(nrow), function(r) {
    do.call(magick::image_append, list(do.call(c, imgs[idx_matrix[r, ]])))
  })

  do.call(magick::image_append, list(do.call(c, rows), stack = TRUE))
}
