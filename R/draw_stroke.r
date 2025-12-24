#' Visualize Stroke Region and Center of Gravity (COG)
#'
#' Visualizes the stroke region of a character-like binary image using the result
#' from \code{\link{cog_stroke}}. Optionally overlays crosshair lines at the computed
#' center of gravity (COG) position.
#'
#' By default (\code{cimg = TRUE}), the function returns a \code{cimg} object if the
#' \pkg{imager} package is available. If \pkg{imager} is not installed or if
#' \code{cimg = FALSE}, a base R raster object created by \code{as.raster()} is returned.
#'
#' @param lst A list returned by \code{\link{cog_stroke}}, containing stroke pixel data
#'   and computed statistics.
#' @param show_cog Logical. If \code{TRUE} (default), draws horizontal and vertical
#'   red lines through the COG to visualize its position.
#' @param plot_image Logical. If \code{TRUE} (default), plots the image. If \code{FALSE},
#'   returns the image object without plotting.
#' @param cimg Logical. If \code{TRUE} (default) and \pkg{imager} is available, returns
#'   a \code{cimg} object. If \code{FALSE}, always returns a raster object.
#'
#' @return If \code{cimg = TRUE} and \pkg{imager} is installed, returns a \code{cimg}
#'   object. Otherwise, returns a raster object (class \code{"raster"}).
#'
#' @examplesIf (identical(Sys.getenv("IN_PKGDOWN"), "true") || interactive())
#' data(img_A)
#' result <- cog_stroke(img_A)
#' draw_stroke(result, show_cog = TRUE, cimg = FALSE)
#'
#' @seealso \code{\link{cog_stroke}}
#'
#' @importFrom dplyr left_join mutate if_else
#' @export
draw_stroke <- function(lst, show_cog = TRUE, plot_image = TRUE, cimg = TRUE){

  if (!identical(attr(lst, "coglyphr_type"), "stroke")) {
    stop("Input must be a result from cog_stroke().")
  }

  statistics <- lst$statistics
  strokes <- lst$strokes
  origin <- lst$origin

  w <- statistics$width_original
  h <- statistics$height_original

  cog <- c(statistics$center_x, statistics$center_y) |> round()

  out <- list(
    x = 1:w,
    y = 1:h,
    cc = 1:3
  ) |>
    expand.grid() |>
    dplyr::left_join(strokes, by = c("x", "y")) |>
    dplyr::mutate(value = dplyr::if_else(is.na(value), 1, 0))

  if (show_cog) {
    if (origin == "bottomleft") {
      cog[2] <- h - cog[2]
    }

    out <- out |>
      dplyr::mutate(
        value = dplyr::if_else(x == cog[1] & cc == 1, 1, value),
        value = dplyr::if_else(x == cog[1] & cc != 1, 0, value),
        value = dplyr::if_else(y == cog[2] & cc == 1, 1, value),
        value = dplyr::if_else(y == cog[2] & cc != 1, 0, value)
      )
  }

  # Build array [h, w, 3] (rows=y, cols=x), values in [0,1]
  arr <- array(1, dim = c(h, w, 3))
  idx <- cbind(out$y, out$x, out$cc)
  arr[idx] <- out$value

  ## --- Return cimg if requested and available ---
  if (isTRUE(cimg)) {
    if (requireNamespace("imager", quietly = TRUE)) {
      # Convert to imager format: [width, height, depth, channels]
      arr_whc  <- aperm(arr, c(2, 1, 3))                # [w, h, 3]
      arr_wh1c <- array(arr_whc, dim = c(w, h, 1, 3))   # [w, h, 1, 3]

      img_out <- imager::as.cimg(arr_wh1c)

      if (plot_image) {
        plot(img_out)
      }
      invisible(img_out)
    } else {
      warning(
        "`cimg = TRUE` was requested, but the 'imager' package is not installed. ",
        "Returning a raster object instead."
      )
    }
  }

  ## --- Fallback: raster ---
  ras <- grDevices::as.raster(arr)

  if (plot_image) {
    plot(ras)
  }

  invisible(ras)

}

utils::globalVariables("cc")
