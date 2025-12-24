#' Visualize Potential-Based Center of Gravity (COG) and Potential Field
#'
#' Visualizes the normalized potential field and center of gravity (COG) computed by
#' \code{\link{cog_potential}}. Each pixel's potential is shown as grayscale intensity,
#' where darker pixels indicate higher potential. Optionally overlays crosshair lines
#' at the computed COG to indicate its position.
#'
#' By default (\code{cimg = TRUE}), the function returns a \code{cimg} object if the
#' \pkg{imager} package is available. If \pkg{imager} is not installed or if
#' \code{cimg = FALSE}, a base R raster object created by \code{as.raster()} is returned.
#'
#' @param lst A list returned by \code{\link{cog_potential}}, containing a data frame of
#'   normalized potentials and computed statistics.
#' @param show_cog Logical. If \code{TRUE} (default), draws horizontal and vertical
#'   red lines through the COG.
#' @param plot_image Logical. If \code{TRUE} (default), plots the generated image.
#'   If \code{FALSE}, returns the image object without displaying it.
#' @param cimg Logical. If \code{TRUE} (default) and \pkg{imager} is available, returns
#'   a \code{cimg} object. If \code{FALSE}, always returns a raster object.
#'
#' @return If \code{cimg = TRUE} and \pkg{imager} is installed, returns a \code{cimg}
#'   object. Otherwise, returns a raster object (class \code{"raster"}).
#'
#' @examplesIf (identical(Sys.getenv("IN_PKGDOWN"), "true") || interactive())
#' \donttest{
#'   data(img_A)
#'   result <- cog_potential(img_A)
#'   draw_potential(result, show_cog = TRUE, cimg = FALSE)
#' }
#'
#' @seealso \code{\link{cog_potential}}
#'
#' @importFrom dplyr mutate if_else select left_join
#' @export
draw_potential <- function(lst, show_cog = TRUE, plot_image = TRUE, cimg = TRUE){

  if (!identical(attr(lst, "coglyphr_type"), "potential")) {
    stop("Input must be a result from cog_potential().")
  }

  statistics <- lst$statistics
  potentials <- lst$potentials |> dplyr::select(x, y, value)
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
    dplyr::left_join(potentials, by = c("x","y")) |>
    dplyr::mutate(
      # show as grayscale: background white; higher potential -> darker
      value = dplyr::if_else(is.na(value), 1, 1 - value)
    )

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
      arr_whc  <- aperm(arr, c(2, 1, 3))               # [w, h, 3]
      arr_wh1c <- array(arr_whc, dim = c(w, h, 1, 3))  # [w, h, 1, 3]

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
