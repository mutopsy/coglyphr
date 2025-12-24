#' Visualize Contour-Based Region and Center of Gravity (COG)
#'
#' Visualizes the character region enclosed by the convex polygon computed using \code{\link{cog_contour}}.
#' Optionally overlays crosshair lines at the computed center of gravity (COG) to aid interpretation.
#'
#' By default (\code{cimg = TRUE}), the function returns a \code{cimg} object if the
#' \pkg{imager} package is available. If \pkg{imager} is not installed or if
#' \code{cimg = FALSE}, a base R raster object created by \code{as.raster()} is returned.
#'
#' @param lst A list returned by \code{\link{cog_contour}}, containing
#'   a data frame of polygon points and computed statistics.
#' @param show_cog Logical. If \code{TRUE} (default), draws horizontal and vertical
#'   red lines through the COG to indicate its location.
#' @param plot_image Logical. If \code{TRUE} (default), plots the reconstructed image. If \code{FALSE},
#'   returns the image object without displaying it.
#' @param cimg Logical. If \code{TRUE} (default) and \pkg{imager} is available, returns
#'   a \code{cimg} object. If \code{FALSE}, always returns a raster object.
#'
#' @return If \code{cimg = TRUE} and \pkg{imager} is installed, returns a \code{cimg}
#'   object. Otherwise, returns a raster object (class \code{"raster"}).
#'
#' @examplesIf (identical(Sys.getenv("IN_PKGDOWN"), "true") || interactive())
#' data(img_A) # load example image from the package
#' result <- cog_contour(img_A)
#' draw_contour(result, show_cog = TRUE, cimg = FALSE)
#'
#' @seealso \code{\link{cog_contour}}
#'
#' @importFrom dplyr mutate if_else select
#' @importFrom sp point.in.polygon
#' @export

draw_contour <- function(lst, show_cog = TRUE, plot_image = TRUE, cimg = TRUE){

  if (!identical(attr(lst, "coglyphr_type"), "contour")) {
    stop("Input must be a result from cog_contour().")
  }

  statistics <- lst$statistics
  points <- lst$points |> dplyr::select(x,y)
  origin <- lst$origin

  w <- statistics$width_original
  h <- statistics$height_original

  cog <- c(statistics$center_x, statistics$center_y) |> round()

  out <- list(
    x = 1:statistics$width_original,
    y = 1:statistics$height_original,
    cc = 1:3
  ) |>
    expand.grid() |>
    dplyr::mutate(
      value = 1,
      inc = sp::point.in.polygon(
        x, y, points$x, points$y
      ),
      value = dplyr::if_else(inc >= 1, 0, 1)
    ) |>
    dplyr::select(-inc)

  if(show_cog){
    if(origin == "bottomleft"){
      cog[2] <- statistics$height_original - cog[2]
    }

    out <- out |>
      dplyr::mutate(
        value = dplyr::if_else(x == cog[1]&cc==1, 1, value),
        value = dplyr::if_else(x == cog[1]&cc!=1, 0, value),
        value = dplyr::if_else(y == cog[2]&cc==1, 1, value),
        value = dplyr::if_else(y == cog[2]&cc!=1, 0, value),
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
