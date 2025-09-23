#' Visualize Contour-Based Region and Center of Gravity (COG)
#'
#' Visualizes the character region enclosed by the convex polygon computed using \code{\link{cog_contour}}.
#' Optionally overlays crosshair lines at the computed center of gravity (COG) to aid interpretation.
#'
#' @param lst A list returned by \code{\link{cog_contour}}, containing
#'   a data frame of polygon points and computed statistics.
#' @param show_cog Logical. If \code{TRUE} (default), draws horizontal and vertical
#'   red lines through the COG to indicate its location.
#' @param plot_image Logical. If \code{TRUE} (default), plots the reconstructed image. If \code{FALSE},
#'   returns the image object without displaying it.
#'
#' @return An object of class \code{cimg} representing the polygon-filled image
#'   (with or without COG lines), which can be further used or saved.
#'
#' @examplesIf (identical(Sys.getenv("IN_PKGDOWN"), "true") || interactive())
#' data(img_A) # load example image from the package
#' result <- cog_contour(img_A)
#' draw_contour(result, show_cog = TRUE)
#'
#' @seealso \code{\link{cog_contour}}
#'
#' @importFrom dplyr mutate if_else
#' @importFrom sp point.in.polygon
#' @importFrom imager as.cimg
#' @export

draw_contour <- function(lst, show_cog = TRUE, plot_image = TRUE){

  if (!identical(attr(lst, "coglyphr_type"), "contour")) {
    stop("Input must be a result from cog_contour().")
  }

  statistics <- lst$statistics
  points <- lst$points |> dplyr::select(x,y)
  origin <- lst$origin

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
      value = if_else(inc >= 1, 0, 1)
    ) |>
    dplyr::select(-inc)

  if(show_cog){
    if(origin == "bottomleft"){
      cog[2] <- statistics$height_original - cog[2]
    }

    out <- out |>
      dplyr::mutate(
        value = if_else(x == cog[1]&cc==1, 1, value),
        value = if_else(x == cog[1]&cc!=1, 0, value),
        value = if_else(y == cog[2]&cc==1, 1, value),
        value = if_else(y == cog[2]&cc!=1, 0, value),
      )
  }

  out <- out |>
    imager::as.cimg(dims = c(statistics$width_original, statistics$height_original, 1, 3))

  if(plot_image){
    plot(out)
  }

  return(out)
}

utils::globalVariables("cc")
