#' Visualize Stroke Region and Center of Gravity (COG)
#'
#' Visualizes the stroke region of a character-like binary image using the result
#' from \code{cog_stroke()}. Optionally overlays crosshair lines at the computed
#' center of gravity (COG) position.
#'
#' @param lst A list returned by \code{\link{cog_stroke}}, containing
#'   stroke pixel data and computed statistics.
#' @param show_cog Logical. If \code{TRUE} (default), draws horizontal and vertical
#'   red lines through the COG to visualize its position.
#' @param plot_image Logical. If \code{TRUE} (default), plots the image. If \code{FALSE},
#'   returns the image object without plotting.
#'
#' @return An object of class \code{cimg} representing the stroke image (with or without
#'   COG lines), suitable for plotting or further manipulation.
#'
#' @examplesIf (identical(Sys.getenv("IN_PKGDOWN"), "true") || interactive())
#' data(img_A) # load example image from the package
#' result <- cog_stroke(img_A)
#' draw_stroke(result, show_cog = TRUE)
#'
#' @seealso \code{\link{cog_stroke}}
#'
#' @importFrom dplyr left_join mutate if_else
#' @importFrom imager as.cimg
#' @export

draw_stroke <- function(lst, show_cog = TRUE, plot_image = TRUE){

  if (!identical(attr(lst, "coglyphr_type"), "stroke")) {
    stop("Input must be a result from cog_stroke().")
  }

  statistics <- lst$statistics
  strokes <- lst$strokes
  origin <- lst$origin

  cog <- c(statistics$center_x, statistics$center_y) |> round()

  out <- list(
    x = 1:statistics$width_original,
    y = 1:statistics$height_original,
    cc = 1:3
  ) |>
    expand.grid() |>
    dplyr::left_join(strokes, by = c("x", "y")) |>
    dplyr::mutate(value = if_else(is.na(value), 1, 0))

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
