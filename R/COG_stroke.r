#' Compute Stroke-Based Center of Gravity (COG)
#'
#' Computes the center of gravity (COG) of a character-like binary image using its stroke region.
#' The COG is defined as the mean of the (x, y) coordinates of all non-white pixels.
#'
#' @param img An image input, either a file path to an image file (e.g., PNG, JPEG),
#'   or a \code{cimg} object from the \pkg{imager} package. The image should be in binary form,
#'   with foreground (glyph) values not equal to 1 and background values equal to 1.
#' @param origin A character string indicating the location of the image origin.
#'   Use \code{"bottomleft"} (default) if the y-axis increases upward (Cartesian),
#'   or \code{"topleft"} if the y-axis increases downward (as in image arrays).
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{statistics}}{A data frame with computed center coordinates (original, trimmed, and standardized),
#'     margins around the glyph, and original image dimensions. The center is calculated based on the stroke region}
#'   \item{\code{strokes}}{A data frame of (x, y) coordinates representing the stroke (non-white) region.}
#' }
#'
#' @examples
#' \dontrun{
#'   cog_stroke(img_A, origin = "bottomleft")
#' }
#'
#' @importFrom imager load.image
#' @import dplyr
#' @export


cog_stroke <- function(img, origin = c("bottomleft", "topleft")){

  # Initialization ------------------------
  origin <- origin[1]

  if (!origin %in% c("topleft", "bottomleft")) {
    warning('Invalid value for `origin`. Defaulting to "bottomleft".')
    origin <- "bottomleft"
  }

  # Load image ------------------------

  if (is.character(img) && length(img) == 1 && file.exists(img)) {
    im <- imager::load.image(img)
  } else if (inherits(img, "cimg")) {
    im <- img
  } else {
    stop("`img` must be either a file path or a valid image object.")
  }

  # Transform to data frame format ------------------------

  im.dat <- im |> as.data.frame() |> dplyr::filter(cc == 1)

  # Extract the non-white region ------------------------

  im.dat.stroke <- im.dat |>
    dplyr::filter(value != 1) |>
    dplyr::mutate(value = 0)

  # Margin ------------------------

  size_original <- dim(im)[1:2]

  margin <- im.dat.stroke |>
    dplyr::summarise(
      xmin = min(x),
      xmax = max(x),
      ymin = min(y),
      ymax = max(y)
    ) |>
    dplyr::mutate(
      margin_left = xmin - 1,
      margin_right = size_original[1] - xmax,
      margin_top = ymin - 1,
      margin_bottom = size_original[2] - ymax
    )

  # Calculate area and COG ------------------------

  statistics <- im.dat.stroke |>
    dplyr::summarise(
      center_x = mean(x), # left = 0, right = 1
      center_y = mean(y), # top = 0, bottom = 1
      area = n()
    ) |>
    dplyr::mutate(
      margin_left = margin$margin_left,
      margin_right = margin$margin_right,
      margin_top = margin$margin_top,
      margin_bottom = margin$margin_bottom,
      width_original = size_original[1],
      height_original = size_original[2]
    ) |>
    dplyr::mutate(
      center_x_trim = center_x - margin_left, # left = 0, right = 1
      center_y_trim = center_y - margin_top, # top = 0, bottom = 1
      width_trim = width_original - margin_left - margin_right,
      height_trim = height_original - margin_top - margin_bottom,
    ) |>
    dplyr::mutate(
      center_x_std = center_x_trim / width_trim, # left = 0, right = 1
      center_y_std = center_y_trim / height_trim # top = 0, bottom = 1
    )

  if(origin == "bottomleft"){
    statistics <- statistics |>
      dplyr::mutate(
        center_y = size_original[2] - center_y, # bottom = 0, top = 1
        center_y_trim = height_trim - center_y_trim, # bottom = 0, top = 1
        center_y_std = 1 - center_y_std # bottom = 0, top = 1
      )
  }

  out <- list(
    statistics = statistics,
    strokes = im.dat.stroke |> dplyr::select(-cc),
    origin = origin
  )

  return(out)
}


utils::globalVariables(
  c("cc", "value", "y", "x", "height", "width", "angle", "distance",
    "xmin", "xmax", "ymin", "ymax", "inc", "center_x", "margin_left",
    "center_y", "margin_top", "margin_right", "height_original", "margin_bottom",
    "center_x_trim", "width_trim", "center_y_trim", "height_trim", "center_y_std",
    "width_original"
  )
)

