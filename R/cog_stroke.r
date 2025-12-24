#' Compute Stroke-Based Center of Gravity (COG)
#'
#' Computes the center of gravity (COG) of a character-like binary image using its stroke region.
#'
#' @details
#' In the stroke-based method, the COG is defined as the arithmetic mean of the (x, y) coordinates
#' of all pixels that belong to the stroke region, i.e., the foreground pixels whose intensity values
#' are not equal to 1 (pure white). This approach assumes that each stroke pixel has unit mass and
#' contributes equally to the center calculation. The image is assumed to be binary, where the background
#' pixels have a value of 1, and all other pixels are treated as part of the glyph.
#'
#' Mathematically, the stroke-based center of gravity \eqn{(G_x, G_y)} is defined as the weighted mean of pixel coordinates
#' within the stroke region, where each pixel contributes a value of 1 (unit mass) and background pixels are excluded.
#' Specifically, let \eqn{p(x, y)} be an indicator function such that \eqn{p(x, y) = 1} if the pixel \eqn{(x, y)}
#' belongs to the stroke region, and \eqn{p(x, y) = 0} otherwise. Then the horizontal and vertical components
#' of the COG are computed as:
#'
#' \deqn{
#'   G_x = \frac{\sum_{x=1}^{w} \sum_{y=1}^{h} p(x, y)\, x}{\sum_{x=1}^{w} \sum_{y=1}^{h} p(x, y)}
#' }{
#'   Gx = (sum_{x=1..w} sum_{y=1..h} p(x,y)*x) / (sum_{x=1..w} sum_{y=1..h} p(x,y))
#' }
#' \deqn{
#'   G_y = \frac{\sum_{x=1}^{w} \sum_{y=1}^{h} p(x, y)\, y}{\sum_{x=1}^{w} \sum_{y=1}^{h} p(x, y)}
#' }{
#'   Gy = (sum_{x=1..w} sum_{y=1..h} p(x,y)*y) / (sum_{x=1..w} sum_{y=1..h} p(x,y))
#' }
#'
#' where \eqn{w} and \eqn{h} denote the width and height of the image, respectively.
#'
#' @param img An image input. Supported inputs are:
#'   \itemize{
#'     \item A file path to an image file (e.g., PNG, JPEG, TIFF, BMP).
#'     \item A \code{cimg} object from the \pkg{imager} package (if \pkg{imager} is installed).
#'     \item A numeric matrix \code{[h, w]} (grayscale) or numeric array \code{[h, w, ch]} (color).
#'   }
#'   The image should be binary in the sense that background pixels take the value 1 (pure white),
#'   and all other pixels are treated as stroke (foreground) pixels.
#' @param origin A character string indicating the location of the image origin.
#'   Use \code{"bottomleft"} (default) if the y-axis increases upward (Cartesian),
#'   or \code{"topleft"} if the y-axis increases downward (as in image arrays).
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{statistics}}{A data frame with the following components:
#'     \itemize{
#'       \item \code{center_x}, \code{center_y}: The (x, y) coordinates of the COG in pixel coordinates of the input image.
#'       \item \code{center_x_trim}, \code{center_y_trim}: The COG coordinates relative to the trimmed glyph region, excluding image margins.
#'       \item \code{center_x_std}, \code{center_y_std}: The standardized COG coordinates ranging from 0 to 1, based on the trimmed region's width and height.
#'       \item \code{margin_left}, \code{margin_right}, \code{margin_top}, \code{margin_bottom}: Margins between the glyph and the image boundary.
#'       \item \code{width_original}, \code{height_original}: Dimensions of the original image.
#'       \item \code{width_trim}, \code{height_trim}: Width and height of the trimmed glyph region, excluding margins.
#'       \item \code{area}: The number of pixels in the stroke region (i.e., the total mass used to compute the COG).
#'     }
#'   }
#'   \item{\code{strokes}}{A data frame of (x, y) coordinates representing the stroke region (i.e., non-white pixels).}
#'   \item{\code{origin}}{The origin specification used for the returned coordinates.}
#' }
#'
#' @examples
#' data(img_A) # load example image from the package
#' result <- cog_stroke(img_A, origin = "bottomleft")
#'
#' result$statistics # summary data frame
#' head(result$strokes) # stroke pixel coordinates
#' result$origin # image origin specification
#'
#' @importFrom dplyr mutate filter summarise n select
#' @export


cog_stroke <- function(img, origin = c("bottomleft", "topleft")){

  # Initialization ------------------------
  origin <- origin[1]

  if (!origin %in% c("topleft", "bottomleft")) {
    warning('Invalid value for `origin`. Defaulting to "bottomleft".')
    origin <- "bottomleft"
  }

  # Load image ------------------------
  im.dat <- coglyphr_load_image(img)
  size_original <- c(max(im.dat$x), max(im.dat$y))

  # Extract the non-white region ------------------------
  im.dat.stroke <- im.dat |>
    dplyr::filter(value != 1) |>
    dplyr::mutate(value = 0)

  # Margin ------------------------

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
      center_x = mean(x), # left = 0
      center_y = mean(y), # top = 0
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
      center_x_trim = center_x - margin_left, # left = 0
      center_y_trim = center_y - margin_top, # top = 0
      width_trim = width_original - margin_left - margin_right,
      height_trim = height_original - margin_top - margin_bottom,
    ) |>
    dplyr::mutate(
      center_x_std = center_x_trim / (width_trim + 1), # left = 0
      center_y_std = center_y_trim / (height_trim + 1) # top  = 0
    ) |>
    dplyr::select(
      center_x, center_y, center_x_trim, center_y_trim, center_x_std, center_y_std,
      margin_left, margin_right, margin_top, margin_bottom,
      width_original, height_original, width_trim, height_trim,
      area
    )

  if(origin == "bottomleft"){
    statistics <- statistics |>
      dplyr::mutate(
        center_y = size_original[2] + 1 - center_y, # bottom = 0
        center_y_trim = center_y - margin_bottom, # bottom = 0
        center_y_std = center_y_trim / (height_trim + 1) # bottom = 0
      )
  }

  out <- list(
    statistics = statistics,
    strokes = im.dat.stroke,
    origin = origin
  )

  attr(out, "coglyphr_type") <- "stroke"
  return(out)
}


utils::globalVariables(
  c("value", "y", "x", "height", "width", "angle", "distance", "area",
    "xmin", "xmax", "ymin", "ymax", "inc", "center_x", "margin_left",
    "center_y", "margin_top", "margin_right", "height_original", "margin_bottom",
    "center_x_trim", "width_trim", "center_y_trim", "height_trim", "center_x_std", "center_y_std",
    "width_original"
  )
)
