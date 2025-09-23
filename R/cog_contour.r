#' Compute Contour-Based Center of Gravity (COG)
#'
#' Computes the center of gravity (COG) of a character-like binary image using its outer contour.
#'
#' @details
#' In the contour-based method, the center of gravity (COG) is defined as the geometrical centroid
#' of the convex hull (the smallest convex polygon) that encloses the stroke region of the character.
#' The convex hull is estimated by tracing the outer contour of the glyph and computing the minimal convex polygon
#' that wraps all stroke pixels, i.e., the foreground pixels whose intensity values are not equal to 1 (pure white).
#' The centroid is then calculated as the arithmetic mean of the (x, y) coordinates
#' of all pixels located within the interior of the convex polygon.
#'
#' Mathematically, the contour-based center of gravity \eqn{(G_x, G_y)} is defined as the weighted mean of pixel coordinates
#' within the polygon region, where each pixel contributes a value of 1 (unit mass) and background pixels are excluded.
#' Specifically, let \eqn{p(x, y)} be an indicator function such that \eqn{p(x, y) = 1} if the pixel \eqn{(x, y)} lies
#' inside the convex polygon and \eqn{p(x, y) = 0} otherwise. Then the horizontal and vertical components
#' of the COG are computed as:
#'
#' \deqn{
#' G_x = (\sum_{x=1}^{w} \sum_{y=1}^{h} p(x, y) \cdot x) / (\sum_{x=1}^{w} \sum_{y=1}^{h} p(x, y))
#' }
#' \deqn{
#' G_y = (\sum_{x=1}^{w} \sum_{y=1}^{h} p(x, y) \cdot y) / (\sum_{x=1}^{w} \sum_{y=1}^{h} p(x, y))
#' }
#'
#' where \eqn{w} and \eqn{h} denote the width and height of the image, respectively.
#'
#' This method was originally proposed by Kotani and colleagues (2004, 2011) and has been used
#' in character analysis and font design to reflect the perceived shape of glyphs more robustly
#' than simple stroke averaging.
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
#'   \item{\code{statistics}}{A data frame with the following components:
#'     \itemize{
#'       \item \code{center_x}, \code{center_y}: The (x, y) coordinates of the COG in pixel coordinates of the input image.
#'       \item \code{center_x_trim}, \code{center_y_trim}: The COG coordinates relative to the trimmed glyph region, excluding image margins.
#'       \item \code{center_x_std}, \code{center_y_std}: The standardized COG coordinates ranging from 0 to 1, based on the trimmed region's width and height.
#'       \item \code{margin_left}, \code{margin_right}, \code{margin_top}, \code{margin_bottom}: Margins between the glyph and the image boundary.
#'       \item \code{width_original}, \code{height_original}: Dimensions of the original image.
#'       \item \code{width_trim}, \code{height_trim}: Width and height of the trimmed glyph region, excluding margins.
#'       \item \code{area}: The number of pixels inside the convex hull (i.e., the total mass used to compute the COG).
#'     }
#'   }
#'   \item{\code{points}}{A data frame of (x, y) coordinates representing the contour points of the convex polygon.}
#' }
#'
#' @references
#' Kotani, A. (2011). Contour-based evaluation method of center of gravity on characters and its application to font development. \emph{Memoirs of Shonan Institute of Technology}, \strong{45}(1), 23–33. \url{https://shonan-it.repo.nii.ac.jp/?action=repository_view_main_item_detail&item_id=368}
#'
#' Kotani, A., Asai, Y., Nakamura, Y., Otuka, M., Mituyama, Y., & Onoye, T. (2004). Contour-based evaluation method of center of gravity on “LCFONT.” \emph{IPSJ SIG Technical Report}, \strong{115}, 63–70. \url{https://ipsj.ixsq.nii.ac.jp/records/36793}
#'
#' @examples
#' data(img_A) # load example image from the package
#' result <- cog_contour(img_A, origin = "bottomleft")
#'
#' result$statistics # summary data frame
#' result$points # contour polygon vertices (x, y, angle)
#' result$origin # image origin specification
#'
#' @importFrom imager load.image
#' @importFrom sp point.in.polygon
#' @importFrom dplyr mutate if_else filter select slice bind_rows summarise n
#' @export

cog_contour <- function(img, origin = c("bottomleft", "topleft")){

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

  # Binarize image  ------------------------

  n_ch <- imager::spectrum(im)

  if (n_ch == 3 || n_ch == 4) {
    r <- imager::R(im)
    g <- imager::G(im)
    b <- imager::B(im)

    white_mask <- (r == 1) & (g == 1) & (b == 1)
    im <- imager::as.cimg(white_mask * 1)

  } else if (n_ch == 1) {
    im <- im |>
      as.data.frame() |>
      mutate(value = if_else(value == 1, 1, 0)) |>
      as.cimg(dim = c(dim(im)))

  } else {
    stop("Cannot convert image: unsupported number of channels (must be 1, 3, or 4).")
  }

  # Transform to data frame format ------------------------

  im.dat <- im |> as.data.frame()

  # Extract the non-white region ------------------------

  im.dat.stroke <- im.dat |>
    dplyr::filter(value != 1) |>
    dplyr::mutate(value = 0)

  # Initialization of points ------------------------

  points <- data.frame(
    x = NA,
    y = NA,
    angle = NA
  )

  # Point 0 ------------------------

  point_0 <- im.dat.stroke |>
    dplyr::filter(y == max(y)) |>
    dplyr::filter(x == min(x))

  points[1,] <- c(point_0$x, point_0$y, 0)

  # Point i ------------------------

  ## Initialization ------------------------

  theta <- 0
  i <- 0
  init <- points[1,]
  ref <- points[1,]
  is_end <- 0

  ## Loop ------------------------

  while(is_end == 0){
    check <- im.dat.stroke |>
      dplyr::filter(x != ref$x | y != ref$y) |>
      dplyr::mutate(
        height = ref$y - y,
        width = x - ref$x,
        slope = height/width,
        angle = atan2(height, width),
        angle = (angle + 2 * pi) %% (2 * pi)
      ) |>
      dplyr::filter(angle >= theta) |>
      dplyr::filter(angle == min(angle)) |>
      dplyr::mutate(
        distance = sqrt(height^2 + width^2)
      ) |>
      dplyr::filter(distance == max(distance))

    i <- i + 1

    if(i > 10000){
      warnings("Too long attempts!")
      is_end <- 1
    }

    if(check$x == init$x & check$y == init$y){
      is_end <- 1
    } else{
      theta <- check$angle[1]
      ref <- check |> dplyr::select(x,y, angle) |> dplyr::slice(1)

      points <- points |>
        dplyr::bind_rows(ref)
    }
  }

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

  tmp <- list(
    x = 1:size_original[1],
    y = 1:size_original[2]
  ) |>
    expand.grid() |>
    dplyr::mutate(value = 1) |>
    dplyr::mutate(
      inc = sp::point.in.polygon(
        x, y, points$x, points$y
      ),
      value = if_else(inc >= 1, 0, 1)
    ) |>
    dplyr::filter(value == 0)

  statistics <- tmp |>
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
    points = points,
    origin = origin
  )

  attr(out, "coglyphr_type") <- "contour"
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
