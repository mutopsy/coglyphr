#' Compute Potential Energy-Based Center of Gravity (COG)
#'
#' Calculates the center of gravity (COG) of a character-like binary image based on potential energy.
#'
#' @details
#' In the potential energy-based method, the center of gravity (COG) is defined as the weighted mean of the coordinates
#' of all pixels within the convex hull of the stroke region, where the weight at each pixel is determined by the potential
#' induced by all other stroke pixels.
#' The method assumes that each stroke pixel contributes a unit mass and exerts an attractive force
#' on all other pixels within the convex polygon, inversely proportional to their distance,
#' modeling a simplified gravitational interaction.
#' To avoid excessive computation, unintended influence from remote regions, and to restrict
#' the analysis to a perceptually relevant area, the potential is computed only within
#' the convex polygon (i.e., the same region used in the contour-based COG calculation),
#' rather than across the entire image.
#'
#' Let \eqn{S} be the set of all stroke pixels, and let \eqn{R} be the set of all pixels within the convex polygon region.
#' The potential at each pixel \eqn{(x, y) \in R} is defined as:
#'
#' \deqn{
#' p(x, y) = \sum_{\substack{(x', y') \in S \\ (x', y') \ne (x, y)}} \frac{1}{\sqrt{(x - x')^2 + (y - y')^2}}
#' }
#'
#' That is, the potential at each point in \eqn{R} is the sum of the inverse distances to all stroke pixels in \eqn{S}, excluding the case where \eqn{(x', y') = (x, y)}.
#' Pixels outside the convex polygon are assigned a potential value of zero and do not contribute to the COG calculation.
#'
#' Then, the center of gravity is computed as:
#'
#' \deqn{
#' G_x = \left( \sum_{(x, y) \in R} p(x, y) \cdot x \right) / \left( \sum_{(x, y) \in R} p(x, y) \right)
#' }
#' \deqn{
#' G_y = \left( \sum_{(x, y) \in R} p(x, y) \cdot y \right) / \left( \sum_{(x, y) \in R} p(x, y) \right)
#' }
#'
#' In other words, the COG corresponds to the weighted mean of pixel coordinates in the convex region,
#' where weights are given by their potential values induced by the distribution of stroke pixels.
#'
#' This method was originally proposed by Kotani et al. (2006) and has been used
#' in character analysis and font design to reflect the perceived shape of glyphs more robustly
#' than simple stroke averaging, and to further improve upon the contour-based COG
#' by incorporating the spatial distribution of strokes within the convex polygon,
#' thereby aligning more closely with the subjective impression of a character's center.
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
#'     }
#'   }
#'   \item{\code{potentials}}{A data frame containing the (x, y) coordinates and the normalized potential value
#'     for each pixel within the convex hull. The potentials are normalized so that their sum equals 1.}

#' }
#'
#' @references
#' Kotani, A., Tanemura, Y., Mitsuyama, Y., Asai, Y., Nakamura, Y., & Onoye, T. (2006). Potential energy-based center of gravity evaluation of characters. \emph{The Journal of the Institute of Image Electronics Engineers of Japan}, \strong{35}(4), 296–305. \doi{10.11371/iieej.35.296}
#'
#' @examples
#' \donttest{
#'   data(img_A) # load example image from the package
#'   result <- cog_potential(img_A, origin = "bottomleft")
#'
#'   result$statistics # summary data frame
#'   head(result$potentials) # pixel coordinates with normalized potential values
#'   result$origin     # image origin specification
#' }
#'
#' @importFrom imager load.image
#' @importFrom sp point.in.polygon
#' @importFrom dplyr mutate if_else filter select slice bind_rows summarise
#' @export

cog_potential <- function(img, origin = c("bottomleft", "topleft")){

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

  # Make Polygon Region -----------------------------------------------------

  im.dat.region <- im.dat |>
    dplyr::mutate(
      inc = sp::point.in.polygon(x, y, points$x, points$y)
    ) |>
    dplyr::filter(inc >= 1) |>
    dplyr::mutate(p = NA_real_)

  # Calculate Potential Energy ----------------------------------------------

  for(i in 1:nrow(im.dat.region)){
    distance <- sqrt((im.dat.region$x[i] - im.dat.stroke$x)^2 + (im.dat.region$y[i] - im.dat.stroke$y)^2)
    distance <- distance[distance != 0] # omit the self-point
    im.dat.region$p[i] <- sum(1/distance)
  }

  # Calculate Potential Energy-based Center ----------------------------------

  statistics_p <- im.dat.region |>
    dplyr::mutate(
      total = sum(p)
    ) |>
    dplyr::summarise(
      center_x = sum(p * x) / total[1], # left = 0
      center_y = sum(p * y) / total[1] # top = 0
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
      width_original, height_original, width_trim, height_trim
    )

  if(origin == "bottomleft"){
    statistics_p <- statistics_p |>
      dplyr::mutate(
        center_y = size_original[2] + 1 - center_y, # bottom = 0
        center_y_trim = center_y - margin_bottom, # bottom = 0
        center_y_std = center_y_trim / (height_trim + 1) # bottom = 0
      )
  }

  out <- list(
    statistics = statistics_p,
    potentials = im.dat.region |>
      dplyr::select(x,y,value = p) |>
      dplyr::mutate(value = value / max(value)),
    origin = origin
  )

  attr(out, "coglyphr_type") <- "potential"
  return(out)
}


utils::globalVariables(
  c("value", "y", "x", "height", "width", "angle", "distance",
    "xmin", "xmax", "ymin", "ymax", "inc", "center_x", "margin_left",
    "center_y", "margin_top", "margin_right", "height_original", "margin_bottom",
    "center_x_trim", "width_trim", "center_y_trim", "height_trim",  "center_x_std"," center_y_std",
    "width_original", "p", "total"
  )
)

