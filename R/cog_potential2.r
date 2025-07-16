#' Compute Potential Energy-Based Center of Gravity (COG)
#'
#' Calculates the center of gravity (COG) of a character-like binary image based on potential energy.
#' This function implements the method proposed by Kotani et al. (2006), in which the COG is defined
#' as the potential energy-weighted mean within the smallest convex polygon (i.e., convex hull)
#' that encloses the character region. Each pixel in the stroke region is treated as a point mass of unit mass.
#' The potential at each pixel is computed as the sum of the inverses of the distances between that pixel
#' and all pixels in the stroke region.
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
#'     margins around the glyph, and original image dimensions. The center is calculated as the centroid of the
#'     smallest convex polygon enclosing the glyph region.}
#'   \item{\code{potentials}}{A data frame containing the (x, y) coordinates and the normalized potential value
#'     for each pixel within the convex hull. The potentials are normalized so that their sum equals 1.}

#' }
#'
#' @references
#' Kotani, A., Tanemura, Y., Mitsuyama, Y., Asai, Y., Nakamura, Y., & Onoye, T. (2006). Potential energy-based center of gravity evaluation of characters. \emph{The Journal of the Institute of Image Electronics Engineers of Japan}, \strong{35}(4), 296–305. \url{https://doi.org/10.11371/iieej.35.296}
#'
#' @examples
#' \dontrun{
#'   cog_potential(img_A, origin = "bottomleft")
#' }
#'
#' @importFrom imager load.image
#' @importFrom sp point.in.polygon
#' @import dplyr
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

  # Transform to data frame format ------------------------

  im.dat <- im |> as.data.frame() |> dplyr::filter(cc == 1)

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
    dplyr::filter(cc == 1) |>
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
      center_x_std = center_x_trim / width_trim, # left = 0
      center_y_std = 1 - center_y_trim / height_trim # top = 0
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
    statistics = statistics_p,
    potentials = im.dat.region |>
      dplyr::select(x,y,cc,value = p) |>
      dplyr::mutate(value = value / max(value)),
    origin = origin
  )

  return(out)
}


utils::globalVariables(
  c("cc", "value", "y", "x", "height", "width", "angle", "distance",
    "xmin", "xmax", "ymin", "ymax", "inc", "center_x", "margin_left",
    "center_y", "margin_top", "margin_right", "height_original", "margin_bottom",
    "center_x_trim", "width_trim", "center_y_trim", "height_trim", "center_y_std",
    "width_original", "p", "total"
  )
)

