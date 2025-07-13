#' Compute Contour-Based Center of Gravity (COG) from Convex Polygon
#'
#' Calculates the center of gravity (COG) of a character-like binary image based on its outer contour.
#' This function implements the method proposed by Kotani and colleagues (2004, 2011), in which the COG
#' is computed as the geometric center of the smallest convex polygon (convex hull) that encloses the
#' character region. The outer contour is traced counterclockwise starting from the bottommost-leftmost pixel,
#' and the resulting closed shape is filled to compute its centroid.
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
#'   \item{\code{points}}{A data frame of (x, y) coordinates representing the contour points of the convex polygon.}
#' }
#'
#' @references
#' Kotani, A. (2011). Contour-based evaluation method of center of gravity on characters and its application to font development. \emph{Memoirs of Shonan Institute of Technology}, \strong{45}(1), 23–33. \url{https://shonan-it.repo.nii.ac.jp/?action=repository_view_main_item_detail&item_id=368}
#'
#' Kotani, A., Asai, Y., Nakamura, Y., Otuka, M., Mituyama, Y., & Onoye, T. (2004). Contour-based evaluation method of center of gravity on “LCFONT.” \emph{IPSJ SIG Technical Report}, \strong{115}, 63–70. \url{https://ipsj.ixsq.nii.ac.jp/records/36793}
#'
#' @importFrom imager load.image
#' @importFrom sp point.in.polygon
#' @import dplyr
#' @import tidyr
#' @export

COG_contour <- function(img, origin = c("bottomleft", "topleft")){

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

  im.dat <- im %>% as.data.frame() %>% dplyr::filter(cc == 1)

  # Extract the non-white region ------------------------

  im.dat.stroke <- im.dat %>%
    dplyr::filter(value != 1) %>%
    dplyr::mutate(value = 0)

  # Initialization of points ------------------------

  points <- data.frame(
    x = NA,
    y = NA,
    angle = NA
  )

  # Point 0 ------------------------

  point_0 <- im.dat.stroke %>%
    dplyr::filter(y == max(y)) %>%
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
    check <- im.dat.stroke %>%
      dplyr::filter(x != ref$x | y != ref$y) %>%
      dplyr::mutate(
        height = ref$y - y,
        width = x - ref$x,
        slope = height/width,
        angle = atan2(height, width),
        angle = (angle + 2 * pi) %% (2 * pi)
      ) %>%
      dplyr::filter(angle >= theta) %>%
      dplyr::filter(angle == min(angle)) %>%
      dplyr::mutate(
        distance = sqrt(height^2 + width^2)
      ) %>%
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
      ref <- check %>% dplyr::select(x,y, angle) %>% dplyr::slice(1)

      points <- points %>%
        dplyr::bind_rows(ref)
    }
  }

  # Margin ------------------------

  size_original <- dim(im)[1:2]

  margin <- im.dat.stroke %>%
    dplyr::summarise(
      xmin = min(x),
      xmax = max(x),
      ymin = min(y),
      ymax = max(y)
    ) %>%
    dplyr::mutate(
      margin_left = xmin - 1,
      margin_right = size_original[1] - xmax,
      margin_top = ymin - 1,
      margin_bottom = size_original[2] - ymax
    )

  # Calculate area and COG ------------------------

  tmp <- list(
    x = 1:size_original[1],
    y = 1:size_original[2],
    cc = 1:3
  ) %>%
    expand.grid() %>%
    dplyr::mutate(value = 1) %>%
    dplyr::mutate(
      inc = sp::point.in.polygon(
        x, y, points$x, points$y
      ),
      value = if_else(inc >= 1, 0, 1)
    ) %>%
    dplyr::filter(value == 0)

  statistics <- tmp %>%
    dplyr::summarise(
      center_x = mean(x), # left = 0, right = 1
      center_y = mean(y), # top = 0, bottom = 1
      area = sum(value)
    ) %>%
    dplyr::mutate(
      margin_left = margin$margin_left,
      margin_right = margin$margin_right,
      margin_top = margin$margin_top,
      margin_bottom = margin$margin_bottom,
      width_original = size_original[1],
      height_original = size_original[2]
    ) %>%
    dplyr::mutate(
      center_x_trim = center_x - margin_left, # left = 0, right = 1
      center_y_trim = center_y - margin_top, # top = 0, bottom = 1
      width_trim = width_original - margin_left - margin_right,
      height_trim = height_original - margin_top - margin_bottom,
    ) %>%
    dplyr::mutate(
      center_x_std = center_x_trim / width_trim, # left = 0, right = 1
      center_y_std = center_y_trim / height_trim # top = 0, bottom = 1
    )

  if(origin == "bottomleft"){
    statistics <- statistics %>%
      dplyr::mutate(
        center_y = size_original[2] - center_y, # bottom = 0, top = 1
        center_y_trim = height_trim - center_y_trim, # bottom = 0, top = 1
        center_y_std = 1 - center_y_std # bottom = 0, top = 1
      )
  }

  out <- list(
    statistics = statistics,
    points = points
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

