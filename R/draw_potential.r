#' Visualize Potential-Based Center of Gravity (COG) and Potential Field
#'
#' Visualizes the normalized potential field and center of gravity (COG) computed by \code{\link{COG_potential}}.
#' Each pixel's potential is shown as grayscale intensity, where darker pixels indicate higher potential.
#' Optionally overlays crosshair lines at the computed COG to indicate its position.
#'
#' @param lst A list returned by \code{\link{COG_potential}}, containing
#'   a data frame of normalized potentials and computed statistics.
#' @param show_cog Logical. If \code{TRUE} (default), draws horizontal and vertical
#'   red lines through the COG.
#' @param plot_image Logical. If \code{TRUE} (default), plots the generated image. If \code{FALSE},
#'   returns the image object without displaying it.
#'
#' @return An object of class \code{cimg} representing the potential field image,
#'   optionally overlaid with COG crosshair lines.
#'
#' @examples
#' \dontrun{
#'   result <- COG_potential(img_A)
#'   draw_potential(result, show_cog = TRUE)
#' }
#'
#' @importFrom dplyr mutate if_else select
#' @importFrom imager as.cimg plot
#' @importFrom utils head
#' @export

draw_potential <- function(lst, show_cog = TRUE, plot_image = TRUE){
  statistics <- lst$statistics
  potentials <- lst$potentials %>% dplyr::select(x,y,value)

  cog <- c(statistics$center_x, statistics$center_y) %>% round()

  out <- list(
    x = 1:statistics$width_original,
    y = 1:statistics$height_original,
    cc = 1:3
  ) %>%
    expand.grid() %>%
    left_join(potentials, by = c("x","y")) %>%
    mutate(
      value = if_else(is.na(value), 1, 1 - value)
    )

  if(show_cog){
    out <- out %>%
      dplyr::mutate(
        value = if_else(x == cog[1]&cc==1, 1, value),
        value = if_else(x == cog[1]&cc!=1, 0, value),
        value = if_else(y == cog[2]&cc==1, 1, value),
        value = if_else(y == cog[2]&cc!=1, 0, value),
      )
  }

  out <- out %>%
    imager::as.cimg(dims = c(statistics$width_original, statistics$height_original, 1, 3))

  if(plot_image){
    plot(out)
  }

  return(out)
}
