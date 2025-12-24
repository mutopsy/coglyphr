#' Internal image I/O helpers
#'
#' Internal utilities for loading images from file paths or supported in-memory
#' objects and converting them to a unified data-frame representation.
#'
#' These helpers implement the package's binary-image convention:
#' background pixels are defined as those with value exactly equal to 1 (pure white),
#' and all other pixels are treated as stroke (foreground) pixels. For RGB(A) images,
#' a pixel is considered background only if all RGB channels are exactly 1. Alpha
#' transparency is ignored.
#'
#' @keywords internal
#' @noRd
NULL

#' Convert an imager::cimg object to a data frame
#'
#' Converts a \code{cimg} object (from \pkg{imager}) to a data frame with columns
#' \code{x}, \code{y}, \code{value}, consistent with \code{as.data.frame.cimg()}.
#' The returned \code{value} follows the binary convention: background = 1 (white),
#' stroke = 0 (non-white).
#'
#' This helper requires \pkg{imager}. It is intended for internal use only.
#'
#' @param img A \code{cimg} object.
#' @return A data frame with columns \code{x}, \code{y}, \code{value}.
#' @keywords internal
#' @noRd
coglyphr_cimg2df <- function(img){
  if (inherits(img, "cimg")) {
    if (!requireNamespace("imager", quietly = TRUE)) {
      stop("To use 'cimg' inputs, please install the 'imager' package.")
    }

    im <- img

    # Binarize image ----------------
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
        dplyr::mutate(value = dplyr::if_else(value == 1, 1, 0)) |>
        imager::as.cimg(dim = c(dim(im)))

    } else {
      stop("Cannot convert image: unsupported number of channels (must be 1, 3, or 4).")
    }

    # Transform to data frame format ----------------
    im.dat <- im |> as.data.frame()
    return(im.dat)

  } else{
    stop("`img` must be a cimg object.")
  }
}

#' Read an image file into a matrix/array
#'
#' Reads an image file from a path using lightweight format-specific readers.
#' Supported formats are PNG, JPEG, TIFF, and BMP, each provided via an optional
#' package in \code{Suggests}. The returned object is typically a numeric matrix
#' (grayscale) or a numeric array \code{[h, w, ch]} (color), with values usually in
#' \code{[0, 1]}.
#'
#' @param img A length-1 character string giving a path to an image file.
#' @return A matrix or array representing the image.
#' @keywords internal
#' @noRd
coglyphr_path2mat <- function(img){
  if (!is.character(img) || length(img) != 1L) {
    stop("`img` must be a length-1 character string.")
  }
  if (!file.exists(img)) stop("File does not exist: ", img)

  ext <- tolower(tools::file_ext(img))

  if (ext == "png") {
    if (!requireNamespace("png", quietly = TRUE)) {
      stop("Reading PNG requires the 'png' package.")
    }
    im.mat <- png::readPNG(img)

  } else if (ext %in% c("jpg", "jpeg")) {
    if (!requireNamespace("jpeg", quietly = TRUE)) {
      stop("Reading JPEG requires the 'jpeg' package.")
    }
    im.mat <- jpeg::readJPEG(img)

  } else if (ext %in% c("tif", "tiff")) {
    if (!requireNamespace("tiff", quietly = TRUE)) {
      stop("Reading TIFF requires the 'tiff' package.")
    }
    im.mat <- tiff::readTIFF(img)

  } else if (ext == "bmp") {
    if (!requireNamespace("bmp", quietly = TRUE)) {
      stop("Reading BMP requires the 'bmp' package.")
    }
    im.mat <- bmp::read.bmp(img)

  } else {
    stop("Unsupported image format: .", ext)
  }

  return(im.mat) # matrix or array
}

#' Convert a matrix/array image to a binary image data frame
#'
#' Converts a numeric matrix \code{[h, w]} or numeric array \code{[h, w, ch]}
#' (typically returned by \code{png::readPNG()}, \code{jpeg::readJPEG()}, etc.)
#' to a data frame with columns \code{x}, \code{y}, \code{value}. The output uses
#' the binary convention: background = 1 (pure white), stroke = 0 (non-white).
#'
#' For RGB(A) images, a pixel is considered background only if all three RGB
#' channels are exactly 1. Alpha transparency is ignored.
#'
#' @param im.mat A numeric matrix \code{[h, w]} or numeric array \code{[h, w, ch]}.
#' @return A data frame with columns \code{x}, \code{y}, \code{value}.
#' @keywords internal
#' @noRd
coglyphr_mat2df <- function(im.mat) {

  # Accept matrix [h,w] or array [h,w,ch]
  if (is.matrix(im.mat) || (is.array(im.mat) && length(dim(im.mat)) == 2L)) {
    im.mat.binary <- ifelse(im.mat == 1, 1, 0)

  } else if (is.array(im.mat) && length(dim(im.mat)) == 3L) {

    ch <- dim(im.mat)[3]
    if (ch < 1L) stop("Invalid channel dimension in `im.mat`.")

    # 1-channel array treated as grayscale
    if (ch == 1L) {
      im.mat.binary <- ifelse(im.mat[, , 1] == 1, 1, 0)

    } else if (ch >= 3L) {
      r <- im.mat[, , 1]
      g <- im.mat[, , 2]
      b <- im.mat[, , 3]

      white_rgb <- (r == 1) & (g == 1) & (b == 1)
      im.mat.binary <- ifelse(white_rgb, 1, 0)

    } else {
      stop("Unsupported number of channels in `im.mat` (must be 1, 3, or 4).")
    }

  } else {
    stop("`im.mat` must be a matrix [h,w] or an array [h,w,ch].")
  }

  # Build data.frame with 1-indexed pixel coordinates:
  # x = column index (left->right), y = row index (top->bottom)
  h <- nrow(im.mat.binary)
  w <- ncol(im.mat.binary)

  im.dat <- data.frame(
    x = rep(seq_len(w), times = h),
    y = rep(seq_len(h), each  = w),
    value = as.vector(t(im.mat.binary))
  )

  return(im.dat)
}

#' Load an image input and return a unified data-frame representation
#'
#' Dispatches on the type of \code{img} and returns a data frame with columns
#' \code{x}, \code{y}, \code{value}. Supported inputs are:
#' \itemize{
#'   \item File path to an image (PNG/JPEG/TIFF/BMP; via optional reader packages)
#'   \item \code{imager::cimg} objects (requires \pkg{imager})
#'   \item Numeric matrices/arrays representing images
#' }
#'
#' The returned \code{value} follows the binary convention: background = 1,
#' stroke = 0.
#'
#' @param img Image input: file path, \code{cimg}, or numeric matrix/array.
#' @return A data frame with columns \code{x}, \code{y}, \code{value}.
#' @keywords internal
#' @noRd
coglyphr_load_image <- function(img){

  if (is.character(img) && length(img) == 1L) {
    im.mat <- coglyphr_path2mat(img)
    im.dat <- coglyphr_mat2df(im.mat)

  } else if (inherits(img, "cimg")) {
    im.dat <- coglyphr_cimg2df(img)

  } else if (is.matrix(img) || (is.array(img) && length(dim(img)) == 3L)) {
    im.dat <- coglyphr_mat2df(img)

  } else {
    stop("Unsupported `img` type. Provide a file path, a cimg, or a matrix/array.")
  }

  return(im.dat)
}
