test_that("cog_contour given a four-channel image file returns correct structure", {
  inputs_dir <- testthat::test_path("images")
  inputs_files <- list.files(inputs_dir, full.names = TRUE)

  for(i in 1:length(inputs_files)){
    img <- imager::load.image(inputs_files[i])

    result <- cog_contour(img)

    expect_type(result, "list")
    expect_true("statistics" %in% names(result))
    expect_true("points" %in% names(result))
  }
})

test_that("cog_contour given a one-channel image file returns correct structure", {
  inputs_dir <- testthat::test_path("images")
  inputs_files <- list.files(inputs_dir, full.names = TRUE)

  for(i in 1:length(inputs_files)){
    img <- imager::load.image(inputs_files[i])

    # Convert image to grayscale
    n_ch <- imager::spectrum(img)

    if (n_ch == 3 || n_ch == 4) {
      r <- imager::R(img)
      g <- imager::G(img)
      b <- imager::B(img)
      gray_vals <- 0.299 * r + 0.587 * g + 0.114 * b
      img <- imager::as.cimg(gray_vals)

    } else if (n_ch == 1) {
      img <- img

    } else {
      stop("Cannot convert image: unsupported number of channels (must be 1, 3, or 4).")
    }

    result <- cog_contour(img)

    expect_type(result, "list")
    expect_true("statistics" %in% names(result))
    expect_true("points" %in% names(result))
  }
})

test_that("cog_contour given a path to an image file returns correct structure", {
  inputs_dir <- testthat::test_path("images")
  inputs_files <- list.files(inputs_dir, full.names = TRUE)

  for(i in 1:length(inputs_files)){
    img <- inputs_files[i]

    result <- cog_contour(img)

    expect_type(result, "list")
    expect_true("statistics" %in% names(result))
    expect_true("points" %in% names(result))
  }
})

test_that("cog_contour with origin specified returns correctlly adjusted values", {
  inputs_dir <- testthat::test_path("images")
  inputs_files <- list.files(inputs_dir, full.names = TRUE)

  for(i in 1:length(inputs_files)){
    img <- imager::load.image(inputs_files[i])

    size <- dim(img)[1:2]

    stats_none <- cog_contour(img)$statistics
    stats_bottomleft <- cog_contour(img, origin = "bottomleft")$statistics
    stats_topleft <- cog_contour(img, origin = "topleft")$statistics

    expect_equal(stats_none, stats_bottomleft, tolerance = 1e-6)

    expect_equal(stats_bottomleft$center_x, stats_topleft$center_x)
    expect_equal(stats_bottomleft$center_x_trim, stats_topleft$center_x_trim)
    expect_equal(stats_bottomleft$center_x_std, stats_topleft$center_x_std)

    expect_equal(stats_bottomleft$center_y + stats_topleft$center_y, size[2] + 1, tolerance = 1e-6)
    expect_equal(stats_bottomleft$center_y_trim + stats_topleft$center_y_trim, stats_bottomleft$height_trim + 1, tolerance = 1e-6)
    expect_equal(stats_bottomleft$center_y_std + stats_topleft$center_y_std, 1, tolerance = 1e-6)
    }
  })

test_that("cog_contour matches expected statistics", {
  expected <- testthat::test_path("expected", "cog_contour.csv") %>%
    read.csv()

  stats <- testthat::test_path("images", expected$filename) %>%
    lapply(function(x){cog_contour(x)$statistics}) %>%
    bind_rows()

  expect_equal(unlist(stats), unlist(expected[,-1]), tolerance = 1e-6)

})
