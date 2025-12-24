test_that("draw_potential returns valid raster object with origin = bottomleft", {
  inputs_files <- testthat::test_path("images", c("A_1.png", "A_2.png"))

  for (i in seq_along(inputs_files)) {
    img <- inputs_files[i]                 # use path (no imager)
    result <- cog_potential(img, origin = "bottomleft")
    out <- draw_potential(result, plot_image = FALSE, cimg = FALSE)

    # Check that the output is a raster (as.raster)
    expect_true(inherits(out, "raster"))

    # Check that the image dimensions match the original size
    stats <- result$statistics
    expect_equal(ncol(out), stats$width_original)   # width
    expect_equal(nrow(out), stats$height_original)  # height

    # Check that pixel values are valid hex colors (#RRGGBB)
    vals <- unique(as.vector(out))
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", vals)))

    # show_cog=TRUE by default -> should contain red crosshair
    expect_true("#FF0000" %in% vals)
  }
})

test_that("draw_potential returns valid raster object with origin = topleft", {
  inputs_files <- testthat::test_path("images", c("A_1.png", "A_2.png"))

  for (i in seq_along(inputs_files)) {
    img <- inputs_files[i]
    result <- cog_potential(img, origin = "topleft")
    out <- draw_potential(result, plot_image = FALSE, cimg = FALSE)

    expect_true(inherits(out, "raster"))

    stats <- result$statistics
    expect_equal(ncol(out), stats$width_original)
    expect_equal(nrow(out), stats$height_original)

    vals <- unique(as.vector(out))
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", vals)))
    expect_true("#FF0000" %in% vals)
  }
})

test_that("draw_potential returns an error when given an input of unmatched type", {
  img <- testthat::test_path("images", "A_1.png")
  expect_error(cog_stroke(img) |> draw_potential())
  expect_error(cog_contour(img) |> draw_potential())
})
