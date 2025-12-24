test_that("draw_stroke returns valid raster object with origin = bottomleft", {
  inputs_dir <- testthat::test_path("images")
  inputs_files <- list.files(inputs_dir, full.names = TRUE)

  for (i in seq_along(inputs_files)) {
    # Use file path (no imager dependency)
    img <- inputs_files[i]
    result <- cog_stroke(img, origin = "bottomleft")
    out <- draw_stroke(result, plot_image = FALSE, cimg = FALSE)

    # raster object (as.raster)
    expect_true(inherits(out, "raster"))

    # Verify dimensions: raster is a matrix of colors [h, w]
    stats <- result$statistics
    expect_equal(ncol(out), stats$width_original)
    expect_equal(nrow(out), stats$height_original)

    # Ensure pixels are only white/black/red (background/stroke/cog lines)
    vals <- unique(as.vector(out))
    expect_true(all(vals %in% c("#FFFFFF", "#000000", "#FF0000")))
  }
})

test_that("draw_stroke returns valid raster object with origin = topleft", {
  inputs_dir <- testthat::test_path("images")
  inputs_files <- list.files(inputs_dir, full.names = TRUE)

  for (i in seq_along(inputs_files)) {
    img <- inputs_files[i]
    result <- cog_stroke(img, origin = "topleft")
    out <- draw_stroke(result, plot_image = FALSE, cimg = FALSE)

    expect_true(inherits(out, "raster"))

    stats <- result$statistics
    expect_equal(ncol(out), stats$width_original)
    expect_equal(nrow(out), stats$height_original)

    vals <- unique(as.vector(out))
    expect_true(all(vals %in% c("#FFFFFF", "#000000", "#FF0000")))
  }
})

test_that("draw_stroke returns an error when given an input of unmatched type", {
  img <- testthat::test_path("images", "A_1.png")
  expect_error(cog_contour(img) |> draw_stroke())
  expect_error(cog_potential(img) |> draw_stroke())
})
