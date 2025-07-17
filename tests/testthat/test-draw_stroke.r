test_that("draw_stroke returns valid cimg object with origin = bottomleft", {
  inputs_dir <- testthat::test_path("images")
  inputs_files <- list.files(inputs_dir, full.names = TRUE)

  for(i in 1:length(inputs_files)){
    img <- imager::load.image(inputs_files[i])
    result<- cog_stroke(img, origin = "bottomleft")
    out <- draw_stroke(result, plot_image = FALSE)

    # Check that the output is a cimg object (image object from imager)
    expect_s3_class(out, "cimg")

    # Verify the image dimensions match the expected values
    stats <- result$statistics
    expect_equal(dim(out)[1], stats$width_original)   # width
    expect_equal(dim(out)[2], stats$height_original)  # height

    # Ensure pixel values are binary (0 or 1)
    expect_true(all(out[] %in% c(0, 1)))
  }

})

test_that("draw_stroke returns valid cimg object with origin = topleft", {
  inputs_dir <- testthat::test_path("images")
  inputs_files <- list.files(inputs_dir, full.names = TRUE)

  for(i in 1:length(inputs_files)){
    img <- imager::load.image(inputs_files[i])
    result<- cog_stroke(img, origin = "topleft")
    out <- draw_stroke(result, plot_image = FALSE)

    # Check that the output is a cimg object (image object from imager)
    expect_s3_class(out, "cimg")

    # Verify the image dimensions match the expected values
    stats <- result$statistics
    expect_equal(dim(out)[1], stats$width_original)   # width
    expect_equal(dim(out)[2], stats$height_original)  # height

    # Ensure pixel values are binary (0 or 1)
    expect_true(all(out[] %in% c(0, 1)))
  }

})

test_that("draw_stroke returns an error when given an input of unmatched type", {
  img <- testthat::test_path("images", c("A_1.png"))
  expect_error(cog_contour(img) |> draw_stroke())
  expect_error(cog_potential(img) |> draw_stroke())
})
