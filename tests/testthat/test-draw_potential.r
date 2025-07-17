test_that("draw_potential returns valid cimg object with origin = bottomleft", {
  inputs_files <- testthat::test_path("images", c("A_1.png", "A_2.png"))

  for(i in 1:length(inputs_files)){
    img <- imager::load.image(inputs_files[i])
    result<- cog_potential(img, origin = "bottomleft")
    out <- draw_potential(result, plot_image = FALSE)

    # Check that the output is a cimg object
    expect_s3_class(out, "cimg")

    # Check that the image dimensions match the original size
    stats <- result$statistics
    expect_equal(dim(out)[1], stats$width_original)   # width
    expect_equal(dim(out)[2], stats$height_original)  # height

    # Check that pixel values are within [0, 1] after normalization
    expect_true(all(out[] >= 0 & out[] <= 1))
  }

})

test_that("draw_potential returns valid cimg object with origin = topleft", {
  inputs_files <- testthat::test_path("images", c("A_1.png", "A_2.png"))

  for(i in 1:length(inputs_files)){
    img <- imager::load.image(inputs_files[i])
    result<- cog_potential(img, origin = "topleft")
    out <- draw_potential(result, plot_image = FALSE)

    # Check that the output is a cimg object
    expect_s3_class(out, "cimg")

    # Check that the image dimensions match the original size
    stats <- result$statistics
    expect_equal(dim(out)[1], stats$width_original)   # width
    expect_equal(dim(out)[2], stats$height_original)  # height

    # Check that pixel values are within [0, 1] after normalization
    expect_true(all(out[] >= 0 & out[] <= 1))
  }

})

test_that("draw_potential returns an error when given an input of unmatched type", {
  img <- testthat::test_path("images", c("A_1.png"))
  expect_error(cog_stroke(img) |> draw_potential())
  expect_error(cog_contour(img) |> draw_potential())
})
