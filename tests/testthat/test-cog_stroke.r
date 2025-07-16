test_that("cog_stroke given an image file returns correct structure", {
  inputs_dir <- testthat::test_path("images")
  inputs_files <- list.files(inputs_dir, full.names = TRUE)

  for(i in 1:length(inputs_files)){
    img <- imager::load.image(inputs_files[i])

    result <- cog_stroke(img)

    expect_type(result, "list")
    expect_true("statistics" %in% names(result))
    expect_true("strokes" %in% names(result))

    stats <- result$statistics
    expect_true(all(c("center_x", "center_y") %in% names(stats)))
    expect_true(is.numeric(stats$center_x))
    expect_true(is.numeric(stats$center_y))
  }
})

test_that("cog_stroke given a path to an image file returns correct structure", {
  inputs_dir <- testthat::test_path("images")
  inputs_files <- list.files(inputs_dir, full.names = TRUE)

  for(i in 1:length(inputs_files)){
    img <- inputs_files[i]

    result <- cog_stroke(img)

    expect_type(result, "list")
    expect_true("statistics" %in% names(result))
    expect_true("strokes" %in% names(result))

    stats <- result$statistics
    expect_true(all(c("center_x", "center_y") %in% names(stats)))
    expect_true(is.numeric(stats$center_x))
    expect_true(is.numeric(stats$center_y))
  }
})

test_that("cog_stroke with origin specified returns correctlly adjusted values", {
  inputs_dir <- testthat::test_path("images")
  inputs_files <- list.files(inputs_dir, full.names = TRUE)

  for(i in 1:length(inputs_files)){
    img <- imager::load.image(inputs_files[i])

    size <- dim(img)[1:2]

    stats_none <- cog_stroke(img)$statistics
    stats_bottomleft <- cog_stroke(img, origin = "bottomleft")$statistics
    stats_topleft <- cog_stroke(img, origin = "topleft")$statistics

    expect_equal(stats_none, stats_bottomleft, tolerance = 1e-6)

    expect_equal(stats_bottomleft$center_x, stats_topleft$center_x)
    expect_equal(stats_bottomleft$center_x_trim, stats_topleft$center_x_trim)
    expect_equal(stats_bottomleft$center_x_std, stats_topleft$center_x_std)

    expect_equal(stats_bottomleft$center_y + stats_topleft$center_y, size[2] + 1, tolerance = 1e-6)
    expect_equal(stats_bottomleft$center_y_trim + stats_topleft$center_y_trim, stats_bottomleft$height_trim + 1, tolerance = 1e-6)
    expect_equal(stats_bottomleft$center_y_std + stats_topleft$center_y_std, 1, tolerance = 1e-6)
    }
  })

test_that("cog_stroke matches expected statistics", {
  expected <- testthat::test_path("expected", "cog_stroke.csv") %>%
    read.csv()

  for(i in 1:nrow(expected)){
    img <- testthat::test_path("images", expected$filename[i]) %>%
      imager::load.image()

    stats <- cog_stroke(img)$statistics
    expect_equal(unlist(stats), unlist(expected[i,-1]), tolerance = 1e-6)

  }
})

# inputs_dir <- testthat::test_path("images")
# inputs_files <- list.files(inputs_dir, full.names = TRUE)
# inputs_filenames <- list.files(inputs_dir, full.names = FALSE)
#
# lapply(inputs_files, function(x){cog_stroke(x)$statistics}) %>%
#   bind_rows() %>%
#   mutate(
#     filename = inputs_filenames, .before = "center_x"
#   ) %>% clipr::write_clip()
#
