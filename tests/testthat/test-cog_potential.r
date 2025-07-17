test_that("cog_potential given an image file returns correct structure", {
  inputs_files <- testthat::test_path("images", c("A_1.png", "A_2.png"))

  for(i in 1:length(inputs_files)){
    img <- imager::load.image(inputs_files[i])

    result <- cog_potential(img)

    expect_type(result, "list")
    expect_true("statistics" %in% names(result))
    expect_true("potentials" %in% names(result))
  }
})

test_that("cog_potential given a path to an image file returns correct structure", {
  inputs_files <- testthat::test_path("images", c("A_1.png", "A_2.png"))

  for(i in 1:length(inputs_files)){
    img <- inputs_files[i]

    result <- cog_potential(img)

    expect_type(result, "list")
    expect_true("statistics" %in% names(result))
    expect_true("potentials" %in% names(result))
  }
})

test_that("cog_potential with origin specified returns correctlly adjusted values", {
  inputs_files <- testthat::test_path("images", c("A_1.png", "A_2.png"))

  for(i in 1:length(inputs_files)){
    img <- imager::load.image(inputs_files[i])

    size <- dim(img)[1:2]

    stats_none <- cog_potential(img)$statistics
    stats_bottomleft <- cog_potential(img, origin = "bottomleft")$statistics
    stats_topleft <- cog_potential(img, origin = "topleft")$statistics

    expect_equal(stats_none, stats_bottomleft, tolerance = 1e-6)

    expect_equal(stats_bottomleft$center_x, stats_topleft$center_x)
    expect_equal(stats_bottomleft$center_x_trim, stats_topleft$center_x_trim)
    expect_equal(stats_bottomleft$center_x_std, stats_topleft$center_x_std)

    expect_equal(stats_bottomleft$center_y + stats_topleft$center_y, size[2] + 1, tolerance = 1e-6)
    expect_equal(stats_bottomleft$center_y_trim + stats_topleft$center_y_trim, stats_bottomleft$height_trim + 1, tolerance = 1e-6)
    expect_equal(stats_bottomleft$center_y_std + stats_topleft$center_y_std, 1, tolerance = 1e-6)
    }
  })

test_that("cog_potential matches expected statistics", {
  expected <- testthat::test_path("expected", "cog_potential.csv") %>%
    read.csv() %>%
    filter(filename %in% c("A_1.png", "A_2.png")) # Sampled to reduce time

  stats <- testthat::test_path("images", expected$filename) %>%
    lapply(function(x){cog_potential(x)$statistics}) %>%
    bind_rows()

  expect_equal(unlist(stats), unlist(expected[,-1]), tolerance = 1e-6)

})
