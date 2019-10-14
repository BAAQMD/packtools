context("unpack_integers")

packed_integers <- c("c(1, 3)", "c(1:4)")
packed_data <- tibble(cat_ids = packed_integers)

test_that("unpack_integers.default", {

  expect_identical(
    unpack_integers.default(packed_integers),
    c(1L, 3L, 1L, 2L, 3L, 4L))

})

test_that("unpack_integers.character", {

  expect_identical(
    unpack_integers.character(packed_integers),
    c(1L, 3L, 1L, 2L, 3L, 4L))

  expect_identical(
    unpack_integers(packed_integers),
    c(1L, 3L, 1L, 2L, 3L, 4L))

})

test_that("unpack_integers.integer", {

  expect_equal(
    unpack_integers.integer(c(1L, 3L, 1L, 2L, 3L, 4L)),
    c(1L, 3L, 1L, 2L, 3L, 4L))

})

test_that("data.frame with cat_ids", {

  packed_data %>%
    unpack_integers(
      var_name = "cat_ids") %>%
    pull(
      cat_ids) %>%
    expect_identical(
      c(1L, 3L, 1L, 2L, 3L, 4L))

})
