packed_ids <- c("c(1, 3)", "c(1:4)")
packed_data <- tibble(cat_ids = packed_ids)

test_that("unpack_ids (character)", {

  expect_identical(
    unpack_ids(packed_ids),
    c(1L, 3L, 1L, 2L, 3L, 4L))

})

test_that("unpack_ids (integer)", {

  x <- c(1L, 3L, 1L, 2L, 3L, 4L)
  expect_identical(
    unpack_ids(x),
    x)

})

test_that("data.frame (defaults)", {

  unpacked_data <-
    packed_data %>%
    unpack_ids()

  expect_true("cat_ids" %in% names(packed_data))
  expect_false("cat_ids" %in% names(unpacked_data))
  expect_true("cat_id" %in% names(unpacked_data))

  unpacked_data %>%
    pull(
      cat_id) %>%
    expect_identical(
      c(1L, 3L, 1L, 2L, 3L, 4L))

})

test_that("data.frame (plural_to_singular = FALSE)", {

  unpacked_data <-
    packed_data %>%
    unpack_ids(
      plural_to_singular = FALSE)

  expect_true("cat_ids" %in% names(packed_data))
  expect_true("cat_ids" %in% names(unpacked_data))

  unpacked_data %>%
    pull(
      cat_ids) %>%
    expect_identical(
      c(1L, 3L, 1L, 2L, 3L, 4L))

})
