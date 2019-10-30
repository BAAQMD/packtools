test_fun <- function (x, ...) {
  unpacked_args <- unpack_args(x, ...)
  return(unpacked_args)
}

numbers <- 1:3
letters <- c("a", "b", "c")

fruits <- c(
  apple = "Golden Delicious",
  apple = "Red Delicious",
  orange = "Navel")

animals <- list(
  mammal = c("dog", "cat"),
  reptile = "snake")

test_that("unnamed integer", {
  expect_equal(
    test_fun(numbers),
    numbers)
})

test_that("unnamed character", {
  expect_equal(
    test_fun(letters),
    letters)
})

test_that("named character", {
  expect_equal(
    test_fun(fruits),
    fruits)
})

test_that("named list", {
  expect_equal(
    test_fun(as.list(fruits)),
    fruits)
})

test_that("packed list", {
  expect_equal(
    test_fun(animals),
    unpack_list(animals))
})

test_that("named character and named args", {
  expect_equal(
    test_fun(fruits, orange = "Valencia"),
    c(fruits, orange = "Valencia"))
})

test_that("named list and named args", {
  expect_equal(
    test_fun(as.list(fruits), orange = "Valencia"),
    c(fruits, orange = "Valencia"))
})

test_that("packed list and named args", {
  expect_equal(
    test_fun(animals, reptile = "frog"),
    c(unpack_list(animals), reptile = "frog"))
})
