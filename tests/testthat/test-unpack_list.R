context("test-unpack_list")

x <- list(foo = 1:3, bar = 5)
expected <- c(foo = 1, foo = 2, foo = 3, bar = 5)

expect_identical(
  unpack_list(x),
  expected)
