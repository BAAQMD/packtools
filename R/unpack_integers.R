#' Unpack a vector of "packed integers" (like facility IDs or category IDs)
#'
#' @examples
#' packed_integers <- c("c(1, 3)", "c(1:4)")
#' unpack_integers(packed_integers)
#'
#' packed_data <- tibble(cat_ids = c("c(1, 3)", "c(1:4)"))
#' unpack_integers(packed_data, var_name = "cat_ids")
#' unpack_integers(packed_data)
#'
#' @export
unpack_integers <- function (x, ...) {
  UseMethod("unpack_integers", x)
}

#' @method unpack_integers integer
unpack_integers.integer <- function (x, ...) {
  return(x)
}

#' @method unpack_integers default
#' @note coerce to character, then unpack
unpack_integers.default <- function (x, ...) {

  unpacked_integers <-
    unpack_integers.character(
      as.character(x),
      ...)

  return(unpacked_integers)

}

#' @method unpack_integers character
#' @note thin wrapper around `strtools::parse_integers()`
unpack_integers.character <- function (
  x,
  ...,
  simplify = TRUE
) {

  unpacked_integers <-
    strtools::parse_integers(
      x,
      ...)

  if (isTRUE(simplify)) {
    unpacked_integers <-
      unlist(unpacked_integers)
  }

  return(unpacked_integers)

}

#' @method unpack_integers data.frame
unpack_integers.data.frame <- function (
  x,
  var_name,
  verbose = getOption("verbose")
)  {

  msg <- function (...) if(isTRUE(verbose)) message("[unpack_integers] ", ...)
  msg("var_name is: ", var_name)

  parsed_data <-
    mutate_at(
      x,
      vars(var_name),
      ~ unpack_integers(., simplify = FALSE))

  unpacked_data <-
    unnest(
      parsed_data,
      cols = c(var_name))

  return(unpacked_data)

}
