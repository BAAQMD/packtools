#' unpack_ids
#'
#' Unpack a vector of "packed IDs" (like facility IDs or category IDs)
#'
#' @examples
#' packed_ids <- c("c(1, 3)", "c(1:4)")
#' unpack_ids(packed_ids)
#'
#' packed_data <- tibble(cat_ids = c("c(1, 3)", "c(1:4)"))
#' unpack_ids(packed_data, id_var = "cat_ids")
#' unpack_ids(packed_data)
#'
#' @export
unpack_ids <- function (x, ...) {
  UseMethod("unpack_ids", x)
}

#' unpack_ids
#'
#' @note presumes that IDs are integers
#' @method unpack_ids default
#' @export
unpack_ids.default <- function (x, ...) {
  unpacked_ids <- unpack_integers(x, ...)
  return(unpacked_ids)
}

#' unpack_ids
#'
#' @method unpack_ids data.frame
#' @export
unpack_ids.data.frame <- function (
  x,
  id_var = NULL,
  plural_to_singular = TRUE,
  verbose = FALSE
)  {

  if (is.null(id_var)) {
    id_var <-
      vartools::find_var(
        x,
        suffix = "_id(s?)")
  }

  unpacked_data <-
    unpack_integers.data.frame(
      x,
      var_name = id_var,
      verbose = verbose)

  if (isTRUE(plural_to_singular)) {
    unpacked_data <-
      unpacked_data %>%
      rename_at(
        vars(id_var),
        ~ stringr::str_remove(., "s$"))
  }

  return(unpacked_data)

}
