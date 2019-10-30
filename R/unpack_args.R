#' unpack_args
#'
#' Use `unpack_list` to unpack multiple *named* arguments
#'
#' @param x a named character vector or list
#' @param ... (optional) more named arguments
#' @return a named vector
#'
#' @export
unpack_args <- function (x, ...) {
  if (missing(x)) {
    arg_list <- list(...)
  } else {
    arg_list <- append(as.list(x), list(...))
  }
  unpacked_args <- unpack_list(arg_list)
  return(unpacked_args)
}
