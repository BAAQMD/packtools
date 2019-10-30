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
  x_list <- as.list(x)
  arg_list <- append(x_list, list(...))
  unpacked_args <- unpack_list(arg_list)
  return(unpacked_args)
}
