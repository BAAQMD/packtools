#' unpack_list
#'
#' @examples
#' x <- list(foo = 1:3, bar = 5)
#' unpack_list(x)
#'
#' @export
unpack_list <- function (x) {
  values <- unlist(x, use.names = FALSE)
  labels <- rep(names(x), times = lengths(x))
  setNames(values, labels)
}
