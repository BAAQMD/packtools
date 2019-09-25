#' Unpack a vector of "packed integers" (like facility IDs or category IDs)
#'
#' @examples
#' input_data <- data_frame(cat_ids = c("c(1, 3)", "c(1:4)"))
#' unpack_integers(input_data, var_name = "cat_ids")
#' unpack_integers(input_data)
#'
#' @export
unpack_integers <- function (
  input_data,
  var_name,
  verbose = getOption("verbose")
)  {

  msg <- function (...) if(isTRUE(verbose)) message("[unpack_integers] ", ...)

  msg("var_name is: ", var_name)

  parsed_data <-
    mutate_at(
      input_data,
      vars(var_name),
      funs(parse_integers))

  unpacked_data <-
    unnest(
      parsed_data,
      cols = c(var_name))

  return(unpacked_data)

}
