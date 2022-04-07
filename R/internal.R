#' @title Extract part of string
#' @param string string
#'
#' @param pattern pattern to find
#' @param which position
#' @param num_char number of characters
#'
#' @keywords internal
#' @export
str_extract_after <- function (string, pattern, which = "first", num_char = NULL)
{
  position_of_patterns <- stringr::str_locate_all(string = string,
    pattern = pattern)
  if (which == "first") {
    end_of_pattern <- purrr::map_int(position_of_patterns,
      function(position) {
        position[, "end"][1]
      })
  }
  else if (which == "last") {
    end_of_pattern <- purrr::map_int(position_of_patterns,
      function(position) {
        position[, "end"][length(position[, "end"])]
      })
  }
  else (stop("which must be one of c('first', 'last')"))
  string_extract <- stringr::str_sub(string = string, start = (end_of_pattern +
    1), end = if (is.null(num_char)) {
    nchar(string)
  }
  else {
    (end_of_pattern + num_char)
  })
  return(string_extract)
}
