#' random id
#'
#' @param n number of ids
#' @param length length of id
#' @seealso pulled from highcharter non exports
#' @export
#' @examples
#' random_id(n = 1, length = 10)


random_id <- function (n = 1, length = 10)
{
  #
  source <- c(seq(0, 9), letters)
  replicate(n, paste0(sample(x = source, size = length, replace = TRUE),
                      collapse = ""))
}
