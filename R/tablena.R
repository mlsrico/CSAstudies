
#' A table with missing data.
#'
#' @param x A vector of any class.
#'
#' @return A table showing frequencies for every unique level in the variable, including missing data.
#' @export
#'
#' @examples
#' # Example without missing data
#' x <- c("a", "a", "a", "b", "b", "c", "d", "d")
#' tablena(x)
#' # x
#' # a    b    c    d <NA>
#' # 3    2    1    2    0
#'
#' # Example with missing data
#' x <- c("a", "a", "a", "b", NA, "c", NA, "d")
#' tablena(x)
#' # x
#' # a    b    c    d <NA>
#' # 3    1    1    1    2


tablena <- function(x){
  table(x, useNA = "always")
}

