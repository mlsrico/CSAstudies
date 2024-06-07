
#' Cross Tab with Missing Data
#'
#' @description
#' A table function that checks the number of missing data whatever the class of the variable.
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

