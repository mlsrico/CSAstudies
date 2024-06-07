
#' Get number of events by group
#'
#' @description
#' A short description...
#'
#'
#' @param gr_var Single character vector with the name of the grouping variable in the data (e.g. treatment). It can be any class, but needs exactly 2 levels.
#' @param status_var Single character vector with the name of the outcome variable in the data (e.g. mortality). It has to be logical (i.e., TRUE / FALSE).
#' @param df Either a `data.frame` or a `tibble` containing both variables.
#' @param exposed_level Name of the exposed group in `gr_var` (e.g., with the treatment). Only required if the `gr_var` is a character vector or a factor.
#'
#' @return A small data.frame with the number of events by number of patients on each group (events / N (%)).
#' @export
#'
#' @examples
#' ### Data
#' test <- data.frame(
#'   id = c(1:10),
#'   death = c(F, T, T, F, T, T, F, F, T, F),
#'   tt = c("a", "b", "b", "a", "a", "a", "b", "b", "b", "a")
#' )
#'
#' ### Example
#' FreqOutcome(gr_var = "tt", status_var = "death", df = test, exposed_level = "a")
#' #               events / N (%)
#' # 1     exposed    3 / 5 (60%)
#' # 2 not exposed    2 / 5 (40%)


FreqOutcome <- function(gr_var, status_var, df, exposed_level = NULL){
  library("tidyverse")

  colnames(df)[which(colnames(df)==gr_var)] <- "gr_var"
  colnames(df)[which(colnames(df)==status_var)] <- "status_var"

  ## Exclusion criteria --------------
  #### outcome
  if(class(df$status_var)!="logical") stop("The outcome (status_var) has to be logical (TRUE/FALSE).")
  #### gr_var - only two levels
  if((df %>% pull(gr_var) %>% unique %>% length)!= 2) stop("The grouping variable (gr_var) has to have exactly 2 levels (e.g., TRUE, FALSE).")

  ## Modify data to control the first level
  if(class(df$gr_var)%in%c("character", "factor")) {

    if(is.null(exposed_level)) stop("Please add the exposed level of the grouping variable. Alternatively, you can use a logical variable (TRUE/FALSE).")
    df <- df %>% mutate(gr_var = as.factor(as.character(gr_var))) %>%
      mutate(gr_var = fct_relevel(gr_var, exposed_level))
  }

  ## Table --------------------------
  tb1 <- table(df$gr_var)
  tb2 <- table(df$gr_var, df$status_var)

  n_exposed <- tb1[2]
  n_ctrl <- tb1[1]

  o_exposed <- tb2[4]
  o_ctrl <- tb2[3]

  p_exposed <- paste(round((o_exposed/n_exposed)*100, 1), "%", sep = "")
  p_ctrl <- paste(round((o_ctrl/n_ctrl)*100, 1), "%", sep = "")


  res_exposed <- paste(o_exposed, " / ", n_exposed, " (", p_exposed, ")", sep = "")
  res_ctrl <- paste(o_ctrl, " / ", n_ctrl, " (", p_ctrl, ")", sep = "")

  resdf <- data.frame(
    var = c("exposed", "not exposed"),
    res = c(res_exposed, res_ctrl))

  colnames(resdf) <- c("", "events / N (%)")

  return(resdf)

}

