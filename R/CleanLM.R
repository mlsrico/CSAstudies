#' Clean Results from a Linear Regression Model
#'
#' @description
#' This function cleans the results of any linear regression object, and returns them in format `B (95%CI; p-value)`.
#'
#' @param res A linear regression object.
#'
#' @return A table with two columns: the label of each variable level and the result `B (95%CI; p-value)`. Variable levels that do not appear in this table are the reference group.
#' @export
#'
#' @examples
#' ### Crude regression model
#' m_crude <- lm(y ~ var1, data = dat)
#' CleanLog(m_crude)
#' # rn                 logres
#' # 1 var1 1.12 (0.85 - 1.06; 0.097)
#'
#' ### Multivariable regression model
#' m_multi <- lm(y ~ var1 + var2 + var3, data = dat)
#' CleanLM(m_multi)
#' # rn                    logres
#' # 1  var1  1.12 (0.85 - 1.06; 0.097)
#' # 2  var2  -0.96 (-0.61 - 1.5; 0.849)
#' # 3  var3  0.99 (0.97 - 1.02; 0.639)


CleanLM <- function(res){
  library("tidyverse")

  if("lm" %in% class(res)){

    ci95 <- confint.default(res, level = 0.95)
    sres <- summary(res)
    dres <- sres$coefficients %>% as.data.frame()
    exp_p <- dres %>%
      mutate(rn = row.names(dres),
             expcoef = round(Estimate,
                             2), pval = round(`Pr(>|t|)`, 3)) %>%
      dplyr::select(rn, expcoef, pval) %>% filter(rn != "(Intercept)")

    coif <- ci95 %>%
      as.data.frame() %>%
      mutate(rn = row.names(dres),
             ci_inf = round(`2.5 %`, 2),
             ci_sup = round(`97.5 %`, 2)) %>%
      mutate(ci_both = paste("(", ci_inf," - ", ci_sup, sep = "")) %>%
      dplyr::select(rn, ci_both) %>%
      filter(rn != "(Intercept)")


    all_coefs <- left_join(exp_p, coif, by = "rn") %>%
      mutate(logres = paste(expcoef, " ", ci_both, "; ", pval, ")", sep = "")) %>%
      dplyr::select(rn, logres)


    return(all_coefs)

  } else {

    stop("The model should be a linear regression.")


  }

}







