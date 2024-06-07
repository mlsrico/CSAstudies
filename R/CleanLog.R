
#' Clean Results from a Logistic Regression Model
#'
#' @description
#' This function cleans the results of any logistic regression object, and returns them in format OR (95%CI; p-value).
#'
#' @param x A logistic regression object.
#'
#' @return A table with two columns: the label of each variable level and the result (95%CI; p-value). Variable levels that do not appear in this table are the reference group.
#' @export
#'
#' @examples
#' ### Crude regression model
#' m_crude <- glm(outcome ~ var1, data = dat, family = "binomial")
#' CleanLog(m_crude)
#' # rn                 logres
#' # 1 var1 1.12 (0.85 - 1.06; 0.097)
#'
#' ### Multivariable regression model
#' m_crude <- glm(outcome ~ var1 + var2 + var3, data = dat, family = "binomial")
#' CleanLog(m_crude)
#' # rn                    logres
#' # 1  var1  1.12 (0.85 - 1.06; 0.097)
#' # 2  var2  0.96 (0.61 - 1.5; 0.849)
#' # 3  var3  0.99 (0.97 - 1.02; 0.639)


CleanLog <- function(res){
  library("tidyverse")

 if("glm" %in% class(res)){

   # ci95
   ci95 <- exp(confint.default(res, level = 0.95))
   # get objects
   sres <- summary(res)
   dres <- sres$coefficients %>% as.data.frame()
   # get exp & pvalue
   exp_p <- dres %>%
     mutate(rn = row.names(dres),
            expcoef = round(exp(`Estimate`), 2),
            pval = round(`Pr(>|z|)`, 3)) %>%
     dplyr::select(rn, expcoef, pval) %>%
     filter(rn!="(Intercept)")
   # get 95%CI
   coif <- ci95 %>%
     as.data.frame() %>%
     mutate(rn = row.names(dres),
            ci_inf = round(`2.5 %`, 2),
            ci_sup = round(`97.5 %`, 2)) %>%
     mutate(ci_both = paste("(", ci_inf, " - ", ci_sup, sep = "")) %>%
     dplyr::select(rn, ci_both) %>%
     filter(rn!="(Intercept)")
   # combine
   all_coefs <- left_join(exp_p, coif, by = "rn") %>%
     mutate(logres = paste(expcoef, " ", ci_both, "; ", pval, ")", sep = "")) %>%
     dplyr::select(rn, logres)

   return(all_coefs)

 } else {

   stop("The model should be a logistic regression.")


 }

}
