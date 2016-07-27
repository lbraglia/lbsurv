#' xtable method for summary.coxph
#'
#' xtable method for summary.coxph
#' @param x a \code{summary.coxph} object
#' @param digits digits
#' @param print.coef print beta?
#' @param print.se.coef print se(beta)?
#' @param print.z print z?
#' @param print.exp.coef print HR
#' @param print.exp.coef.lci print HR lower confint
#' @param print.exp.coef.uci print HR upper confint
#' @param print.p print p?
#' @param ... other things passed to \code{xtable::xtable}
#' 
#' @export
xtable.summary.coxph <- function(x,
                                 digits = 3,
                                 print.coef = FALSE,
                                 print.se.coef = FALSE,
                                 print.z = FALSE,
                                 print.exp.coef = TRUE,
                                 print.exp.coef.lci = TRUE,
                                 print.exp.coef.uci = TRUE,
                                 print.p = TRUE,
                                 ...)                         
{

    savedig <- options(digits = digits)
    on.exit(options(savedig))

    coeffs <- x$coefficients
    ci <- x$conf.int
    results <- as.data.frame(cbind(coeffs, ci))

    column_select <- c('coef',
                       'se(coef)',
                       'z',
                       'exp(coef)',
                       'lower .95',
                       'upper .95',
                       'Pr(>|z|)')[
        c(print.coef,
          print.se.coef,
          print.z,
          print.exp.coef,
          print.exp.coef.lci,
          print.exp.coef.uci,
          print.p)
    ]
  
    final_result <- results[ , column_select]
    names(final_result)[names(final_result) %in% 'exp(coef)'] <- 'HR'

    final_result$`Pr(>|z|)` <- lbmisc::pretty_pval(
        final_result$`Pr(>|z|)`,
        equal = FALSE
    )
    
    xt <- xtable::xtable(final_result,
                         digits = digits,
                         ...)
    xtable::align(xt) <- rep('r', ncol(final_result) + 1)
    xt
}
