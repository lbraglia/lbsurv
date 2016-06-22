#' xtable method for summary.coxph
#'
#' xtable method for summary.coxph
#' @param x a \code{summary.coxph} object
#' @param digits digits
#' @param print.coef print beta?
#' @param print.se.coef print se(beta)?
#' @param print.z print z?
#' @param print.p print p?
#' @param print.exp.coef print exp(beta)
#' @param print.exp.coef.lci print confint(beta) low
#' @param print.exp.coef.uci print confint(beta) low
#' @param ... other things passed to \code{xtable::xtable}
#' 
#' @export
xtable.summary.coxph <- function(x,
                                 digits = getOption("digits"),
                                 print.coef = TRUE,
                                 print.se.coef = FALSE,
                                 print.z = FALSE,
                                 print.p = TRUE,
                                 print.exp.coef = TRUE,
                                 print.exp.coef.lci = TRUE,
                                 print.exp.coef.uci = TRUE,
                                 ...)                         
{

    savedig <- options(digits = digits)
    on.exit(options(savedig))

    coeffs <- x$coefficients
    ci <- x$conf.int
    results <- cbind(coeffs, ci)

    column_select <- c(print.coef,
                       FALSE,             #exp(coef)
                       print.se.coef,
                       print.z,
                       print.p,
                       print.exp.coef,
                       FALSE,             #exp(-coef)
                       print.exp.coef.lci,
                       print.exp.coef.uci)
  
    final_result <- results[ , column_select]

    xtable::xtable(final_result, digits = digits, ...)
}
