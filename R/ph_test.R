#' A wrapper for cox.zph which plot Schoenfeld residuals of variables
#' violating PH assumption
#'
#' A wrapper for cox.zph which plot Schoenfeld residuals of variables
#' violating PH assumption
#'
#' @param cox_mod a coxph object
#' @param plot_signif plot Schoenfeld residuals for variables violating
#'                    PH assumption?
#' @param mfrow optimized (optional) mfrow
#' @export
ph_test <- function(cox_mod, plot_signif = TRUE, mfrow = NULL){
    test <- survival::cox.zph(cox_mod)
    tab <- test$table
    ## individuo variabili significative e le plotto 
    signifs <- which(tab[-nrow(tab), 'p'] < 0.05)
    ## determino un quadro che plotti se non è stato specificato
    if (is.null(mfrow)){
        rows <- cols <- ceiling(sqrt(length(signifs)))
        mfrow <- c(rows, cols)
    }
    if (plot_signif) {
        if (length(signifs) > 0L) {
            par(mfrow = mfrow)
            plot(test[signifs])
            mtext('Variables violating PH assumption', 
                  cex = 1.5,
                  outer = TRUE, 
                  line = -2)
        }
    }
    test
}
