#' Plot and compare cumulative/crude incidence in a competing risk
#' setting
#'
#' Plot and compare cumulative/crude incidence in a competing risk
#' setting
#' 
#' @param time survival time variable
#' @param status integer with 0 = censored or factor with
#' first level = censored
#' @param strata Stratifying variable (optional)
#' @param plot plot (default = TRUE) or only return estimates?
#' @param time_unit Time unit of x axis
#' @param time_by Time step x axis (in days) @param quantile_probs
# quantile calculated (default to 0.5, aka median) @param main Graph
# main title
#' @param ylab Y-axis label.
#' @param xlab X-axis label. If NULL a suitable default based on
#'     time_unit will be provided
#' @param xlim X-axis limit. If NULL a suitable default based on
#'     time_unit will be provided
#' @param ylim Y-axis limit. Default to c(0,1) be provided @param
# reverse plot cumulative events @param mark_censored mark censored
# observation @param pch_censored pch used for censored observation
# @param conf_int character. Can be 'default', 'none' or 'lines' or
# 'shades' @param conf_int_alpha a base level for alpha if
# \code{conf_int = 'alpha'} (splitted by number of groups)
#' @param test tests: 'none' = don't plot tests, 'gray' = gray test
#' @param cex_test cex parameter for test string
#' @param plot_grid plot grid @param plot_n_at_risk Logical value:
# plot number at risk?  @param cex_n_at_risk cex of number at risk
#' @param legend_cmd Graph command to add legend, as string
#' @param ... Further \code{\link{lines.survfit}} parameters
#' @examples
#' \dontrun{
#' ## Gray test validation
#' library(lbdatasets)
#' (ci.fns <- with(mros, cmprsk::cuminc(time, status, cutbmd))$Tests)
#' ## example
#' res <- with(mros, comprisk(time = time, status = as.integer(status),
#'                            strata = cutbmd))
#' res$gray_test
#' }
#' 
#' @export
comprisk <- function(time = NULL, status = NULL,
                     ## NULL = non stratified, otherwise stratifying variable
                     strata = NULL,
                     ## plot anything,
                     plot = TRUE,
                     ## Time unit x axis
                     time_unit = c('days', 'weeks', 'months', 'years'),
                     ## Time step x axis (in days)
                     time_by = NULL,
                     ## ## quantiles calculated
                     ## quantile_probs = 0.5,
                     ## ## Main title
                     ## main = '',
                     ## Y axis label
                     ylab = NULL,
                     ## X axis nbel
                     xlab = NULL,
                     ## Y axis limits
                     ylim = c(0L, 1L),
                     ## X axis limits
                     xlim = NULL,
                     ## ## plot cumulative events?
                     ## reverse = FALSE,
                     ## ## mark censored observation
                     ## mark_censored = TRUE,
                     ## ## pch used for censored observation
                     ## pch_censored = "'",
                     ## ## Plot Confidence interval
                     ## conf_int = c('default', 'none', 'lines', 'shades'),
                     ## conf_int_alpha = 0.8,
                     ## Test: none = don't plot tests, logr = logranktest,
                     ##       hr = hazratio, both = both
                     test = c('none', 'gray'),
                     cex_test = par("cex") * 0.8,
                     ## plot grid
                     plot_grid = TRUE,
                     ## ## Plot number ad risk in the km
                     ## plot_n_at_risk = TRUE,
                     ## cex_n_at_risk = par("cex") * 0.8,
                     ## Graph command to add legend, as string
                     legend_cmd = NULL,
                     ## Further lines.survfit params
                     ...
                     )
{
    
    if (! (is.integer(status) || is.factor(status)))
        stop("'status' must be an integer (0=censored) or a factor(first.")
    if (is.null(time)) stop("'time' needed.")
    ## normalize not factor strata
    if (!is.null(strata) && !is.factor(strata))
        strata <- factor(strata)
    ## time_by: NULL or numeric
    if(! (is.null(time_by) || is.numeric(time_by)))
        stop("'time_by' must be NULL or numeric")
    ## xlab, ylab: NULL or character
    if (! (is.null(ylab) || is.character(ylab)))
        stop('ylab must be NULL or character')
    if (! (is.null(xlab) || is.character(xlab)))
        stop('xlab must be NULL or character')
    ## time_unit, main, test: character
    if (! is.character(time_unit)) stop('time_unit must be character')
    ## xlim: NULL or numeric (of length 2)
    if (! (is.null(xlim) || (is.numeric(xlim) && (2 == length(xlim)))))
        stop("'xlim' must be NULL or numeric vector of 2 elements")
    ## ylim: NULL or numeric (of length 2)
    if (! (is.null(ylim) || (is.numeric(ylim) && (2 == length(ylim)))))
        stop("'ylim' must be NULL or numeric vector of 2 elements")
    
    ## Argument matching, ... 'handling'
    time_unit <- match.arg(time_unit)
    test <- match.arg(test)
    dots <- list(...)

    ## ## Coerce status to a factor for uniformity
    # browser()
    status   <- factor(status)
    n_status <- nlevels(status)
    status_lab <- levels(status)
    
    ## Default xlab and ylab if NULL is provided
    if (is.null(xlab))
        xlab <- lbmisc::upcase(time_unit, method = 'sentence')
    if (is.null(ylab))
        ylab <- 'Probability'
    ## Default time_by if NULL is provided
    if (is.null(time_by))
        time_by <- default_time_by(time_unit)
    ## time_divisor
    time_divisor <- time_unit_to_time_divisor(time_unit)
    
    ## Check if it's a univariate or stratified plot; setup the
    ## estimates dataset and parameter defaults accordingly
    if (is.null(strata)) {
        db <- data.frame(time = time, status = status)
        db <- lbmisc::NA_remove(db)
        mod_formula <- survival::Surv(time = time, event = status) ~ 1
        univariate <- TRUE
        n_stratas <- 1
        strata_labels <- 'All '
        ## if ('default' %in% conf_int) conf_int <- 'lines'
    } else {
        db <- data.frame(time = time, status = status, strata = strata)
        db <- lbmisc::NA_remove(db)
        mod_formula <- survival::Surv(time = time, event = status) ~ strata
        univariate <- FALSE
        n_stratas <- nlevels(strata)
        strata_labels <- levels(strata)
        ## if ('default' %in% conf_int) conf_int <- 'none'
        ## if ((n_stratas != 2) & (test == 'hr')) {
        ##     warning(paste0('HR can be plotted only with 2 groups. ',
        ##                    'Changing to Log-rank tests'))
        ##     test <- 'logr'
        ## }
    }

    ## ---------
    ## Estimates
    ## ---------
    
    fit <- survival::survfit(formula = mod_formula, data = db)
    sfit <- summary(fit)

    ## times for plot ticks, n at risk, summaries and so on
    ## x has to be based on time_by, if specified
    ## if not specified make 4 step
    times <- if (is.null(time_by)) seq(0, max(fit$time), length = 4)
             else seq(0, max(fit$time), by = time_by * time_divisor)

    ## Summary estimates for presentation purposes
    est_times <- seq(from = 0L, to = max(times), by = time_divisor)
    sfit2 <- summary(fit, times = est_times)
#    browser()
    prob_colnames <- sprintf("Pr(%s)", sfit2$states)
    prob_colnames[length(prob_colnames)] <- "Pr(Event Free)"
    probs <- setNames(data.frame(sfit2$pstate), prob_colnames)
    ret_sfit <- if (univariate)
                    cbind(data.frame('time' = sfit2$time / time_divisor),
                          probs)
                else 
                    cbind(data.frame('time' = sfit2$time / time_divisor,
                                     'group' = gsub('strata=', '', sfit2$strata)),
                          probs)
    names(ret_sfit)[1] <- time_unit

    ## Tests
    if( !univariate ) {
        ## Gray test
        gray <- data.frame(with(db, cmprsk::cuminc(ftime = time,
                                        ## better integer with 0 as censored
                                        fstatus = as.integer(status) - 1L,
                                        group = strata)$Tests[, c(1,3,2)]))
        
        rownames(gray) <- status_lab[-1]
        
        ## gray_strings <- sprintf('Log-rank Test=%.2f, df=%d, p%s',	
        ##                         gray$chisq, 
        ##                         gray$df, 
        ##                         lbmisc::pretty_pval(gray$p, equal = TRUE))
        
        ## ## Cox Model (and his summary)
        ## cox <- survival::coxph(mod_formula, data = db)
        ## scox <- summary(cox)
        ## scox_coefs <- stats::coefficients(scox)
        ## hr_string  <- sprintf('HR=%.2f (95%% CI, %.2f-%.2f, p%s)',
        ##                       scox_coefs[2],
        ##                       scox$conf.int[3],
        ##                       scox$conf.int[4],
        ##                       lbmisc::pretty_pval(scox_coefs[5], equal = TRUE))
        ## both_string <- paste(logr_string, hr_string, sep = ' - ')
	
        ## ## Choose which stat to print in the graph
        ## test_string <- switch(test,
        ##                       logr = logr_string,
        ##                       hr = hr_string,
        ##                       both= both_string)

        ## test_string <- gray_string
    }
    
    ## return Stats
    if (univariate)
        invisible(list('cuminc' = fit, 'estimates' = ret_sfit))
    else
        invisible(list('cuminc' = fit, 'estimates' = ret_sfit, 'gray_test' = gray))
                           
}

time_unit_to_time_divisor <- function(tu){ # given time unit
        (tu %in% 'days'   *   1)    + 
        (tu %in% 'weeks'  *   7)    + 
        (tu %in% 'months' *  30.43) + 
        (tu %in% 'years'  * 365.25)
}


default_time_by <- function(tu){ # given time unit
    (tu %in% 'days'   *  30)  + 
    (tu %in% 'weeks'  *   4)  + 
    (tu %in% 'months' *  12)  + 
    (tu %in% 'years'  *   1)
}
