#' Plot and compare cumulative/crude incidence in a competing risk
#' setting
#'
#' Plot and compare cumulative/crude incidence in a competing risk
#' setting
#' 
#' @param time survival time variable
#' @param status integer with 0 = censored or factor with first level
#'     = censored
#' @param strata Stratifying variable (optional)
#' @param time_unit Time unit of x axis
#' @param time_by Time step x axis (in days) @param quantile_probs
#' @param plot plot (default = TRUE) or only return estimates?
# quantile calculated (default to 0.5, aka median) @param main Graph
# main title
#' @param plot_ci plot confidence intervalse for estimates (for
#'     univariate unstacked and multivariate plots)
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
#' @param col_status color for exit status (mainly for univariate
#'     plotting)
#' @param col_strata color for group status (mainly for bivariate
#'     plotting)
#' @param cex_test cex parameter for test string
#' @param cex_legend cex parameter for legend string
#' @param plot_grid plot grid @param plot_n_at_risk Logical value:
# @param legend_cmd Graph command to add legend, as string @param
# ... Further \code{\link{lines.survfit}} parameters
#' @examples
#' \dontrun{
#'
#' library(lbdatasets)
#'
#' par(mfrow = c(1,2))
#' res <- comprisk(time = mros$time,
#'                 status = factor(mros$status, levels = 0:2,
#'                                 labels = c("Censored", "Fractured",
#'                                            "Death")),
#'                 strata = factor(mros$cutbmd, levels = 1:2,
#'                                 labels = c('LowBMD', 'HighBMD')),
#'                 time_by = 50, ylim = c(0, 0.25),
#'                 col_strata = c('red', 'black'), xlab = 'Weeks')
#'
#' @export
comprisk <- function(time = NULL, status = NULL,
                     ## NULL = non stratified, otherwise stratifying variable
                     strata = NULL,
                     ## Time unit x axis
                     time_unit = c('days', 'weeks', 'months', 'years'),
                     ## Time step x axis (in days)
                     time_by = NULL,
                     ## plot anything,
                     plot = TRUE,
                     ## plot ci in univariate unstacked and multivariate plots
                     plot_ci = FALSE,
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
                     test = c('gray', 'none'),
                     ## color for status levels
                     col_status = seq_len(nlevels(status) - 1),
                     col_strata = seq_len(nlevels(strata)),
                     cex_test = par("cex") * 0.8,
                     cex_legend = par("cex") * 0.8,
                     ## plot grid
                     plot_grid = TRUE#,
                     ## ## Plot number ad risk in the km
                     ## plot_n_at_risk = TRUE,
                     ## cex_n_at_risk = par("cex") * 0.8,
                     ## ## Graph command to add legend, as string
                     ## legend_cmd = NULL#,
                     ## Further lines.survfit params
                     #...      ## perché non va pkmdn
                     )
{
    
    ## Argument matching, ... 'handling'
    time_unit <- match.arg(time_unit)
    test <- match.arg(test)
    ## dots <- list(...)

    ## input check
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
    
    
    ## Coerce status to a factor for uniformity
    status     <- factor(status)
    n_status   <- nlevels(status)
    status_lab <- levels(status)
    event_lab  <- status_lab[-1]
    n_event    <- n_status - 1
    
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
    if (is.null(strata)){
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
    max_time <- max(fit$time)
    
    ## times for plot ticks, n at risk, summaries and so on
    ## x has to be based on time_by, if specified
    ## if not specified make 4 step
    times <- if (is.null(time_by)) seq(0, max_time, length = 4)
             else seq(0, max_time, by = time_by * time_divisor)
    
    ## Summary estimates for presentation purposes
    est_times <- seq(from = 0L, to = max(times), by = time_divisor)
    sfit2 <- summary(fit, times = est_times)
    prob_colnames <- sprintf("Pr(%s)", sfit2$states)
    prob_colnames[length(prob_colnames)] <- "Pr(Event Free)"
    probs <- setNames(data.frame(sfit2$pstate), prob_colnames)
    lower <- setNames(data.frame(sfit2$lower), prob_colnames)
    upper <- setNames(data.frame(sfit2$upper), prob_colnames)
    
    ## returned tables
    if (univariate) {
        preamble <- data.frame('time' = sfit2$time / time_divisor)
        r_estimates <- cbind(preamble, probs)
        r_lower     <- cbind(preamble, lower)
        r_upper     <- cbind(preamble, upper)                                 
    } else {
        preamble <- data.frame('time' = sfit2$time / time_divisor,
                               'group' = gsub('strata=', '', sfit2$strata))
        r_estimates <- cbind(preamble, probs)
        r_lower     <- cbind(preamble, lower)
        r_upper     <- cbind(preamble, upper)
    }
    
    ## all estimates (with lower and upper CI)
    r_all <- {
        indexes <- matrix(seq_len(3L * n_status), ncol = n_status, byrow = TRUE)
        dim(indexes) <- NULL
        all_estimates <- cbind(probs,
                               setNames(lower, rep('low CI', ncol(lower))),
                               setNames(upper, rep('up CI' , ncol(upper)))) [, indexes]
        cbind(preamble, all_estimates)
    }
    
    names(r_estimates)[1] <- names(r_lower)[1] <-
        names(r_upper)[1] <- names(r_all)[1]   <- time_unit
    
    ## -----
    ## Tests
    ## -----
    
    if( !univariate ) {
        
        if ('gray' %in% test) {
            ## Gray test
            gray <- data.frame(with(db, cmprsk::cuminc(ftime = time,
                                                       ## better integer with 0 as censored
                                                       fstatus = as.integer(status) - 1L,
                                                       group = strata)$Tests[, c(1,3,2)]))
            rownames(gray) <- event_lab
            
            gray_strings <- sprintf('Gray Test=%.2f, df=%d, p%s',
                                    gray$stat,
                                    gray$df,
                                    lbmisc::pretty_pval(gray$pv, equal = TRUE))
            test_strings <- gray_strings
            
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
            
        } else if ('none' %in% test) {
            gray <- NA
            test_strings <- rep("", n_event)
        } else
            stop("something wrong with test")

    }
    
    ## ------------
    ## plotting
    ## ------------
    if (plot) {
        
        ## Color set-up
        ## Strata (eg treatment) cols: if not given, set it black;
        ## then check for length
        ## browser()
        ## col_strata <- if ('col' %in% names(dots)) dots$col
        ##               else rep('black', n_stratas)
        if (length(col_strata) != n_stratas)
            stop("numbers of col must be", n_stratas)
        
        ## Events (es relapse, death) colors
        if (length(col_status) != n_event)
            stop("numbers of col_status must be", n_event)
        
        ## xlim definition
        if (is.null(xlim)) {
            xlim_inf <-  -(max_time/15)
            xlim_sup <- max_time + (- xlim_inf/3)
        } else {
            xlim_inf <- xlim[1]
            xlim_sup <- xlim[2]
        }

        ## Univariate plots: cumulative incidence and stacked version
        if (univariate){
            
            ## first graph: not stacked
            graphics::plot(NA, NA, #fit,
                           xlim = c(xlim_inf, xlim_sup),
                           ylim = ylim,
                           axes = FALSE,
                           ylab = ylab,
                           xlab = xlab)
            graphics::axis(2, las = 1)
            graphics::axis(1, at = times, labels = times/time_divisor)
            if (plot_grid)
                lbmisc::add_grid(at_x = times, at_y = graphics::axTicks(2))
            graphics::box()
            legend('topleft', legend = event_lab, lty = 1,
                   col = col_status, cex = cex_legend, bg = 'white')
            graphics::lines(fit, fun = 'event', conf.int = plot_ci,
                            col = col_status, ...)

            ## second graph: stacked, TODOHERE
            
        }

        ## Bivariate plots: cumulative incidence for each event, stratified by group/strata
        if (!univariate){

            comprisk_event_plotter <- function(fit, event, test = NULL){

                ylab <- sprintf('Pr(%s)', event)
                graphics::plot(NA, NA, #fit,
                               xlim = c(xlim_inf, xlim_sup),
                               ylim = ylim,
                               axes = FALSE,
                               ylab = ylab,
                               xlab = xlab)
                graphics::axis(2, las = 1)
                graphics::axis(1, at = times, labels = times/time_divisor)
                if (plot_grid)
                    lbmisc::add_grid(at_x = times, at_y = graphics::axTicks(2))
                graphics::box()
                legend('topleft',
                       legend = strata_labels, 
                       col = col_strata,
                       lty = 1,
                       cex = cex_legend, bg = 'white')
                graphics::lines(fit[, event], fun = 'event',
                                conf.int = plot_ci,
                                col = col_strata)
                graphics::mtext(text = test, line = 0.2,
                                cex = cex_test,
                                family = 'sans', font = 3)
            }
            
            Map(comprisk_event_plotter, list(fit), as.list(event_lab), as.list(test_strings))

        }
        
    }
        
    ## ------------
    ## return Stats
    ## ------------
    if (univariate)
        invisible(list('fit' = fit,
                       'estimates' = r_estimates, 'lower' = r_lower, 'upper' = r_upper, 'all' = r_all))
    else
        invisible(list('fit' = fit,
                       'estimates' = r_estimates, 'lower' = r_lower, 'upper' = r_upper, 'all' = r_all,
                       'gray_test' = gray))
                           
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

    
