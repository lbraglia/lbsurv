#' Make a legend command suitable for km
#'
#' Make a legend command suitable for km
#'
#' @param x same as \code{legend}'s x
#' @param y same as \code{legend}'s y
#' @param levels factor levels
#' @param colors colors
#' @param lty lty
#' @param lwd lwd
#' @param title legend title
#' @export
km_legend <- function(x, y = NULL,
                      levels = NULL,
                      colors = NULL,
                      lty = 'solid',
                      lwd = 1,
                      title = NULL){
    x <- deparse(x)
    y <- deparse(y)
    lev <- deparse(levels)
    cols <- deparse(colors)
    lt <- deparse(lty)
    lw <- deparse(lwd)
    titl <- deparse(title)

    paste0("legend(",
           "x = ", x, ", ",
           "y = ", y, ", ",
           "title = ", titl, ", ",
           "bg = \"white\", ",
           "legend = ", lev, ", ",
           "col = ", cols, ", ",
           "lty = ", lt, ", ",
           "lwd = ", lw, ")")

}

#' Plots an 'enhanced' Kaplan-Meier plot, with base graphics package.
#' 
#' 
#' This function plots a Kaplan-Meier plot.
#' 
#' @param time survival time variable
#' @param status survival indicator variable
#' @param strata Stratifying variable (optional)
#' @param plot plot (default = TRUE) or only return estimates?
#' @param time_unit Time unit of x axis
#' @param time_by Time step x axis (in days)
#' @param main Graph main title
#' @param ylab Y-axis label.
#' @param xlab X-axis label. If NULL a suitable default based on
#'     time_unit will be provided
#' @param xlim X-axis limit. If NULL a suitable default based on
#'     time_unit will be provided
#' @param ylim Y-axis limit. Default to c(0,1) be provided
#' @param reverse plot cumulative events
#' @param mark_censored mark censored observation
#' @param pch_censored pch used for censored observation
#' @param conf_int character. Can be 'default', 'none' or 'lines' or
#'     'shades'
#' @param conf_int_alpha a base level for alpha if \code{conf_int =
#'     'alpha'} (splitted by number of groups)
#' @param test tests: 'none' = don't plot tests, 'logr' = log-rank
#'     test, 'hr' = hazard ratio, 'both' = log-rank test and hazard
#'     ratio
#' @param cex_test cex parameter for test string
#' @param plot_n_at_risk Logical value: plot number at risk?
#' @param legend_cmd Graph command to add legend, as string
#' @param ... Further \code{\link{lines.survfit}} parameters
#' @return The function plot the graph and return a list with
#'     Laplan-Meier statistics
#' @details The function make the hypothesis that times are measured
#'     in days; in not leave time_unit not specified
#' @examples
#' library(survival)
#' km(time = aml$time, status = aml$status, strata = aml$x)
#' @export
km <- function(time = NULL,
               status = NULL,
               ## NULL = non stratified, otherwise stratifying variable
               strata = NULL,
               ## plot anything,
               plot = TRUE,
               ## Time unit x axis
               time_unit = c('days', 'weeks', 'months', 'years'),
               ## Time step x axis (in days)
               time_by = NULL,
               ## Main title
               main = '',
               ## Y axis label
               ylab = NULL,
               ## X axis nbel
               xlab = NULL,
               ## Y axis limits
               ylim = c(0L, 1L),
               ## X axis limits
               xlim = NULL,
               ## plot cumulative events?
               reverse = FALSE,
               ## mark censored observation
               mark_censored = TRUE,
               ## pch used for censored observation
               pch_censored = "'",
               ## Plot Confidence interval
               conf_int = c('default', 'none', 'lines', 'shades'),
               conf_int_alpha = 0.8,
               ## Test: none = don't plot tests, logr = logranktest,
               ##       hr = hazratio, both = both
               test = c('logr', 'hr', 'both', 'none'),
               cex_test = par("cex") * 0.8,
               ## Plot number ad risk in the km
               plot_n_at_risk = TRUE,
               ## Graph command to add legend, as string
               legend_cmd = NULL,
               ## Further lines.survfit params
               ...)
{

    ## TODO
    ## - permettere al plot di incastrarsi in un mfrow specificato a monte
    ## - inserire la possibilita' di visualizzare la mediana 
    ##   di sopravvivenza
    ## - se il plot non ha titolo e non ha test ottimizzare lo spazio
    ##   superiore eliminandolo
    ## - sistemare il settore sinistro del grafico per permettere che 
    ##   stringhe molto lunghe (il nome dei gruppi, se lunghi) 
    ##   non vengano tagliate negli at risk 
    ## - inserire il parametro data come specificabile,
    ##   per prendere da un data.frame??
   
    ## time, status: check existence
    if (is.null(status)) stop("'status' needed.")
    if (is.null(time)) stop("'time' needed.")

    ## strata
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
    if (! is.character(main)) stop('main must be character')
    if (! is.character(test)) stop('test must be character')
    
    ## plot_n_at_risk: logical
    if (! is.logical(plot_n_at_risk)) stop("'plot_n_at_risk' must be logical")

    ## conf_int: NULL or logical
    conf_int <- match.arg(conf_int)
    
    ## xlim: NULL or numeric (of length 2)
    if (! (is.null(xlim) || (is.numeric(xlim) && (2 == length(xlim)))))
        stop("'xlim' must be NULL or numeric vector of 2 elements")

    ## xlim: NULL or numeric (of length 2)
    if (! (is.null(ylim) || (is.numeric(ylim) && (2 == length(ylim)))))
        stop("'ylim' must be NULL or numeric vector of 2 elements")
    
    ## Argument matching, ... 'handling'
    time_unit <- match.arg(time_unit)
    test <- match.arg(test)
    dots <- list(...)
    
    ## Time divisor for days for the plot axis
    time_divisor <-
        (time_unit %in% 'days'   *   1)    + 
        (time_unit %in% 'weeks'  *   7)    + 
        (time_unit %in% 'months' *  30.43) + 
        (time_unit %in% 'years'  * 365.25)
    
    ## Default xlab and ylab if NULL is provided
    if (is.null(xlab)) xlab <- paste(toupper(substring(time_unit, 1, 1)),
                                     substring(time_unit, 2),
                                     sep = '')
    if (is.null(ylab)) ylab <- 'Probability'

    ## Default time_by if NULL is provided
    if (is.null(time_by)) {
        time_by <-
            (time_unit %in% 'days'   *  30)  + 
            (time_unit %in% 'weeks'  *   4)  + 
            (time_unit %in% 'months' *  12)  + 
            (time_unit %in% 'years'  *   1)
    }
        
    ## Check if it's a univariate or stratified plot; setup the
    ## estimates dataset and parameter defaults accordingly
    if (is.null(strata)) {
        db <- data.frame(time = time, status = status)
        db <- lbmisc::NA_remove(db)
        mod_formula <- survival::Surv(time, status) ~ 1
        univariate <- TRUE
        n_stratas <- 1
        strata_labels <- 'All '
        if ('default' %in% conf_int) conf_int <- 'lines'
    } else {
        db <- data.frame(time = time, status = status, strata = strata)
        db <- lbmisc::NA_remove(db)
        mod_formula <- survival::Surv(time, status) ~ strata
        univariate <- FALSE
        n_stratas <- nlevels(strata)
        strata_labels <- levels(strata)
        if ('default' %in% conf_int) conf_int <- 'none'
        if ((n_stratas != 2) & (test == 'hr')) {
            warning(paste0('HR can be plotted only with 2 groups. ',
                           'Changing to Log-rank tests'))
            test <- 'logr'
        }
    }

    ## -------------------------------------
    ## Estimates (km, curve comparison, cox)
    ## -------------------------------------

    ## Kaplan-Meyer survival estimate
    fit <- survival::survfit(mod_formula, data = db)
    sfit <- summary(fit)
    
    if( !univariate ) {
        ## Log-rank test
        logr <- survival::survdiff(mod_formula, data = db)
        logr$df <- n_stratas - 1
        logr$p <- stats::pchisq(q = logr$chisq, df = logr$df,
                                lower.tail = FALSE)
        logr_string <- sprintf('Log-rank Test=%.2f, df=%d, p%s',	
                               logr$chisq, 
                               logr$df, 
                               lbmisc::pretty_pval(logr$p, equal = TRUE))
        ## Cox Model (and his summary)
        cox <- survival::coxph(mod_formula, data = db)
        scox <- summary(cox)
        hr_string  <- sprintf('HR=%.3f (95%% CI, %.3f-%.3f)',
                              stats::coefficients(scox)[2],
                              scox$conf.int[3],
                              scox$conf.int[4])
        both_string <- paste(logr_string, hr_string, sep = ' - ')
	
        ## Choose which stat to print in the graph
        test_string <- switch(test,
                              logr = logr_string,
                              hr = hr_string,
                              both= both_string) 
    }

    ## obtain data for confidence intervals
    CI <- data.frame((unclass(sfit))[c('time', 'lower', 'upper')])
    CI_spl <- if (univariate) list('All' = CI)
              else  split(CI, f = sfit$strata)

    ## ------------
    ## Plot section
    ## ------------

    if (plot){

        ## reset default graphical parameters on exit
        old_par <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(old_par))
    
        ## Color set-up: if given, otherwise set it black
        strata_col <- if ('col' %in% names(dots)) dots$col
                      else rep('black', n_stratas)

        ## Se si desidera inserire la tabella dei number at risk
        ## occorre impostare il margine inferiore, prevedendo un tot
        ## di righe opportune (determinate dal numero degli strati) 
        if (plot_n_at_risk) graphics::par('oma' = c(n_stratas + 0.1, 0, 0, 0))

        ## xlim definition
        if (is.null(xlim)) {
            xlim_inf <-  -(max(fit$time)/15)
            xlim_sup <- max(fit$time) + (- xlim_inf/3)
        } else {
            xlim_inf <- xlim[1]
            xlim_sup <- xlim[2]
        }
        
        ## Axis'n grid: 
        ## y defaults are ok 
        ## x has to be based on time_by, if specified
        ## if not specified make 4 step
        times <- if (is.null(time_by)) seq(0, max(fit$time), length = 4)
                 else seq(0, max(fit$time), by = time_by * time_divisor)
        
        ## Main plotting section
        graphics::plot(NA, NA,
                       xlim = c(xlim_inf, xlim_sup),
                       ylim = ylim,
                       axes = FALSE,
                       ylab = ylab,
                       xlab = xlab,
                       main = main)
        graphics::axis(2, las = 1)
        graphics::axis(1, at = times, labels = times/time_divisor)
        lbmisc::add_grid(at_x = times, at_y = graphics::axTicks(2))
        graphics::box()
        ## main line and confidence intervals
        if (reverse) {
            lines_fun <- 'event'
            switch(conf_int,
                   none  = graphics::lines(fit,
                                           fun = lines_fun,
                                           conf.int = FALSE,
                                           ...),
                   lines = graphics::lines(fit,
                                           fun = lines_fun,
                                           conf.int = TRUE,
                                           ...))
        } else {
            switch(conf_int,
                   none  = graphics::lines(fit,
                                           conf.int = FALSE,
                                           mark.time = mark_censored,
                                           pch = pch_censored,
                                           ...),
                   lines = graphics::lines(fit,
                                           conf.int = TRUE,
                                           mark.time = mark_censored,
                                           pch = pch_censored,
                                           ...),
                   shades = {
                       ## http://stackoverflow.com/questions/18584815/
                       mapply(FUN = function(ci, cols){
                           alpha <- conf_int_alpha/n_stratas
                           ## if it's a base color add alpha, otherwise if
                           ## it is already a hexadecimal, leave it
                           ## unchanged
                           col <- if (cols %in% grDevices::colors()) 
                                      lbmisc::col2hex(cols, alpha = alpha)
                                  else
                                      cols
                           graphics::polygon(
                                         c(ci$time,  rev(ci$time)),
                                         c(ci$lower, rev(ci$upper)),
                                         col = col,
                                         border = FALSE)},
                           CI_spl,
                           strata_col)
                       graphics::lines(fit,
                                       conf.int = FALSE,
                                       pch = pch_censored,
                                       mark.time = mark_censored,
                                       ...)
                   })
        }
        
        ## Add legend
        if (!is.null(legend_cmd)) {
            eval(parse(text = legend_cmd))
        }

        ## Add stat string to title
        if (!univariate && (test %in% c('logr','hr','both') )) {
            graphics::mtext(test_string, line = 0.2,
                            cex = cex_test,
                            family = 'sans', font = 3)
        }

        ## Add number at risk
        if (plot_n_at_risk) {

            ## Print header
            graphics::mtext('At risk', side = 1, line = 4, adj = 1,
                            cex = old_par$cex,
                            at = xlim_inf, font = 2)

            ## Utilizzo axis per plottare gli a rischio negli strati
            ## (la linea utilizzabile in presenza di titolo di asse
            ## delle x e' dalla 4 in poi: la 4 e' per il titolo, dalla 5
            ## in poi per i dati
            
            my_time <- summary(fit, times = times, extend = TRUE)$time
            n_risk <- summary(fit, times = times, extend = TRUE)$n.risk
            if (univariate) {
                strata <- rep(strata_labels, length(my_time))
            } else {
                strata <- summary(fit, times = times, extend = TRUE)$strata
            }
            
            risk_data <- data.frame(strata = strata,
                                    time = my_time,
                                    n_risk = n_risk)
            if (!univariate) {
                levels(risk_data$strata) <- sub('(^strata=)(.*$)',
                                                '\\2', 
                                                levels(risk_data$strata))
            }
            
            ## Lo split pone i dati nell'ordine della lista
            ## nell'ordine dei dati, quindi per coerenza con l'ordine
            ## dei colori e' necessario riordinare la lista
            
            spl_risk_data <- split( risk_data, risk_data$strata)
            spl_risk_data <- spl_risk_data[ strata_labels ]

            for(label in names(spl_risk_data) ) {
                prog <- which( names(spl_risk_data) %in% label ) 
                group_line_lab <- 4 + prog
                group_line_num <- group_line_lab - 1
                group_col <- strata_col[prog]
                ## plot label del gruppo
                graphics::mtext(label,
                                side = 1,
                                cex = old_par$cex,
                                line = group_line_lab,
                                at = xlim_inf,
                                adj = 1,
                                col = group_col)
                ## plot dati per ogni time_by
                graphics::axis(1,
                               at = times,
                               labels = spl_risk_data[[label]]$n_risk,
                               line = group_line_num,
                               tick = FALSE,
                               col.axis = group_col)
            }

        }

    }
    ## Return Stats wheter or not plot has been done
    if (univariate) {
        invisible(list('km' = fit))
    } else {
        invisible(list('km' = fit,
                       'logrank' = logr,
                       'cox' = cox,
                       'scox' = scox))
    }
    
}
