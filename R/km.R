#' Plots an 'enhanced' Kaplan-Meier plot, with base graphics package.
#' 
#' 
#' This function plots a Kaplan-Meier plot.
#' 
#' 
#' @param time survival time variable
#' @param status survival indicator variable
#' @param strata Stratifying variable (optional)
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
#' @param conf_int logical ... Plot confidence intervall? If NULL
#'     confidence interval are plotted only if strata has two or more
#'     levels
#' @param test tests: 'none' = don't plot tests, 'logr' = log-rank test,
#'     'hr'=hazard ratio, 'both' = log-rank test and hazard ratio
#' @param plot_n_at_risk Logical value: plot number at risk?
#' @param gr_legend Graph command to add legend, as string
#' @param ... Further \code{\link{lines.survfit}} parameters
#' @return The function plot the graph and return a list with
#'     Laplan-Meier statistics
#' @details The function make the hypothesis that times are measured
#'     in days; in not leave time_unit not specified
#' @export
km2 <- function(time = NULL,
               status = NULL,
               ## NULL = non stratified, otherwise stratifying variable
               strata = NULL,
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
               ## PLot Confidence interval
               conf_int = NULL,
               ## Test: none=don't plot tests, logr=logranktest,
               ##		hr=hazratio,both=both
               test = c('logr','hr','both','none'),
               ## Plot number ad risk in the km
               plot_n_at_risk = TRUE,
               ## Graph command to add legend, as string
               gr_legend = NULL,
               ## Further lines.survfit params
               ...                     
               ){

    ## TODO
    ## - permettere al plot di incastrarsi in un mfrow specificato a monte
    ##   Attualmente possibile solo se plot_n_at_risk = FALSE (perchè in tal
    ##   caso non si usa oma)
    ## - inserire la possibilità di visualizzare la mediana 
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
    if(! (is.logical(conf_int) || is.null(conf_int)))
        stop("'conf_int' must be NULL or logical")
    
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
    
    ## Restore graph defaults at the end
    if (plot_n_at_risk) {
        old.par <- par(no.readonly = TRUE)
        on.exit(par(old.par))
    }
    
    ## Time divisor for days for the plot axis
    time_divisor <-
        (time_unit %in% 'days'   *   1)    + 
        (time_unit %in% 'weeks'  *   7)    + 
        (time_unit %in% 'months' *  30.43) + 
        (time_unit %in% 'years'  * 365.25)
    
    ## Default xlab and ylab if NULL is provided
    if (is.null(xlab)) {  ## upcase first letter of time_unit
        xlab <- paste(toupper(substring(time_unit, 1, 1)),
                      substring(time_unit, 2),
                      sep = '')
    }
    if (is.null(ylab)) {
        ylab <- 'Probability'
    }

    ## Default time_by if NULL is provided
    if (is.null(time_by)) {
        time_by <- if('days' == time_unit) {
            30
        } else if ('weeks' == time_unit) {
            4
        } else if ('months' == time_unit) {
            12
        } else if ('years' == time_unit) {
            1
        }
    }
        
    ## Check if it's a stratified km and set up a few things
    if (is.null(strata)) {
        univariate <- TRUE
        stratified <- FALSE
        n.strata <- 1
        strata.labels <- c('All')
        my.formula <- survival::Surv(time, status) ~ 1
    } else {
        univariate <- FALSE
        stratified <- TRUE
        stopifnot(all( ! is.na(strata)) )
        n.strata <- nlevels(as.factor(strata))
        strata.labels <- levels(as.factor(strata))
        my.formula <- survival::Surv(time, status) ~ strata
    }

    ## Default conf_int if NULL is provided
    if (is.null(conf_int)) {
        if (univariate) {
            conf_int <- TRUE
        } else {
            conf_int <- FALSE
        }
    }
    

    ## Se si è chiesto un HR, ma gli strati sono piu di 2
    ## tramutarlo in log rank e dare un warning
    if ((n.strata !=2) & (test=='hr')) {
        warning('HR can be plotted only with 2 groups. Changing to Log-rank tests')
        test <- 'logr'
    }
    

    ## -------------------------------------
    ## Estimates (km, curve comparison, cox)
    ## -------------------------------------

    ## Kaplan-Meyer survival estimate
    fit <- survival::survfit(my.formula)
    sfit <- summary(fit)
    
    if(stratified) {
        ## Log-rank test
        logr <- survival::survdiff(my.formula)
        logr$df <- n.strata-1
        logr$p <- pchisq( q=logr$chisq, df=logr$df, lower.tail=FALSE )
        logr.string <- sprintf('Log-rank Test=%.2f, df=%d, p%s',	
                               logr$chisq, 
                               logr$df, 
                               prettyPval(logr$p) )
        ## Cox Model (and summary
        cox <- survival::coxph(my.formula)
        scox <- summary(cox)
        hr.string  <- sprintf('HR=%.3f (95%% CI, %.3f-%.3f)',
                              coefficients(scox)[2],
                              scox$conf.int[3],
                              scox$conf.int[4])
        both.string <- paste(logr.string, hr.string, sep=' - ')
	
        ## Choose which stat to print in the graph
        test.string <- switch(test,
                              logr = logr.string,
                              hr = hr.string,
                              both= both.string) 
    }

    ## ------------
    ## Plot section
    ## ------------

    ## Color set-up: if given, otherwise set it black
    if ( 'col' %in% names(dots)) {
        strata.col <- dots$col
    } else {
        strata.col <- rep('black', n.strata)
    }

    ## Se si desidera inserire la tabella dei number at risk
    ## occorre impostare il margine inferiore, prevedendo un tot
    ## di righe opportune (determinate dal numero degli strati) 
    if (plot_n_at_risk) par('oma'=c(n.strata+1,0,0,0))

    ## xlim definition
    if (is.null(xlim)) {
        xlim.inf <-  -(max(fit$time)/15)
        xlim.sup <- max(fit$time) + (- xlim.inf/3)
    } else {
        xlim.inf <- xlim[1]
        xlim.sup <- xlim[2]
    }
        
    ## Axis'n grid: 
    ## y defaults are ok 
    ## x has to be based on time_by, if specified
    ## if not specified make 4 step
    if (is.null(time_by)) {
        times <- seq(0, max(fit$time), length=4)
    } else {
        times <- seq(0, max(fit$time), by=time_by*time_divisor)
    }

    
    ## Main plotting section
    plot(NA,NA, 
         xlim=c(xlim.inf, xlim.sup), 
         ylim=ylim,
         axes=F,
         ylab=ylab,
         xlab=xlab,
         main=main
         )
    axis(2)		
    axis(1, at=times, labels=times/time_divisor)
    addGrid(at.y=axTicks(2), at.x=times)
    box()
    if (reverse) {
      lines(fit, fun = 'event', conf.int=conf_int,  ...)
    } else {
      lines(fit, conf.int=conf_int, ...)
    }
    
    ## Add legend
    if (!is.null(gr_legend)) {
        eval(parse(text = gr_legend))
    }

    ## Add stat string to title
    if (stratified & (test %in% c('logr','hr','both') )) {
        mtext(test.string, line=0.2, family='sans', font=3)
    }

    ## Add number at risk
    if (plot_n_at_risk) {

        ## Print header
        mtext('At risk', side=1, line=4, adj=1, at=xlim.inf,font=2)

        ## Utilizzo axis per plottare gli a rischio negli strati
        ## (la linea utilizzabile in presenza di titolo di asse
        ## delle x è dalla 4 in poi: la 4 è per il titolo, dalla 5
        ## in poi per i dati
        
        my.time <- summary(fit, times = times, extend=TRUE)$time
        n.risk <- summary(fit, times = times, extend=TRUE)$n.risk
        if (univariate) {
            strata <- rep(strata.labels, length(my.time))
        } else {
            strata <- summary(fit, times = times, extend =TRUE)$strata
        }
        
        risk.data <- data.frame(strata = strata,
                                time = my.time,
                                n.risk = n.risk)
        if (!univariate) {
            levels(risk.data$strata) <- sub('(^strata=)(.*$)',
                                            '\\2', 
                                            levels(risk.data$strata))
        }
        
        ## Lo split pone i dati nell'ordine della lista
        ## nell'ordine dei dati, quindi per coerenza con l'ordine
        ## dei colori è necessario riordinare la lista
        
        spl.risk.data <- split( risk.data, risk.data$strata)
        spl.risk.data <- spl.risk.data[ strata.labels ]

        for( label in names(spl.risk.data) ) {
            prog <- which( names(spl.risk.data) %in% label ) 
            group.line.lab <- 4 + prog
            group.line.num <- group.line.lab - 1
            group.col <- strata.col[prog]
            ## plot label del gruppo
            mtext(	label, side=1, line=group.line.lab, 
                  at=xlim.inf, adj=1, col=group.col) 
            ## plot dati per ogni time_by
            axis(1, at=times, labels=spl.risk.data[[label]]$n.risk,
                 line=group.line.num ,tick=FALSE, col.axis=group.col) 
        }

    }

    ## Return Stats wheter or not plot has been done
    if (univariate) {
        invisible(list('km'=fit))
    } else {
        invisible(list('km'=fit,'logrank'=logr, 'cox'=cox, 'scox'=scox))
    }
    
}
