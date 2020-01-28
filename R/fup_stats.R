#' Calculate follow up statistics
#' 
#' Calculate median follow up time (reverse KM method) and C index 
#' for follow up completeness.
#' 
#' @param time Follow up (in days?)
#' @param status event indicator
#' @param cutoff (in days?)
#' @param strata group
#' @param time_unit Time unit table returned
#' @param time_by Time step for completeness table axis (in days)
#' @return A data.frame with a global C and a C for each group
#' @references Clark T., Altman D., De Stavola B. (2002),
#'     Quantification of the completeness of follow-up. Lancet 2002;
#'     359: 1309-10
#' @examples
#'
#' time   <- c(180,  12, 240, 250 )
#' status <- c(  0,   1,   0,   1 )
#' group  <- c("A", "A", "B", "B" )
#' 
#' ## example: 
#' ## quantify fup completeness to 200 days (eg minimum potential
#' ## follow up in a hypotethic prospective trial)
#' 
#' fup_stats(time = time, status = status, strata = group, time_by = 10)
#'
#' time2   <- as.numeric(1:(365*3 + 1))
#' status2 <- rep(c(0,1), 548) 
#' fup_stats(time = time2, status = status2, time_unit = 'year')
#' 
#' @export
fup_stats <- function(time = NULL,
                      status = NULL,
                      # cutoff = seq(1, max(time), length = 10),
                      strata = NULL,
                      time_unit = c('days',
                                    'weeks',
                                    'months',
                                    'years'),
                      time_by = NULL
                      )
{
    ## input validation
    if (! is.numeric(time))
        stop('time is mandatory and must be numeric.')
    if (! is.numeric(status))
        stop('status is mandatory and must be numeric.')
    ## if (! is.numeric(cutoff))
    ##     stop('censoring_time is mandatory and must be numeric.')
    if(! (is.null(time_by) || is.numeric(time_by)))
        stop("'time_by' must be NULL or numeric")

    if (! all(status %in% c(NA, 0, 1)))
        stop('statuses must be 0, 1 or NA.')
    
    ## time unit, divisor, time_by
    time_unit <- match.arg(time_unit)
    time_divisor <- time_unit_to_time_divisor(time_unit)
    if (is.null(time_by)) time_by <- default_time_by(time_unit)
    ## times for completeness table
    max_time <- max(time, na.rm = TRUE)
    times <- seq(0, max_time, by = time_by * time_divisor)
    
    ## --------------------------------------
    ## median follow up by reverse KM method
    ## --------------------------------------
    if (is.null(strata)){
        local_db <- data.frame(time, status)
    } else
        local_db <- data.frame(time, status, strata)

    f_overall <-
        survival::Surv(time = time, event = 1 - status) ~ 1
    fit_overall <-
        survival::survfit(formula = f_overall, data = local_db)
    median_overall  <-
        as.data.frame(quantile(fit_overall, prob = 0.5))
    rownames(median_overall) <- 'All'
    median_fup <- median_overall
    
    ## stratified
    if (!is.null(strata)) {
        f_strata <- survival::Surv(time = time, event = 1 - status) ~ strata
        fit_strata <- survival::survfit(formula = f_strata, data = local_db)
        median_strata  <- as.data.frame(quantile(fit_strata, prob = 0.5))
        names(median_strata) <- c("quantile", "lower", "upper")
        median_fup <- rbind(median_fup, median_strata)
    }

    median_fup <- median_fup / time_divisor    
    median_fup <- cbind(data.frame('group' = rownames(median_fup)),
                        median_fup)
    rownames(median_fup) <- NULL
                        
    ## --------------------------------------
    ## completeness
    ## --------------------------------------
    completeness <- lapply(times, function(x){
        db <- censor_at(time = time,
                        status = status,
                        censoring_time = x)
        names(db) <- c('time', 'status')
        db$potential_fup <- ifelse(db$status == 0, x, db$time)
        
        ## global and individual C
        res <- data.frame('time' = x,
                          'overall' = sum(db$time) / sum(db$potential_fup))
        
        ## strata C
        if (!is.null(strata)){
            agg <- split(db[c('time', 'potential_fup')], f = list(strata))
            strata_C <- lapply(agg, function(x)
                sum(x$time)/sum(x$potential_fup))
            ## names(strata_C) <- paste0('group_', names(strata_C))
            res <- cbind(res, strata_C)
        }
        res
    })
    
    completeness <- do.call(rbind, completeness)
    completeness$time <- completeness$time / time_divisor
    names(completeness)[1] <- lbmisc::upcase(time_unit)
    
    rval <- list('median' = median_fup,
                 'completeness' = completeness)
    return(rval)
}
