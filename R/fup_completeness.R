#' Calculate C index of of follow up completeness
#' 
#' Calculate C index of of follow up completeness.
#' 
#' @param time Follow up (in days?)
#' @param status event indicator
#' @param cutoff (in days?)
#' @param strata group
#' @return A data.frame with a global C and a C for each group
#' @references Clark T., Altman D., De Stavola B. (2002),
#' Quantification of the completeness of follow-up. Lancet 2002;
#' 359: 1309-10
#' @examples
#'
#' time   <- c(180, 12, 240, 250 )
#' status <- c(  0,  1,   0, 1   )
#' group  <- c("A","A", "B", "B" )
#' 
#' ## example: 
#' ## quantify fup completeness to 200 days (eg minimum potential
#' ## follow up in a hypotethic prospective trial)
#' 
#' fup_completeness(time = time, status = status,
#'                  cutoff = seq(150, 250, 10),
#'                  strata = group)
#' 
#' @export
fup_completeness <- function(time = NULL,
                             status = NULL,
                             cutoff = seq(1, max(time), length = 10),
                             strata = NULL)
{
    ## input validation
    if (! is.numeric(time))
        stop('time is mandatory and must be numeric.')
    if (! is.numeric(status))
        stop('status is mandatory and must be numeric.')
    if (! is.numeric(cutoff))
        stop('censoring_time is mandatory and must be numeric.')

    rval <- lapply(cutoff, function(x){

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
    
    rval <- do.call(rbind, rval)
    return(rval)
    
}
