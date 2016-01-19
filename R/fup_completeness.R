#' Calculate C index of of follow up completeness
#' 
#' Calculate C index of of follow up completeness.
#' 
#' @param time Follow up (in days?)
#' @param status event indicator
#' @param cutoff (in days?)
#' @param strata group
#' @return A list with individual global C-index, strata C-indexes
#' (optional), and individual C-indexes
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
#' fup_completeness(time = time, status = status, cutoff = 200, strata = group)
#' 
#' @export
fup_completeness <- function(time = NULL,
                             status = NULL,
                             cutoff = NULL,
                             strata = NULL) {
  ## input validation
  if (! is.numeric(time))
    stop('time is mandatory and must be numeric.')
  if (! is.numeric(status))
    stop('status is mandatory and must be numeric.')
  if (! is.numeric(cutoff))
    stop('censoring_time is mandatory and must be numeric.')
  
  ## censoring to cutoff ...
  db <- censor_at(time = time,
                  status = status,
                  censoring_time = cutoff)
  names(db) <- c('time', 'status')
  db$potential_fup <- ifelse(db$status == 0, cutoff, db$time)

  ## global and individual C
  rval <- list('globalC' = sum(db$time) / sum(db$potential_fup),
               'individualC' = db$time / db$potential_fup )

  ## strata C
  if (!is.null(strata)){
    agg <- aggregate(db[c('time', 'potential_fup')],
                     by = list(strata),
                     FUN = sum )
    agg$strataC <- agg[,2]/agg[,3]
    rval$strataC <-  agg
  }

  return(rval)
}
