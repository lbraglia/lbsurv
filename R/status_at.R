#' Return patients status at given time
#' 
#' Return patients status at given time. Implemented for
#' right-censored dataset.
#'
#' @param start_date time variable
#' @param prog_date status variable
#' @param death_date death variable
#' @param status_time censoring time
#' @return a factor vector
#' @examples
#' db <- data.frame(start_date  = as.Date(c("1900-01-01", "1900-01-01", "1900-01-01", "1900-01-01",           NA)),
#'                  prog_date   = as.Date(c("1900-03-01", "1900-03-01",           NA,           NA,           NA)),
#'                  death_date  = as.Date(c("1900-06-01",           NA, "1900-06-01",           NA,           NA)))
#' with(db, status_at(start_date, prog_date, death_date, status_time = 10))
#' with(db, status_at(start_date, prog_date, death_date, status_time = 100))
#' with(db, status_at(start_date, prog_date, death_date, status_time = 200))
#' @export
status_at <- function(start_date = NULL,
                      prog_date  = NULL,
                      death_date = NULL,
                      status_time = NULL)
{
    if (is.null(start_date)) stop("start_date can't be null")
    if (is.null(prog_date)) stop("prog_date can't be null")
    if (is.null(death_date)) stop("death_date can't be null")
    if (is.null(status_time) || anyNA(status_time))
        stop("status_time can't be NULL and cannot contain NAs.")
    
    ## if prog_delta > 0 pt has already had a progression
    prog_delta <- as.integer(start_date + status_time - prog_date)
    prog_status <- factor(ifelse(is.na(prog_delta) | prog_delta < 0, 'No progression', 'Progression'),
                          levels = c('No progression', 'Progression'))

    ## the same for death_date
    death_delta <- as.integer(start_date + status_time - death_date)
    death_status <- factor(ifelse(is.na(death_delta) | death_delta < 0, 'no death', 'death'),
                           levels = c('no death', 'death'))
    
    ## put togheter
    status <- interaction(prog_status, death_status, sep = ', ')
    
    ## handle NA start_date
    status[is.na(start_date)] <- NA

    status
}


