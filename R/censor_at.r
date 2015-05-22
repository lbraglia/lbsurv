#' Censor time to event end point at a given time/period of follow up.
#' 
#' Censor time to event end point at a given time. Implemented for
#' right-censored dataset.
#'
#' The algorithm works as follow.
#' If time of censoring is higher than (or equal to) patient's time, does
#' nothing and returns time and status as they are.
#' Otherwise:
#' 
#' - if the patients didn't experienced the event at full follow up,
#'   he/she didn't experienced even at the previous censoring time:
#'   therefore the algorithm should set censored time but leave indicator
#'   variable unchanged
#' - if the patients experienced the event at time t, we hyphotize he/she
#'   was without event at time t-1 (aka an event is experienced the same
#'   day which is registered in the dataset), therefore the algorithm set
#'   0 to indicator variable and set time to censoring time
#' 
#' ... synthesizing, in both cases set indicator variable to 0 and time
#' to censoring time.
#' 
#' @param time time variable
#' @param status status variable
#' @param censoring_time censoring time
#' @return A data frame to be used with \code{\link{cbind}}
#' @examples
#'
#' ctimes <- c(160, 150, 125, 75)
#'
#' ## Example 1:

#' time   <- c(100,150)
#' status <- c(  0,  1)
#' cbind(data.frame(time, status), # original
#'       censor_at(time = time, status = status, censoring_time = ctimes))
#'
#' ## Example 2
#' time   <- c(100,150)
#' status <- c(  1,  0)
#' cbind(data.frame(time, status), # original
#'       censor_at(time = time, status = status, censoring_time = ctimes))
#' 
#' @export
censor_at <- function(time = NULL, status = NULL, censoring_time = NULL) {

  ## check input
  if (! is.numeric(time))
    stop("time is mandatory and must be numeric.")
  if (! is.numeric(status))
    stop("status is mandatory and must be numeric.")
  if (! is.numeric(censoring_time))
    stop("censoring_time is mandatory and must be numeric.")
  if( length(status) != length(time))
    stop("time and status must have the same length.")

  ## normalize input
  censoring_time <- sort(censoring_time[!is.na(censoring_time)])
  
  ## working dataset: repeat/rbind the dataset for the number of censoring
  ## times and add those in a column
  db <- do.call(rbind,
                list(data.frame(time, status))[rep(1, length(censoring_time))])
  db$censoring_time <- rep(censoring_time, each = length(time))
  
  ## Move to C, please
  subst <- function(t,s,c) {
    if (t <= c) {
      c(t, s)
    } else {
      c(c, 0)
    }
  }
  ## now rval is a dataframe with long style
  rval <- data.frame(t(apply(db, 1, function(x) {
                               subst(t = x[1], s = x[2], c = x[3])
                             })))
                             

  ## normalize output: rval as a wide style data.frame
  rval <- do.call(cbind, split(rval, db$censoring_time))

  ## TODO: names from deparse(substitute()) or similar
  censoring_suffixes <- rep(sprintf("c%s", round(sort(censoring_time))),
                            each = 2)   # for time and status
  names(rval) <- paste(c("time", "status"), censoring_suffixes, sep = "_")

  rval
    
}

