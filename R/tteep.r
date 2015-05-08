#' Common time to event end-points calculator
#' 
#' This function calculates common oncology time to event end-points (Overall
#' Survival, Progression Free Survival, Time to Relapse).
#'
#' Overall survival (OS) is computed as time from \code{start_date} to
#' \code{death_date}. Time of patients who did not experienced the event
#' (with missing \code{death_date}) is censored at the time of last follow
#' up (\code{last_fup}).
#'
#' Progression free survival (PFS) is calculated as time from
#' \code{start_date} to \code{prog_date} or \code{death_date}, whichever
#' comes first. Time of patients who did not experienced any event (missing
#' both \code{prog_date} and \code{death_date}) is censored at the time of
#' last follow up (\code{last_fup}).
#'
#' Time to progression (TTP) is computed as time from \code{start_date} to
#' \code{prog_date}. Time of patients who did not experienced the event
#' (missing \code{prog_date}) is censored at the time of last follow up
#' (\code{last_fup}) or death (\code{death_date}) whichever comes first.
#' 
#' @param start_date Date: starting follow up date
#' @param prog_date Date: progression date
#' @param death_date Date: death date
#' @param last_fup Date: last follow up date
#' @param ep character: which end points to calculate, default to \code{c("os","pfs","ttp")}
#' @return a \code{data.frame} to be used with \code{\link{cbind}}.
#' @examples
#' db <- data.frame(start_date = as.Date(c("1900-01-01", "1900-01-01", "1900-01-01", "1900-01-01", NA          , "1900-01-01", NA)),
#'                  prog_date  = as.Date(c("1900-03-01", "1900-03-01", NA          , NA          , NA          , NA          , NA)),
#'                  death_date = as.Date(c("1900-06-01", NA          , "1900-06-01", NA          , NA          , NA          , NA)),
#'                  last_fup   = as.Date(c("1900-06-01", "1900-12-31", "1900-06-01", "1900-12-31", "1900-12-31", NA          , NA)))
#' db
#' with(db, tteep(start_date, prog_date, death_date, last_fup))
#' @references
#' Guidance for Industry, Clinical Trial Endpoints for the approval of
#' cancer drugs and biologics, FDA, May 2007
#' @export
tteep <- function(start_date = NULL,
                  prog_date = NULL,
                  death_date = NULL,
                  last_fup = NULL,
                  ep = c("os", "pfs", "ttp"))
{

  ## Input check
  if(is.null(start_date) || is.null(last_fup))
    stop("start_date and last_fup are mandatory")

  ## Stop if prog_date or death date are not null or a Date
  stopifnot( is.null(prog_date)  || inherits(prog_date, "Date") )
  stopifnot( is.null(death_date) || inherits(death_date, "Date") )

  ## Normalize parameters
  ep <- gsub(" ", "", tolower(ep))

  ## Where a prog_date or death_date are not null, do the indicator
  ## variables (as non missing date), otherwise a column of NA 
  prog <- if(is.null(prog_date)) {
    rep(NA, length(prog_date))
  } else {
    !is.na(prog_date)
  }
  death <- if(is.null(death_date)) {
    rep(NA, length(death_date))
  } else {
    !is.na(death_date)
  }

  ## Return value
  rval <- list()

  ## Overall survival
  if ("os"  %in% ep) { 
    rval$os_status <- os_status <- as.integer(death)
    os_last_date <- ifelse(os_status, death_date, last_fup)
    rval$os_time <- os_last_date - as.integer(start_date)
  } 
  ## Progression Free Survival
  if ("pfs" %in% ep) { 
    rval$pfs_status <- pfs_status <- as.integer(death | prog)
    min_prog_death <- apply(cbind(death_date, prog_date),
                            1,
                            min,
                            na.rm = TRUE)
    ## line that follows is to fix when death_date and prog_date are both
    ## NA (default min returns Inf when na.rm = TRUE)
    min_prog_death[!is.finite(min_prog_death)] <- NA
    pfs_last_date <- ifelse(pfs_status, min_prog_death, last_fup)
    rval$pfs_time <- pfs_last_date - as.integer(start_date)
  } 
  ## Time to progression
  if ("ttp" %in% ep) {
    rval$ttp_status <- ttp_status <- as.integer(prog)
    min_death_lfup <- apply(cbind(death_date, last_fup),
                            1,
                            min,
                            na.rm = TRUE)
    ## as above
    min_death_lfup[!is.finite(min_death_lfup)] <- NA
    ttp_last_date <- ifelse(ttp_status, prog_date, min_death_lfup)
    rval$ttp_time <- ttp_last_date - as.integer(start_date)
  } 

  as.data.frame(rval)[paste(rep(ep, each = 2), c("time", "status"), sep = "_")]

}
