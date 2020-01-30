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
#' @param ep character: which end points to calculate, default to
#'     \code{c("os","pfs","ttp")}
#' @param sequential_check check that date are sequential and warn
#'     otherwise
#' @param sequential_strict set to missing all times (and
#'     corresponding statuses) that due to not sequential dates are
#'     negative
#' @param comprisk return data suitable for competing events analysis
#'     as well
#' @return a \code{data.frame} to be used with \code{\link{cbind}}.
#' @examples
#' \dontrun{
#' db <- data.frame(start_date = as.Date(c("1900-01-01", "1900-01-01", "1900-01-01", "1900-01-01",          NA, "1900-01-01", "1900-04-01", NA)),
#'                  prog_date  = as.Date(c("1900-03-01", "1900-03-01",           NA,           NA,          NA,           NA, "1900-02-01", NA)),
#'                  death_date = as.Date(c("1900-06-01",           NA, "1900-06-01",           NA,          NA,           NA, "1900-06-01", NA)),
#'                  last_fup   = as.Date(c("1900-06-01", "1900-12-31", "1900-06-01", "1900-12-31", "1900-12-31",          NA, "1900-06-01", NA)))
#' db
#' with(db, tteep(start_date, prog_date, death_date, last_fup))
#' } 
#' db2 <- data.frame(start_date = c(0,  0,  0,  0, NA,  0, NA),
#'                   prog_date  = c(3,  3, NA, NA, NA, NA, NA),
#'                   death_date = c(6, NA,  6, NA, NA, NA, NA),
#'                   last_fup   = c(6, 12,  6, 12, 12, NA, NA))
#' with(db2, tteep(start_date, prog_date, death_date, last_fup))
#'
#' @references Guidance for Industry, Clinical Trial Endpoints for the
#'     approval of cancer drugs and biologics, FDA, May 2007
#' @export
tteep <- function(start_date = NULL,
                  prog_date  = NULL,
                  death_date = NULL,
                  last_fup   = NULL,
                  ep = c("os", "pfs", "ttp"),
                  sequential_check   = TRUE,
                  sequential_strict = TRUE,
                  comprisk = TRUE)
{

    if( is.null(start_date) || (! (inherits(start_date, 'Date') || (is.numeric(start_date)))))
        stop('start_date must be date or a numeric')

    if( is.null(last_fup) || (! (inherits(last_fup, 'Date') || (is.numeric(last_fup)))))
        stop('last_fup must be date or a numeric')
    
    if( (!is.null(prog_date)) && (!inherits(prog_date, 'Date')) && (!is.numeric(prog_date)))
        stop('prog_date must be NULL, a Date or a numeric')
    
    if( (!is.null(death_date)) && (!inherits(death_date, 'Date')) && (!is.numeric(death_date)))
        stop('death_date must be NULL, a Date or a numeric')

    if( !is.character(ep) )
        stop('ep must be a character')
    
    ## Create indicator variables if missing
    prog <- if (is.null(prog_date)) rep(NA, length(prog_date))
            else !is.na(prog_date)
    death <- if(is.null(death_date)) rep(NA, length(death_date))
             else !is.na(death_date)
    
    all_dates <- data.frame('start_date' = start_date,
                            'prog_date' =  prog_date,
                            'death_date' = death_date,
                            'last_fup' = last_fup)
    
    ## argument preprocessing
    start_date <- as.numeric(start_date)
    prog_date  <- as.numeric(prog_date)
    death_date <- as.numeric(death_date)
    last_fup   <- as.numeric(last_fup)
    ep <- gsub(" ", "", tolower(ep))
    
    ## ---------------------------
    ## Check sequential dates 
    ## ---------------------------
    if (sequential_check){
        check1 <- lbmisc::compare_columns(all_dates, operator = '<=')
        
        if (is.data.frame(check1) && (nrow(check1) > 0L)) {
            warning("Some dates are not sequential")
            print(check1)
        }
    }
        
    ## Return value
    rval <- list()

    ## ---------------------------
    ## Separated/standard endpoint 
    ## ---------------------------
    
    ## Overall survival
    if ("os" %in% ep) { 
        rval$os_status <- os_status <- as.integer(death)
        os_last_date <- ifelse(os_status, death_date, last_fup)
        rval$os_time <- os_last_date - start_date
        ## if time is NA (eg missing start date, but not death) set time to NA
        rval$os_status[is.na(rval$os_time)] <- NA
        ## checks
        neg_times <- (!is.na(rval$os_time)) & rval$os_time < 0
        any_neg <- any(neg_times, na.rm = TRUE)
        if (any_neg) warning("some OS times are < 0")
        if (sequential_strict && any_neg) {
            rval$os_time[neg_times]   <- NA
            rval$os_status[neg_times] <- NA
        }
    } 
    ## Progression Free Survival
    if ("pfs" %in% ep) { 
        rval$pfs_status <- pfs_status <- as.integer(death | prog)
        min_prog_death <- suppressWarnings(
            apply(cbind(death_date, prog_date), 1, min, na.rm = TRUE))
        ## line that follows is to fix when death_date and prog_date are both
        ## NA (default min returns Inf when na.rm = TRUE)
        min_prog_death[!is.finite(min_prog_death)] <- NA
        pfs_last_date <- ifelse(pfs_status, min_prog_death, last_fup)
        rval$pfs_time <- pfs_last_date - start_date
        ## if time is NA (eg missing start date, but not death) set time to NA
        rval$pfs_status[is.na(rval$pfs_time)] <- NA
        ## checks
        neg_times <- (!is.na(rval$pfs_time)) & rval$pfs_time < 0
        any_neg <- any(neg_times, na.rm = TRUE)
        if (any_neg) warning("some PFS times are < 0")
        if (sequential_strict && any_neg) {
            rval$pfs_time[neg_times]   <- NA
            rval$pfs_status[neg_times] <- NA
        }
    } 
    ## Time to progression
    if ("ttp" %in% ep) {
        rval$ttp_status <- ttp_status <- as.integer(prog)
        min_death_lfup <- suppressWarnings(
            apply(cbind(death_date, last_fup), 1, min, na.rm = TRUE))
        ## The same as above
        min_death_lfup[!is.finite(min_death_lfup)] <- NA
        ttp_last_date <- ifelse(ttp_status, prog_date, min_death_lfup)
        rval$ttp_time <- ttp_last_date - start_date
        ## if time is NA (eg missing start date, but not death) set time to NA
        rval$ttp_status[is.na(rval$ttp_time)] <- NA
        ## checks
        neg_times <- (!is.na(rval$ttp_time)) & rval$ttp_time < 0
        any_neg <- any(neg_times, na.rm = TRUE)
        if (any_neg) warning("some TTP times are < 0")
        if (sequential_strict && any_neg) {
            rval$ttp_time[neg_times]   <- NA
            rval$ttp_status[neg_times] <- NA
        }
    }
    
    name_order <- paste(rep(ep, each = 2), c("time", "status"), sep = "_")
    rval <- as.data.frame(rval)[name_order]

    ## -----------------------
    ## Competing risk endpoint 
    ## -----------------------
    if (comprisk){

        ## remove dates before start date (data.frame with only
        ## progression, death and exit from FUP
        all_dates2 <- Map(date_cleaner,
                         list(all_dates[, 1])[rep(1, 3)],
                         as.list(all_dates[, -1]))
        all_dates2 <- as.data.frame(all_dates2)
        names(all_dates2) <- names(all_dates)[-1]
        ## same numeric data.frame
        all_dates2_n <- as.data.frame(lapply(all_dates2, as.integer))
       
        rval$first_event_date <- with(
            all_dates2,
            pmin(prog_date, death_date, last_fup, na.rm = TRUE)
        )

        rval$first_event_time <- as.integer(
            rval$first_event_date - all_dates$start_date
        )

        rval$first_event_status <- factor(
            apply(all_dates2_n, 1, lbmisc::which.min2),
            levels = 1:3,
            labels = c('Progression', 'Death', 'FUP exit')
        )

        rval$first_event_status <- relevel(
            rval$first_event_status, 'FUP exit'
        )

        ## if time is NA (eg missing start date, but not death) set time to NA
        rval$first_event_status[is.na(rval$first_event_time)] <- NA
        ## checks
        neg_times <-
            (!is.na(rval$first_event_time)) &
            rval$first_event_time < 0
        any_neg <- any(neg_times, na.rm = TRUE)
        if (any_neg) warning("some first times are < 0")
        if (sequential_strict && any_neg) {
            rval$first_event_time[neg_times]   <- NA
            rval$first_event_status[neg_times] <- NA
        }
        
    }
    
    ## ------
    ## Return
    ## ------
    rval
}
    
date_cleaner <- function(base, date) {
    check <- date < base
    date[check %in% TRUE] <- NA
    date
}

    

    
