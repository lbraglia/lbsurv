#' Apply contal o quigley method
#' 
#' @param time time variable
#' @param status status variable
#' @param marker marker variable
#' @examples
#' 
#' ## Klein Moeschberg example pag 274
#' data(kidtran, package = "KMsurv")
#' kidtran <- within(kidtran, {
#'     gender <- factor(gender, levels = c(1,2), labels = c('male', 'female'))
#'     race <- factor(race, levels = c(1,2), labels = c('white', 'black'))
#' })
#' k_spl <- split(kidtran, f = list(kidtran$race, kidtran$gender))
#' k_spl <- k_spl[c(2,1,4,3)]
#' expanded <- lapply(k_spl, function(x){
#'     contal(time = x$time,
#'            status = x$delta,
#'            marker = x$age)
#' })
#' expanded[1] 
#' 
#' ## expanded_survmisc <- lapply(k_spl, function(x){
#' ##     df = data.frame(time = x$time,
#' ##                     status = x$delta,
#' ##                     marker = x$ag)
#' ##     cox <- survival::coxph(Surv(time = time, event = status) ~ marker,
#' ##                            data = df)
#' ##     survMisc::cutp(cox)
#' ## })
#' 
#' compact <- do.call(rbind, lapply(expanded, function(x) x$optimal))
#' compact
#' 
#' @export
contal <- function(time = NULL, status = NULL, marker = NULL## ,
                   ## cut_maker = function(x, y) mean(c(x, y))
                   ){
    db <- na.omit(data.frame(time = time, status = status, marker = marker))
    # times
    event_times <- sort(unique(db$time[db$status %in% 1]))
    D <- length(event_times)
    # marker cutoffs
    unique_mark <- sort(unique(db$marker))
    possible_cutoffs <- unique_mark[- c(1, length(unique_mark))]
    ## first   <- unique_mark[-length(unique_mark)]
    ## second  <- unique_mark[-1]
    ## possible_cutoffs <- Map(cut_maker, as.list(first), as.list(second))
    cutoff_explorer <- function(cut) {
        ## browser()
        db_loc <- db
        db_loc$marker_cut <- db_loc$marker >= cut
        f <- Surv(time = time, event = status) ~ marker_cut
        ## logr <- tryCatch(
        ##     survival::survdiff(f, data = db_loc),
        ##     error = function(m) data.frame(chisq = NA)
        ## )
        sfit <- summary(survival::survfit(f, data = db_loc),
                        times = event_times)        
        data.frame(cut = cut, abs_sk = abs(find_sk(sfit)))
    }
    cuts <- do.call(rbind, lapply(possible_cutoffs, cutoff_explorer))
    chosen <- cuts[which.max(cuts$abs_sk), ]
    # numero di tempi unici con un decesso
    s <- sqrt(compute_s2(D))
    Q <- chosen$abs_sk / (s * sqrt(D - 1))
    p <- lbmisc::pretty_pval(contal_p(Q))
    optimal <- data.frame('cut_point' = chosen$cut,
                          'abs_sk' = chosen$abs_sk,
                          's' = s,
                          'D' = D,
                          'Q' = Q,
                          'p' = p)
    list(unique_marker_values = unique_mark,
         cuts = cuts,
         optimal = optimal)
}


compute_s2 <- function(D){
    sumlist <- function(x) sum(unlist(x))
    s2_num <- sumlist(lapply(seq_len(D), function(i){
        the_sum <- lapply(seq_len(i), function(j){
            1 / (D - j + 1)
        })
        (1 - sumlist(the_sum))^2
    }))
    s2_den <- (D - 1)
    s2 <- s2_num / s2_den
    s2
}

contal_p <- function(q, lim = 1e4){
## from survmisc::findP, changed a minimum
## lim = limit (accuracy)
##  should be to Inf but generally 1e3 is enough
    if (q < 0.2) 1
    else {
        f <- function(j){(-1)^(j+1) * exp(-2 * j^2 * q^2)}
        terms <- lapply(seq_len(lim), f)
        2 * sum(unlist(terms))
    }
}


find_sk <- function(sfit){
    tmp <- with(sfit, data.frame(time, n.risk, n.event, strata, n.censor))
    tmp$strata <- as.integer(tmp$strata) - 1
    tmp$n.risk.a <- tmp$n.risk * tmp$strata
    tmp$n.risk.b <- tmp$n.risk * (1 - tmp$strata)
    tmp$n.event.a <- tmp$n.event * tmp$strata
    tmp$n.event.b <- tmp$n.event * (1 - tmp$strata)
    ## tmp$n.censor.a <- tmp$n.censor * tmp$strata
    ## tmp$n.censor.b <- tmp$n.censor * (1 - tmp$strata)
    vars <- c("time",
              'n.event.a', 'n.event.b', 'n.event',
              'n.risk.a', 'n.risk.b', 'n.risk'## ,
          ## 'n.censor.a', 'n.censor.b', 'n.censor'
              )
    tmp <- tmp[vars]
    tmp <- aggregate(tmp, by = list(tmp$time), FUN = sum)
    tmp$time <- tmp$Group.1
    tmp['Group.1'] <- NULL
    ## review <- c('time','n.event.a', 'n.censor.a', 'n.risk.a')
    ## review2 <- c('time','n.event.b', 'n.censor.b', 'n.risk.b')
    ## tmp[review]
    ## tmp[review2]
    tmp$n.exp.a <- tmp$n.event * (tmp$n.risk.a / tmp$n.risk)
    tmp$var.a <- with(
        tmp,
        (n.risk.a * (n.event / n.risk) * (1 - (n.event / n.risk))) * 
        ((n.risk - n.risk.a) / (n.risk - 1))
    )
    obs.a <- sum(tmp$n.event.a)
    exp.a <- sum(tmp$n.exp.a)
    var.a <- sum(tmp$var.a)
    sk <- obs.a - exp.a
    sk
}
