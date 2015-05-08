#' Censor time to event end point at a certain time.
#' 
#' Censor time to event end point at a certain time. Implemented only for
#' "right" type of censored data.
#' 
#' @param time time variable
#' @param status status variable
#' @param censoring_time censoring time
#' @return A data frame to be used with \code{\link{cbind}}
#' @examples
#'
#' time   <- c(100,150)
#' status <- c(  0,  1)
#' (original <- data.frame(time, status))
#' censor_at(time = time, status = status, censoring_time = 160)
#' censor_at(time = time, status = status, censoring_time = 150)
#' censor_at(time = time, status = status, censoring_time = 125)
#' censor_at(time = time, status = status, censoring_time = 75)
#' 
#' time   <- c(100,150)
#' status <- c(  1,  0)
#' (original <- data.frame(time, status))
#' censor_at(time = time, status = status, censoring_time = 160)
#' censor_at(time = time, status = status, censoring_time = 150)
#' censor_at(time = time, status = status, censoring_time = 125)
#' censor_at(time = time, status = status, censoring_time = 75)
#' 
#' @export
censor_at <- function(time = NULL, status = NULL, censoring_time = NULL) {

  if (sum(sapply(list(time, status, censoring_time), is.null))>0) {
    stop("time, status and censoring_time are mandatory.")
  }

  ## Se il  tempo a cui si vuol tagliare è maggiore del tempo rilevato, 
  ## non fare nulla e restituisci la coppia time status così come sono
  
  ## Se invece è inferiore del tempo rilevato (time):
  ## - nel caso il paziente non abbia avuto l'evento al tempo rilevato, non 
  ##   l'ha avuto neanche al tempo precedente, pertanto bisogna
  ##   portare indietro 
  ##   il tempo lasciando stare la variabile indicatrice
  ## - nel caso il paziente abbia avuto l'evento al tempo t, si
  ##   ipotizza che a  t-1 non l'avesse (ovvero che un evento si
  ##   verifica nel giorno in cui è registrato),  
  ##   per cui si tratta di porre a 0 la variabile indicatrice
  ##   e di riportare indietro il conteggio dei giorni al
  ##   cutoff
  ## In sostanza in entrambi i casi dummy=0 e time=censoring_time

  subst <- function(t,s,c) {
    if (t <= c) {
      c(t, s)
    } else {
      c(c, 0)
    }
  }
  
  df <- data.frame(time, status)
  rval <- data.frame(t(apply(df, 1,
                             function(x)
                               subst(t=x[1], s=x[2], c=censoring_time)
                             ))) 
  names(rval) <- paste(c("time", "status"),
                       paste("c", censoring_time ,sep=""),
                       sep="_")

  rval
    
}




