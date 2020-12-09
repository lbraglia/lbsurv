#' Check if a patient has a censored data
#'
#' 
#'@param x a Surv object
#'@export
is_censored <- function(x) unclass(x)[, 'status'] == 0L
