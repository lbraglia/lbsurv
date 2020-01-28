time_unit_to_time_divisor <- function(tu){ # given time unit
        (tu %in% 'days'   *   1)    + 
        (tu %in% 'weeks'  *   7)    + 
        (tu %in% 'months' *  30.43) + 
        (tu %in% 'years'  * 365.25)
}

default_time_by <- function(tu){ # given time unit
    (tu %in% 'days'   *  30)  + 
    (tu %in% 'weeks'  *   4)  + 
    (tu %in% 'months' *   6)  + 
    (tu %in% 'years'  *   1)
}
