time_unit_to_time_divisor <-
    function(time_unit = c("days", "weeks", "months", "years")){ 
        time_unit <- match.arg(time_unit)
        switch(time_unit,
               days = 1,
               weeks = 7,
               months = 30.43,
               years = 365.25)
}

default_time_by <- function(time_unit = c("days", "weeks", "months", "years")){
    time_unit <- match.arg(time_unit)
    switch(time_unit,
           days = 30,
           weeks = 4,
           months = 6,
           years = 1)
}
