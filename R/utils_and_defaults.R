time_unit_to_time_divisor <-
    function(time_unit = c("days", "weeks", "months", "years")){ 
        time_unit <- match.arg(time_unit)
        mean_month = (365*3 + 366)/(12*4)
        mean_year  = (365*3 + 366)/(4) 
        switch(time_unit,
               days = 1,
               weeks = 7,
               months = mean_month,
               years = mean_year)
}

default_time_by <- function(time_unit = c("days", "weeks", "months", "years")){
    time_unit <- match.arg(time_unit)
    switch(time_unit,
           days = 30,
           weeks = 4,
           months = 6,
           years = 1)
}
