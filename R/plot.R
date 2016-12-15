#' illness death multi-state model plot
#'
#' illness death multi-state model plot
#' 
#' @param entry_lab char entry state label
#' @param illness_lab char illness state label
#' @param death_lab char death state label
#' @param recover draw transition between illness and entry states
#' @param box add graphics::box
#' @examples
#' ill_death(recover = TRUE, box = TRUE)
#' @export
ill_death <- function(entry_lab = 'Entry', illness_lab = 'Relapse',
                      death_lab = 'Death',  recover = FALSE, 
                      box = FALSE)
{
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(par(oldpar))
    graphics::par(mai = rep(0.05, 4))
        
    layout <- c(1, 2)
    states <- c(entry_lab, illness_lab, death_lab)
    connect <- matrix(0, 3, 3, dimnames = list(states, states))
    connect[1, 2:3]  <- c(1.1, 0.9)
    connect[2, 3] <- c(0.9)
    if (recover) connect[2, 1] <- c(1.1)
    survival::statefig(layout = layout, connect = connect, cex = 2)
    if (box) graphics::box()
    invisible(list('layout' = layout, 'connect' = connect))
}


