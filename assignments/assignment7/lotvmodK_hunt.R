#' Lotka-Volterra Model with Carrying Capacity and Hunting
#'
#' Computes the rate of change of populations in a predator-prey interaction
#' with logistic growth for prey and an added hunting term.
#'
#' @param t     time (years)
#' @param pop   named numeric vector with initial conditions: prey, pred
#' @param pars  named list/vector of parameters:
#'   \emph{rprey}    prey intrinsic growth rate (yr^-1)
#'   \emph{alpha}    predator-prey interaction coefficient
#'   \emph{eff}      predator conversion efficiency
#'   \emph{pmort}    predator mortality rate (yr^-1)
#'   \emph{K}        prey carrying capacity
#'   \emph{hunt}     hunting rate (proportion of prey harvested per year)
#'   \emph{min_prey} minimum prey population required before hunting is allowed
#'
#' @return list with dprey and dpred (rates of change)
#'
#' @examples
#' pars <- c(rprey=0.95, alpha=0.01, eff=0.6, pmort=0.4, K=2000,
#'           hunt=0.1, min_prey=200)
#' currpop <- c(prey=1000, pred=100)
#' times <- seq(0, 50, by=0.1)
#' res <- ode(func=lotvmodK_hunt, y=currpop, times=times, parms=pars)

lotvmodK_hunt <- function(t, pop, pars) {
  with(as.list(c(pars, pop)), {

    # Hunting is only applied if prey exceeds the minimum threshold
    # and is capped so we never remove more prey than exist
    actual_hunt <- ifelse(prey > min_prey,
                          min(hunt * prey, prey - min_prey),
                          0)

    dprey <- rprey * (1 - prey / K) * prey - alpha * prey * pred - actual_hunt
    dpred <- eff * alpha * prey * pred - pmort * pred

    return(list(c(dprey, dpred)))
  })
}
