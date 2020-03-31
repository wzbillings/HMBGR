#' Analytic Solution to the Logistic Equation
#'
#' Uses the known analytic solution to the one-population logistic growth
#' differential equation to calculate the size of a population at a specific
#' time point, given certain population parameters.
#'
#' @param P0 Real scalar; the initial population value.
#' @param K Real scalar; the carrying capacity.
#' @param r Real scalar; the intrinsic population growth rate.
#' @param t Real scalar; the time point at which to evaluate the population.
#'
#' @return Real scalar; the size of the population with the given parameters at
#'     time t.
#' @export
#'
#' @examples solve_logistic_equation(100, 1000, 0.1, 10)
solve_logistic_equation <- function(P0, K, r, t) {
    # This function solves the logistic equation with the given parameters at
    #  returns the value of P at time t, using the known analytic solution.

    A = (K - P0)/P0
    P = K/(1 + A*exp(-r*t))
    return(P)
  }
