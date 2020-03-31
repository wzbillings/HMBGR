#' Calculate the Rate of Change of a Logistic Population
#'
#' Calculates the value of dP/dt when the population is at size P.
#' This assumes the population is growing logistically with know parameters r,
#' the intrinsic growth rate, and K, the carrying capacity.
#'
#' Designed for use with the package deSolve.
#'
#' @param t Numeric (optionally a vector); the time steps to evaluate at. Note
#'     that the logistic ODE is autonomous, but this is required for deSolve
#'     compatibility.
#' @param P Numeric (optionally a vector); the population size(s) at which the
#'     derivative should be evaluated.
#' @param parms Named list containing the entries "r" and "K", both positive
#'     numeric values. The list MUST be of the form list("r" = #, "K" = #), but
#'     the order does not matter. Formatting is specific for deSolve
#'     compatibility, so it has to be like this until a tidyverse version of
#'     deSolve is released, probably.
#'
#' @return A list containing the calculated derivatives.
#' @export
#'
#' @examples calculate_logistic_derivative(10, 50, list("r" = 0.1, "K" = 100))
calculate_logistic_derivative <- function(t, P, parms) {
    # This function calcualtes the derivative of the logistic equation, dP/dt at
    #  time t with the inputted parameters (a list with names "r" and "K").
    # Designed to be implemented in the ODE solution method supplied by deSolve.
    with(
      data = as.list(c(P, parms)),
      expr = {
        # Calculate rate of pop growth.
        dP = r * P * (1 - P/K)
        # Return rate as a list.
        return(list(c(dP)))
      }
    )
  }
