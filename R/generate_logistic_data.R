#' Generate Example Logistic Growth Time-Series Data
#'
#' @param P0 Positive numeric; the initial population value.
#' @param K Positive numeric; the carrying capacity. Defaults to 1,
#'     which is the value to use if the model is dimensionless/relative.
#' @param r Positive numeric; the intrinsic population growth rate.
#' @param max_t Positive numeric; the maximum time step at which to evaluate
#'     the model (note that the minimum time step is always 0).
#' @param time_step Positive numeric; the size of steps to take between each
#'     evaluation of the model. The vector of times will be generated as a
#'     sequence from 0 to max_t in step-size of this parameter.
#' @param noise Positive numeric; the amount of noise to add to the data, as a
#'     proportion of the equilibrium value. E.g. setting this to some "s"
#'     will add a normal random vector of noise to the data where all entries
#'     are drawn from the normal distribution with mean 0 and sd s*K. If you
#'     have an sd that you want to add manually, you will have to do the
#'     algebra to get it as a proportion of K.
#' @param method Character; chosen from "analytic" ("a"), "discretized" ("d"),
#'     or "RK4" ("r"), which determines which method to use to generate the
#'     data.
#' @param make_plot Logical; if true, the function will generate a plot of the
#'     generated time series for you.
#'
#' @return a data frame with columns "t", the time values at which the model was
#'     evaluated, and "P", the population size at each time step.
#' @export
#'
generate_logistic_data <- function(P0, K = 1, r, max_t, time_step, noise = 0,
                                   method = "analytic", make_plot = FALSE) {
  # This function generates time-series data modeling logistic growth based on
  # the input parameters.

  # Validate inputs:
    # All numerical inputs must be strictly positive numeric values.
    if ((P0 <= 0) | (!is.numeric(P0))) {
      stop("The parameter 'P0' must be a positive numeric value.")
    }
    if (r <= 0 | (!is.numeric(r))) {
      stop("The parameter 'r' must be a positive numeric value.")
    }
    if (max_t <= 0 | (!is.numeric(max_t))) {
      stop("The parameter 'max_t' must be a positive numeric value.")
    }
    if (time_step <= 0 | (!is.numeric(time_step))) {
      stop("The parameter 'time_step' must be a positive numeric value.")
    }
    # If noise < 0, stop
    if (noise < 0 | (!is.numeric(noise))) {
      stop("The noise parameter must be a nonnegative numeric value.")
    }
    # Method must come from a specific list of allowed methods.
    if (!(method %in% list("analytic", "a", "discretized", "d", "RK4", "r"))) {
      stop("Method not defined. Check documentation for allowable methods.")
    }
    # make_plot argument must be logicals.
    if (!is.logical(make_plot)) {
      stop("The arguments 'reduced' and 'make_plot' must be TRUE or FALSE.")
    }
    # K must be positive numeric.
    if (K <= 0 || !is.numeric(K)) {
      stop("If you are not using the dimensionless model, K must be a positive
           numeric value.")
    }

  # Generate a list of time steps to solve for.
  t <- seq(from = 0, to = max_t, by = time_step)

  # Choose solving method and solve
  if (method == "analytic" | method == "a") {
    # use analytic method
    P <- solve_logistic_equation(P0, K, r, t)
    P[1] <- P0
  } else if (method == "discretized" | method == "d") {
    # use discrete method
    P <- numeric(length(t))
    P[1] <- P0
    for (j in 2:length(t)) {
      P[j] <- P[j - 1] + r*P[j - 1] * (1 - P[j - 1]/K) * time_step
    }
  } else if (method == "RK4" | method == "r") {
    # use RK4 ode solver
    solver_output <- deSolve::ode(
      y = c(P = P0),
      times = seq(from = 0, to = max_t, by = time_step),
      func = calculate_logistic_derivative,
      parms = c("r" = r, "K" = K)
    )
    P <- as.data.frame(solver_output)$P
  }

  # Add noise if needed
  if (noise != 0) {
    noise_vector <- stats::rnorm(length(output$P), mean = 0, sd = noise * K)
    P <- P + noise_vector
  }

  # Make plot if requested
  if (make_plot) {
    graphics::plot(x = t, y = P, ylab = "P(t)", type = "l")
  }

  # Prep and return outputs
  output <- data.frame(t,P)
  return(output)
}
