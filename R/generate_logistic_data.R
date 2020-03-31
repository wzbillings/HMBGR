generate_logistic_data <- function(P0, r, K = NULL, max_t, time_step, noise = 0,
                                   method = "analytic", reduced = FALSE,
                                   make_plot = FALSE) {
  # This function generates time-series data modeling logistic growth based on
  # the input parameters.

  # Validate inputs:
    # All numerical inputs must be strictly positive numeric values.
    if (P0 <= 0 | mode(P0 != "numeric")) {
      stop("The parameter 'P0' must be a positive numeric value.")
    }
    if (r <= 0 | mode(r != "numeric")) {
      stop("The parameter 'r' must be a positive numeric value.")
    }
    if (max_t <= 0 | mode(max_t != "numeric")) {
      stop("The parameter 'max_t' must be a positive numeric value.")
    }
    if (time_step <= 0 | mode(time_step != "numeric")) {
      stop("The parameter 'time_step' must be a positive numeric value.")
    }
    # If noise < 0, stop
    if (noise < 0 | mode(noise != "numeric")) {
      stop("The noise parameter must be a nonnegative numeric value.")
    }
    # Method must come from a specific list of allowed methods.
    if (!(method %in% list("analytic", "a", "discretized", "d", "RK4", "r"))) {
      stop("Method not defined. Check documentation for allowable methods.")
    }
    # Reduced and make_plot arguments must be logicals.
    if (!(mode(reduced) == "logical" && mode(make_plot) == "logical")) {
      stop("The arguments 'reduced' and 'make_plot' must be TRUE or FALSE.")
    }
    # If not reduced model, K must be positive numeric.
    if (!reduced && (K <= 0 || mode(K) != "numeric")) {
      stop("If you are not using the dimensionless model, K must be a positive
           numeric value.")
    }

  # Check if dimensionless
  if(reduced) {
    # Choose method and generate data (dimensionless)
    K = 1
  } else {
    # Choose method and generate data (2 parameter)
  }

  # Add noise if needed
  if (noise != 0) {
    noise_vector <- rnorm(length(output$P), mean = 0, sd = noise * K)
    P <- P + noise_vector
  }

  # Make plot if requested

  # Prep and return outputs
}
