#' k-Fold cross validation for OLS model with smoothing
#'
#' Performs cross validation using the method of Stein et al., "Ecological
#' Moldeing from Time Series Infernece: Insight into Dynamics and Stability of
#' Intestinal Microbiota; PLoS Comp Bio 2013.
#'
#' Stein et al. recommended performing 3-fold cross validation 10 times. This
#' should be accomplished fairly easily with the `replicate` command.
#'
#' @param data A data frame of logistic growth time series data, one column
#'     should be named "t" and one should be named "P".
#' @param k Numeric; the number of partitions for k-fold cross validation.
#'     defaults to 3 in accordance with Stein et al's recommendation.
#' @param time_step Numeric; also strictly positive. Can be inputted manually
#'     but defaults to the maximum time value divided by the number of time
#'     points used.
#' @param reduced Logical; if TRUE, the carrying capacity is assumed to be 1 and
#'     only r is fitted in the model. If FALSE, both K and r are fitted.
#' @param lambda Numeric; strictly positive. The smoothing parameter to use
#'     for Tikhonov regularization in the regression model. Defaults to zero.
#'
#' @return The cross-validation error for k-fold cross validation on the model.
#' @export
#'
cross_validate <- function(data, k = 3, time_step = NULL, reduced = TRUE,
                           lambda = 0) {
  #-----------------------------------------------------------------------------
  # Set up constant stuff
  # Calculate time step if not specified.
  time_step = max(data$t) / length(data$t)

  # Generate possible combinations of parts for later.
  combos <- combn(1:k, k-1, simplify = F)

  #-----------------------------------------------------------------------------
  # Construct a random partition into k parts of the data

  num_obs <- nrow(data)
  part_size <- floor(num_obs/k)
  rem <- seq(1, num_obs, 1)
  parts <- list()
  for (i in 1:(k - 1)) {
    choices <- sample(rem, part_size, replace = F)
    parts[[i]] <- data[choices, ]
    rem <- setdiff(rem, choices)
  }
  parts[[k]] <- data[rem, ]

  #-----------------------------------------------------------------------------
  # Fit the model k times, once using each part as the testing data.
  # The SSE is calculated as a goodness of fit metric for each model.
  # Then, the mean of all k calculated SSEs is taken as the CVE.

  errors <- numeric(length(parts)) # Holder for SSEs

  # Now fit the model k number of times.
  for (i in 1:k) {
    # Grab the first combo of indices to use
    to_use <- combos[[i]]
    # Make a blank dataframe
    train <- data.frame()
    # Add k-1 of the parts (usings combos to get indices) to training data.
    # Every time, j will contain k-1 indices to call.
    for (j in to_use) {
      # Get the next part to include.
      new_dat <- parts[[j]]
      # Add this data frame to the rest.
      train <- rbind(train, new_dat)
    }
    # Now train has k-1 of the parts in it.
    # Set testing data to be the part that wasn't selected.
    # This selects the part whose index is NOT in the to_use list.
    not_used <- (1:k)[!(1:k %in% to_use)]
    test <- parts[[not_used]]

    # Model the data, specify time step manually.
    mod <- model_logistic_data(df,
                               ts = time_step,
                               dimensionless = reduced,
                               smoothing = lambda)
    # Extract all values needed for predictions.
    t <- test$t
    P0 <- test$P[[1]]
    r <- mod[[1]]
    if (reduced) {
      K <- 1
    } else{
      K <- mod[[2]]
    }

    # Compute the prediction using the analytic solution.
    pred <- (P0 * K) / (P0 + (K - P0) * exp(-r*t))

    # Calculate the SSE for this fit.
    errors[i] <- sum((test$P - pred)^2)
    }

  # Take the average of all SSEs--this is the "cross validation error".
  cves <- mean(errors)

}
