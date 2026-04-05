#' Inverse probability weights for linked samples
#'
#' Estimates the probability that each record appears in the linked sample
#' and returns inverse probability weights to adjust for selective linkage
#' (missed matches). Implements the weighting approach described in Equation 2
#' of Breen and Joo (2026): \eqn{w_i = 1 / p_i}.
#'
#' @param population A data.frame of the full population.
#' @param linked A data.frame of the linked subsample. Must contain the same
#'   covariates as `population`.
#' @param covariates A character vector of covariate names used to model the
#'   linkage probability. Must be present in both data.frames.
#' @param method Character. Either `"logistic"` (default) for logistic regression
#'   or `"ml"` for a random forest via [ranger::ranger()].
#' @param trim Numeric vector of length 2 giving quantiles at which to
#'   trim propensity scores. Default `c(0.01, 0.99)`.
#' @param stabilize Logical. If `TRUE` (default), use stabilized weights
#'   \eqn{w_i = P(\text{linked}) / p_i} instead of \eqn{1/p_i}.
#' @param ranger_args A list of additional arguments passed to
#'   [ranger::ranger()] when `method = "ml"`. Default: `list(num.trees = 500)`.
#'
#' @return A list of class `"linkinfer_ipw"` with elements:
#'   \describe{
#'     \item{weights}{Numeric vector of adjustment weights for linked records.}
#'     \item{propensity_scores}{Numeric vector of estimated propensity scores
#'       for all records (population rows, then linked rows).}
#'     \item{model}{The fitted model object (glm or ranger).}
#'     \item{method}{Character string indicating the method used.}
#'     \item{n_population}{Number of rows in the population.}
#'     \item{n_linked}{Number of rows in the linked sample.}
#'     \item{covariates}{The covariates used.}
#'   }
#'
#' @examples
#' data(census_population)
#' data(linked_sample)
#' result <- weight_ipw(
#'   population = census_population,
#'   linked = linked_sample,
#'   covariates = c("age", "sex", "race", "education", "urban")
#' )
#'
#' @export
weight_ipw <- function(population, linked, covariates,
                       method = c("logistic", "ml"),
                       trim = c(0.01, 0.99),
                       stabilize = TRUE,
                       ranger_args = list(num.trees = 500)) {

  method <- match.arg(method)

  if (is.null(covariates) || length(covariates) == 0) {
    rlang::abort("`covariates` must be provided.")
  }

  # Stack population and linked into one data.frame with indicator
  linked_col <- ".linkinfer_linked."
  pop_subset <- population[, covariates, drop = FALSE]
  lnk_subset <- linked[, covariates, drop = FALSE]

  pop_subset[[linked_col]] <- 0L
  lnk_subset[[linked_col]] <- 1L

  data <- rbind(pop_subset, lnk_subset)
  linked_ind <- data[[linked_col]] == 1L

  # Build formula
  formula <- stats::as.formula(
    paste(linked_col, "~", paste(covariates, collapse = " + "))
  )

  # Check for missing values in covariates
  complete <- stats::complete.cases(data[, covariates, drop = FALSE])
  if (any(!complete)) {
    n_miss <- sum(!complete)
    cli::cli_warn(
      "{n_miss} row{?s} with missing covariates dropped from propensity model."
    )
    data_model <- data[complete, , drop = FALSE]
    linked_ind_model <- linked_ind[complete]
  } else {
    data_model <- data
    linked_ind_model <- linked_ind
  }

  # Fit model
  if (method == "logistic") {
    fit <- stats::glm(formula, data = data_model, family = stats::binomial())
    pscores <- stats::predict(fit, newdata = data_model, type = "response")
  } else {
    rlang::check_installed("ranger", reason = "for ML-based propensity scores")
    data_model[[linked_col]] <- factor(data_model[[linked_col]], levels = c("0", "1"))
    rargs <- c(
      list(formula = formula, data = data_model, probability = TRUE),
      ranger_args
    )
    fit <- do.call(ranger::ranger, rargs)
    pscores <- fit$predictions[, "1"]
  }

  # Trim propensity scores
  pscores <- .trim_weights(pscores, trim)

  # Compute weights for linked records
  prev <- mean(linked_ind_model)
  if (stabilize) {
    all_weights <- prev / pscores
  } else {
    all_weights <- 1 / pscores
  }

  # Extract weights for linked records only
  linked_weights <- all_weights[linked_ind_model]

  # Map back propensity scores to full stacked data if rows were dropped
  if (any(!complete)) {
    full_pscores <- rep(NA_real_, nrow(data))
    full_pscores[complete] <- pscores
    pscores <- full_pscores
  }

  result <- list(
    weights = linked_weights,
    propensity_scores = pscores,
    model = fit,
    method = method,
    n_population = nrow(population),
    n_linked = nrow(linked),
    covariates = covariates,
    stabilized = stabilize
  )
  class(result) <- "linkinfer_ipw"
  result
}

#' @export
print.linkinfer_ipw <- function(x, ...) {
  cli::cli_h2("Inverse Probability Weights")
  cli::cli_text("Method: {.val {x$method}}")
  cli::cli_text("Stabilized: {.val {x$stabilized}}")
  cli::cli_text("Covariates: {.val {x$covariates}}")
  cli::cli_text("Linked records: {.val {x$n_linked}}")
  cli::cli_text("")

  w <- x$weights
  cli::cli_text("Weight summary:")
  cli::cli_text("  Min: {.val {round(min(w), 3)}}  Median: {.val {round(stats::median(w), 3)}}  Max: {.val {round(max(w), 3)}}")
  cli::cli_text("  Effective N: {.val {round(.effective_n(w), 1)}}  Design effect: {.val {round(.design_effect(w), 3)}}")
  invisible(x)
}
