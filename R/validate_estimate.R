#' Validate adjusted estimates using a validation variable
#'
#' Tests whether adjustment weights successfully correct the linked sample
#' by comparing estimates of a validation variable (known at the population
#' level) before and after adjustment. Optionally applies the multiplicative
#' adjustment factor from Equation 7 of Breen and Joo (2026).
#'
#' @param linked_data A data.frame of the linked subsample.
#' @param population_data A data.frame of the full population. If `NULL`,
#'   `population_value` must be provided.
#' @param outcome Character string naming the validation variable.
#' @param weights Optional. A character string naming existing sample weights
#'   in both data.frames.
#' @param adj_weights Adjustment weights for the linked sample. Either a
#'   numeric vector, or a character string naming a column in `linked_data`.
#' @param population_value Optional. A pre-computed population statistic
#'   (overrides computing from `population_data`).
#' @param adjustment_factor Optional numeric scalar. If provided, the adjusted
#'   estimate is further multiplied by this factor (Eq. 7 from Breen and Joo).
#'   Use [compute_adjustment_factor()] to derive this from a validation
#'   subsample.
#' @param ci_level Confidence level for intervals. Default: 0.95.
#'
#' @return A list of class `"linkinfer_validation"` with elements:
#'   \describe{
#'     \item{pop_estimate}{Population mean/proportion of the outcome.}
#'     \item{linked_unadj}{Unadjusted linked sample estimate.}
#'     \item{linked_adj}{Adjusted estimate (using adj_weights).}
#'     \item{linked_adj_factor}{Adjusted estimate after applying the
#'       adjustment factor (if provided).}
#'     \item{bias_unadj}{Unadjusted bias (linked_unadj - pop_estimate).}
#'     \item{bias_adj}{Adjusted bias.}
#'     \item{pct_bias_reduction}{Percentage of bias eliminated.}
#'   }
#'
#' @export
validate_estimate <- function(linked_data, population_data = NULL,
                              outcome, weights = NULL,
                              adj_weights = NULL,
                              population_value = NULL,
                              adjustment_factor = NULL,
                              ci_level = 0.95) {

  # Resolve outcome
  y_lnk <- linked_data[[outcome]]
  if (is.null(y_lnk)) {
    rlang::abort("Column {.var {outcome}} not found in `linked_data`.")
  }

  # Resolve existing weights
  w_lnk <- NULL
  w_pop <- NULL
  if (!is.null(weights)) {
    if (is.character(weights) && length(weights) == 1) {
      w_lnk <- linked_data[[weights]]
      if (!is.null(population_data)) w_pop <- population_data[[weights]]
    }
  }

  # Resolve adjustment weights
  aw <- NULL
  if (!is.null(adj_weights)) {
    if (is.character(adj_weights) && length(adj_weights) == 1) {
      aw <- linked_data[[adj_weights]]
    } else if (is.numeric(adj_weights)) {
      aw <- adj_weights
    }
  }

  # Population estimate
  if (!is.null(population_value)) {
    pop_est <- population_value
  } else if (!is.null(population_data)) {
    y_pop <- population_data[[outcome]]
    pop_est <- .weighted_mean(y_pop, w_pop)
  } else {
    rlang::abort("Either `population_data` or `population_value` must be provided.")
  }

  # Unadjusted linked estimate
  linked_unadj <- .weighted_mean(y_lnk, w_lnk)

  # Adjusted linked estimate
  if (!is.null(aw)) {
    # Combine existing weights with adjustment weights
    final_w <- if (!is.null(w_lnk)) w_lnk * aw else aw
    linked_adj <- .weighted_mean(y_lnk, final_w)
  } else {
    linked_adj <- linked_unadj
  }

  # Apply adjustment factor (Eq. 7)
  linked_adj_factor <- NULL
  if (!is.null(adjustment_factor)) {
    linked_adj_factor <- linked_adj * adjustment_factor
  }

  # Bias calculations
  bias_unadj <- linked_unadj - pop_est
  bias_adj <- linked_adj - pop_est
  bias_adj_factor <- if (!is.null(linked_adj_factor)) linked_adj_factor - pop_est else NULL

  # Percent bias reduction
  if (bias_unadj != 0) {
    pct_reduction <- (1 - abs(bias_adj) / abs(bias_unadj)) * 100
  } else {
    pct_reduction <- NA_real_
  }

  # CI for the adjusted estimate (normal approximation)
  n_eff <- if (!is.null(aw)) .effective_n(final_w) else sum(!is.na(y_lnk))
  se_adj <- sqrt(.weighted_var(y_lnk, if (!is.null(aw)) final_w else w_lnk) / n_eff)
  z <- stats::qnorm(1 - (1 - ci_level) / 2)
  ci_adj <- c(linked_adj - z * se_adj, linked_adj + z * se_adj)

  se_unadj <- sqrt(.weighted_var(y_lnk, w_lnk) / sum(!is.na(y_lnk)))
  ci_unadj <- c(linked_unadj - z * se_unadj, linked_unadj + z * se_unadj)

  result <- list(
    pop_estimate = pop_est,
    linked_unadj = linked_unadj,
    linked_adj = linked_adj,
    linked_adj_factor = linked_adj_factor,
    bias_unadj = bias_unadj,
    bias_adj = bias_adj,
    bias_adj_factor = bias_adj_factor,
    pct_bias_reduction = pct_reduction,
    ci_unadj = ci_unadj,
    ci_adj = ci_adj,
    adjustment_factor = adjustment_factor
  )
  class(result) <- "linkinfer_validation"
  result
}

#' Compute adjustment factor from a validation subsample
#'
#' Implements the adjustment factor from Equation 7 of Breen and Joo (2026).
#' Given a validation subsample where match quality can be assessed (e.g.,
#' using middle initials), computes the ratio of the corrected rate to the
#' observed rate.
#'
#' @param observed_rate The observed rate in the full linked sample.
#' @param false_match_rate The estimated false match rate, typically derived
#'   from a validation variable (e.g., share of records with disagreeing
#'   middle initials).
#' @param rate_among_false The estimated rate of the outcome among false
#'   matches. For rare outcomes, this is often close to the base rate.
#'
#' @return A numeric scalar: the adjustment factor \eqn{R_{true} / R'}.
#'   Multiply the observed rate by this factor to obtain the corrected rate.
#'
#' @details
#' The adjustment factor is derived from Equation 6:
#' \deqn{R_{true} = (R' - R_{false} \cdot f_r) / (1 - f_r)}
#'
#' The adjustment factor is then \eqn{R_{true} / R'}.
#'
#' @export
compute_adjustment_factor <- function(observed_rate, false_match_rate,
                                      rate_among_false) {
  r_true <- (observed_rate - rate_among_false * false_match_rate) /
    (1 - false_match_rate)
  r_true / observed_rate
}

#' @export
print.linkinfer_validation <- function(x, ...) {
  cli::cli_h2("Validation of Adjusted Estimates")

  cli::cli_text("Population estimate: {.val {round(x$pop_estimate, 4)}}")
  cli::cli_text("Linked (unadjusted): {.val {round(x$linked_unadj, 4)}}  [bias: {.val {round(x$bias_unadj, 4)}}]")
  cli::cli_text("Linked (adjusted):   {.val {round(x$linked_adj, 4)}}  [bias: {.val {round(x$bias_adj, 4)}}]")

  if (!is.null(x$linked_adj_factor)) {
    cli::cli_text("Linked (adj+factor): {.val {round(x$linked_adj_factor, 4)}}  [bias: {.val {round(x$bias_adj_factor, 4)}}]")
  }

  if (!is.na(x$pct_bias_reduction)) {
    cli::cli_text("")
    cli::cli_text("Bias reduction: {.val {round(x$pct_bias_reduction, 1)}}%")
  }

  cli::cli_text("")
  cli::cli_text("95% CI (unadjusted): [{.val {round(x$ci_unadj[1], 4)}}, {.val {round(x$ci_unadj[2], 4)}}]")
  cli::cli_text("95% CI (adjusted):   [{.val {round(x$ci_adj[1], 4)}}, {.val {round(x$ci_adj[2], 4)}}]")
  invisible(x)
}
