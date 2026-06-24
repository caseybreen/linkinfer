#' Adjust estimates for linkage bias using a validation (out‑of‑model) variable
#'
#' This function follows the workflow described in Breen and Joo (2026).
#'
#' @param weight_result An object of class `"linkinfer_ipw"`, `"linkinfer_poststrat"`
#'   or `"linkinfer_rake"` containing adjustment weights for the linked sample.
#' @param linked_data A data.frame of the linked subsample (the rows used to
#'   compute `weight_result`). Must contain the outcome variable and the
#'   validation variable.
#' @param population_data A data.frame of the full population. Used to compute
#'   the population‑level estimate of the outcome for bias assessment.
#' @param outcome Character string naming the variable of interest (e.g.,
#'   "race_diff" or other outcome).
#' @param validation_var Character string naming the out‑of‑model validation
#'   variable (e.g., `middle_initial_match`). Should be binary where 1 indicates
#'   a true match and 0 indicates a false match.

#' @param n_boot Number of bootstrap replications for standard‑error
#'   estimation. Default 200.
#' @param ci_level Confidence level for intervals. Default 0.95.
#' @return A list of class `"linkinfer_adjusted"` with elements:
#'   \describe{
#'     \item{pop_estimate}{Population (true) estimate of the outcome.}
#'     \item{estimate}{Weighted estimate for the full linked sample (no validation).}
#'     \item{sestimate}{Weighted estimate for the subset with a valid validation variable.}
#'     \item{testimate}{Weighted estimate for the subset where the validation variable indicates a true match (value 1).}
#'     \item{adjustment_factor}{Factor = testimate / sestimate.}
#'     \item{aestimate}{Adjusted estimate = estimate * adjustment_factor.
#'                        If `thresholds` are supplied, additional elements
#'                        `aestimate1`, `aestimate2`, `aestimate3` are returned
#'                        as described in the AGENTS.md.}
#'     \item{se_adj}{Bootstrap standard error of the adjusted estimate.}
#'     \item{ci_adj}{Confidence interval for the adjusted estimate.}
#'   }
#' @export
adjust_estimate <- function(weight_result, linked_data, population_data,
                            outcome = NULL, validation_var,
                            model_formula = NULL, coef_of_interest = NULL,
                            model_fit = stats::lm, model_args = list(), extract_estimate = NULL,
                            n_boot = 200, ci_level = 0.95) {
  # -------------------- Input checks -------------------------------------
  if (!any(class(weight_result) %in% c("linkinfer_ipw", "linkinfer_poststrat", "linkinfer_rake"))) {
    rlang::abort("`weight_result` must be a weighting object from this package.")
  }
  # Ensure validation variable exists; outcome may be NULL for model‑based workflow
  if (!validation_var %in% names(linked_data)) {
    rlang::abort("`validation_var` must exist in `linked_data`.")
  }
  if (!is.null(outcome) && !(outcome %in% names(linked_data))) {
    rlang::abort("`outcome` must exist in `linked_data` when provided.")
  }
  # Population outcome may be missing; handle gracefully
  if (!is.null(outcome)) {
    outcome_in_pop <- outcome %in% names(population_data)
    if (!outcome_in_pop) {
      rlang::warn("`outcome` not found in `population_data`; population estimate will be NA.")
    }
  } else {
    outcome_in_pop <- FALSE
  }

  # -------------------- Combine weights ---------------------------------
  w <- weight_result$weights

  # -------------------- Core estimation ------------------------
  if (!is.null(outcome)) {
    # Original mean/rate based workflow
    estimate <- .weighted_mean(linked_data[[outcome]], w)
    # Subset with observed validation variable (non‑NA)
    valid_idx <- which(!is.na(linked_data[[validation_var]]))
    if (length(valid_idx) == 0) rlang::abort("No observations with a valid `validation_var`.")
    w_valid <- w[valid_idx]
    y_valid <- linked_data[[outcome]][valid_idx]
    v_valid <- linked_data[[validation_var]][valid_idx]
    sestimate <- .weighted_mean(y_valid, w_valid)                # all with validation
    testimate <- .weighted_mean(y_valid[v_valid == 1], w_valid[v_valid == 1])
  } else {
    # Model‑based workflow (generic for any model)
    if (is.null(model_formula) || is.null(extract_estimate)) {
      rlang::abort("When `outcome` is NULL, `model_formula` and `extract_estimate` must be provided.")
    }
    # Full linked sample estimate
    fit_full <- do.call(model_fit, c(list(formula = model_formula, data = linked_data, weights = w), model_args))
    estimate <- extract_estimate(fit_full)

    # Subset with observed validation variable
    valid_idx <- which(!is.na(linked_data[[validation_var]]))
    if (length(valid_idx) == 0) rlang::abort("No observations with a valid `validation_var`.")
    ld_valid <- linked_data[valid_idx, , drop = FALSE]
    w_valid <- w[valid_idx]
    fit_valid <- do.call(model_fit, c(list(formula = model_formula, data = ld_valid, weights = w_valid), model_args))
    sestimate <- extract_estimate(fit_valid)

    # True‑match subset (validation_var == 1)
    true_idx <- which(!is.na(linked_data[[validation_var]]) & linked_data[[validation_var]] == 1)
    if (length(true_idx) == 0) rlang::abort("No true‑match observations for adjustment.")
    ld_true <- linked_data[true_idx, , drop = FALSE]
    w_true <- w[true_idx]
    fit_true <- do.call(model_fit, c(list(formula = model_formula, data = ld_true, weights = w_true), model_args))
    testimate <- extract_estimate(fit_true)
  }

  adjustment_factor <- testimate / sestimate
  aestimate <- estimate * adjustment_factor

  # -------------------- Population estimate ----------------------------
  pop_estimate <- if (outcome_in_pop) .weighted_mean(population_data[[outcome]]) else NA_real_

  # -------------------- Bootstrap SE -----------------------------------
  boot_est <- numeric(n_boot)
  N <- nrow(linked_data)
  for (b in seq_len(n_boot)) {
    idx <- sample.int(N, size = N, replace = TRUE)
    w_b <- w[idx]
    # Re‑compute estimates for this bootstrap sample using the same workflow
    if (!is.null(outcome)) {
      y_b <- linked_data[[outcome]][idx]
      v_b <- linked_data[[validation_var]][idx]
      est_b <- .weighted_mean(y_b, w_b)
      if (sum(!is.na(v_b)) == 0) next
      w_vb <- w_b[!is.na(v_b)]
      y_vb <- y_b[!is.na(v_b)]
      v_vb <- v_b[!is.na(v_b)]
      sest_b <- .weighted_mean(y_vb, w_vb)
      test_b <- .weighted_mean(y_vb[v_vb == 1], w_vb[v_vb == 1])
    } else {
      # Model‑based bootstrap
      ld_b <- linked_data[idx, , drop = FALSE]
      # Full model
      fit_full_b <- do.call(model_fit, c(list(formula = model_formula, data = ld_b, weights = w_b), model_args))
      est_b <- extract_estimate(fit_full_b)
      # Validation subset
      v_b <- ld_b[[validation_var]]
      valid_idx_b <- which(!is.na(v_b))
      if (length(valid_idx_b) == 0) next
      ld_valid_b <- ld_b[valid_idx_b, , drop = FALSE]
      w_valid_b <- w_b[valid_idx_b]
      fit_valid_b <- do.call(model_fit, c(list(formula = model_formula, data = ld_valid_b, weights = w_valid_b), model_args))
      sest_b <- extract_estimate(fit_valid_b)
      # True‑match subset
      true_idx_b <- which(!is.na(v_b) & v_b == 1)
      if (length(true_idx_b) == 0) next
      ld_true_b <- ld_b[true_idx_b, , drop = FALSE]
      w_true_b <- w_b[true_idx_b]
      fit_true_b <- do.call(model_fit, c(list(formula = model_formula, data = ld_true_b, weights = w_true_b), model_args))
      test_b <- extract_estimate(fit_true_b)
    }
    adj_b <- test_b / sest_b
    boot_est[b] <- est_b * adj_b
  }
  se_adj <- stats::sd(boot_est, na.rm = TRUE)
  z <- stats::qnorm(1 - (1 - ci_level) / 2)
  ci_adj <- c(aestimate - z * se_adj, aestimate + z * se_adj)


  out <- list(
    pop_estimate = pop_estimate,
    estimate = estimate,
    sestimate = sestimate,
    testimate = testimate,
    adjustment_factor = adjustment_factor,
    aestimate = aestimate,
    se_adj = se_adj,
    ci_adj = ci_adj
  )

  class(out) <- "linkinfer_adjusted"
  out
}

#' Print method for adjusted estimates
#' @export
print.linkinfer_adjusted <- function(x, ...) {
  cli::cli_h2("Adjusted Estimate")
  cli::cli_text("Population (true) estimate: {.val {round(x$pop_estimate, 4)}}")
  cli::cli_text("Full linked estimate (unadjusted): {.val {round(x$estimate, 4)}}")
  cli::cli_text("Subset with validation var: {.val {round(x$sestimate, 4)}}")
  cli::cli_text("True‑match subset estimate: {.val {round(x$testimate, 4)}}")
  cli::cli_text("Adjustment factor: {.val {round(x$adjustment_factor, 4)}}")
  cli::cli_text("Adjusted estimate: {.val {round(x$aestimate, 4)}}")
  cli::cli_text("Bootstrap SE: {.val {round(x$se_adj, 4)}}")
  cli::cli_text("{.val {round(100 * (1 - (x$ci_adj[2] - x$ci_adj[1]) / (2 * x$aestimate)), 1)}%} confidence interval: [{.val {round(x$ci_adj[1], 4)}}, {.val {round(x$ci_adj[2], 4)}}]")
  # No extra threshold‑based estimates are returned in the current API
  invisible(x)
}
