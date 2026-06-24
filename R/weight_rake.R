#' Raking weights for linked samples (custom implementation)
#'
#' Adjusts the linked sample to match known population marginal distributions
#' using a simple iterative proportional fitting (raking) algorithm that does
#' not depend on external packages.
#'
#' @param linked_data A data.frame of the linked subsample.
#' @param population A data.frame of the full population.
#' @param vars A character vector of categorical variable names to be used for
#'   raking. Marginal distributions for these variables are computed from
#'   `population`.
#' @param weights Optional. A character string naming a column of starting
#'   weights in `linked_data` (or a numeric vector of weights). If `NULL`
#'   (default) all observations start with weight 1.
#' @param max_iter Maximum number of raking iterations (default 10).
#' @param tol Convergence tolerance for the maximum absolute change in the
#'   marginal distributions (default 1e-6).
#' @return A list of class `"linkinfer_rake"` with a single element:
#'   \describe{\item{weights}{Numeric vector of raking weights.}}.
#' @export
weight_rake <- function(linked_data, population, vars,
                        weights = NULL, max_iter = 10, tol = 1e-6) {

  # Resolve starting weights
  if (is.null(weights)) {
    w <- rep(1, nrow(linked_data))
  } else if (is.character(weights) && length(weights) == 1) {
    w <- linked_data[[weights]]
  } else if (is.numeric(weights)) {
    w <- weights
  } else {
    rlang::abort("`weights` must be NULL, a column name, or a numeric vector.")
  }

  # Pre-compute population marginal proportions for each variable
  pop_margins <- lapply(vars, function(v) {
    prop.table(table(population[[v]], useNA = "no"))
  })
  names(pop_margins) <- vars

  # Iterative proportional fitting
  for (iter in seq_len(max_iter)) {
    w_old <- w
    for (v in vars) {
      # Weighted distribution in linked sample for variable v
      tabs <- tapply(w, linked_data[[v]], sum, na.rm = TRUE)
      tabs <- tabs / sum(w)
      # Align levels with population margins
      pop_tab <- pop_margins[[v]]
      # Ensure both have same names (levels)
      all_levels <- union(names(pop_tab), names(tabs))
      # Compute adjustment factors (default 1 for missing levels)
      adj <- rep(1, length(all_levels))
      names(adj) <- all_levels
      adj[names(pop_tab)] <- pop_tab
      adj_factor <- adj[names(tabs)] / tabs
      # Apply factor to each row
      factor_vec <- adj_factor[as.character(linked_data[[v]])]
      w <- w * factor_vec
    }
    # Check convergence (max absolute change in weights)
    if (max(abs(w - w_old)) < tol) break
  }

  result <- list(weights = w)
  class(result) <- "linkinfer_rake"
  result
}

#' @export
print.linkinfer_rake <- function(x, ...) {
  cli::cli_h2("Raking Weights")
  w <- x$weights
  cli::cli_text("Records: {.val {length(w)}}")
  cli::cli_text("")
  cli::cli_text("Weight summary:")
  cli::cli_text("  Min: {.val {round(min(w), 3)}}  Median: {.val {round(stats::median(w), 3)}}  Max: {.val {round(max(w), 3)}}")
  cli::cli_text("  Effective N: {.val {round(.effective_n(w), 1)}}")
  invisible(x)
}
