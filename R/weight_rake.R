#' Raking weights for linked samples
#'
#' Adjusts the linked sample to match known population marginal distributions
#' using iterative proportional fitting (raking) via the
#' [autumn](https://github.com/aaronrudkin/autumn) package.
#'
#' @param linked_data A data.frame of the linked subsample.
#' @param targets A named list of target marginal distributions, where each
#'   element is a named numeric vector (names = factor levels, values =
#'   proportions). Use [make_targets()] to generate this from a population
#'   data.frame.
#' @param weights Optional. A character string naming a column of starting
#'   weights in `linked_data`.
#' @param max_weight Numeric. Maximum weight ratio (passed to autumn's `cap`
#'   parameter). Default: 5.
#' @param ... Additional arguments passed to [autumn::harvest()].
#'
#' @return A list of class `"linkinfer_rake"` with elements:
#'   \describe{
#'     \item{weights}{Numeric vector of raking weights.}
#'     \item{harvest_result}{The full result from [autumn::harvest()].}
#'   }
#'
#' @export
weight_rake <- function(linked_data, targets, weights = NULL,
                        max_weight = 5, ...) {

  rlang::check_installed("autumn", reason = "for raking weights")

  # Prepare arguments for autumn::harvest
  harvest_args <- list(
    data = linked_data,
    target = targets,
    max_weight = max_weight,
    ...
  )

  # Add starting weights if provided
  if (!is.null(weights)) {
    if (is.character(weights) && length(weights) == 1) {
      harvest_args$start_weights <- linked_data[[weights]]
    } else if (is.numeric(weights)) {
      harvest_args$start_weights <- weights
    }
  }

  harvest_result <- tryCatch(
    do.call(autumn::harvest, harvest_args),
    error = function(e) {
      if (grepl("Weighting unnecessary", e$message)) {
        cli::cli_alert_info(
          "Raking unnecessary: linked sample already matches targets."
        )
        return(NULL)
      }
      rlang::abort(e$message)
    }
  )

  # Extract weights
  if (is.null(harvest_result)) {
    raked_weights <- rep(1, nrow(linked_data))
  } else {
    raked_weights <- harvest_result$weights
  }

  result <- list(
    weights = raked_weights,
    harvest_result = harvest_result
  )
  class(result) <- "linkinfer_rake"
  result
}

#' Compute population marginal targets for raking
#'
#' Convenience function that computes marginal proportions from a population
#' data.frame in the format expected by [autumn::harvest()].
#'
#' @param data A data.frame containing the full population.
#' @param vars A character vector of categorical variable names.
#' @param weights Optional. A character string naming a weight column, or a
#'   numeric vector.
#'
#' @return A named list suitable for the `targets` argument of [weight_rake()].
#'
#' @export
make_targets <- function(data, vars, weights = NULL) {

  w <- NULL
  if (!is.null(weights)) {
    if (is.character(weights) && length(weights) == 1) {
      w <- data[[weights]]
    } else if (is.numeric(weights)) {
      w <- weights
    }
  }

  targets <- list()
  for (v in vars) {
    x <- data[[v]]
    if (is.null(w)) {
      tbl <- table(x, useNA = "no")
      props <- prop.table(tbl)
    } else {
      lvls <- sort(unique(x[!is.na(x)]))
      counts <- vapply(lvls, function(l) sum(w[x == l & !is.na(x)]), numeric(1))
      props <- counts / sum(counts)
      names(props) <- lvls
    }
    targets[[v]] <- as.numeric(props)
    names(targets[[v]]) <- names(props)
  }

  targets
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
