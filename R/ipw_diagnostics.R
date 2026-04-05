#' Diagnostics for inverse probability weights
#'
#' Computes balance statistics and weight summaries to assess IPW quality.
#'
#' @param ipw_result An object of class `"linkinfer_ipw"` from [weight_ipw()].
#' @param population A data.frame of the full population (same as passed to
#'   [weight_ipw()]).
#' @param linked A data.frame of the linked subsample (same as passed to
#'   [weight_ipw()]).
#'
#' @return A list of class `"linkinfer_ipw_diagnostics"` with elements:
#'   \describe{
#'     \item{balance}{A data.frame comparing unweighted and weighted standardized
#'       differences for each covariate.}
#'     \item{weight_summary}{A data.frame with weight distribution statistics.}
#'   }
#'
#' @export
ipw_diagnostics <- function(ipw_result, population, linked) {
  if (!inherits(ipw_result, "linkinfer_ipw")) {
    rlang::abort("`ipw_result` must be an object from `weight_ipw()`.")
  }

  covariates <- ipw_result$covariates
  w <- ipw_result$weights

  balance_rows <- list()
  for (v in covariates) {
    x_pop <- population[[v]]
    x_lnk <- linked[[v]]
    vtype <- .detect_var_type(x_pop)

    if (vtype == "continuous") {
      pop_mean <- .weighted_mean(x_pop)
      pop_var <- .weighted_var(x_pop)

      lnk_mean_uw <- .weighted_mean(x_lnk)
      lnk_var_uw <- .weighted_var(x_lnk)
      sd_uw <- .standardized_diff(pop_mean, lnk_mean_uw, pop_var, lnk_var_uw)

      lnk_mean_w <- .weighted_mean(x_lnk, w)
      lnk_var_w <- .weighted_var(x_lnk, w)
      sd_w <- .standardized_diff(pop_mean, lnk_mean_w, pop_var, lnk_var_w)

      balance_rows[[length(balance_rows) + 1]] <- data.frame(
        variable = v,
        level = NA_character_,
        pop_mean = pop_mean,
        linked_unweighted = lnk_mean_uw,
        linked_weighted = lnk_mean_w,
        std_diff_unweighted = sd_uw,
        std_diff_weighted = sd_w,
        stringsAsFactors = FALSE
      )
    } else {
      pop_props <- .weighted_prop_table(x_pop)
      lnk_props_uw <- .weighted_prop_table(x_lnk)
      lnk_props_w <- .weighted_prop_table(x_lnk, w)

      all_levels <- union(names(pop_props), names(lnk_props_uw))
      for (lvl in all_levels) {
        p_pop <- ifelse(lvl %in% names(pop_props), pop_props[lvl], 0)
        p_uw <- ifelse(lvl %in% names(lnk_props_uw), lnk_props_uw[lvl], 0)
        p_w <- ifelse(lvl %in% names(lnk_props_w), lnk_props_w[lvl], 0)

        var_pop <- p_pop * (1 - p_pop)
        var_uw <- p_uw * (1 - p_uw)
        var_w <- p_w * (1 - p_w)

        balance_rows[[length(balance_rows) + 1]] <- data.frame(
          variable = v,
          level = as.character(lvl),
          pop_mean = as.numeric(p_pop),
          linked_unweighted = as.numeric(p_uw),
          linked_weighted = as.numeric(p_w),
          std_diff_unweighted = .standardized_diff(p_pop, p_uw, var_pop, var_uw),
          std_diff_weighted = .standardized_diff(p_pop, p_w, var_pop, var_w),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  balance <- do.call(rbind, balance_rows)
  rownames(balance) <- NULL

  # Weight summary
  weight_summary <- data.frame(
    min = min(w),
    p5 = stats::quantile(w, 0.05),
    p25 = stats::quantile(w, 0.25),
    median = stats::median(w),
    mean = mean(w),
    p75 = stats::quantile(w, 0.75),
    p95 = stats::quantile(w, 0.95),
    max = max(w),
    cv = stats::sd(w) / mean(w),
    effective_n = .effective_n(w),
    design_effect = .design_effect(w),
    row.names = NULL
  )

  result <- list(
    balance = balance,
    weight_summary = weight_summary
  )
  class(result) <- "linkinfer_ipw_diagnostics"
  result
}

#' @export
print.linkinfer_ipw_diagnostics <- function(x, ...) {
  cli::cli_h2("IPW Diagnostics")

  cli::cli_h3("Balance")
  print.data.frame(x$balance, digits = 3, row.names = FALSE)

  cli::cli_text("")
  cli::cli_h3("Weight Summary")
  cli::cli_text("  Effective N: {.val {round(x$weight_summary$effective_n, 1)}}")
  cli::cli_text("  Design effect: {.val {round(x$weight_summary$design_effect, 3)}}")
  cli::cli_text("  CV of weights: {.val {round(x$weight_summary$cv, 3)}}")
  cli::cli_text("  Range: [{.val {round(x$weight_summary$min, 3)}}, {.val {round(x$weight_summary$max, 3)}}]")

  invisible(x)
}

#' Plot IPW diagnostics
#'
#' Produces a Love plot comparing standardized differences before and after
#' IPW adjustment.
#'
#' @param x An object of class `"linkinfer_ipw_diagnostics"`.
#' @param type Character. One of `"balance"` (default).
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot2 object (invisibly).
#' @export
plot.linkinfer_ipw_diagnostics <- function(x, type = c("balance"), ...) {
  rlang::check_installed("ggplot2", reason = "for diagnostic plots")
  type <- match.arg(type)

  if (type == "balance") {
    bal <- x$balance
    bal$label <- ifelse(
      is.na(bal$level),
      bal$variable,
      paste0(bal$variable, ": ", bal$level)
    )

    plot_data <- data.frame(
      label = rep(bal$label, 2),
      std_diff = c(bal$std_diff_unweighted, bal$std_diff_weighted),
      type = rep(c("Unweighted", "Weighted"), each = nrow(bal)),
      stringsAsFactors = FALSE
    )

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(
      x = .data$std_diff,
      y = stats::reorder(.data$label, abs(.data$std_diff)),
      color = .data$type,
      shape = .data$type
    )) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_vline(xintercept = c(-0.1, 0, 0.1), linetype = c(2, 1, 2)) +
      ggplot2::labs(
        x = "Standardized Difference",
        y = NULL,
        color = NULL,
        shape = NULL,
        title = "Covariate Balance: Unweighted vs. IPW-Weighted"
      ) +
      ggplot2::theme_minimal()

    print(p)
    return(invisible(p))
  }
}
