#' Diagnostics for weighting adjustments (post‑stratification and raking)
#'
#' Provides balance statistics and weight summaries for weighting methods other than
#' inverse‑probability weighting. The interface mirrors `ipw_diagnostics()` but
#' works with any vector of adjustment weights.
#'
#' @param weights Numeric vector of adjustment weights for linked records.
#' @param population A data.frame of the full population (same variables as
#'   `linked`).
#' @param linked A data.frame of the linked subsample.
#' @param covariates Character vector of covariate names used to construct the
#'   weights. Must be present in both data frames.
#'
#' @return An object of class `"linkinfer_weight_diagnostics"` containing a
#'   balance table and a weight‑summary table.
#' @export
weight_diagnostics <- function(weights, population, linked, covariates, extra_covariates = NULL) {
  if (length(weights) != nrow(linked)) {
    rlang::abort("Length of `weights` must equal number of linked records.")
  }

  balance_rows <- list()
  # Combine primary covariates (used for weighting) with any extra covariates
  all_covs <- unique(c(covariates, extra_covariates))
  for (v in all_covs) {
    x_pop <- population[[v]]
    x_lnk <- linked[[v]]
    vtype <- .detect_var_type(x_pop)

    if (vtype == "continuous") {
      pop_mean <- .weighted_mean(x_pop)
      pop_var <- .weighted_var(x_pop)

      lnk_mean_uw <- .weighted_mean(x_lnk)
      lnk_var_uw <- .weighted_var(x_lnk)
      sd_uw <- .standardized_diff(pop_mean, lnk_mean_uw, pop_var, lnk_var_uw)

      lnk_mean_w <- .weighted_mean(x_lnk, weights)
      lnk_var_w <- .weighted_var(x_lnk, weights)
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
      lnk_props_w <- .weighted_prop_table(x_lnk, weights)

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

  weight_summary <- data.frame(
    min = min(weights),
    p5 = stats::quantile(weights, 0.05),
    p25 = stats::quantile(weights, 0.25),
    median = stats::median(weights),
    mean = mean(weights),
    p75 = stats::quantile(weights, 0.75),
    p95 = stats::quantile(weights, 0.95),
    max = max(weights),
    cv = stats::sd(weights) / mean(weights),
    effective_n = .effective_n(weights),
    design_effect = .design_effect(weights),
    row.names = NULL
  )

  out <- list(balance = balance, weight_summary = weight_summary)
  class(out) <- "linkinfer_weight_diagnostics"
  out
}

#' @export
print.linkinfer_weight_diagnostics <- function(x, ...) {
  cli::cli_h2("Weight Diagnostics")
  cli::cli_h3("Balance")
  print.data.frame(x$balance, digits = 3, row.names = FALSE)
  cli::cli_text("")
  cli::cli_h3("Weight Summary")
  ws <- x$weight_summary[1, ]
  cli::cli_text("  Effective N: {.val {round(ws$effective_n, 1)}}")
  cli::cli_text("  Design effect: {.val {round(ws$design_effect, 3)}}")
  cli::cli_text("  CV of weights: {.val {round(ws$cv, 3)}}")
  cli::cli_text(
    "  Range: [{.val {round(ws$min, 3)}}, {.val {round(ws$max, 3)}}]"
  )
  invisible(x)
}

#' Plot diagnostics for weight adjustments
#'
#' @param x An object of class `"linkinfer_weight_diagnostics"`.
#' @param type Currently only "balance" is supported.
#' @export
plot.linkinfer_weight_diagnostics <- function(x, type = c("balance"), ...) {
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
        title = "Covariate Balance: Unweighted vs. Weighted"
      ) +
      ggplot2::theme_minimal()
    print(p)
    invisible(p)
  }
}
