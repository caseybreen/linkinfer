#' Compare characteristics of linked sample vs. population
#'
#' Produces a comparison table showing means or proportions for each variable
#' in the full population and the linked subsample, along with standardized
#' differences. This is the first diagnostic step recommended by Breen and Joo
#' (2026) for evaluating the representativeness of a linked sample.
#'
#' @param population A data.frame of the full population.
#' @param linked A data.frame of the linked subsample.
#' @param vars A character vector of variable names to compare. Must be present
#'   in both `population` and `linked`.
#' @param weights Optional. A character string naming a column of sampling
#'   weights present in both data.frames.
#' @param categorical Optional. A character vector explicitly naming categorical
#'   variables. If `NULL`, types are auto-detected.
#' @param continuous Optional. A character vector explicitly naming continuous
#'   variables. If `NULL`, types are auto-detected.
#'
#' @return A data.frame of class `"linkinfer_comparison"` with columns:
#'   `variable`, `level` (NA for continuous), `pop_mean`, `linked_mean`,
#'   `pop_n`, `linked_n`, `std_diff`, `type`.
#'
#' @examples
#' data(census_population)
#' data(linked_sample)
#' compare_linked(
#'   population = census_population,
#'   linked = linked_sample,
#'   vars = c("age", "sex", "race", "education")
#' )
#'
#' @export
compare_linked <- function(population, linked, vars,
                           weights = NULL,
                           categorical = NULL,
                           continuous = NULL) {

  # Resolve weights
  w_pop <- NULL
  w_lnk <- NULL
  if (!is.null(weights)) {
    if (is.character(weights) && length(weights) == 1) {
      w_pop <- population[[weights]]
      w_lnk <- linked[[weights]]
    }
  }

  results <- list()

  for (v in vars) {
    if (!v %in% names(population)) {
      cli::cli_warn("Variable {.var {v}} not found in `population`, skipping.")
      next
    }
    if (!v %in% names(linked)) {
      cli::cli_warn("Variable {.var {v}} not found in `linked`, skipping.")
      next
    }

    x_pop <- population[[v]]
    x_lnk <- linked[[v]]

    # Determine type
    if (v %in% categorical) {
      vtype <- "categorical"
    } else if (v %in% continuous) {
      vtype <- "continuous"
    } else {
      vtype <- .detect_var_type(x_pop)
    }

    if (vtype == "continuous") {
      pop_mean <- .weighted_mean(x_pop, w_pop)
      lnk_mean <- .weighted_mean(x_lnk, w_lnk)
      pop_var <- .weighted_var(x_pop, w_pop)
      lnk_var <- .weighted_var(x_lnk, w_lnk)
      sd <- .standardized_diff(pop_mean, lnk_mean, pop_var, lnk_var)

      results[[length(results) + 1]] <- data.frame(
        variable = v,
        level = NA_character_,
        pop_mean = pop_mean,
        linked_mean = lnk_mean,
        pop_n = sum(!is.na(x_pop)),
        linked_n = sum(!is.na(x_lnk)),
        std_diff = sd,
        type = "continuous",
        stringsAsFactors = FALSE
      )
    } else {
      pop_props <- .weighted_prop_table(x_pop, w_pop)
      lnk_props <- .weighted_prop_table(x_lnk, w_lnk)

      all_levels <- union(names(pop_props), names(lnk_props))

      for (lvl in all_levels) {
        p_pop <- ifelse(lvl %in% names(pop_props), pop_props[lvl], 0)
        p_lnk <- ifelse(lvl %in% names(lnk_props), lnk_props[lvl], 0)

        var_pop <- p_pop * (1 - p_pop)
        var_lnk <- p_lnk * (1 - p_lnk)
        sd <- .standardized_diff(p_pop, p_lnk, var_pop, var_lnk)

        results[[length(results) + 1]] <- data.frame(
          variable = v,
          level = as.character(lvl),
          pop_mean = as.numeric(p_pop),
          linked_mean = as.numeric(p_lnk),
          pop_n = sum(!is.na(x_pop)),
          linked_n = sum(!is.na(x_lnk)),
          std_diff = sd,
          type = "categorical",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  class(out) <- c("linkinfer_comparison", "data.frame")
  out
}

#' @export
print.linkinfer_comparison <- function(x, ..., threshold = 0.1) {
  cli::cli_h2("Linked Sample vs. Population Comparison")
  n_vars <- length(unique(x$variable))
  cli::cli_text("{.val {n_vars}} variables compared")
  cli::cli_text("")

  flag <- abs(x$std_diff) > threshold
  if (any(flag)) {
    flagged_vars <- unique(x$variable[flag])
    cli::cli_alert_warning(
      "Variables with |std_diff| > {threshold}: {.val {flagged_vars}}"
    )
  } else {
    cli::cli_alert_success("All standardized differences within +/- {threshold}")
  }

  cli::cli_text("")
  print.data.frame(x, digits = 3, row.names = FALSE)
  invisible(x)
}
